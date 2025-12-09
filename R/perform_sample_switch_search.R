
perform_sample_switch_search <- function(max_depth, wins_required, max_attempts,
                                         plate_df_list, sample_allocation_outputs,
                                         well_pair_distances_df,
                                         splitting_ss_thresh, splitting_wd_thresh,
                                         replacing_ss_thresh, replacing_wd_thresh,
                                         columns_for_scoring, column_weights, plate_num_rows, plate_num_cols,
                                         plate_size, internal_control_well_indices, scoring_mask,
                                         pds_local_weight=1, patch_weight=NULL){



  sample_communities <- sample_allocation_outputs[[1]]
  samples_reordered <- sample_allocation_outputs[[2]]
  sample_similarities_matrix_reordered <- sample_allocation_outputs[[3]]
  best_score <- sample_allocation_outputs[[4]]

  plate_df <- plate_df_list[[1]]
  plate_df_aux <- plate_df_list[[2]]

  jiggled_indices_df <- data.frame(depth=0, pds=best_score, jiggled_matrix_indices=I(list(1:plate_size)))

  num_leaves_at_last_depth <- 1
  current_depth <- 1

  while((current_depth <= max_depth) & (num_leaves_at_last_depth > 0)){

    jiggled_indices_subset_df <- jiggled_indices_df[which(jiggled_indices_df$depth == (current_depth-1)),]

    for(ri in 1:nrow(jiggled_indices_subset_df)) {
      num_wins <- 0
      num_attempts <- 0

      while( (num_wins<wins_required) & (num_attempts<max_attempts) ){

        jiggled_indices_aux_df <- find_independent_switches_using_pds(current_depth,plate_df,
                                                                      sample_similarities_matrix_reordered,
                                                                      well_pair_distances_df$d,
                                                                      unlist(jiggled_indices_subset_df[ri,]$jiggled_matrix_indices),
                                                                      splitting_ss_thresh, splitting_wd_thresh,
                                                                      replacing_ss_thresh, replacing_wd_thresh,
                                                                      columns_for_scoring, column_weights, plate_num_rows, plate_num_cols,
                                                                      plate_size, internal_control_well_indices,
                                                                      sample_communities, scoring_mask,
                                                                      pds_local_weight, patch_weight)

        if(jiggled_indices_aux_df[1,]$pds > jiggled_indices_subset_df[ri,]$pds){
          num_wins <- num_wins + 1
          jiggled_indices_df <- rbind(jiggled_indices_df, jiggled_indices_aux_df)
        }

        num_attempts <- num_attempts + 1
      }
    }

    num_leaves_at_last_depth <- nrow( jiggled_indices_df[which(jiggled_indices_df$depth == current_depth),] )
    current_depth <- current_depth + 1
  }

  top_switch_df <- head(jiggled_indices_df[which(jiggled_indices_df$pds == max(jiggled_indices_df$pds)),], n=1)
  samples_final_order  <- samples_reordered[unlist(top_switch_df[1,]$jiggled_matrix_indices)]
  return(samples_final_order)
}


#' Find independent switches using PDS
#'
#' This function is run within perform_sample_switch_search(). **TO DO: Avi elaborate**
#'
#' @param depth **TO DO: Avi explain**
#' @param plate_df Data frame of samples and associated clinical metadata variables.
#' @param ss_matrix Sample similarity matrix output by make_ss_matrices()[[3]]
#' @param well_distances Output of make_well_distance_df()
#' @param jiggled_matrix_indices **TO DO: Avi explain**
#' @param splitting_ss_thresh Numeric scalar. Similarity threshold for generating adjacency matrix.
#' @param splitting_wd_thresh **TO DO: Avi explain**
#' @param replacing_ss_thresh **TO DO: Avi explain**
#' @param replacing_wd_thresh **TO DO: Avi explain**
#' @param sample_communities **TO DO: Avi explain**
#' @inheritParams allocate_similar_samples_to_distal_wells
#'
#' @returns
#' **TO DO: Avi explain**
#'
find_independent_switches_using_pds <- function(depth, plate_df,
                                                ss_matrix, well_distances,jiggled_matrix_indices,
                                                splitting_ss_thresh, splitting_wd_thresh,
                                                replacing_ss_thresh, replacing_wd_thresh,
                                                columns_for_scoring, column_weights, plate_num_rows, plate_num_cols,
                                                plate_size, internal_control_well_indices, sample_communities, scoring_mask,
                                                pds_local_weight=1, patch_weight = NULL){

  internal_control_well_inidices_one_indexed <- internal_control_well_indices + 1

  ss_matrix_jiggled <- ss_matrix[jiggled_matrix_indices, jiggled_matrix_indices]

  ss_vec=lowerTriangle(ss_matrix_jiggled)

  pairs_for_splitting_indicator_vec_aux <- rep(0,length(ss_vec))
  pairs_for_splitting_indicator_vec_aux[which((ss_vec > splitting_ss_thresh) & (well_distances < splitting_wd_thresh))] <- 1
  pairs_for_splitting_inidicator_matrix <- matrix(0, nrow = plate_size, ncol=plate_size)
  lowerTriangle(pairs_for_splitting_inidicator_matrix) <- pairs_for_splitting_indicator_vec_aux
  upperTriangle(pairs_for_splitting_inidicator_matrix) <- upperTriangle(t(pairs_for_splitting_inidicator_matrix))
  pairs_for_splitting <- sample(which(pairs_for_splitting_inidicator_matrix==1))

  pairs_available_for_switching_indicator_vec_aux <- rep(0, length(ss_vec))
  pairs_available_for_switching_indicator_vec_aux[which((ss_vec <= replacing_ss_thresh) & (well_distances>replacing_wd_thresh))] <- 1
  pairs_available_for_switching_inidicator_matrix <- matrix(0, nrow=plate_size, ncol=plate_size)
  lowerTriangle(pairs_available_for_switching_inidicator_matrix) <- pairs_available_for_switching_indicator_vec_aux
  upperTriangle(pairs_available_for_switching_inidicator_matrix) <- upperTriangle(t(pairs_available_for_switching_inidicator_matrix))
  #pairs_for_switching <- sample(which(pairs_available_for_switching_inidicator_matrix==1))

  switches_df <- data.frame(matrix(ncol = 2, nrow = 0))

  moved_samples <- c()

  for (pair_for_splitting in pairs_for_splitting){
    anchor_sample <- ((pair_for_splitting - 1) %% plate_size) + 1
    moveable_sample <- ((pair_for_splitting - 1) %/% plate_size) + 1

    if (!(moveable_sample %in% internal_control_well_inidices_one_indexed) & !(anchor_sample %in% moved_samples) & !(moveable_sample %in% moved_samples)){

      # SANITY_CHECK_PRINTING = FALSE
      #
      # if(SANITY_CHECK_PRINTING){
      #   print("TRYING TO FIND SOME MOVABLE SAMPLE PAIRS")
      #   print(anchor_sample)
      #   print(moveable_sample)
      # }

      moveable_sample_2_options_vec <- pairs_available_for_switching_inidicator_matrix[anchor_sample,]

      # if(SANITY_CHECK_PRINTING){
      #   print("moveable_sample_2_options_vec before filtering:")
      #   print(moveable_sample_2_options_vec)
      #   print("****")
      #   print("Filters:")
      #   print("moveable_sample")
      #   print(moveable_sample)
      #   print("moved_samples")
      #   print(moved_samples)
      #   print("internal_control_well_inidices_one_indexed")
      #   print(internal_control_well_inidices_one_indexed)
      # }

      moveable_sample_2_options_vec[moveable_sample] <- 0  ## SHOULD ALREADY BE ZERO,I think (splitting pair cannot be switching pair if thresholds set properly)... but just in case
      moveable_sample_2_options_vec[moved_samples] <- 0
      moveable_sample_2_options_vec[internal_control_well_inidices_one_indexed] <- 0

      # if(SANITY_CHECK_PRINTING){
      #   print("moveable_sample_2_options_vec after filtering:")
      #   print(moveable_sample_2_options_vec)
      #   print("***************")
      # }

      if (any(moveable_sample_2_options_vec > 0)){
        if (length(which(moveable_sample_2_options_vec>0)) == 1){
          moveable_sample_2 <- which(moveable_sample_2_options_vec > 0)
        } else {
          moveable_sample_2 <- sample(which(moveable_sample_2_options_vec > 0))[1]
        }

        switches_df <- rbind(switches_df, sort(c(moveable_sample, moveable_sample_2)))
        moved_samples <- c(moved_samples,  sort(c(moveable_sample, moveable_sample_2)))
      }
    }
  }

  switches_df <- unique(switches_df)
  colnames(switches_df) <- c("A","B")

  new_jiggled_matrix_indices <- jiggled_matrix_indices
  new_jiggled_matrix_indices[switches_df$A] <- jiggled_matrix_indices[switches_df$B]
  new_jiggled_matrix_indices[switches_df$B] <- jiggled_matrix_indices[switches_df$A]

  ss_matrix_rejiggled <- ss_matrix[new_jiggled_matrix_indices, new_jiggled_matrix_indices]

  temp_plate_df <- plate_df  %>% slice(match(rownames(ss_matrix_rejiggled), SampleID))

  plate_design_score <- calc_pds(temp_plate_df, columns_for_scoring, column_weights,
                                 scoring_mask,
                                 plate_num_rows, plate_num_cols,
                                 internal_control_well_indices,
                                 pds_local_weight, patch_weight)

  #return(data.frame(depth=depth, total_score=sas_score + plating_score, jiggled_matrix_indices=I(list(new_jiggled_matrix_indices))))
  return(data.frame(depth=depth, pds=plate_design_score, jiggled_matrix_indices=I(list(new_jiggled_matrix_indices))))
}
