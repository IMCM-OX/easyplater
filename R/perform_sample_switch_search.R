#' Perform sample switch search
#'
#' @description
#' This function implements the method described in <a href='https://arxiv.org/abs/2512.17988'>\[1\]</a>; section 2.2, steps 3.
#'
#' @references
#' \[1\] Taylor A. & Fletcher MP.
#' easyplater: The easy way to generate microplate designs deconvolved from multivariate clinical data.
#' *arXiv* 2026. doi: \url{https://arxiv.org/abs/2512.17988}
#'
#' @inheritParams make_easyplater_design
#' @inheritParams allocate_similar_samples_to_distal_wells
#' @param sample_allocation_outputs Output of allocate_similar_samples_to_distal_wells()
#' @param plate_num_rows Numeric scalar. Default: 8.
#' @param plate_num_cols Numeric scalar. Default: 12.
#'
#' @returns
#' Character vector of SampleIDs in their final order after searching for an improved plate design using sample switches.
#'
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

  top_switch_df <- utils::head(jiggled_indices_df[which(jiggled_indices_df$pds == max(jiggled_indices_df$pds)),], n=1)
  samples_final_order  <- samples_reordered[unlist(top_switch_df[1,]$jiggled_matrix_indices)]
  return(samples_final_order)
}


#' Find independent switches using PDS
#'
#' @description
#' This function implements the method described in <a href='https://arxiv.org/abs/2512.17988'>\[1\]</a>; section 2.2, step 3, subsection "Generate a set of independent sample switches". Additionally, the function performs the switches and scores the resultant plate design using PDS. See also Supplementary Figure 4.
#'
#' @references
#' \[1\] Taylor A. & Fletcher MP.
#' easyplater: The easy way to generate microplate designs deconvolved from multivariate clinical data.
#' *arXiv* 2026. doi: \url{https://arxiv.org/abs/2512.17988}
#'
#' @param depth Integer. Depth of sample switching search reached so far. See <a href='https://arxiv.org/abs/2512.17988'>\[1\]</a>; section 2.2, steps 3, variables \eqn{D_n}.
#' @param plate_df Data frame of samples and associated clinical metadata variables.
#' @param ss_matrix (plate size) x (plate size) numeric matrix. Sample similarity matrix output as element 3 of the list output by allocate_similar_samples_to_distal_wells().
#' @param well_distances Column "d" of data frame output by make_well_distance_df().
#' @param jiggled_matrix_indices List of integers. Matrix indices are for re-ordering ss_matrix and represent the best scoring plate found so far in the sample switching step. See <a href='https://arxiv.org/abs/2512.17988'>\[1\]</a>; section 2.2, steps 3.
#' @param sample_communities **TO DO: Avi REMOVE this parameter - it is not used in this function.**
#' @inheritParams perform_sample_switch_search
#' @inheritParams allocate_similar_samples_to_distal_wells
#'
#' @returns
#' Data frame. One row containing the depth, PDS, and re-ordered (jiggled) indices for ss_matrix for the sample switching step performed.
#'
find_independent_switches_using_pds <- function(depth, plate_df,
                                                ss_matrix, well_distances, jiggled_matrix_indices,
                                                splitting_ss_thresh, splitting_wd_thresh,
                                                replacing_ss_thresh, replacing_wd_thresh,
                                                columns_for_scoring, column_weights, plate_num_rows, plate_num_cols,
                                                plate_size, internal_control_well_indices, sample_communities, scoring_mask,
                                                pds_local_weight=1, patch_weight = NULL){

  internal_control_well_inidices_one_indexed <- internal_control_well_indices + 1

  ss_matrix_jiggled <- ss_matrix[jiggled_matrix_indices, jiggled_matrix_indices]

  ss_vec=gdata::lowerTriangle(ss_matrix_jiggled)

  pairs_for_splitting_indicator_vec_aux <- rep(0,length(ss_vec))
  pairs_for_splitting_indicator_vec_aux[which((ss_vec > splitting_ss_thresh) & (well_distances < splitting_wd_thresh))] <- 1
  pairs_for_splitting_inidicator_matrix <- matrix(0, nrow = plate_size, ncol=plate_size)
  gdata::lowerTriangle(pairs_for_splitting_inidicator_matrix) <- pairs_for_splitting_indicator_vec_aux
  gdata::upperTriangle(pairs_for_splitting_inidicator_matrix) <- gdata::upperTriangle(t(pairs_for_splitting_inidicator_matrix))
  pairs_for_splitting <- sample(which(pairs_for_splitting_inidicator_matrix==1))

  pairs_available_for_switching_indicator_vec_aux <- rep(0, length(ss_vec))
  pairs_available_for_switching_indicator_vec_aux[which((ss_vec <= replacing_ss_thresh) & (well_distances>replacing_wd_thresh))] <- 1
  pairs_available_for_switching_inidicator_matrix <- matrix(0, nrow = plate_size, ncol=plate_size)
  gdata::lowerTriangle(pairs_available_for_switching_inidicator_matrix) <- pairs_available_for_switching_indicator_vec_aux
  gdata::upperTriangle(pairs_available_for_switching_inidicator_matrix) <- gdata::upperTriangle(t(pairs_available_for_switching_inidicator_matrix))
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

  temp_plate_df <- plate_df |> dplyr::slice(match(rownames(ss_matrix_rejiggled), .data$SampleID))

  plate_design_score <- calc_pds(temp_plate_df, columns_for_scoring, column_weights,
                                 scoring_mask,
                                 plate_num_rows, plate_num_cols,
                                 internal_control_well_indices,
                                 pds_local_weight, patch_weight)

  #return(data.frame(depth=depth, total_score=sas_score + plating_score, jiggled_matrix_indices=I(list(new_jiggled_matrix_indices))))
  return(data.frame(depth=depth, pds=plate_design_score, jiggled_matrix_indices=I(list(new_jiggled_matrix_indices))))
}
