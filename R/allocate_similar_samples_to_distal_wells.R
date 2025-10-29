# allocate_similar_samples_to_distal_wells <- function(plate_df_list, columns_for_scoring, column_weights, imbalance_fixer,
#                                                      plate_num_rows, plate_num_cols, plate_size,
#                                                      full_mask, splitting_ss_thresh,
#                                                      internal_control_ids, internal_control_well_indices,
#                                                      pds_local_weight=1, patch_weight=NULL){
#
#   plate_df <- plate_df_list[[1]]
#   plate_df_aux <- plate_df_list[[2]]
#
#   ss_matrices_list <- make_ss_matrices(plate_df, column_weights, imbalance_fixer, plate_size)
#
#   sample_similarities_matrix <- ss_matrices_list[[1]]
#   sample_similarities_si_names <- ss_matrices_list[[2]]
#   sample_similarities_sj_names <- ss_matrices_list[[3]]
#
#   sample_communities <- find_sample_communities(sample_similarities_matrix, splitting_ss_thresh)
#
#   best_score <- calc_pds(plate_df,columns_for_scoring, column_weights,
#                          scoring_mask,
#                          plate_num_rows, plate_num_cols,internal_control_well_indices,
#                          pds_local_weight, patch_weight)
#
#   samples_reordered <- plate_df$SampleID
#   sample_similarities_matrix_reordered <- sample_similarities_matrix
#
#   for(s in 1:initial_perms) {
#
#     samples_reordered_list_starter <- reorder_samples_in_plate(sample_similarities_matrix, sample_communities, full_mask,
#                                                                internal_control_ids, internal_control_well_indices, plate_size)
#
#     samples_reordered_starter <- samples_reordered_list_starter[[1]]
#     sample_similarities_matrix_reordered_starter <- samples_reordered_list_starter[[2]]
#
#
#     temp_plate_df <- plate_df  %>% slice(match(rownames(sample_similarities_matrix_reordered_starter), SampleID))
#
#     starter_pds_score <- calc_pds(temp_plate_df,columns_for_scoring, column_weights,
#                                   scoring_mask,
#                                   plate_num_rows, plate_num_cols,internal_control_well_indices,
#                                   pds_local_weight, patch_weight)
#
#     if(starter_pds_score > best_score){
#       samples_reordered <- samples_reordered_starter
#       sample_similarities_matrix_reordered <- sample_similarities_matrix_reordered_starter
#       best_score <- starter_pds_score
#     }
#   }
#
#   return(list(sample_communities, samples_reordered, sample_similarities_matrix_reordered, best_score))
# }
#
# make_ss_matrices <- function(plate_df, column_weights, imbalance_fixer, plate_size){
#
#   weights <- c(0, column_weights)
#
#   if(imbalance_fixer[[1]]){
#     weights <- c(weights,as.numeric(imbalance_fixer[[4]]))
#   }
#   sample_similarities_matrix <- matrix(0, nrow=plate_size, ncol=plate_size)
#   rownames(sample_similarities_matrix) <- plate_df$SampleID
#   colnames(sample_similarities_matrix) <- plate_df$SampleID
#
#   sample_similarities_si_names <- matrix("",nrow=plate_size, ncol=plate_size)
#   sample_similarities_sj_names <- matrix("",nrow=plate_size, ncol=plate_size)
#
#   for (si in 1:(plate_size-1)){
#     sample_si <- plate_df[si,]
#
#     for (sj in (si+1):plate_size){
#       sample_sj <- plate_df[sj,]
#
#       ss = sum(weights * (sample_si == sample_sj), na.rm=TRUE)/sum(weights)
#       sample_similarities_matrix[si,sj] <- ss
#       sample_similarities_matrix[sj,si] <- ss
#
#       sample_similarities_si_names[sj,si] <- sample_si$SampleID
#       sample_similarities_sj_names[sj,si] <- sample_sj$SampleID
#
#     }
#   }
#   return(list(sample_similarities_matrix, sample_similarities_si_names, sample_similarities_sj_names))
# }
#
# find_sample_communities <- function(sample_similarities_matrix, splitting_ss_thresh){
#   sample_similarities_matrix_mask <- sample_similarities_matrix
#   sample_similarities_matrix_mask[which(is.na(sample_similarities_matrix))] <- 0
#   sample_similarities_matrix_mask[which(sample_similarities_matrix <= splitting_ss_thresh)] <- 0
#   sample_similarities_matrix_mask[which(sample_similarities_matrix > splitting_ss_thresh)] <- 1
#
#   sample_similarities_graph <- graph_from_adjacency_matrix(sample_similarities_matrix_mask, mode="max", diag=FALSE)
#
#   sample_communities <- cluster_edge_betweenness(sample_similarities_graph)
#   return(sample_communities)
# }


#' Reorder samples in plate
#'
#' @param sample_similarities_matrix
#' @param sample_communities
#' @param full_mask
#' @param internal_control_ids
#' @param internal_control_well_indices
#' @param plate_size
#'
#' @returns
#' @export
#'
reorder_samples_in_plate <- function(sample_similarities_matrix, sample_communities, full_mask,
                                     internal_control_ids, internal_control_well_indices, plate_size=96){

  samples_allocated <- internal_control_ids
  wells_allocated <- internal_control_well_indices

  community_sizes <- sizes(sample_communities)
  for(community_size in unique(community_sizes[which(community_sizes>1)])){

    sample_communities_subset <- sample_communities[which(community_sizes==community_size)]

    for(sample_community_index in sample(length(sample_communities_subset))){
      sample_community <- sample_communities_subset[[sample_community_index]]

      wells_allocated_to_community <- sample(find_n_wells(full_mask, length(sample_community), wells_allocated, plate_size))
      # Using sample here to randomize well allocation order. By definition we know that > 1 wells have been allocated, so this is safe from odd behaviour from sample.

      samples_allocated <- c(samples_allocated, sample_community)
      wells_allocated <- c(wells_allocated, wells_allocated_to_community)

      if(length(sample_community) != length(wells_allocated_to_community)){

        print("Found potential problem. Sample community not allocated the correct number of maps.")
        print(sample_community)
        print(wells_allocated_to_community)
        stop()
      }
    }
  }

  # Now allocate singleton samples that are left over

  singleton_samples <- unlist(sample_communities[igraph::sizes(sample_communities)==1])
  singleton_samples <- singleton_samples[which(!(singleton_samples %in% internal_control_ids))]

  if(length(singleton_samples)>1){
    # Need to randomize the order of the singleton samples if there is more than one.
    singleton_samples <- sample(singleton_samples)
  }

  samples_allocated <- c(samples_allocated, singleton_samples)
  wells_allocated <- c(wells_allocated, (0:(plate_size-1))[ !(0:(plate_size-1) %in% wells_allocated)])

  ### Finish writing code so that singlton samples are also allocated a well. Then return results as in reorder_samples, but also return community information
  ### (I feel like there was something else, too??) so that new scoring method based on sample community allocation combined with well allocation can also be calculated in future.

  sample_well_allocation_df <- data.frame(sample=samples_allocated,well=wells_allocated)
  sample_well_allocation_df_sorted <- sample_well_allocation_df %>% arrange(well)

  samples_reordered <- sample_well_allocation_df_sorted$sample
  sample_similarities_matrix_reordered <- sample_similarities_matrix[samples_reordered, samples_reordered]

  return(list(samples_reordered, sample_similarities_matrix_reordered))
}


#' Find wells to reallocate to distal wells
#'
#' @description
#' **TO DO: Avi, fill this in.**
#'
#' @param n_wells Integer scalar. Number of wells to search for.
#' @param wells_pre_allocated Numeric vector. Indices of wells that are excluded from allocation.
#' @param plate_size Numeric scalar. Size of plate. Note that currently `easyplater` is currently only implemented for 96-well plates.
#'
#' @returns Numeric vector with length `n_wells`
#' @export
#'
#' @examples
#' plate_size <- 96
#' full_mask <- plate_size |>
#'   make_well_distances_matrix() |>
#'   make_full_mask()
#' find_n_wells(full_mask, 17L, 86:95, plate_size)
find_n_wells <- function(full_mask, n_wells, wells_pre_allocated, plate_size=96){

  if((length(wells_pre_allocated) + n_wells) > plate_size){
    stop("Cannot allocate more wells on plate than there are available.")
  }

  n_wells_still_to_find <- n_wells
  wells_allocated <- wells_pre_allocated

  sub_mask <- full_mask[rownames(full_mask)[(!as.numeric(rownames(full_mask)) %in% wells_allocated)], rownames(full_mask)[(!as.numeric(colnames(full_mask)) %in% wells_allocated)]]
  if(length(sub_mask)==1){
    sub_mask <- matrix(sub_mask)
    rownames(sub_mask)<- rownames(full_mask)[!as.numeric(rownames(full_mask)) %in% wells_allocated]
    colnames(sub_mask)<- rownames(full_mask)[!as.numeric(rownames(full_mask)) %in% wells_allocated]
  }
  sub_graph <- igraph::graph_from_adjacency_matrix(sub_mask, mode="max", diag=FALSE)
  largest_clique_size <- igraph::clique_num(sub_graph)

  while((n_wells_still_to_find > 0) & (largest_clique_size <= n_wells_still_to_find)){

    largest_cliques_in_sub_graph <- igraph::largest_cliques(sub_graph)
    randomly_chosen_well_clique <- as.numeric(igraph::as_ids(largest_cliques_in_sub_graph[[sample(length(largest_cliques_in_sub_graph))[1]]]))

    n_wells_still_to_find <- n_wells_still_to_find - length(randomly_chosen_well_clique)
    wells_allocated <- c(wells_allocated, randomly_chosen_well_clique)

    sub_mask <- full_mask[(!as.numeric(rownames(full_mask)) %in% wells_allocated), (!as.numeric(colnames(full_mask)) %in% wells_allocated)]
    if(length(sub_mask)==1){
      sub_mask <- matrix(sub_mask)
      rownames(sub_mask)<- rownames(full_mask)[!as.numeric(rownames(full_mask)) %in% wells_allocated]
      colnames(sub_mask)<- rownames(full_mask)[!as.numeric(rownames(full_mask)) %in% wells_allocated]
    }

    sub_graph <- igraph::graph_from_adjacency_matrix(sub_mask, mode="max", diag=FALSE)
    largest_clique_size <- igraph::clique_num(sub_graph)
  }

  if(n_wells_still_to_find > 0){
    # We can only be here if largest_clique_size > n_wells_still_to_find because we have exited the while loop, above, and we know the
    # n_wells_still_to_find > 0 holds.

    max_cliques_in_sub_graph_found_with_minsize <- igraph::max_cliques(sub_graph, min=n_wells_still_to_find)
    randomly_chosen_well_clique_aux <- igraph::as_ids(max_cliques_in_sub_graph_found_with_minsize[[sample(length(max_cliques_in_sub_graph_found_with_minsize))[1]]])

    if(length(randomly_chosen_well_clique_aux)== 1){
      randomly_chosen_well_clique <- c(strtoi(randomly_chosen_well_clique_aux))
    }else{
      last_mask <- full_mask[randomly_chosen_well_clique_aux,randomly_chosen_well_clique_aux]
      last_graph <- igraph::graph_from_adjacency_matrix(last_mask, mode="max", diag=FALSE)
      last_cliques <- igraph::cliques(last_graph, min=n_wells_still_to_find, max=n_wells_still_to_find)
      randomly_chosen_well_clique <- as.numeric(igraph::as_ids(last_cliques[[sample(length(last_cliques))[1]]]))
    }
    wells_allocated <- c(wells_allocated, randomly_chosen_well_clique)
  }

  wells_allocated <- wells_allocated[!(wells_allocated %in% wells_pre_allocated)]
  return(wells_allocated)
}
