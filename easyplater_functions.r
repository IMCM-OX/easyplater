# R: version 4.2.1

################################################################################
# easyplater algorithm variables
################################################################################
plate_size <- 96 # Can only be 96
plate_wells <- c("A1","B1","C1","D1","E1","F1","G1","H1",
                 "A2","B2","C2","D2","E2","F2","G2","H2",
                 "A3","B3","C3","D3","E3","F3","G3","H3",
                 "A4","B4","C4","D4","E4","F4","G4","H4",
                 "A5","B5","C5","D5","E5","F5","G5","H5",
                 "A6","B6","C6","D6","E6","F6","G6","H6",
                 "A7","B7","C7","D7","E7","F7","G7","H7",
                 "A8","B8","C8","D8","E8","F8","G8","H8",
                 "A9","B9","C9","D9","E9","F9","G9","H9",
                 "A10","B10","C10","D10","E10","F10","G10","H10",
                 "A11","B11","C11","D11","E11","F11","G11","H11",
                 "A12","B12","C12","D12","E12","F12","G12","H12")

plate_row_names <- c("A","B","C","D","E","F","G","H")
plate_col_names <- c(",1","2","3","4","5","6","7","8","9","10","11","12")

plate_num_rows <- 8
plate_num_cols <- 12
mask_edge_thresh <- 3

well_pair_distances_df <- make_well_distance_df(plate_size)
well_distances_matrix <- make_well_distances_matrix(plate_size)
full_mask <- make_full_mask(well_distances_matrix, mask_edge_thresh)
scoring_mask <- make_scoring_mask(well_distances_matrix)

splitting_ss_thresh <- 0.5 # ss = sample similarity
splitting_wd_thresh <- 1   # wd = well distance
replacing_ss_thresh <- 0.5
replacing_wd_thresh <- 6

initial_perms <- 20
################################################################################

################################################################################
# Functions
################################################################################

# Need to add input checks for every function (have only started adding this to
# comments at top of functions; most still need thinking about)
# Also need to add error handling

################################################################################

get_and_format_plate_df_from_manifest <- function(manifest_df, plateID, columns_for_scoring, cols_to_categorize, imbalance_fixer, 
						  plate_size, plate_wells, internal_control_well_indices, internal_control_ids){

  # There are so many assumptions here that need to be checked! (Off the top of my head... Not an exhaustive list...):
  # Columns to categorize must be numeric
  # columns_for_scoring only has column names that either already exist in manifest_df or will be made and added to manifest_df here
  # "SampleID" is always a column name in manifest_df
  # There are no samples with an ID "Empty"*, or one of the internal control label names (unlikely!, but still worth checking for.)
  # Internal controls have been assigned wells *but these are not given in the manifest* ***AND*** their positions are FIXED! ... Our randomization process won't 
  # move them! *** MIGHT WANT TO MAKE THIS AN OPTION IN FUTURE (is this even possible?) ***

  cols_for_analysis <- c("SampleID", columns_for_scoring)
  
  plate_df_aux <- filter(manifest_df, plate == plateID)
  num_internal_controls <- length(internal_control_well_indices)
  real_plate_size <- plate_size - num_internal_controls

  if(length(cols_to_categorize)>0){ #cols_to_categorize is list of tuples with structure (col_name,num_cats,na_replacement,categorized_col_name)
    for(col_to_categorize_tuple in cols_to_categorize){
      
      col_name <- col_to_categorize_tuple[1]
      num_cats <- as.numeric(col_to_categorize_tuple[2])
      
      cut_interval_has_worked = FALSE
      while((!cut_interval_has_worked) & num_cats > 0){
      tryCatch(
        {
          categorized_col_vec <- as.numeric(cut_interval(as.matrix(plate_df_aux[,col_name]),num_cats))
          cut_interval_has_worked <- TRUE
          #print(paste0("Final num_cats: ", num_cats))
          
        }, error = function(msg){
        }, warning = function(msg){
        })
        num_cats <- num_cats - 1
      }
      if(!cut_interval_has_worked){
        categorized_col_vec <- plate_df_aux[,col_name]
      }
      
      if(length(col_to_categorize_tuple)==4){
        na_replacement <- col_to_categorize_tuple[3]
        categorized_col_name <- col_to_categorize_tuple[4]
        replace(categorized_col_vec, is.na(categorized_col_vec), na_replacement)
      }else{
        categorized_col_name <- col_to_categorize_tuple[3]
      }

      plate_df_aux[paste(categorized_col_name)] <- categorized_col_vec
      #print(categorized_col_name)
    }
  }

  plate_df <- select(plate_df_aux, cols_for_analysis)
  plate_df_aux <- select(plate_df_aux, c(cols_for_analysis, c("plate","column","row","well")))

  # Now account for imbalance in samples, as required
  imbalanceFix_vec <- c()
  if(imbalance_fixer[[1]]){
    plate_df_column_for_fixing <- select(plate_df,imbalance_fixer[[2]])
    imbalanceFix_vec <- rep(1,length(plate_df$SampleID))
    for(imbalance_val in imbalance_fixer[[3]]){
      imbalanceFix_vec <- replace(imbalanceFix_vec,which(plate_df_column_for_fixing == imbalance_val),unlist(plate_df[which(plate_df_column_for_fixing == imbalance_val),'SampleID']))
    }
    plate_df <- cbind(plate_df, imbalanceFix_vec)
  }

  # Find out how many samples there are on the plate.
  num_samples_on_plate <- length(plate_df$SampleID)

  # If num_samples_on_plate < real_plate_size, then we need to add missing samples to the end of plate_df

  if(num_samples_on_plate < real_plate_size){

    sample_wells <- plate_df_aux[["well"]]
    available_wells <- plate_wells[which(!(plate_wells %in% c(plate_wells[internal_control_well_indices+1], sample_wells)))]

    for(ni in (num_samples_on_plate+1):real_plate_size){
      row<-c(paste("Empty",ni,sep="_"),rep(NA,length(cols_for_analysis)-1))
      if(imbalance_fixer[[1]]){
        row<- c(row,"NA")
      }
      plate_df <- rbind(plate_df,row)
 
      assigned_well <- available_wells[ni - num_samples_on_plate]
      assigned_column <- get_column_from_well_coords(assigned_well)
      assigned_row <- get_row_from_well_coords(assigned_well)
      row_aux <- c(paste("Empty",ni,sep="_"),rep(NA,length(cols_for_analysis)-1),c(plateID,assigned_column, assigned_row, assigned_well))
      plate_df_aux <- rbind(plate_df_aux, row_aux)

    }
  }

  # Now add internal controls to plate_df and plate_df_aux

  for(ici in 1:num_internal_controls){
    row<-c(internal_control_ids[ici],rep(NA,length(cols_for_analysis)-1))
    if(imbalance_fixer[[1]]){
      row<- c(row,"NA")
    }
    plate_df <- rbind(plate_df,row)
 
    assigned_well <- plate_wells[internal_control_well_indices[ici]+1]
    assigned_column <- get_column_from_well_coords(assigned_well)
    assigned_row <- get_row_from_well_coords(assigned_well)
    row_aux <- c(internal_control_ids[ici], rep(NA,length(cols_for_analysis)-1),c(plateID,assigned_column, assigned_row, assigned_well))
    plate_df_aux <- rbind(plate_df_aux, row_aux)

  }


  # Finally, sort plate_df and plate_df_aux by well index

  #well_indices <- unlist(lapply(plate_df_aux$well), function(w) which(plate_wells==w))

  well_indices <- match(plate_df_aux$well, plate_wells)-1

  plate_df$well_indices <- well_indices
  plate_df <- plate_df %>% arrange(well_indices)
  plate_df <- subset(plate_df, select = -c(well_indices))  

  plate_df_aux$well_indices <- well_indices
  plate_df_aux <- plate_df_aux %>% arrange(well_indices)
  plate_df_aux <- subset(plate_df_aux, select = -c(well_indices))

  return(list(plate_df, plate_df_aux))
}


#*******************************************************************************
make_ss_matrices <- function(plate_df, column_weights, imbalance_fixer, plate_size){

  weights <- c(0, column_weights)
  
  if(imbalance_fixer[[1]]){
    weights <- c(weights,as.numeric(imbalance_fixer[[4]]))
  }
  sample_similarities_matrix <- matrix(0, nrow=plate_size, ncol=plate_size)
  rownames(sample_similarities_matrix) <- plate_df$SampleID
  colnames(sample_similarities_matrix) <- plate_df$SampleID

  sample_similarities_si_names <- matrix("",nrow=plate_size, ncol=plate_size)
  sample_similarities_sj_names <- matrix("",nrow=plate_size, ncol=plate_size)

  for (si in 1:(plate_size-1)){
    sample_si <- plate_df[si,]

    for (sj in (si+1):plate_size){
      sample_sj <- plate_df[sj,]

      ss = sum(weights * (sample_si == sample_sj), na.rm=TRUE)/sum(weights)
      sample_similarities_matrix[si,sj] <- ss
      sample_similarities_matrix[sj,si] <- ss

      sample_similarities_si_names[sj,si] <- sample_si$SampleID
      sample_similarities_sj_names[sj,si] <- sample_sj$SampleID

    }
  }
  return(list(sample_similarities_matrix, sample_similarities_si_names, sample_similarities_sj_names))
}

#*******************************************************************************
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
  sub_graph <- graph_from_adjacency_matrix(sub_mask, mode="max", diag=FALSE)
  largest_clique_size <- clique_num(sub_graph)

  #while((n_wells_still_to_find >= largest_clique_size) & (largest_clique_size >= 2 )){

  while((n_wells_still_to_find > 0) & (largest_clique_size <= n_wells_still_to_find)){

     largest_cliques_in_sub_graph <- largest_cliques(sub_graph)
     randomly_chosen_well_clique <- as.numeric(as_ids(largest_cliques_in_sub_graph[[sample(length(largest_cliques_in_sub_graph))[1]]]))

     n_wells_still_to_find <- n_wells_still_to_find - length(randomly_chosen_well_clique)
     wells_allocated <- c(wells_allocated, randomly_chosen_well_clique)

     sub_mask <- full_mask[(!as.numeric(rownames(full_mask)) %in% wells_allocated), (!as.numeric(colnames(full_mask)) %in% wells_allocated)]
     if(length(sub_mask)==1){
       sub_mask <- matrix(sub_mask)
       rownames(sub_mask)<- rownames(full_mask)[!as.numeric(rownames(full_mask)) %in% wells_allocated]
       colnames(sub_mask)<- rownames(full_mask)[!as.numeric(rownames(full_mask)) %in% wells_allocated]
     }

     sub_graph <- graph_from_adjacency_matrix(sub_mask, mode="max", diag=FALSE)
     largest_clique_size <- clique_num(sub_graph)
  }

  if(n_wells_still_to_find > 0){
    # We can only be here if largest_clique_size > n_wells_still_to_find because we have exited the while loop, above, and we know the 
    # n_wells_still_to_find > 0 holds.

    max_cliques_in_sub_graph_found_with_minsize <- max_cliques(sub_graph, min=n_wells_still_to_find)
    randomly_chosen_well_clique_aux <- as_ids(max_cliques_in_sub_graph_found_with_minsize[[sample(length(max_cliques_in_sub_graph_found_with_minsize))[1]]])
    
    if(length(randomly_chosen_well_clique_aux)== 1){
       randomly_chosen_well_clique <- c(strtoi(randomly_chosen_well_clique_aux))
    }else{
      last_mask <- full_mask[randomly_chosen_well_clique_aux,randomly_chosen_well_clique_aux]
      last_graph <- graph_from_adjacency_matrix(last_mask, mode="max", diag=FALSE)
      last_cliques <- cliques(last_graph, min=n_wells_still_to_find, max=n_wells_still_to_find)
      randomly_chosen_well_clique <- as.numeric(as_ids(last_cliques[[sample(length(last_cliques))[1]]]))
    }
    wells_allocated <- c(wells_allocated, randomly_chosen_well_clique)
  }

  wells_allocated <- wells_allocated[!(wells_allocated %in% wells_pre_allocated)]
  return(wells_allocated)

  ####
  #### copy for later:
  #### rev_k <- rev(k)
  #### rev_k[which(rev(sizes(k))>1)]
}

#*******************************************************************************
find_sample_communities <- function(sample_similarities_matrix, splitting_ss_thresh){
  sample_similarities_matrix_mask <- sample_similarities_matrix
  sample_similarities_matrix_mask[which(is.na(sample_similarities_matrix))] <- 0
  sample_similarities_matrix_mask[which(sample_similarities_matrix <= splitting_ss_thresh)] <- 0
  sample_similarities_matrix_mask[which(sample_similarities_matrix > splitting_ss_thresh)] <- 1

  sample_similarities_graph <- graph_from_adjacency_matrix(sample_similarities_matrix_mask, mode="max", diag=FALSE)

  sample_communities <- cluster_edge_betweenness(sample_similarities_graph)
  return(sample_communities)
}

#*******************************************************************************
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
  				     # Using sample here to randomize well allocation order. By definition we know that > 1 wells have been allocated, so this is safe from odd
				     # behaviour from sample.

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
  

  singleton_samples <- unlist(sample_communities[which(sizes(sample_communities)==1)])
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

#*******************************************************************************
calc_sas_edge_max <- function(n, nrow, ncol){

  edge_max_temp <- (n*(n-1))/2
  row_penalty <- ((n %/% nrow) * (n %% nrow)) + (nrow * (( ((n %/% nrow) - 1) * (n %/% nrow) )/2) )
  col_penalty <- ((n %/% ncol) * (n %% ncol)) + (ncol * (( ((n %/% ncol) - 1) * (n %/% ncol) )/2) )

  edge_max <- edge_max_temp - row_penalty - col_penalty
  return(edge_max)
}

calc_sas_edge_min <- function(n, nrow, ncol){


  max_dim <- max(c(nrow,ncol))

  min_dim <- n %/% max_dim
  edge_min_temp <- ((min_dim * max_dim) * ((min_dim-1)*(max_dim-1)))/2

  remainder <- n %% max_dim
  remainder_edges <- remainder * (min_dim * (max_dim - 1))

  edge_min <- edge_min_temp + remainder_edges

  return(edge_min)
}

#*******************************************************************************
calc_sas_edge_range <- function(n, nrow, ncol){
  edge_max <- calc_sas_edge_max(n, nrow, ncol)
  edge_min <- calc_sas_edge_min(n, nrow, ncol)
  return(edge_max - edge_min)
}

#*******************************************************************************
calc_pds_global <- function(plate_df, columns_for_scoring, column_weights, 
                            mask, plate_n_rows, plate_n_cols, 
                            internal_control_well_indices)
{
  # This bit of code assumes that internal controls are pre-placed in the last column.
  # Later, will add an if statement so that this only runs when internal controls are in a fixed position...
  # Note that this will remove one assumption, but the assumption that fixed internal controls are in right-most column(s)
  # of plate will remain... The upshot is that in situations when this is not the case the score is an approximation and,
  # moreover, we need to bound the score between 0 and 1 for the cases where the approximation is outside of this boundary.
  # One last, albeit less important, assumption is that we will always think of the smaller dimension of a plate as being its rows, and the
  # larger dimension will be its columns... This just aligns with the usual "8x12" implied layout of 96 well plates, which
  # (for the moment) is a primary assumption of this library.
 
  num_samples <- (plate_n_rows * plate_n_cols) - length(internal_control_well_indices)
  min_dim <- min(c(plate_n_rows, plate_n_cols))
  max_dim_floor <- num_samples %/% min_dim

  score <- 0
  cwi <- 1
  for(cs in columns_for_scoring){
    column_data <- pull(plate_df, cs)
    column_values_aux <- na.omit(unique(column_data))
    column_values <- c()
    for(cv in column_values_aux){
       if(length(which(column_data==cv)) > 1){
         column_values <- c(column_values, cv)
       }
    }
    column_weight <- column_weights[cwi]

    val <- column_values[1]
    val <- column_values[2]

    sub_score <- column_weight * median(unlist(lapply(column_values, 
                                                      function(val) { max(min(((sum(lowerTriangle(mask[ which(column_data==val), which(column_data==val)])) - 
								                 calc_sas_edge_min(sum(na.omit(column_data==val)), min_dim, max_dim_floor))/ 
                                                                                calc_sas_edge_range(sum(na.omit(column_data==val)), min_dim, max_dim_floor)),1),0) } )))

    score <- sum(c(score, sub_score), na.rm=TRUE)

    cwi <- cwi + 1
  }
  return(score)
}

#*******************************************************************************
calc_row_column_score <- function(plate_df, columns_for_scoring, column_weights, plate_n_rows, plate_n_cols)
{

  score <- 0
  cwi <- 1
  for(cs in columns_for_scoring){

    column_data <- unlist(as.list(select(plate_df, cs)))
    column_weight <- column_weights[cwi]
    column_as_plate <- matrix(column_data, nrow=plate_n_rows, ncol=plate_n_cols)

    column_penalty <- sum(apply(column_as_plate,1,function(r) length(unique(r[which(!is.na(r))]))==1 & length(which(!is.na(r)))>(0.7 * plate_n_cols)  )) 
                      + sum(apply(column_as_plate,2,function(c) length(unique(c[which(!is.na(c))]))==1 & length(which(!is.na(c)))>(0.7 * plate_n_rows)  ))
    column_score <- column_weight* ((plate_n_rows + plate_n_cols) - column_penalty)
 
    score <- score + column_score
    cwi <- cwi + 1
  }

  return(score)
}

#*******************************************************************************
calc_patch_score <- function(plate_df, columns_for_scoring, column_weights, plate_n_rows, plate_n_cols, patch_weight=NULL)
{
  # setup *******************************************************************
  sub_plate_n_cols <- plate_n_cols - 2  
  sub_plate_n_rows <- plate_n_rows - 2  
  sub_plate_size <- sub_plate_n_cols * sub_plate_n_rows

  if(is.null(patch_weight)){
     patch_weight <- min(c(1, (plate_n_rows + plate_n_cols)/sub_plate_size))
  }

  x <- matrix(1:(plate_n_rows * plate_n_cols), plate_n_rows, plate_n_cols)
  x[,1] <- 0
  x[1,] <- 0
  x[,plate_n_cols] <- 0
  x[plate_n_rows,] <- 0

  y <- x[which(x>0)]

  n <- plate_n_rows # this assignment just makes the next line of code neater...
  mask <- c( (-n-1), (-n), (-n+1), -1, 0 , 1, (n-1), (n), (n+1) )
  # *************************************************************************

  score <- 0
  cwi <- 1
  for(cs in columns_for_scoring){

    column_data <- unlist(as.list(select(plate_df, cs)))
    column_weight <- column_weights[cwi]
    column_as_plate <- matrix(column_data, nrow=plate_n_rows, ncol=plate_n_cols)
    
    
    column_as_patches <- matrix(unlist(lapply(y, function(a) lapply(mask, function(b) column_as_plate[b + a]))),9,sub_plate_size)    
    column_penalty <- sum(apply(column_as_patches, 2, function(c) length(unique(c[which(!is.na(c))]))==1 & length(which(!is.na(c)))>(0.7 * 9)  ))
    column_score <- patch_weight * (column_weight * ( sub_plate_size - column_penalty ))
 
    score <- score + column_score
    cwi <- cwi + 1
  }

  return(score)
}


#***************************************************************************
calc_pds_local <- function(plate_df, columns_for_scoring, column_weights, 
                           plate_n_rows, plate_n_cols, 
                           patch_weight=NULL){
  
  row_column_score <- calc_row_column_score(plate_df, columns_for_scoring, column_weights, plate_n_rows, plate_n_cols)
  patch_score <- calc_patch_score(plate_df, columns_for_scoring, column_weights, plate_n_rows, plate_n_cols, patch_weight)

  return(row_column_score + patch_score)
}

#***************************************************************************
calc_pds <- function(plate_df, columns_for_scoring, column_weights, mask, 
                                                    plate_n_rows, plate_n_cols, 
                                                    internal_control_well_indices, 
                                                    pds_local_weight=1, patch_weight=NULL){
  
  
  
  pds_global <- calc_pds_global(plate_df, columns_for_scoring, column_weights, 
                                mask, plate_n_rows, plate_n_cols, 
                                internal_control_well_indices)

  pds_local <- calc_pds_local(plate_df, columns_for_scoring, column_weights, plate_n_rows, plate_n_cols, patch_weight)

  return(pds_global + (pds_local_weight*pds_local))
}

#***************************************************************************
find_independent_switches_using_pds <- function(depth, plate_df,
			                                          ss_matrix,well_distances,jiggled_matrix_indices,
                                                splitting_ss_thresh, splitting_wd_thresh,
                                                replacing_ss_thresh, replacing_wd_thresh,
                                                columns_for_scoring, column_weights, plate_n_rows, plate_n_cols,
                                                plate_size, internal_control_well_indices, sample_communities, mask,
			                                          pds_local_weight=1, patch_weight=NULL){

  internal_control_well_inidices_one_indexed <- internal_control_well_indices + 1

  ss_matrix_jiggled <- ss_matrix[jiggled_matrix_indices, jiggled_matrix_indices] 

  ss_vec=lowerTriangle(ss_matrix_jiggled)


  pairs_for_splitting_indicator_vec_aux <- rep(0,length(ss_vec))
  pairs_for_splitting_indicator_vec_aux[which((ss_vec>splitting_ss_thresh) & (well_distances<splitting_wd_thresh))] <- 1
  pairs_for_splitting_inidicator_matrix <- matrix(0, nrow=plate_size, ncol=plate_size)
  lowerTriangle(pairs_for_splitting_inidicator_matrix) <- pairs_for_splitting_indicator_vec_aux
  upperTriangle(pairs_for_splitting_inidicator_matrix) <- upperTriangle(t(pairs_for_splitting_inidicator_matrix))
  pairs_for_splitting <- sample(which(pairs_for_splitting_inidicator_matrix==1))

  pairs_available_for_switching_indicator_vec_aux <- rep(0,length(ss_vec))
  pairs_available_for_switching_indicator_vec_aux[which((ss_vec<=replacing_ss_thresh) & (well_distances>replacing_wd_thresh))] <- 1
  pairs_available_for_switching_inidicator_matrix <- matrix(0, nrow=plate_size, ncol=plate_size)
  lowerTriangle(pairs_available_for_switching_inidicator_matrix) <- pairs_available_for_switching_indicator_vec_aux
  upperTriangle(pairs_available_for_switching_inidicator_matrix) <- upperTriangle(t(pairs_available_for_switching_inidicator_matrix))
  #pairs_for_switching <- sample(which(pairs_available_for_switching_inidicator_matrix==1))

  switches_df <- data.frame(matrix(ncol = 2, nrow = 0))
  
  moved_samples <- c()


  for(pair_for_splitting in pairs_for_splitting){
    anchor_sample <- ((pair_for_splitting-1) %% plate_size) + 1
    moveable_sample <- ((pair_for_splitting-1) %/% plate_size) + 1

    if(!(moveable_sample %in% internal_control_well_inidices_one_indexed) & !(anchor_sample %in% moved_samples) & !(moveable_sample %in% moved_samples)){
      
       SANITY_CHECK_PRINTING = FALSE
       
       if(SANITY_CHECK_PRINTING){
         print("TRYING TO FIND SOME MOVABLE SAMPLE PAIRS")
         print(anchor_sample)
         print(moveable_sample)
       }

       moveable_sample_2_options_vec <- pairs_available_for_switching_inidicator_matrix[anchor_sample,]

       if(SANITY_CHECK_PRINTING){
         print("moveable_sample_2_options_vec before filtering:")
         print(moveable_sample_2_options_vec)
         print("****")
         print("Filters:")
         print("moveable_sample")
         print(moveable_sample)
         print("moved_samples")
         print(moved_samples)
         print("internal_control_well_inidices_one_indexed")
         print(internal_control_well_inidices_one_indexed)
       }
       
       moveable_sample_2_options_vec[moveable_sample] <- 0  ## SHOULD ALREADY BE ZERO,I think (splitting pair cannot be switching pair if thresholds set properly)... but just in case
       moveable_sample_2_options_vec[moved_samples] <- 0
       moveable_sample_2_options_vec[internal_control_well_inidices_one_indexed] <- 0
       
       if(SANITY_CHECK_PRINTING){
         print("moveable_sample_2_options_vec after filtering:")
         print(moveable_sample_2_options_vec)
         print("***************")
       }       

       if(any(moveable_sample_2_options_vec>0)){
          if(length(which(moveable_sample_2_options_vec>0))==1){
            moveable_sample_2 <- which(moveable_sample_2_options_vec>0)
          }else{
            moveable_sample_2 <- sample(which(moveable_sample_2_options_vec>0))[1]
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
                                 mask, 
                                 plate_n_rows, plate_n_cols, 
                                 internal_control_well_indices,
                                 pds_local_weight, patch_weight)
  
  #return(data.frame(depth=depth, total_score=sas_score + plating_score, jiggled_matrix_indices=I(list(new_jiggled_matrix_indices))))
  return(data.frame(depth=depth, pds=plate_design_score, jiggled_matrix_indices=I(list(new_jiggled_matrix_indices))))
}


#**********************************************************
get_column_from_well_coords <- function(well_coords){
  return(paste("Column", substr(well_coords,2,3), sep=" "))
}

#**********************************************************
get_row_from_well_coords <- function(well_coords){
  return(substr(well_coords,1,1))
}

#**********************************************************
get_column_as_zero_indexed_index_from_well_coords <- function(well_coords){
  return(as.numeric(substr(well_coords,2,3))-1)
}

#**********************************************************
get_row_as_zero_indexed_index_from_well_coords <- function(well_coords){
  row_labels <- c("A","B","C","D","E","F","G","H")
  return(which(row_labels == substr(well_coords,1,1))-1)
}

#***********************************************************
make_chessboard_matrix_and_count_edges <- function(nrow, ncol, set_zero, set_one, zero_start){

  n <- ncol

  if(zero_start){
    x <-  matrix(as.numeric(!(seq(1:n^2) %% 2)), nrow = n)[1:nrow,1:ncol]
  }else{
    x <-  matrix(seq(1:n^2) %% 2, nrow = n)[1:nrow,1:ncol]
  }
  x[set_zero] <- 0
  x[set_one] <- 1

  #print(x)
  #print(sum(x))

  
  x_with_totals <- matrix(0,nrow+1,ncol+1)
  x_with_totals[1:nrow,1:ncol] <- x
  x_with_totals[nrow+1,] <- c(colSums(x),0)
  x_with_totals[,ncol+1] <- c(rowSums(x),0)
  #print(x_with_totals)

  one_indices <- which(x==1) 

  d <- matrix(0, nrow*ncol, nrow*ncol)

  for(i in one_indices){
    d[i, one_indices] <- 1
    d[one_indices, i] <- 1
  }


  num_edges <- sum((lowerTriangle(d)))

  # ***********
  t <- sum(x)
  edge_max <- (t*(t-1))/2
  row_penalty <- ((t %/% nrow) * (t %% nrow)) + (nrow * (( ((t %/% nrow) - 1) * (t %/% nrow) )/2) )
  col_penalty <- ((t %/% ncol) * (t %% ncol)) + (ncol * (( ((t %/% ncol) - 1) * (t %/% ncol) )/2) )


  calc_max_edges <- edge_max - row_penalty - col_penalty 
  # ***********

  return(c(num_edges, calc_max_edges))
}

#***********************************************************
allocate_similar_samples_to_distal_wells <- function(plate_df_list, columns_for_scoring, column_weights, imbalance_fixer,
                                                     plate_num_rows, plate_num_cols, plate_size,
                                                     full_mask, splitting_ss_thresh, 
                                                     internal_control_ids, internal_control_well_indices,
                                                     pds_local_weight=1, patch_weight=NULL){
  
  plate_df <- plate_df_list[[1]]
  plate_df_aux <- plate_df_list[[2]]
  
  ss_matrices_list <- make_ss_matrices(plate_df, column_weights, imbalance_fixer, plate_size)
  
  sample_similarities_matrix <- ss_matrices_list[[1]]
  sample_similarities_si_names <- ss_matrices_list[[2]]
  sample_similarities_sj_names <- ss_matrices_list[[3]]
  
  sample_communities <- find_sample_communities(sample_similarities_matrix, splitting_ss_thresh)
  
  best_score <- calc_pds(plate_df,columns_for_scoring, column_weights,                       
                         scoring_mask,
                         plate_num_rows, plate_num_cols,internal_control_well_indices,
                         pds_local_weight, patch_weight)
  
  samples_reordered <- plate_df$SampleID
  sample_similarities_matrix_reordered <- sample_similarities_matrix
  
  for(s in 1:initial_perms) {
    
    samples_reordered_list_starter <- reorder_samples_in_plate(sample_similarities_matrix, sample_communities, full_mask,
                                                               internal_control_ids, internal_control_well_indices, plate_size)
    
    samples_reordered_starter <- samples_reordered_list_starter[[1]]
    sample_similarities_matrix_reordered_starter <- samples_reordered_list_starter[[2]]
    
    
    temp_plate_df <- plate_df  %>% slice(match(rownames(sample_similarities_matrix_reordered_starter), SampleID))
    
    starter_pds_score <- calc_pds(temp_plate_df,columns_for_scoring, column_weights,
                                  scoring_mask,
                                  plate_num_rows, plate_num_cols,internal_control_well_indices,
                                  pds_local_weight, patch_weight)
    
    if(starter_pds_score > best_score){ 
      samples_reordered <- samples_reordered_starter
      sample_similarities_matrix_reordered <- sample_similarities_matrix_reordered_starter
      best_score <- starter_pds_score
    }
  }  

  return(list(sample_communities, samples_reordered, sample_similarities_matrix_reordered, best_score))
}


#***********************************************************
perform_sample_switch_search <- function(max_depth, wins_required, max_attempts,
                                         plate_df_list, sample_allocation_outputs, 
                                         well_pair_distances_df,
                                         splitting_ss_thresh, splitting_wd_thresh,
                                         replacing_ss_thresh, replacing_wd_thresh,
                                         columns_for_scoring, column_weights, plate_num_rows, plate_num_cols,
                                         plate_size, internal_control_well_indices,scoring_mask,
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

#***********************************************************
make_easyplater_design_aux <- function(plate_df_list, samples_final_order, columns_for_scoring){
  
  plate_df_aux <- plate_df_list[[2]]
  
  # Need to reset rows and columns (otherwise nothing changes!)
  plate_rows <- plate_df_aux$row
  plate_columns <- plate_df_aux$column
  plate_wells <- plate_df_aux$well
  
   
  plate_design_df <- plate_df_aux  %>% slice(match(samples_final_order, SampleID))  
  # # Need to reset rows and columns (otherwise nothing changes!)
  # plate_design_df$PseudoSubjectID <- unlist(lapply(unlist(lapply(plate_design_df$SampleID, function(x) {substr(x,1,max(unlist(gregexpr("_",x)))-1)})),function(y){which(unique(unlist(lapply(plate_design_df$SampleID, function(x) {substr(x,1,max(unlist(gregexpr("_",x)))-1)})))==y)}))
  # randomizedPSID_aux <- sample(unique(plate_design_df$PseudoSubjectID))
  # plate_design_df$randomizedPSID <- unlist(lapply(plate_design_df$PseudoSubjectID, function(x) which(randomizedPSID_aux==x)))
  
  plate_design_df$row <- plate_rows
  plate_design_df$column <- plate_columns
  plate_design_df$well <- plate_wells
  
  return(plate_design_df)
}

#***********************************************************
make_easyplater_design <- function(manifest_df, p, 
                                   columns_for_scoring, column_weights, cols_to_categorize, imbalance_fixer, 
                                   plate_num_rows, plate_num_cols, plate_size, plate_wells, 
                                   internal_control_well_indices, internal_control_ids,
                                   full_mask, scoring_mask,
                                   well_pair_distances_df,
                                   splitting_ss_thresh, splitting_wd_thresh,
                                   replacing_ss_thresh, replacing_wd_thresh,
                                   max_depth, wins_required, max_attempts,
                                   pds_local_weight=1, patch_weight=NULL){
    
  print("Getting and formatting plate data fram from manifest.")
  plate_df_list <- get_and_format_plate_df_from_manifest(manifest_df, p, columns_for_scoring, cols_to_categorize, imbalance_fixer, 
                                                         plate_size, plate_wells, internal_control_well_indices, internal_control_ids)
  
  # if(is.null(patch_weight)){
  #   sub_plate_num_cols <- plate_num_cols - 2  
  #   sub_plate_num_rows <- plate_num_rows - 2  
  #   sub_plate_size <- sub_plate_num_cols * sub_plate_num_rows
  #   patch_weight <- min(c(1, (plate_num_rows + plate_num_cols)/sub_plate_size))
  # }
  
  print("Allocating similar samples to distal wells.")
  sample_allocation_outputs <- allocate_similar_samples_to_distal_wells(
    plate_df_list, columns_for_scoring, column_weights, imbalance_fixer,
    plate_num_rows, plate_num_cols, plate_size,
    full_mask, splitting_ss_thresh,
    internal_control_ids, internal_control_well_indices,
    pds_local_weight, patch_weight)
  
  print("Performing sample switching search.")
  samples_final_order <- perform_sample_switch_search(max_depth, wins_required, max_attempts,
                                                      plate_df_list, sample_allocation_outputs, 
                                                      well_pair_distances_df,
                                                      splitting_ss_thresh, splitting_wd_thresh,
                                                      replacing_ss_thresh, replacing_wd_thresh,
                                                      columns_for_scoring, column_weights, plate_num_rows, plate_num_cols,
                                                      plate_size, internal_control_well_indices,scoring_mask,
                                                      pds_local_weight, patch_weight)
  
  print("Store the easyPlateR plate design in a data frame.")
  easy_plate_df <- make_easyplater_design_aux(plate_df_list, samples_final_order, columns_for_scoring)
}

#***********************************************************
print_easyplater_manifest <- function(easy_plate_df, colnames, filepath){
  write.csv(easy_plate_df[,c("SampleID","plate","column","row","well", colnames)],
            filepath,
            row.names=FALSE,quote=FALSE)
}

#***********************************************************
print_easyplater_design <- function(easy_plate_df, filepath){
  plate_design <- lapply(plate_row_names, function(x) {easy_plate_df[easy_plate_df$row==x,]$SampleID })
  
  
  write.table(do.call("rbind",plate_design),
              filepath,
              row.names=plate_row_names, col.names=plate_col_names,quote=FALSE,sep=",")
}

#***********************************************************
print_easyplater_submissionform <- function(easy_plate_df, colnames, filepath){
  easy_plate_df_for_submission_temp <- easy_plate_df[1:86,c("well","SampleID","plate",colnames)]
  
  easy_plate_df_for_submission_temp['Volume provided'] <- NA
  easy_plate_df_for_submission_temp['Sample/Well Location'] <- easy_plate_df_for_submission_temp$well
  easy_plate_df_for_submission_temp['Sample/Name'] <- easy_plate_df_for_submission_temp$SampleID
  easy_plate_df_for_submission_temp['PlateName'] <- easy_plate_df_for_submission_temp$plate
  
  easy_plate_df_for_submission <- easy_plate_df_for_submission_temp[,c("Sample/Well Location","Sample/Name","PlateName","Volume provided",colnames)]
  
  write.csv(easy_plate_df_for_submission,
            filepath,
            row.names=FALSE,quote=FALSE)
}

#***********************************************************
print_easyplater_log <- function(filepath){
  writeLines(c(paste("pseed =", pseed),
               paste("initial_perms =", initial_perms)),
             filepath)
}

#***********************************************************


