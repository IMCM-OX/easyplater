# R: version 4.2.1

# Load packages required
library(OlinkAnalyze)
library(readxl)
library(writexl)
library(tidyverse)
library("dplyr")
library("ggpubr")
library('gdata')
library(igraph)
library(networkD3)

################################################################################
# Functions
################################################################################

# Need to add input checks for every function (have only started adding this to
# comments at top of functions; most still need thinking about)
# Also need to add error handling

################################################################################

make_well_distance_df <- function(plate_size){

  well_distances_df = data.frame()

  for (i in 0:(plate_size-2)){
    for (j in (i+1):(plate_size-1)){
      irow <- i %% 8
      icol <- i %/% 8
      jrow <- j %% 8
      jcol <- j %/% 8

      if((irow==jrow) || (icol==jcol)){
        d <- 0
      }else{
        d <- sqrt((irow-jrow)^2 + (icol-jcol)^2)
      }
      row <- c(i,j,d)
      well_distances_df <- rbind(well_distances_df,row)
    }
  }
  names(well_distances_df) <- c("i","j","d")
  return(well_distances_df)
}

#*******************************************************************************

make_well_distances_matrix <- function(plate_size, plate_n_rows=8){

  well_distances_matrix <- matrix(0, nrow=plate_size, ncol=plate_size)
  rownames(well_distances_matrix) <- 0:(plate_size-1)
  colnames(well_distances_matrix) <- 0:(plate_size-1)

  for (i in 0:(plate_size-2)){
    for (j in (i+1):(plate_size-1)){
      irow <- i %% plate_n_rows
      icol <- i %/% plate_n_rows
      jrow <- j %% plate_n_rows
      jcol <- j %/% plate_n_rows

      if((irow==jrow) || (icol==jcol)){
        d <- 0
      }else{
	d <- sqrt((irow-jrow)^2 + (icol-jcol)^2)
      }
      well_distances_matrix[as.character(i), as.character(j)] <- d 
      well_distances_matrix[as.character(j), as.character(i)] <- d 
    }
  }
  return(well_distances_matrix)
}

#*******************************************************************************

shuffle_well_pairs_within_distance_groups <- function(well_distances_df, close_wells_thresh){
  well_distances_sorted_df <- well_distances_df %>% arrange(desc(d))
  well_distances_shuffled_df <- well_distances_sorted_df

  distant_wells_sorted_df <- well_distances_sorted_df[which(well_distances_sorted_df$d > close_wells_thresh),]
  distant_wells_shuffled_df <- distant_wells_sorted_df[sample(1:nrow(distant_wells_sorted_df)), ]
  well_distances_shuffled_df[which(well_distances_shuffled_df$d > close_wells_thresh),] <- distant_wells_shuffled_df

  close_wells_sorted_df <- well_distances_sorted_df[which(well_distances_sorted_df$d <= close_wells_thresh),]
  close_wells_shuffled_df <- close_wells_sorted_df[sample(1:nrow(close_wells_sorted_df)), ]
  well_distances_shuffled_df[which(well_distances_shuffled_df$d <= close_wells_thresh),] <- close_wells_shuffled_df

  return(well_distances_shuffled_df)
}

#*******************************************************************************

get_and_format_plate_df_from_manifest <- function(manifest_df, plateID, cols_for_analysis, cols_to_categorize, imbalance_fixer, 
						  plate_size, plate_wells, internal_control_well_indices, internal_control_ids){

  # There are so many assumptions here that need to be checked! (Off the top of my head... Not an exhaustive list...):
  # Columns to categorize must be numeric
  # cols_for_analysis only has column names that either already exist or will be made here
  # "SampleID" is always in cols_for_analysis, and is always listed first
  # There are no samples with an ID "Empty"*, or one of the internal control label names (unlikely!, but still worth checking for.)
  # Internal controls have been assigned wells *but these are not given in the manifest* ***AND*** their positions are FIXED! ... Our randomization process won't 
  # move them! *** MIGHT WANT TO MAKE THIS AN OPTION IN FUTURE (is this even possible?) ***


  plate_df_aux <- filter(manifest_df, plate == plateID)
  num_internal_controls <- length(internal_control_well_indices)
  real_plate_size <- plate_size - num_internal_controls

  if(length(cols_to_categorize)>0){ #cols_to_categorize is list of tuples with structure (col_name,num_cats,na_replacement,categorized_col_name)
    for(col_to_categorize_tuple in cols_to_categorize){
      
      col_name <- col_to_categorize_tuple[1]
      num_cats <- as.numeric(col_to_categorize_tuple[2])
      
      cut_number_has_worked = FALSE
      while((!cut_number_has_worked) & num_cats > 0){
      tryCatch(
        {
          categorized_col_vec <- as.numeric(cut_number(as.matrix(plate_df_aux[,col_name]),num_cats))
          cut_number_has_worked <- TRUE
          print(paste0("Final num_cats: ", num_cats))
          
        }, error = function(msg){
        }, warning = function(msg){
        })
        num_cats <- num_cats - 1
      }
      if(!cut_number_has_worked){
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
      print(categorized_col_name)
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

make_ss_matrices <- function(plate_df, weights, imbalance_fixer, plate_size){

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

      print(sample_si)
      print(sample_sj)
      print(weights)
      ss = sum(weights * (sample_si == sample_sj), na.rm=TRUE)/sum(weights)
      print(ss)
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

    last_mask <- full_mask[randomly_chosen_well_clique_aux,randomly_chosen_well_clique_aux]
    last_graph <- graph_from_adjacency_matrix(last_mask, mode="max", diag=FALSE) 
    last_cliques <- cliques(last_graph, min=n_wells_still_to_find, max=n_wells_still_to_find)
    randomly_chosen_well_clique <- as.numeric(as_ids(last_cliques[[sample(length(last_cliques))[1]]]))
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
make_full_mask <- function(well_distances_matrix, mask_edge_thresh=3){
  if(mask_edge_thresh < 3){
    stop("mask_edge_thresh in make_full_mask cannot be < 3.")
  }

  full_mask <- well_distances_matrix
  full_mask[which(well_distances_matrix<=mask_edge_thresh)] <- 0
  full_mask[which(well_distances_matrix>mask_edge_thresh)] <- 1

  return(full_mask)
}

#*******************************************************************************
make_scoring_mask <- function(well_distances_matrix, scoring_mask_edge_thresh=1){

  scoring_mask <- well_distances_matrix
  scoring_mask[which(well_distances_matrix<=scoring_mask_edge_thresh)] <- 0
  scoring_mask[which(well_distances_matrix>scoring_mask_edge_thresh)] <- 1

  return(scoring_mask)
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

calc_spatial_auto_score <- function(sample_reordering, sample_communities, mask){

   sample_communities_non_singleton <- sample_communities[which(sizes(sample_communities)>1)]

   print("In calc_spatial_auto_score")

   print(unlist(lapply(sample_communities_non_singleton, function(x) { sum(lowerTriangle(mask[ which(sample_reordering %in% x), which(sample_reordering %in% x)])) }  )))
   print(unlist(lapply(sample_communities_non_singleton , function(x) {(length(x)*(length(x)-1))/2})))

   observed_connectivity <- sum(unlist(lapply(sample_communities_non_singleton, function(x) { sum(lowerTriangle(mask[ which(sample_reordering %in% x), which(sample_reordering %in% x)])) }  )))
   upper_bound_connectivity <- sum(unlist(lapply(sample_communities_non_singleton , function(x) {(length(x)*(length(x)-1))/2})))

   print(observed_connectivity)
   print(upper_bound_connectivity)

   return(observed_connectivity/upper_bound_connectivity)
}

#********************************************************************************


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

calc_sas_edge_range <- function(n, nrow, ncol){
  edge_max <- calc_sas_edge_max(n, nrow, ncol)
  edge_min <- calc_sas_edge_min(n, nrow, ncol)
  return(edge_max - edge_min)
}


calc_sas <- function(plate_df, columns_for_scoring, column_weights, sample_reordering, mask, plate_n_rows, plate_n_cols, internal_control_well_indices)
{
  # This bit of code assumes that internal controls are pre-placed in the last column.
  # Later, will add an if statement so that this only runs when internal controls are in a fixed position...
  # Note that this will remove one assumption, but the assumption that fixed internal controls are in right-most column(s)
  # of plate will remain... The upshot is that in situations when this is not the case the score is an approximation and,
  # moreover, we need to bound the score between 0 and 1 for the cases where the approximation is outside of this boundary.
  # One last, albeit less important, assumption is that we will always think of the smaller dimension of a plate as being its rows, and the
  # larger dimension will be its columns... This just aligns with the usual "8x12" implied layout of 96 well plates, which
  # (for the moment) is a primary assumption of this library.
 
  print("In calc_sas")
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

    print(cs)
    print(column_values)

    val <- column_values[1]
    print(((sum(lowerTriangle(mask[ which(column_data==val), which(column_data==val)])) -
                                                                                 calc_sas_edge_min(sum(na.omit(column_data==val)), min_dim, max_dim_floor))/
                                                                                calc_sas_edge_range(sum(na.omit(column_data==val)), min_dim, max_dim_floor)))

    val <- column_values[2]
    print(((sum(lowerTriangle(mask[ which(column_data==val), which(column_data==val)])) -
                                                                                 calc_sas_edge_min(sum(na.omit(column_data==val)), min_dim, max_dim_floor))/
                                                                                calc_sas_edge_range(sum(na.omit(column_data==val)), min_dim, max_dim_floor)))



    sub_score <- column_weight * median(unlist(lapply(column_values, 
                                                      function(val) { max(min(((sum(lowerTriangle(mask[ which(column_data==val), which(column_data==val)])) - 
								                 calc_sas_edge_min(sum(na.omit(column_data==val)), min_dim, max_dim_floor))/ 
                                                                                calc_sas_edge_range(sum(na.omit(column_data==val)), min_dim, max_dim_floor)),1),0) } )))

    print(sub_score)

    score <- sum(c(score, sub_score), na.rm=TRUE)

    cwi <- cwi + 1
  }
  print("DEBUG")
  print(score)
  return(score)
}


#calc_spatial_auto_score_v2 <- function(plate_df, columns_for_scoring, column_weights, sample_reordering, mask, plate_n_rows, plate_n_cols)
#{
#  score <- 0
#  cwi <- 1
#  for(cs in columns_for_scoring){
#    column_data <- pull(plate_df, cs)
#    column_values <- na.omit(unique(column_data))
#    column_weight <- column_weights[cwi]

#    sub_score <- column_weight * median(unlist(lapply(column_values, 
#                                                      function(val) {sum(lowerTriangle(mask[ which(column_data==val), which(column_data==val)]))/ 
#                                                                     calc_spatial_auto_score_v2_aux(sum(na.omit(column_data==val)), plate_n_rows, plate_n_cols)} )))
#
#    score <- score + sub_score
#
#    cwi <- cwi + 1
#  }
#
#  return(score)
#}



#*******************************************************************************
reorder_samples <- function(sample_similarities_matrix,sample_similarities_si_names, sample_similarities_sj_names, well_distances_shuffled_df, plate_size,
                            internal_control_ids, internal_control_well_indices){

  sample_similarities_df <- data.frame(sample_si=lowerTriangle(sample_similarities_si_names),
                                       sample_sj=lowerTriangle(sample_similarities_sj_names),
                                       ss=lowerTriangle(sample_similarities_matrix))

  sample_similarities_df_sorted <- sample_similarities_df %>% arrange(desc(ss))

  #samples_allocated <- list()
  #wells_allocated <- list()

  #samples_allocated <- list(internal_control_ids)
  #wells_allocated <- list(internal_control_well_indices)

  samples_allocated <- internal_control_ids
  wells_allocated <- internal_control_well_indices

  row_id <- 1
  while(length(samples_allocated)<plate_size){
    sample_1 <- sample_similarities_df_sorted[row_id,]$sample_si
    sample_2 <- sample_similarities_df_sorted[row_id,]$sample_sj
    row_id <- row_id + 1

    if(!(sample_1 %in% samples_allocated) & !(sample_2 %in% samples_allocated)){
      wells_df <- well_distances_shuffled_df[ !(well_distances_shuffled_df$i %in% wells_allocated) & !(well_distances_shuffled_df$j %in% wells_allocated),]
      well_1 <- wells_df[1,c("i")]
      well_2 <- wells_df[1,c("j")]

      #samples_allocated <- append(samples_allocated,sample_1)
      #samples_allocated <- append(samples_allocated,sample_2)

      #wells_allocated <- append(wells_allocated,well_1)
      #wells_allocated <- append(wells_allocated,well_2)

      samples_allocated <- c(samples_allocated,sample_1)
      samples_allocated <- c(samples_allocated,sample_2)

      wells_allocated <- c(wells_allocated,well_1)
      wells_allocated <- c(wells_allocated,well_2)


    }else if((sample_1 %in% samples_allocated) & !(sample_2 %in% samples_allocated)){

      well_1 <- wells_allocated[which(samples_allocated == sample_1)]

      wells_df <- well_distances_shuffled_df[ ((well_distances_shuffled_df$i == well_1) & !(well_distances_shuffled_df$j %in% wells_allocated)) |
                            ((well_distances_shuffled_df$j == well_1) & !(well_distances_shuffled_df$i %in% wells_allocated)),]

      wells_options <- wells_df[1,c("i","j")]
      well_2 <- unlist(wells_options[which(wells_options != well_1)],use.names=FALSE)

      #samples_allocated <- append(samples_allocated,sample_2)
      #wells_allocated <- append(wells_allocated,well_2)

      samples_allocated <- c(samples_allocated,sample_2)
      wells_allocated <- c(wells_allocated,well_2)


   }else if(!(sample_1 %in% samples_allocated) & (sample_2 %in% samples_allocated)){

      well_2 <- wells_allocated[which(samples_allocated == sample_2)]

      wells_df <- well_distances_shuffled_df[ ((well_distances_shuffled_df$i == well_2) & !(well_distances_shuffled_df$j %in% wells_allocated)) |
                            ((well_distances_shuffled_df$j == well_2) & !(well_distances_shuffled_df$i %in% wells_allocated)),]

      wells_options <- wells_df[1,c("i","j")]
      well_1 <- unlist(wells_options[which(wells_options != well_2)],use.names=FALSE)

      #samples_allocated <- append(samples_allocated,sample_1)
      #wells_allocated <- append(wells_allocated,well_1)

      samples_allocated <- c(samples_allocated,sample_1)
      wells_allocated <- c(wells_allocated,well_1)

    }
  }

  #sample_well_allocation_df <- data.frame(sample=unlist(samples_allocated),well=unlist(wells_allocated))
  sample_well_allocation_df <- data.frame(sample=samples_allocated,well=wells_allocated)
  sample_well_allocation_df_sorted <- sample_well_allocation_df %>% arrange(well)

  samples_reordered <- sample_well_allocation_df_sorted$sample


  sample_similarities_matrix_reordered <- sample_similarities_matrix[samples_reordered, samples_reordered]

  return(list(samples_reordered, sample_similarities_matrix_reordered))
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

calc_patch_score <- function(plate_df, columns_for_scoring, column_weights, plate_n_rows, plate_n_cols, patch_down_weighting = NULL)
{
  # setup *******************************************************************
  sub_plate_n_cols <- plate_n_cols - 2  
  sub_plate_n_rows <- plate_n_rows - 2  
  sub_plate_size <- sub_plate_n_cols * sub_plate_n_rows

  if(is.null(patch_down_weighting)){
    patch_down_weighting <- min(c(1, (plate_n_rows + plate_n_cols)/sub_plate_size))
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
    column_score <- patch_down_weighting * (column_weight * ( sub_plate_size - column_penalty ))
 
    score <- score + column_score
    cwi <- cwi + 1
  }

  return(score)
}


#***************************************************************************

find_independent_switches <- function(depth, plate_df,
			       ss_matrix,well_distances,jiggled_matrix_indices,
                               splitting_ss_thresh, splitting_wd_thresh,
                               switching_ss_thresh, switching_wd_thresh,
                               columns_for_scoring, column_weights, plate_n_rows, plate_n_cols,
                               plate_size, internal_control_well_indices){

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
  pairs_available_for_switching_indicator_vec_aux[which((ss_vec<=switching_ss_thresh) & (well_distances>switching_wd_thresh))] <- 1
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
      
  corrcoef=cor(as.numeric(well_distances), as.numeric(lowerTriangle(ss_matrix_rejiggled)),use='complete.obs')

  temp_plate_df <- plate_df  %>% slice(match(rownames(ss_matrix_rejiggled), SampleID))
      
  plating_score<-calc_row_column_score(temp_plate_df, columns_for_scoring, column_weights, plate_n_rows, plate_n_cols) +
                     calc_patch_score(temp_plate_df, columns_for_scoring, column_weights, plate_n_rows, plate_n_cols)

  
  return(data.frame(depth=depth, total_score=corrcoef + plating_score, jiggled_matrix_indices=I(list(new_jiggled_matrix_indices))))
}


#***************************************************************************

find_independent_switches_using_sas <- function(depth, plate_df,
			                        ss_matrix,well_distances,jiggled_matrix_indices,
                                                splitting_ss_thresh, splitting_wd_thresh,
                                                switching_ss_thresh, switching_wd_thresh,
                                                columns_for_scoring, column_weights, plate_n_rows, plate_n_cols,
                                                plate_size, internal_control_well_indices, sample_communities, mask){

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
  pairs_available_for_switching_indicator_vec_aux[which((ss_vec<=switching_ss_thresh) & (well_distances>switching_wd_thresh))] <- 1
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
      
  #sas_score <- calc_spatial_auto_score(rownames(ss_matrix_rejiggled),sample_communities,mask)

  temp_plate_df <- plate_df  %>% slice(match(rownames(ss_matrix_rejiggled), SampleID))
      
  sas_score <- calc_sas(temp_plate_df, columns_for_scoring, column_weights, rownames(ss_matrix_rejiggled), mask, plate_n_rows, plate_n_cols, internal_control_well_indices)
  
  plating_score<-calc_row_column_score(temp_plate_df, columns_for_scoring, column_weights, plate_n_rows, plate_n_cols) +
                     calc_patch_score(temp_plate_df, columns_for_scoring, column_weights, plate_n_rows, plate_n_cols)

  
  return(data.frame(depth=depth, total_score=sas_score + plating_score, jiggled_matrix_indices=I(list(new_jiggled_matrix_indices))))
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

  print(x)
  print(sum(x))

  
  x_with_totals <- matrix(0,nrow+1,ncol+1)
  x_with_totals[1:nrow,1:ncol] <- x
  x_with_totals[nrow+1,] <- c(colSums(x),0)
  x_with_totals[,ncol+1] <- c(rowSums(x),0)
  print(x_with_totals)

  one_indices <- which(x==1) 

  d <- matrix(0, nrow*ncol, nrow*ncol)

  for(i in one_indices){
    d[i, one_indices] <- 1
    d[one_indices, i] <- 1
  }


  
  #for(i in 0:87){
  #  #d[(i),which(unlist(lapply(0:87, function(x) {(x %/% 8) == ((i-1) %/% 8)})))] <- 0
  #  #d[which(unlist(lapply(0:87, function(x) {(x %% 8) == ((i-1) %% 8)}))),(i)] <- 0

  #  d[(i+1),which(unlist(lapply(0:87, function(x) {(x %/% 8) == ((i) %/% 8)})))] <- 0
  #  d[which(unlist(lapply(0:87, function(x) {(x %% 8) == ((i) %% 8)}))),(i+1)] <- 0

  #}


  num_edges <- sum((lowerTriangle(d)))

  # ***********
  t <- sum(x)
  edge_max <- (t*(t-1))/2
  row_penalty <- ((t %/% nrow) * (t %% nrow)) + (nrow * (( ((t %/% nrow) - 1) * (t %/% nrow) )/2) )
  col_penalty <- ((t %/% ncol) * (t %% ncol)) + (ncol * (( ((t %/% ncol) - 1) * (t %/% ncol) )/2) )

  print("*****")
  print(sum(x))
  print(edge_max)
  print(row_penalty)
  print(col_penalty)
  print("*****")


  calc_max_edges <- edge_max - row_penalty - col_penalty 
 

  # ***********

  return(c(num_edges, calc_max_edges))
}
