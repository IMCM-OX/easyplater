#### CONSIDERATION OF SUBJECTID

# R: version 4.2.1 
source("optimize_sample_wells_in_plate_functions.r")

################################################################################
# I am trying to neaten up code here, so have to remover some comments.
# Please see optimize_sample_wells_using_matrices_v4_FOR_NOTES_AND_REFERENCE.r
# (which is a copy of optimize_sample_wells_using_matrices_v4.r in the 
# retired_161023 directory) for the original comments and TODOs that I had in here
# before I edited the code.
################################################################################

# Later on, will pass these parameters to the script. Hard coding for the moment.
# Code further down also hard codes a bunch of study related variable - these
# are hardcoded to match ALS requirements, but this obviously has to change later!

# Constants ***************************************************************
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

plate_num_rows <- 8
plate_num_cols <- 12
mask_edge_thresh <- 3
# *************************************************************************

internal_control_well_indices <- c(88,89,90,91,92,93,94,95) # This can change! Expecting zero index, and numbering going first top to bottom, then left to right, as per the order of plate_wells constant.
internal_control_ids <- c("SC1","SC2","SC3","NC1","NC2","PC1","PC2","PC3") # SC = sample control, NC = negative control, PC = plate control (aka callibrator)

CLOSE_THRESH <- 2

#weights = c(0,5,5,1,1,1) # first weight is 0: this just corresponds to the SampleID
                         # column comparison (which will always be 0 anyway, as we
			 # don't compare samples to themselves).)
                         # Doing this just means fewer steps in terms of extracting
			 # columns from vectors, in the sample similarity calculations.
weights = c(0,1)

#columns_for_scoring <- c("SubjectGroup","Sex","TimePoint","AgeGroup","LatencyGroup")
#column_weights <- c(5,5,1,1,1) # See notes below... should this always just be weights[2:6]?

columns_for_scoring <- c("Sex")
column_weights <- c(1)

splitting_ss_thresh <- 0.5
splitting_wd_thresh <- 1
switching_ss_thresh <- 0.5
switching_wd_thresh <- 6


# NOTE: If this works, will need to put try-catch around the code that uses this!

imbalance_fixer <- c(F,"","",0)

# IMPORTANT NOTE: At the moment only thinking about 96 well plates. And thresholds
# for what is a big or small distance between wells is HARDCODED accordingly...
# this will need to change in more flexible code that also works for plate sizes
# of 48.

# NEED TO PLAY AROUND WITH:
# Whether to use plate score or plate penalty for rows and colums same-ness
# How to use weighting here
# How to use weighting for calculating overlap. Should these be the same weights as above?
# Should similarity columns be same set of columns used for calculating column and row sameness?
# In find_next_switches need to play with thresholds for how to choose spitting and splitting pairs (4 thresholds to set here.)
# Also need to think about how deep to go in search, whether to stop search based on improvement being too low (or negative),
# and also how many nodes to take forward from one depth to the next... Also want to think about re-running the
# search a few times, from the re-ordering step, to see if we reach a better local minima.

# How is this all going to work in terms of overall workflow for choosing plates?
# Will I also want to add a final check to ensure there are no totally same rows/columns for any of the important attributes?
# In the cases that there are such columns, do I want to add a check to see that this doesn't happen in the same rows/ columns
# across plates?

# How many times will we repeat this process to see if we can get it right, before we give up?

################################################################################



################################################################################
# Open PDF for plotting to:
pdf("myplot.pdf")

################################################################################

well_pair_distances_df <- make_well_distance_df(plate_size)
well_distances_matrix <- make_well_distances_matrix(plate_size)
full_mask <- make_full_mask(well_distances_matrix, mask_edge_thresh)
scoring_mask <- make_scoring_mask(well_distances_matrix)


# Read in randomized manifest
#manifest_df <- read_csv("../../data/ALS_example_randomized_manifest.csv")
manifest_df <- read_csv("test_data/pos_spatial_correlation_manifest_2.csv")
#manifest_df <- cbind(manifest_df[sample(1:nrow(manifest_df)),c("SampleID","Sex")],manifest_df[,c("plate","column","row","well")])
# Make a list of the plates:
plates <- unique(manifest_df$plate)

for (p in plates[1]){

  #cols_for_analysis <- c("SampleID","SubjectGroup","Sex","TimePoint","AgeGroup","LatencyGroup")
  #cols_to_categorize <- list(c("Age",10,0,"AgeGroup"), c("Latency",10,0,"LatencyGroup"))

  cols_for_analysis <- c("SampleID","Sex")
  cols_to_categorize <- list()

  plate_df_list <- get_and_format_plate_df_from_manifest(manifest_df, p, cols_for_analysis, cols_to_categorize, imbalance_fixer, 
							 plate_size, plate_wells, internal_control_well_indices, internal_control_ids) 

  plate_df <- plate_df_list[[1]]
  plate_df_aux <- plate_df_list[[2]]

  ss_matrices_list <- make_ss_matrices(plate_df, weights, imbalance_fixer, plate_size)
  
  sample_similarities_matrix <- ss_matrices_list[[1]]
  sample_similarities_si_names <- ss_matrices_list[[2]]
  sample_similarities_sj_names <- ss_matrices_list[[3]]

  sample_communities <- find_sample_communities(sample_similarities_matrix, splitting_ss_thresh)
  sas_original <- calc_spatial_auto_score(plate_df$SampleID , sample_communities, scoring_mask)

  #cc_original=cor(as.numeric(well_pair_distances_df$d), as.numeric(lowerTriangle(sample_similarities_matrix)),use='complete.obs')
  
  temp_plate_df_original <- plate_df  %>% slice(match(rownames(sample_similarities_matrix), SampleID))
  plating_score_original<-calc_row_column_score(temp_plate_df_original, columns_for_scoring, column_weights, plate_num_rows, plate_num_cols) +
                          calc_patch_score(temp_plate_df_original, columns_for_scoring, column_weights, plate_num_rows, plate_num_cols)


  print("Developing function...")
  #calc_spatial_auto_score_v2(temp_plate_df_original,columns_for_scoring, column_weights, temp_plate_df_original$SampleID, scoring_mask, plate_num_rows, plate_num_cols)
  calc_sas(temp_plate_df_original,columns_for_scoring, column_weights, temp_plate_df_original$SampleID, scoring_mask, plate_num_rows, plate_num_cols,internal_control_well_indices)


  #print(cc_original)
  print("Original plating scores:")
  print(sas_original)
  print(plating_score_original)
  print(sas_original + plating_score_original)

  samples_reordered <- plate_df$SampleID
  sample_similarities_matrix_reordered <- sample_similarities_matrix
  #cc_reordered <- cc_original 
  plating_score_reordered <- plating_score_original
  sas_reordered <- sas_original

  #best_score <- cc_reordered + plating_score
  best_score <- sas_reordered + plating_score_reordered

  scores <- rep(0,10)
  for(s in 1:10) {
    print(s)
    shuffled_df <- shuffle_well_pairs_within_distance_groups(well_pair_distances_df, CLOSE_THRESH)

#    samples_reordered_list_starter <- reorder_samples(sample_similarities_matrix, sample_similarities_si_names,sample_similarities_sj_names, shuffled_df, plate_size,
#                                                     internal_control_ids, internal_control_well_indices)


    samples_reordered_list_starter <- reorder_samples_in_plate(sample_similarities_matrix, sample_communities, full_mask,
		                                               internal_control_ids, internal_control_well_indices, plate_size)

    samples_reordered_starter <- samples_reordered_list_starter[[1]]
    sample_similarities_matrix_reordered_starter <- samples_reordered_list_starter[[2]]


    #cc_reordered_starter=cor(as.numeric(well_pair_distances_df$d), as.numeric(lowerTriangle(sample_similarities_matrix_reordered_starter)),use='complete.obs')

    temp_plate_df <- plate_df  %>% slice(match(rownames(sample_similarities_matrix_reordered_starter), SampleID))
    plating_score_starter<-calc_row_column_score(temp_plate_df, columns_for_scoring, column_weights, plate_num_rows, plate_num_cols) +
                           calc_patch_score(temp_plate_df, columns_for_scoring, column_weights, plate_num_rows, plate_num_cols)

    #print(cc_reordered_starter)
    #print(plating_score_starter)

    ##if((plating_score_starter + cc_reordered_starter) > best_score){ 
    ##  print("New Best Score!")
    ##  best_score <- (plating_score_starter + cc_reordered_starter)
    ##  print(best_score)
    ##  cc_reordered <- cc_reordered_starter
    ##  plating_score <- plating_score_starter
    ##  samples_reordered <- samples_reordered_starter
    ##  sample_similarities_matrix_reordered <- sample_similarities_matrix_reordered_starter
    ##}else{
    ##  print(cc_reordered_starter + plating_score_starter)
    #}
    ##scores[s] <- (cc_reordered_starter + plating_score_starter)

    sas_starter <- calc_spatial_auto_score(samples_reordered_starter, sample_communities, scoring_mask) 

    if((sas_starter + plating_score_starter) > best_score){ 
      print("New Best Score!")
      best_score <- (sas_starter + plating_score_starter)
      print(best_score)
      #cc_reordered <- cc_reordered_starter
      plating_score_reordered <- plating_score_starter
      sas_reordered <- sas_starter
      samples_reordered <- samples_reordered_starter
      sample_similarities_matrix_reordered <- sample_similarities_matrix_reordered_starter
    }else{
      print(sas_starter)
    }
    scores[s] <- (sas_starter)   
  }

  hist(scores)
  
  #print(cc_original)
  #print(cc_reordered)

  #print(cc_original + plating_score_original)
  #print(cc_reordered + plating_score)

  print(sas_original)
  print(sas_reordered)
  print(plating_score_original)
  print(plating_score_reordered)


  # IMPORTANT QUESTIONS: *****************************************
  # Before we launch into switching pairs around to separate similar samples, do we first want to 
  # try finding a plating which most closely sticks to the rule of no rows and columns having
  # identical samples for any of the variables (eg, all ALS, all control, all female, etc.)...
  # One idea would be to run the following code but with different thresholds for splitting and switching..
  # Eg, only looking to split samples in the same column (row) (so distance = 0), and not requiring
  # lots of overlap to do so?... Or are we happy to leave algorithm as it is, because it's already
  # going to try and split samples in the same row and column due to these distances already being set to 0?
  #
  # A related question is, are we going to try and run the reordering a few times to find best starting set up,
  # prior to optimizing with switching?
  # 
  # And is it worth testing to see whether simply running the reordering again and again will give a good enough,
  # if not as good, plating as the switching steps obtain?
  # **************************************************************


  jiggled_indices_df <- data.frame(depth=0, total_score=best_score, jiggled_matrix_indices=I(list(1:plate_size)))

  num_leaves_at_last_depth <- 1
  current_depth <- 1

  # These are constants that the user should choose ******
  MAX_DEPTH<-2
  WINS_REQUIRED <- 10
  MAX_ATTEMPTS <- 100
  # ******************************************************

  while((current_depth <= MAX_DEPTH) & (num_leaves_at_last_depth > 0)){
 
    print(paste("Current depth:", current_depth))
    jiggled_indices_subset_df <- jiggled_indices_df[which(jiggled_indices_df$depth == (current_depth-1)),]

    print(paste("Number of jiggled matrices to go through from previous depth:", nrow(jiggled_indices_subset_df)))

    for(ri in 1:nrow(jiggled_indices_subset_df)) {
      print(ri)
      num_wins <- 0
      num_attempts <- 0

      while( (num_wins<WINS_REQUIRED) & (num_attempts<MAX_ATTEMPTS) ){

          jiggled_indices_aux_df <- find_independent_switches_using_sas(current_depth,plate_df,
                                         sample_similarities_matrix_reordered,
                                         well_pair_distances_df$d,
                                         unlist(jiggled_indices_subset_df[ri,]$jiggled_matrix_indices),
                                         splitting_ss_thresh, splitting_wd_thresh,
                                         switching_ss_thresh, switching_wd_thresh,
                                         columns_for_scoring, column_weights, plate_num_rows, plate_num_cols,
					 plate_size, internal_control_well_indices, 
					 sample_communities, scoring_mask) ## IMPORTANT! these last two parameters are new and are for calculating the spatial auto score!

          if(jiggled_indices_aux_df[1,]$total_score > jiggled_indices_subset_df[ri,]$total_score){
             num_wins <- num_wins + 1
             jiggled_indices_df <- rbind(jiggled_indices_df, jiggled_indices_aux_df)
          }
          
          num_attempts <- num_attempts + 1
       }
     }
     
     num_leaves_at_last_depth <- nrow( jiggled_indices_df[which(jiggled_indices_df$depth == current_depth),] )
     current_depth <- current_depth + 1
  }

  print(paste("Best total score: ", max(jiggled_indices_df$total_score) ,sep=""))
  top_switch_df <- head(jiggled_indices_df[which(jiggled_indices_df$total_score == max(jiggled_indices_df$total_score)),], n=1)
  print(top_switch_df$jiggled_matrix_indices)

  print(top_switch_df[1,]$total_score) 
  print(top_switch_df[1,]$jiggled_matrix_indices)

  samples_final_order  <- samples_reordered[unlist(top_switch_df[1,]$jiggled_matrix_indices)]

  print(samples_final_order)


  #******************************************************************************************
  # Plot Scatter plots to show correlation between well distance and sample similarity ######
  #******************************************************************************************
  sample_similarities_matrix_final_order <- sample_similarities_matrix_reordered[samples_final_order, samples_final_order]


  original_df <- data.frame(ss=lowerTriangle(sample_similarities_matrix), d=well_pair_distances_df$d)
  reordered_df <- data.frame(ss=lowerTriangle(sample_similarities_matrix_reordered), d=well_pair_distances_df$d)
  final_df <- data.frame(ss=lowerTriangle(sample_similarities_matrix_final_order), d=well_pair_distances_df$d)

  plot(original_df$d, original_df$ss, main = cor(as.numeric(original_df$d), as.numeric(original_df$ss),use='complete.obs'),
     xlab = "Distance between wells", ylab = "Sample similarity", frame = FALSE)
     abline(lm(ss ~ d, data = original_df), col = "blue")

  plot(reordered_df$d, reordered_df$ss, main = cor(as.numeric(reordered_df$d), as.numeric(reordered_df$ss),use='complete.obs'),
     xlab = "Distance between wells", ylab = "Sample similarity", frame = FALSE)
     abline(lm(ss ~ d, data = reordered_df), col = "blue")

  plot(final_df$d, final_df$ss, main = cor(as.numeric(final_df$d), as.numeric(final_df$ss),use='complete.obs'),
     xlab = "Distance between wells", ylab = "Sample similarity", frame = FALSE)
     abline(lm(ss ~ d, data = final_df), col = "blue")


  #******************************************************************************************
  # Plot sample plating by variable
  #******************************************************************************************


  first_reordered_plate_df <- plate_df_aux  %>% slice(match(samples_reordered, SampleID))
  # Need to reset rows and columns (otherwise nothing changes!)
  plate_rows <- plate_df_aux$row
  plate_columns <- plate_df_aux$column
  plate_wells <- plate_df_aux$well
  first_reordered_plate_df$row <- plate_rows
  first_reordered_plate_df$column <- plate_columns
  first_reordered_plate_df$well <- plate_wells


  new_plate_df <- plate_df_aux  %>% slice(match(samples_final_order, SampleID))  
  # Need to reset rows and columns (otherwise nothing changes!)
  new_plate_df$row <- plate_rows
  new_plate_df$column <- plate_columns
  new_plate_df$well <- plate_wells


  for(label in c("Sex")){

  # Visualize the old and new plate layouts coloured by the chosen group
    my_plot <- olink_displayPlateLayout(data = plate_df_aux,
                           fill.color = label,include.label = T)+
    theme(legend.position = "none") + ggtitle(paste(p, ". First plating: Coloured samples by ", label, sep=""))
    print(my_plot)


    my_plot <- olink_displayPlateLayout(data = first_reordered_plate_df,
                           fill.color = label,include.label = T)+
    theme(legend.position = "none") + ggtitle(paste(p, ". First reordered plating: Coloured samples by ", label, sep=""))
    print(my_plot)


    my_plot <- olink_displayPlateLayout(data = new_plate_df,
                           fill.color = label,include.label = T)+
    theme(legend.position = "none") + ggtitle(paste(p, ". Optimized plating: Coloured samples by ", label, sep=""))
    print(my_plot)

  }

}
dev.off()

