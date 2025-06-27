# R: version 4.2.1 
source("~/IMCM/projects/Bioinformatics/well plate design/src/easyPlateR/easyplater_functions.r")


# SETUP ########################################################################

# Read in example manifest and give the manifest a name for the purpose of outputs
manifest_df <- read_csv("~/IMCM/projects/Bioinformatics/well plate design/src/easyPlateR/data/example_manifest.csv")

# Make a list of the plates:
plates <- str_sort(unique(manifest_df$plate),numeric=TRUE)

# Set seeds
top_seed = 1
set.seed(top_seed)
plate_seeds <- sample(1000000,length(plates))

# Initialise control well indices and IDs.
internal_control_well_indices <- c(86,87,88,89,90,91,92,93,94,95) # This can change! Expecting zero index, and numbering going first top to bottom, then left to right, as per the order of plate_wells constant.
internal_control_ids <- c("SC1","SC2","NC1","NC2","NC3","PC1","PC2","PC3","PC4","PC5") # SC = sample control, NC = negative control, PC = plate control (aka callibrator)

cols_to_categorize <- list(c("Age",10,NULL,"AgeGroup"))

# Give list of variables for scoring
columns_for_scoring <- c("Cohort","Group","Sex","AgeGroup")


# Supply variable weights
column_weights <- c(5,5,10,4)


# Build an imbalance_fixer, if required.
imbalance_fixer <- list(T,"Group",list("D1","HC1","D7","D8"),3)
# NOTE: Need to put try-catch around the code that uses this!
# Also, this is an ugly piece of code and needs fixing. Probably best wrapped in a function that builds
# an imbalance_fixer.

# Variables to control sample switching search
max_depth<-2
wins_required <- 10
max_attempts <- 100

################################################################################
# Main for loop - score each plate in the manifest
################################################################################
for (pi in 1:length(plates)){
  p <- plates[pi]
  
  plate_df_list <- get_and_format_plate_df_from_manifest(manifest_df, p, columns_for_scoring, cols_to_categorize, imbalance_fixer, 
                                                         plate_size, plate_wells, internal_control_well_indices, internal_control_ids)
  
  plate_df <- plate_df_list[[1]]
  plate_df_aux <- plate_df_list[[2]]
  
  score <- calc_pds(plate_df,columns_for_scoring, column_weights,                       
                         scoring_mask,
                         plate_num_rows, plate_num_cols,internal_control_well_indices)
  
  print(score)
}










