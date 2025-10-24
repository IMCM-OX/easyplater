# R: version 4.2.1 
library(OlinkAnalyze)
library(tidyverse)
library(readxl)
library(writexl)
library(ggpubr)
library(gdata)
library(igraph)
library(networkD3)
library(here)

source(here("easyplater_init_functions.r"))
source(here("easyplater_functions.r"))

# SETUP ########################################################################

# Read in example manifest and give the manifest a name for the purpose of outputs
manifest_df <- read_csv(here("data/example_manifest.csv"))
manifest_name = "example_design"

# Create main directory for output:
manifest_output_dir = here(paste0("outputs/manifest_",manifest_name))
if (file.exists(manifest_output_dir)){
  stop("Output directory already exists. Please delete it or create a new directory for your output.")
}
dir.create(here(manifest_output_dir), recursive = TRUE)

# Make a list of the plates:
plates <- str_sort(unique(manifest_df$plate),numeric=TRUE)

# Set seeds
top_seed = 1
set.seed(top_seed)
plate_seeds <- sample(1000000,length(plates))

# Initialise control well indices and IDs.
internal_control_well_indices <- c(86,87,88,89,90,91,92,93,94,95) # This can change! Expecting zero index, and numbering going first top to bottom, then left to right, as per the order of plate_wells constant.
internal_control_ids <- c("SC1","SC2","NC1","NC2","NC3","PC1","PC2","PC3","PC4","PC5") # SC = sample control, NC = negative control, PC = plate control (aka callibrator)


# Set up variables that need to be turned into categorical variables
# (variables are referred to as columns because each variable has a column in manifest_df)
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
# Main for loop - design each plate in manifest
################################################################################
for (pi in 1:length(plates)){
  print(paste0("Making easyplater design for plate: ", plates[pi]))
  # extract plate name and set plate seed as seed
  p <- plates[pi]
  pseed <- plate_seeds[pi]
  set.seed(pseed)
  
  # create output directory for plate, with a subdirectory for plots
  single_plate_dir = paste0(manifest_output_dir,"/manifest_", manifest_name,"_", trimws(p))
  dir.create(here(single_plate_dir), recursive = TRUE)
  
  single_plate_plots_dir = paste0(manifest_output_dir,"/manifest_", manifest_name,"_", trimws(p),"/plots")
  dir.create(here(single_plate_plots_dir), recursive = TRUE)
  
  #******************************************************************************************
  # Design the plate
  #******************************************************************************************
  
  easy_plate_df <- make_easyplater_design(manifest_df, p, 
                                          columns_for_scoring, column_weights, cols_to_categorize, imbalance_fixer, 
                                          plate_num_rows, plate_num_cols, plate_size, plate_wells, 
                                          internal_control_well_indices, internal_control_ids,
                                          full_mask, scoring_mask,
                                          well_pair_distances_df,
                                          splitting_ss_thresh, splitting_wd_thresh,
                                          replacing_ss_thresh, replacing_wd_thresh,
                                          max_depth, wins_required, max_attempts)

  #******************************************************************************************
  # Plot sample plating by variable using olink_displayPlateLayout function from the OlinkAnalyze library
  # Save using ggsave
  #******************************************************************************************

  # print("Visualize and save to file plate layouts coloured by variables.")
  # for(label in columns_for_scoring){ 
  # # Visualize plate layouts coloured by the chosen variable
  # 
  #   my_plot <- olink_displayPlateLayout(data = easy_plate_df,
  #                                         fill.color = label,include.label = T)+
  #       theme(legend.position = "none") + ggtitle(paste(p, ": ", label, sep=""))
  # 
  # 
  #   ggsave(my_plot, filename=paste(p, "_", label, ".png", sep=""), path=single_plate_plots_dir)
  # }
  # 
  # #******************************************************************************************
  # # Print output files
  # #******************************************************************************************
  # 
  # print("Printing output files.")
  # print_easyplater_manifest(easy_plate_df, 
  #                           c("Cohort","Group","Sex"),
  #                           paste0(single_plate_dir,"/plate_manifest_", manifest_name, "_", trimws(p),".csv"))
  # 
  # print_easyplater_design(easy_plate_df, paste0(single_plate_dir,"/plate_design_", manifest_name, "_", trimws(p),".csv"))
  # 
  # print_easyplater_submissionform(easy_plate_df, 
  #                                 c("Cohort","Group","Sex"), 
  #                                 paste0(single_plate_dir,"/submission_form_", manifest_name, "_", trimws(p),".csv"))
  # 
  # print_easyplater_log(paste0(single_plate_dir,"/LOG_", manifest_name, "_", trimws(p),".csv"))
  
}
