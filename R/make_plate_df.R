#' Prepare plate data frame from full manifest
#'
#' This function subsets a single plate from the input manifest and wrangles the data for downstream processing. **TO DO: Micah, elaborate on this.**
#'
#' @inheritParams make_easyplater_design
#'
#' @returns List of length two.
#'
#'  - The first element is data.frame for the specified plate with categorized columns specified in`cols_to_categorize` argument, and an additional column `imbalanceFix_vec`.
#'  - The second element is a tibble with columns "SampleID", "plate", "column", "row", "well", plus columns specified in `columns_for_scoring` argument.
#'
#' @export
#'
#' @examples
#' # We can use easyplater's built-in example manifest
#' str(example_manifest)
#' get_and_format_plate_df_from_manifest(
#'   manifest_df = example_manifest,
#'   plateID = "plate 1",
#'   columns_for_scoring = c("Cohort", "Group", "Sex", "AgeGroup"),
#'   cols_to_categorize = list(c("Age", "10", "AgeGroup")),
#'   imbalance_fixer = list(TRUE,"Group",list("D1","HC1","D7","D8"),3),
#'   plate_size = 96,
#'   plate_wells = paste0(rep(LETTERS[1:8], times = 12), rep(1:12, each = 8)),
#'   internal_control_well_indices = 86:95,
#'   internal_control_ids = c(paste0("SC", 1:2), paste0("NC", 1:3), paste0("PC", 1:5))
#' )
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

  plate_df_aux <- dplyr::filter(manifest_df, .data$plate == plateID)

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
            categorized_col_vec <- as.numeric(ggplot2::cut_interval(as.matrix(plate_df_aux[,col_name]),num_cats))
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

  plate_df <- dplyr::select(plate_df_aux, dplyr::all_of(cols_for_analysis))
  plate_df_aux <- dplyr::select(plate_df_aux, dplyr::all_of(c(cols_for_analysis, c("plate","column","row","well"))))

  # Now account for imbalance in samples, as required
  imbalanceFix_vec <- c()
  if(imbalance_fixer[[1]]){
    plate_df_column_for_fixing <- dplyr::select(plate_df, dplyr::all_of(imbalance_fixer[[2]]))
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

  if(num_internal_controls>0){
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
  }


  # Finally, sort plate_df and plate_df_aux by well index

  #well_indices <- unlist(lapply(plate_df_aux$well), function(w) which(plate_wells==w))

  well_indices <- match(plate_df_aux$well, plate_wells)-1

  plate_df$well_indices <- well_indices
  plate_df <- plate_df |> dplyr::arrange(well_indices)
  plate_df <- subset(plate_df, select = -c(well_indices))

  plate_df_aux$well_indices <- well_indices
  plate_df_aux <- plate_df_aux |> dplyr::arrange(well_indices)
  plate_df_aux <- subset(plate_df_aux, select = -c(well_indices))

  return(list(plate_df, plate_df_aux))
}

# Take plate_df_list and return the same with samples reordered by samples_final_order
# Micah: A clearer name may be apply_final_well_locations()
make_easyplater_design_aux <- function(plate_df_list, samples_final_order, columns_for_scoring){

  plate_df_aux <- plate_df_list[[2]]

  plate_rows <- plate_df_aux$row
  plate_columns <- plate_df_aux$column
  plate_wells <- plate_df_aux$well

  plate_design_df <- plate_df_aux |> dplyr::slice(match(samples_final_order, .data$SampleID))

  plate_design_df$row <- plate_rows
  plate_design_df$column <- plate_columns
  plate_design_df$well <- plate_wells

  return(plate_design_df)
}

get_column_from_well_coords <- function(well_coords){
  return(paste("Column", substr(well_coords,2,3), sep=" "))
}

get_row_from_well_coords <- function(well_coords){
  return(substr(well_coords,1,1))
}
