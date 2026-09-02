# Take input manifest for a single plate, add imbalance fixer, add fixed wells, and arrange by well index
# Note: This replaces get_and_format_plate_df_from_manifest() with simpler inputs and outputs
make_plate_df <- function(sample_df, fixed_wells, imbalance_fixer, plate_wells) {
  # SampleID shouldn't be numeric or factor
  sample_df <- sample_df |> dplyr::mutate(SampleID = as.character(SampleID))
  # Add sample wells (excluding fixed wells)
  plate_df <- add_sample_wells(sample_df, fixed_wells$well)
  # Add imbalance fixer column. Keeping for backwards compatibility, but not
  # likely to encourage users to use this functionality
  if (imbalance_fixer[[1]]) {
    plate_df <- easyplater:::add_imbalance_fixer(plate_df, imbalance_fixer)
  }
  # Add fixed wells to plate_df and arrange by well index
  plate_df <- plate_df |>
    dplyr::full_join(fixed_wells, by = c("well", "SampleID" = "lab")) |>
    dplyr::select(-idc) |>
    dplyr::mutate(
      # Fixed/empty wells have these values missing up until now
      plate = dplyr::first(plate),
      column = paste0("Column ", stringr::str_extract(well, "[0-9]*$")),
      row = stringr::str_extract(well, "^[A-Z]*")
    )
  ## Do we need to arrange the wells at all? Keeping this for now to prevent lots of changes to tests.
  plate_df <- plate_df[match(plate_wells, plate_df$well),]

  ## Match historical format of plate_df_aux
  plate_df <- plate_df |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.character(.x)))

  return(plate_df)
}

# Take plate_df_list and return the same with samples reordered by samples_final_order
apply_final_well_locations <- function(plate_df, samples_final_order, columns_for_scoring){

  plate_rows <- plate_df$row
  plate_columns <- plate_df$column
  plate_wells <- plate_df$well

  plate_design_df <- plate_df |> dplyr::slice(match(samples_final_order, .data$SampleID))

  plate_design_df$row <- plate_rows
  plate_design_df$column <- plate_columns
  plate_design_df$well <- plate_wellsplate_df |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.character(.x)))

  return(plate_design_df)
}

get_column_from_well_coords <- function(well_coords){
  return(paste("Column", substr(well_coords,2,3), sep=" "))
}

get_row_from_well_coords <- function(well_coords){
  return(substr(well_coords,1,1))
}
