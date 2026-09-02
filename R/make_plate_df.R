# Take plate_df_list and return the same with samples reordered by samples_final_order
# Micah: A clearer name may be apply_final_well_locations()
apply_final_well_locations <- function(plate_df, samples_final_order, columns_for_scoring){

  plate_rows <- plate_df$row
  plate_columns <- plate_df$column
  plate_wells <- plate_df$well

  plate_design_df <- plate_df |> dplyr::slice(match(samples_final_order, .data$SampleID))

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
