#' Design a plate using the easyplater algorithm
#'
#' Given a manifest, run the easyplater algorithm on a single plate. **TO DO: Avi and/or Micah, elaborate on this.**
#'
#' @param manifest_df
#' @param p
#' @param columns_for_scoring
#' @param column_weights
#' @param cols_to_categorize
#' @param imbalance_fixer
#' @param plate_num_rows
#' @param plate_num_cols
#' @param plate_size
#' @param plate_wells
#' @param internal_control_well_indices
#' @param internal_control_ids
#' @param full_mask
#' @param scoring_mask
#' @param well_pair_distances_df
#' @param splitting_ss_thresh
#' @param splitting_wd_thresh
#' @param replacing_ss_thresh
#' @param replacing_wd_thresh
#' @param max_depth
#' @param wins_required
#' @param max_attempts
#' @param pds_local_weight
#' @param patch_weight
#'
#' @returns
#' @export
#'
#' @examples
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
