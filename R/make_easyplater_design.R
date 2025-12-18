#' Design a plate using the easyplater algorithm
#'
#' Given a manifest, run the easyplater algorithm on a single plate. **TO DO: Avi and/or Micah, elaborate on this.**
#'
#' @param manifest_df Data frame or tibble with a `SampleID` column, a `plateID` column, and additional columns for each variable to be used for plate design score. See [easyplater::example_manifest] for an example.
#' @param plateID String. Value from `plateID` column to generate a plate for.
#' @param columns_for_scoring Character vector. Names of columns to use for calculating plate design score.
#' @param column_weights Numeric vector of weights to use for the variables in `columns_for_scoring`. Must be same length as `columns_for_scoring`.
#' @param cols_to_categorize List of character vectors. **TO DO: This needs re-factoring. Leaving for now to populate package functions and create tests.**
#' @param imbalance_fixer Length 4 list. First element is logical, second element is character string of a column name, third element is a list of well IDs, and fourth element is a numeric scalar. **TO DO: Avi, explain this.**
#' @param plate_size Numeric scalar. Size of plate. Note that currently `easyplater` is currently only implemented for 96-well plates.
#' @param internal_control_well_indices Numeric vector containing indices of control wells. Expecting zero index, and numbering going first top to bottom, then left to right.
#' @param internal_control_ids Character vector. Names of internal control wells.
#' @param full_mask `nrow(plate_df) x nrow(plate_df)` numeric matrix. **TO DO Avi: explain this**
#' @param scoring_mask `nrow(plate_df) x nrow(plate_df)` numeric matrix. **TO DO Avi: explain this**
#' @param well_pair_distances_df **TO DO: Avi explain**
#' @param splitting_ss_thresh Numeric scalar. Similarity threshold for generating adjacency matrix.
#' @param splitting_wd_thresh **TO DO: Avi explain**
#' @param replacing_ss_thresh **TO DO: Avi explain**
#' @param replacing_wd_thresh **TO DO: Avi explain**
#' @param max_depth **TO DO: Avi explain**
#' @param wins_required **TO DO: Avi explain**
#' @param max_attempts **TO DO: Avi explain**
#' @param pds_local_weight Numeric scalar. Weight to give the \eqn{PDS_{local}} relative to \eqn{PDS_{local}}. A sensible default is 1, but may be adjusted as desired.
#' @param patch_weight down-weighting for \eqn{PDS_{patch}}, required because \eqn{|patches|=3(|rows|+|columns|)}. If NULL, weight is calculated automatically. Default value for 96-well plates is 1/6. **TO DO: Avi, check this description**
#'
#' @returns **TO DO: Micah, fill this in.**
#'
#' @export
#'
#' @examples
#' easyplater comes with a built-in example plate manifest
#' example_manifest
#'
#' # Run easyplater in one step
#' easyplater_design <- make_easyplater_design(
#'   manifest_df = example_manifest,
#'   plateID = "plate 1",
#'   columns_for_scoring = c("Cohort","Group","Sex","AgeGroup"),
#'   column_weights = c(5, 5, 10, 4),
#'   cols_to_categorize = list(c("Age", 10, NULL, "AgeGroup")),
#'   imbalance_fixer = list(T, "Group", list("D1", "HC1", "D7", "D8"), 3),
#'   plate_size = 96
#' )
#'
#' # Use a function exported from the OlinkAnalyze package to display plate layout
#' olink_displayPlateLayout(data = easyplater_design, fill.color = "Group", include.label = T)
make_easyplater_design <- function(manifest_df, plateID,
                                   columns_for_scoring, column_weights, cols_to_categorize, imbalance_fixer,
                                   plate_size = 96,
                                   internal_control_well_indices = 86:95,
                                   internal_control_ids = c(paste0("SC", 1:2), paste0("NC", 1:3), paste0("PC", 1:5)),
                                   full_mask = NULL, scoring_mask = NULL,
                                   well_pair_distances_df = NULL,
                                   splitting_ss_thresh = 0.5, splitting_wd_thresh = 1,
                                   replacing_ss_thresh = 0.5, replacing_wd_thresh = 6,
                                   max_depth = 2, wins_required = 10, max_attempts = 100,
                                   pds_local_weight=1, patch_weight = NULL){

  # Note: Re-write this to surface a more useful message to the user
  stopifnot(plate_size == 96)

  if (plate_size == 96) {
    plate_num_rows <- 8
    plate_num_cols <- 12
  }

  plate_wells <- paste0(rep(LETTERS[1:plate_num_rows], times = plate_num_cols),
                        rep(1:plate_num_cols, each = plate_num_rows))

  well_distances_matrix <- make_well_distances_matrix(plate_size)

  if (is.null(full_mask)) {
    full_mask <- make_full_mask(well_distances_matrix)
  }

  if (is.null(scoring_mask)) {
    scoring_mask <- make_scoring_mask(well_distances_matrix)
  }

  if (is.null(well_pair_distances_df)) {
    well_pair_distances_df <- make_well_distance_df(plate_size)
  }


  print("Getting and formatting plate data fram from manifest.")
  plate_df_list <- get_and_format_plate_df_from_manifest(manifest_df, plateID, columns_for_scoring, cols_to_categorize, imbalance_fixer,
                                                         plate_size, plate_wells, internal_control_well_indices, internal_control_ids)

  # Note: We may want to move the patch_weight calculation from calc_patch_score() up to here, so that this computation isn't repeated with each iteration

  print("Allocating similar samples to distal wells.")
  sample_allocation_outputs <- allocate_similar_samples_to_distal_wells(
    plate_df_list, columns_for_scoring, column_weights, imbalance_fixer,
    full_mask, scoring_mask, splitting_ss_thresh,
    internal_control_ids, internal_control_well_indices,
    plate_num_rows, plate_num_cols, plate_size,
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

#' @export
#' @importFrom OlinkAnalyze olink_displayPlateLayout
OlinkAnalyze::olink_displayPlateLayout

