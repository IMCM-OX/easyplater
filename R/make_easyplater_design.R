SampleID <- NULL

#' Design a plate using the easyplater algorithm
#'
#' @description
#' This top-level function implements easyplater, the algorithm for generating 96-well plate designs under the constraint that clinical variables are decoupled from plate location effects <a href='https://arxiv.org/abs/2512.17988'>\[1\]</a>.
#'
#' @references
#' \[1\] Taylor A. & Fletcher MP.
#' easyplater: The easy way to generate microplate designs deconvolved from multivariate clinical data.
#' *arXiv* 2026. doi: \url{https://arxiv.org/abs/2512.17988}
#'
#' @param manifest_df Data frame or tibble with a `SampleID` column, a plate column (default: "plate", but this can be changed with `plate_col` argument), and additional columns for each variable to be used for plate design score. See [easyplater::input_manifest] for an example.
#' @param plateID Character vector. Value from column specified by `plate_col` to generate a  deconvolved plate for. If NULL (default), every plate in the manifest will be deconvolved.
#' @param columns_for_scoring Character vector. Names of columns to use for calculating plate design score.
#' @param column_weights Numeric vector of weights to use for the variables in `columns_for_scoring`. Must be same length as `columns_for_scoring`.
#' @param cols_to_categorize List of character vectors. **TO DO: This needs re-factoring. Leaving for now to populate package functions and create tests.**
#' @param imbalance_fixer Boolean or length 4 list. Default: FALSE. Use imbalance_fixer to create a new column (variable) designed to ameliorate a known category imbalance in an existing column which is being scored. In particular, we are interested in addressing the situation where there are several minority categories of a column which individually are assigned to only a small proportion of samples, but together total a more substantial proportion of samples. See User Guide for further details. First element is logical (indicating whether or not to use an imbalance_fixer), second element is character string of the column name with imbalance, third element is a list of minority category values in the column, and fourth element is a numeric scalar weighting for the imbalance_fixer column that will be created. **TO DO: Avi, add explanation to User Guide.**
#' @param plate_size Numeric scalar. Default: 96. Size of plate. Note that currently `easyplater` is currently only implemented for 96-well plates.
#' @param internal_control_well_indices Numeric vector containing indices of control wells. Expecting zero index, and numbering going first top to bottom, then left to right.
#' @param internal_control_ids Character vector. Names of internal control wells.
#' @param full_mask `nrow(plate_df) x nrow(plate_df)` numeric matrix. **TO DO: Micah, please hide this technical, internal variable that needs to be hidden/ de-surfaced from the user.**
#' @param scoring_mask `nrow(plate_df) x nrow(plate_df)` numeric matrix. **TO DO: Micah, please hide this technical, internal variable that needs to be hidden/ de-surfaced from the user**
#' @param well_pair_distances_df **TO DO: Micah, please hide this technical, internal variable that needs to be hidden/ de-surfaced from the user**
#' @param splitting_ss_thresh Numeric scalar. Default: 0.5. Sample similarity (ss) threshold. Used to identify pairs of similar samples, i.e., whose pairwise similarity is greater than this threshold; if found to be in nearby wells, these samples might be moved into distal wells (i.e. split apart) to potentially improve the plate design. See <a href='https://arxiv.org/abs/2512.17988'>\[1\]</a>; section 2.2, step 3. See also Supplementary Figure 4.
#' @param splitting_wd_thresh Numeric scalar. Default: 1. Well distance (wd) threshold. Used to identify pairs of wells which are nearby to one another, i.e., whose distance is less than this threshold. See <a href='https://arxiv.org/abs/2512.17988'>\[1\]</a>; section 2.2, step 3. See also Supplementary FIgure 4.
#' @param replacing_ss_thresh Numeric scalar. Default: 0.5. Sample similarity (ss) threshold. Used to identify pairs of dissimilar samples, i.e., whose pairwise similarity is less than or equal to this threshold; if found to be in distal wells, these samples might be switched with a sample that is under consideration for a split. See <a href='https://arxiv.org/abs/2512.17988'>\[1\]</a>; section 2.2, step 3. See also Supplementary Figure 4.
#' @param replacing_wd_thresh Numeric scalar. Default: 6. Well distance (wd) threshold. Used to identify pairs of wells which are distal to one another, i.e., whose distance is greater than this threshold. See <a href='https://arxiv.org/abs/2512.17988'>\[1\]</a>; section 2.2, step 3. See also Supplementary Figure 4.
#' @param max_depth Integer. Default: 2. Depth of sample switching search. See <a href='https://arxiv.org/abs/2512.17988'>\[1\]</a>; section 2.2, step 3, variable \eqn{M}.
#' @param wins_required Integer. Default: 10. Number of improved designs required at a given depth of the switching search before that level of the search is ended. See <a href='https://arxiv.org/abs/2512.17988'>\[1\]</a>; section 2.2, step 3, variable \eqn{j}.
#' @param max_attempts Integer. Default: 100. Number of designs to search at a given depth of the switching search before that level of the search is ended. Note that this variable ensures that the search for improved designs stops after a sensible number of attempts, even if not enough improvements are found. See <a href='https://arxiv.org/abs/2512.17988'>\[1\]</a>; section 2.2, step 3, variable \eqn{k}.
#' @param pds_local_weight Numeric scalar. Default: 1. Weight to give the \eqn{PDS_{local}} relative to \eqn{PDS_{global}}. See <a href='https://arxiv.org/abs/2512.17988'>\[1\]</a>; section 2.
#' @param patch_weight Numeric scalar. Default: 1/6. Down-weighting for \eqn{PDS_{patch}}, required because \eqn{|patches|=3(|rows|+|columns|)}. See <a href='https://arxiv.org/abs/2512.17988'>\[1\]</a>; section 2. **TO DO: Avi, refactor - currently NULL here and later calculated in code, but should just be set to 1/6 here.**
#' @param plate_col String. Default: "plate". Name of the column specifying which plate each sample belongs to.
#' @param seed Numeric scalar. Default: 1. Seed to set for reproducibility.
#'
#' @returns A data.frame (of class tibble) with the same contents as the input, but with sample locations deconvolved from the clinical variables specified in `columns_for_scoring` argument, and with columns in `cols_to_categorize` converted into bins.
#'
#' @export
#'
#' @examples
#' # Run easyplater in one step
#' easyplater_design <- make_easyplater_design(
#'   manifest_df = input_manifest,
#'   plateID = "plate 1",
#'   columns_for_scoring = c("Cohort","Group","Sex","AgeGroup"),
#'   column_weights = c(5, 5, 10, 4),
#'   cols_to_categorize = list(c("Age", 10, NULL, "AgeGroup")),
#'   plate_size = 96
#' )
#'
#' # Use a function exported from the OlinkAnalyze package to display plate layout
#' olink_displayPlateLayout(data = easyplater_design, fill.color = "Group", include.label = TRUE)
make_easyplater_design <- function(manifest_df, plateID = NULL,
                                   columns_for_scoring, column_weights, cols_to_categorize, imbalance_fixer=FALSE,
                                   plate_size = 96,
                                   internal_control_well_indices = 86:95,
                                   internal_control_ids = c(paste0("SC", 1:2), paste0("NC", 1:3), paste0("PC", 1:5)),
                                   full_mask = NULL, scoring_mask = NULL,
                                   well_pair_distances_df = NULL,
                                   splitting_ss_thresh = 0.5, splitting_wd_thresh = 1,
                                   replacing_ss_thresh = 0.5, replacing_wd_thresh = 6,
                                   max_depth = 2, wins_required = 10, max_attempts = 100,
                                   pds_local_weight=1, patch_weight = NULL,
                                   plate_col = "plate",
                                   seed = 1){

  plateIDs <- manifest_df[[plate_col]] |> unique() |> stringr::str_sort()
  # If no subset of plates is given, run easyplater on all plates
  if (is.null(plateID)) {
    plateID <- plateIDs
  }

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

  # Create a temporarily modified environment with seed set to `seed` input, without changing user's RNG
  withr::with_seed(seed, {
    plate_seeds <- sample(1000000, length(plateIDs))
    names(plate_seeds) <- plateIDs

    easy_plates_list <- list()
    for (p in plateID) {
      # Set seed for reproducibility
      set.seed(plate_seeds[[p]])

      print(paste0("[:::] ", p, " [:::]"))
      print("Getting and formatting plate data from manifest.")
      plate_df_list <- get_and_format_plate_df_from_manifest(manifest_df, p, columns_for_scoring, cols_to_categorize, imbalance_fixer,
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

      print("Storing the easyplater plate design in a data frame.")
      easy_plates_list[[p]] <- make_easyplater_design_aux(plate_df_list, samples_final_order, columns_for_scoring)
    }
  })

  easy_plate_df <- easy_plates_list |> dplyr::bind_rows(.id = plate_col)

  # Convert empty wells to NA. Numeric suffix after "Empty_" is meaningless and NAs display better in plate layouts.
  easy_plate_df <- easy_plate_df |>
    dplyr::mutate(SampleID = ifelse(grepl("Empty_", SampleID, fixed = TRUE),
                                    NA, SampleID))

  return(easy_plate_df)
}
