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
#' @param imbalance_fixer Boolean or length 4 list. Default: FALSE. Use imbalance_fixer to create a new column (variable) designed to ameliorate a known category imbalance in an existing column which is being scored. In particular, we are interested in addressing the situation where there are several minority categories of a column which individually are assigned to only a small proportion of samples, but together total a more substantial proportion of samples. See User Guide for further details. First element is logical (indicating whether or not to use an imbalance_fixer), second element is character string of the column name with imbalance, third element is a list of minority category values in the column, and fourth element is a numeric scalar weighting for the imbalance_fixer column that will be created. **TO DO: Avi, add explanation to User Guide.**
#' @param plate_size Numeric scalar. Default: 96. Size of plate. Note that `easyplater` does not currently support non-96-well plates.
#' @param fixed_wells Data frame output by [easyplater::assign_fixed_wells]. If this argument is used, `internal_control_well_indices` and `internal_control_ids` will be ignored.
#' @param internal_control_well_indices (Deprecated) Numeric vector containing indices of control wells. Expecting zero index, and numbering going first top to bottom, then left to right. This argument is ignored if `fixed_wells` argument is used.
#' @param internal_control_ids (Deprecated) Character vector. Names of internal control wells. This argument is ignored if `fixed_wells` argument is used.
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
#' ## Run easyplater on a single plate
#'
#' # Decide which wells to keep fixed (not randomized), such as those for internal
#' # controls and deliberately empty wells.
#' # In this example, we have 81 samples and 10 Olink Explore HT internal controls,
#' # and we want to plate all the internal controls in the rightmost two columns
#' # (wells 87-96):
#' plateID <- unique(input_manifest$plate)[1]
#' olink_ht_ic_labels <- c(paste0("SC", 1:2), paste0("NC", 1:3), paste0("PC", 1:5))
#' fixed_wells <- assign_fixed_wells(input_manifest, 87:96, olink_ht_ic_labels)
#'
#' # easyplater's algorithm treats all input columns as discrete, so it's advised
#' # to cut numeric columns with many unique values into bins
#' input_manifest_cut <- input_manifest |>
#'   dplyr::mutate(AgeGroup = ggplot2::cut_interval(Age, 10),
#'                 .by = "plate")
#'
#' # Now we can use easyplater to make a randomized plate design
#' easyplater_design <- make_easyplater_design(
#'   manifest_df = input_manifest_cut,
#'   plateID = plateID,
#'   fixed_wells = fixed_wells,
#'   columns_for_scoring = c("Cohort","Group","Sex","AgeGroup"),
#'   column_weights = c(5, 5, 10, 4),
#'   plate_size = 96
#' )
#'
#' # Use a function exported from the OlinkAnalyze package to display plate layout
#' easyplater_design |>
#'    olink_displayPlateLayout(fill.color = "SampleID", include.label = TRUE) +
#'    ggplot2::theme(legend.position = "none")
#'
#' ## We can run easyplater on multiple plates simply by leaving the "plateID" argument unspecified
#' easyplater_multiplate_design <- make_easyplater_design(
#'   manifest_df = input_manifest_cut,
#'   fixed_wells = fixed_wells,
#'   columns_for_scoring = c("Cohort","Group","Sex","AgeGroup"),
#'   column_weights = c(5, 5, 10, 4),
#'   plate_size = 96
#' )
#'
#' # Use a function exported from the OlinkAnalyze package to display plate layout
#' easyplater_multiplate_design |> split(~plate) |>
#' lapply(function(plate_design) {
#'   plate_design |>
#'     olink_displayPlateLayout(fill.color = "SampleID", include.label = TRUE) +
#'     ggplot2::theme(legend.position = "none")
#' })
#'
make_easyplater_design <- function(manifest_df, plateID = NULL,
                                   columns_for_scoring, column_weights, imbalance_fixer=FALSE,
                                   plate_size = 96,
                                   fixed_wells = NULL,
                                   internal_control_well_indices = NULL,
                                   internal_control_ids = NULL,
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

  well_pair_distances_df <- make_well_distance_df(plate_size)
  well_distances_matrix <- make_well_distances_matrix(plate_size)
  full_mask <- make_full_mask(well_distances_matrix)
  scoring_mask <- make_scoring_mask(well_distances_matrix)

  # Check that fixed_wells has a plate_col column
  if (!is.null(fixed_wells)) {
    if (is.null(fixed_wells[[plate_col]])) {
      stop("fixed_wells must contain a column matching the 'plate_col' argument (default: 'plate')")
    }
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
      sample_df <- manifest_df |> dplyr::filter(.data[[plate_col]] == p)

      # Create fixed_wells if not input by user
      if (!is.null(fixed_wells)) {
        fixed_wells_plate <- fixed_wells |> dplyr::filter(.data[[plate_col]] == p)
        ic_idcs_plate <- fixed_wells_plate$idc - 1
        ic_labs_plate <- fixed_wells_plate$lab
      # If no fixed_wells, but internal_control... (original, deprecated system)
      } else if (!is.null(internal_control_well_indices) &
                 !is.null(internal_control_ids)) {
        fixed_wells_plate <- assign_fixed_wells(
          sample_df,
          internal_control_well_indices+1,
          internal_control_ids,
          randomize_empties = TRUE,
          plate_col = plate_col
          )
        ic_idcs_plate <- fixed_wells_plate$idc - 1
        ic_labs_plate <- fixed_wells_plate$lab
      } else {
        # If nothing is given, the default is to randomize empty wells among samples
        fixed_wells_plate <- NULL
        ic_idcs_plate <- NULL
        ic_labs_plate <- NULL
      }

      plate_df <- make_plate_df(sample_df, fixed_wells_plate, imbalance_fixer, plate_wells)

      # Note: We may want to move the patch_weight calculation from calc_patch_score() up to here, so that this computation isn't repeated with each iteration

      print("Allocating similar samples to distal wells.")
      sample_allocation_outputs <- plate_df |>
        dplyr::select(dplyr::all_of(c("SampleID", columns_for_scoring)),
                      dplyr::any_of(c("imbalanceFix_vec"))) |>
        allocate_similar_samples_to_distal_wells(
          columns_for_scoring, column_weights, imbalance_fixer,
          full_mask, scoring_mask, splitting_ss_thresh,
          ic_labs_plate, ic_idcs_plate,
          plate_num_rows, plate_num_cols, plate_size,
          pds_local_weight, patch_weight
          )

      print("Performing sample switching search.")
      samples_final_order <-  plate_df |>
        dplyr::select(dplyr::all_of(c("SampleID", columns_for_scoring)),
                      dplyr::any_of(c("imbalanceFix_vec"))) |>
        perform_sample_switch_search(
          max_depth, wins_required, max_attempts,
          sample_allocation_outputs,
          well_pair_distances_df,
          splitting_ss_thresh, splitting_wd_thresh,
          replacing_ss_thresh, replacing_wd_thresh,
          columns_for_scoring, column_weights, plate_num_rows, plate_num_cols,
          plate_size, ic_idcs_plate, scoring_mask,
          pds_local_weight, patch_weight
          )

      print("Storing the easyplater plate design in a data frame.")
      easy_plates_list[[p]] <- plate_df |>
        dplyr::select(dplyr::all_of(c("SampleID", columns_for_scoring, "plate", "column", "row", "well"))) |>
        apply_final_well_locations(samples_final_order, columns_for_scoring)
    }
  })

  easy_plate_df <- easy_plates_list |> dplyr::bind_rows(.id = plate_col)

  # Convert empty wells to NA. Numeric suffix after "Empty_" is meaningless and NAs display better in plate layouts.
  easy_plate_df <- easy_plate_df |>
    dplyr::mutate(SampleID = ifelse(grepl("Empty_", SampleID, fixed = TRUE),
                                    NA, SampleID))

  return(easy_plate_df)
}
