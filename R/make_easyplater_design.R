SampleID <- NULL

#' Design a plate using the easyplater algorithm
#'
#' Given a manifest, run the easyplater algorithm on a single plate. **TO DO: Avi and/or Micah, elaborate on this.**
#'
#' @param manifest_df Data frame or tibble with a `SampleID` column, a plate column (default: "plate", but this can be changed with `plate_col` argument), and additional columns for each variable to be used for plate design score. See [easyplater::input_manifest] for an example.
#' @param plateID Character vector. Value from column specified by `plate_col` to generate a  deconvolved plate for. If NULL (default), every plate in the manifest will be deconvolved.
#' @param columns_for_scoring Character vector. Names of columns to use for calculating plate design score.
#' @param column_weights Numeric vector of weights to use for the variables in `columns_for_scoring`. Must be same length as `columns_for_scoring`.
#' @param imbalance_fixer FALSE (default) or length 4 list. First element is logical, second element is character string of a column name, third element is a list of well IDs, and fourth element is a numeric scalar. **TO DO: Avi, explain this.**
#' @param plate_size Numeric scalar. Size of plate. Note that currently `easyplater` is currently only implemented for 96-well plates.
#' @param fixed_wells Data frame output by [easyplater::assign_fixed_wells]. If this argument is used, `internal_control_well_indices` and `internal_control_ids` will be ignored.
#' @param internal_control_well_indices (Deprecated) Numeric vector containing indices of control wells. Expecting zero index, and numbering going first top to bottom, then left to right. This argument is ignored if `fixed_wells` argument is used.
#' @param internal_control_ids (Deprecated) Character vector. Names of internal control wells. This argument is ignored if `fixed_wells` argument is used.
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
#' @param patch_weight Down-weighting for \eqn{PDS_{patch}}, required because \eqn{|patches|=3(|rows|+|columns|)}. If NULL, weight is calculated automatically. Default value for 96-well plates is 1/6. **TO DO: Avi, check this description**
#' @param plate_col String. Name of the column specifying which plate each sample belongs to. Default: "plate".
#' @param seed Numeric scalar. Seed to set for reproducibility. Default: 1.
#'
#' @returns A data.frame (of class tibble) with the same contents as the input, but with sample locations deconvolved from the clinical variables specified in `columns_for_scoring` argument, and with columns in `cols_to_categorize` converted into bins.
#'
#' @export
#'
#' @examples
#' ## Run easyplater
#'
#' # Decide which wells to keep fixed (not randomized), such as those for internal
#' # controls and deliberately empty wells.
#' # In this example, we have 81 samples and 10 Olink Explore HT internal controls,
#' # and we want to plate all the internal controls in the rightmost two columns
#' # (wells 87-96):
#' n_samples_plate1 <- sum(input_manifest$plate == "plate 1") # 81
#' olink_ht_ic_labels <- c(paste0("SC", 1:2), paste0("NC", 1:3), paste0("PC", 1:5))
#' fixed_wells <- assign_fixed_wells(n_samples_plate1, 87:96, olink_ht_ic_labels)
#'
#' # easyplater's algorithm treats all input columns as discrete, so it's advised
#' # to cut numeric columns with many unique values into bins
#' input_manifest_cut <- input_manifest |>
#'   dplyr::mutate(AgeGroup = ggplot2::cut_interval(Age, 10) |> as.numeric(),
#'                 .by = "plate")
#'
#' # Now we can use easyplater to make a randomized plate design
#' easyplater_design <- make_easyplater_design(
#'   manifest_df = input_manifest_cut,
#'   plateID = "plate 1",
#'   fixed_wells = fixed_wells,
#'   columns_for_scoring = c("Cohort","Group","Sex","AgeGroup"),
#'   column_weights = c(5, 5, 10, 4),
#'   plate_size = 96
#' )
#'
#' # Use a function exported from the OlinkAnalyze package to display plate layout
#' olink_displayPlateLayout(data = easyplater_design, fill.color = "Group", include.label = TRUE)
make_easyplater_design <- function(manifest_df, plateID = NULL,
                                   columns_for_scoring, column_weights, imbalance_fixer=FALSE,
                                   plate_size = 96,
                                   fixed_wells = NULL,
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
      sample_df <- manifest_df |> dplyr::filter(.data[[plate_col]] == p)

      # Create fixed_wells if not input by user
      if (!is.null(fixed_wells)) {
        internal_control_well_indices <- fixed_wells$idc-1
        internal_control_ids <- fixed_wells$lab
      } else if (is.null(fixed_wells) &
                 !is.null(internal_control_well_indices) &
                 !is.null(internal_control_ids)) {
        fixed_wells <- assign_fixed_wells(nrow(sample_df),
                                          internal_control_well_indices+1,
                                          internal_control_ids,
                                          randomize_empties = TRUE)
      } else {
        stop("fixed_wells was not supplied to make_easyplater_design() and neither were (deprecated) internal_control_well_indices nor internal_control_ids ")
      }

      plate_df <- make_plate_df(sample_df, fixed_wells, imbalance_fixer, plate_wells)

      # Note: We may want to move the patch_weight calculation from calc_patch_score() up to here, so that this computation isn't repeated with each iteration

      print("Allocating similar samples to distal wells.")
      sample_allocation_outputs <- plate_df |>
        dplyr::select(dplyr::all_of(c("SampleID", columns_for_scoring)),
                      dplyr::any_of(c("imbalanceFix_vec"))) |>
        allocate_similar_samples_to_distal_wells(
          columns_for_scoring, column_weights, imbalance_fixer,
          full_mask, scoring_mask, splitting_ss_thresh,
          internal_control_ids, internal_control_well_indices,
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
          plate_size, internal_control_well_indices,scoring_mask,
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
