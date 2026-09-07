lets <- NULL

#' Create a data frame for fixed wells on a plate
#'
#' Given a number of samples and the location of known fixed wells (i.e. internal controls), returns a dataframe of all wells on the plate that should be fixed in place, including spaces for empty wells.
#'
#' @inheritParams make_easyplater_design
#' @param ic_idcs Numeric vector. Indices of internal controls, filling columnwise from left to right (i.e. 1=A1, 2=B1)
#' @param ic_labs Character vector. Labels of internal controls.
#' @param fill_rowwise Logical. Whether to fill samples rowwise (default: FALSE)
#' @param fill_from_bottom Logical. Whether to fill samples from bottom (default: FALSE)
#' @param randomize_empties Logical. Whether to assign empty wells to fixed wells (i.e. treat them like internal controls, default) or randomize them (i.e. treat them like samples). (default: FALSE)

#'
#' @returns A data frame of fixed wells, their labels and indices.
#' @export
#'
#' @examples
#' # Prepare for a plate with 80 samples and 10 internal controls per Olink Explore HT
#' input_plate <- input_manifest[input_manifest$plate == "plate 1",]
#' fixed_idcs <- 87:96
#' fixed_labs <- c(paste0("SC", 1:2), paste0("NC", 1:3), paste0("PC", 1:5))
#' assign_fixed_wells(input_plate, fixed_idcs, fixed_labs)
#'
#' # Prepare for a plate with 80 samples and 10 internal controls per Alamar NULISAseq
#' fixed_idcs <- (3:12)*8
#' fixed_labs <- paste0("IC", 1:10)
#' assign_fixed_wells(input_plate, fixed_idcs, fixed_labs, fill_rowwise = TRUE)
#'
#' # Randomize empty wells among samples, rather than grouping them into fixed wells
#' assign_fixed_wells(input_plate, fixed_idcs, fixed_labs, randomize_empties = TRUE)
#'
#' # We can also prepare multiple plates at once, assuming each have the same fixed wells and labels
#' input_plates <- input_manifest[input_manifest$plate %in% c("plate 1", "plate 2"),]
#' assign_fixed_wells(input_manifest, fixed_idcs, fixed_labs)
#'
#' # To assign different fixed wells across plates, pass a list of indices and labels
#' input_plates <- input_manifest[input_manifest$plate %in% c("plate 1", "plate 2"),]
#' fixed_idcs <- list(
#'   "plate 1" = 87:96,
#'   "plate 2" = 85:96
#' )
#' fixed_labs <- list(
#'   "plate 1" = c(paste0("SC", 1:2), paste0("NC", 1:3), paste0("PC", 1:5)),
#'   "plate 2" = c("BR1", "BR2", paste0("SC", 1:2), paste0("NC", 1:3), paste0("PC", 1:5))
#' )
#' assign_fixed_wells(input_plates, fixed_idcs, fixed_labs)
assign_fixed_wells <- function(
    manifest_df, ic_idcs, ic_labs, plate_size = 96,
    fill_rowwise = FALSE, fill_from_bottom = FALSE,
    randomize_empties = FALSE, plate_col = "plate") {

  if (is.null(manifest_df[[plate_col]])) {
    stop("manifest_df must contain a column referring to the plate id")
  }
  plate_names <- unique(manifest_df[[plate_col]])
  if (!is.list(ic_idcs)) {
    ic_idcs <- lapply(plate_names, function(x) ic_idcs) |> stats::setNames(plate_names)
  }
  if (!is.list(ic_labs)) {
    ic_labs <- lapply(plate_names, function(x) ic_labs) |> stats::setNames(plate_names)
  }
  fixed_wells <- plate_names |>
    lapply(function(p) {
      n_samples_plate <- sum(manifest_df[[plate_col]] == p)
      ic_idcs_plate <- ic_idcs[[p]]
      ic_labs_plate <- ic_labs[[p]]
      assign_fixed_wells_plate(
        n_samples = n_samples_plate,
        ic_idcs = ic_idcs_plate,
        ic_labs = ic_labs_plate,
        plate = p,
        plate_col = plate_col,
        plate_size = plate_size,
        fill_rowwise = fill_rowwise,
        fill_from_bottom = fill_from_bottom,
        randomize_empties = randomize_empties)
    }) |>
    dplyr::bind_rows()

  fixed_wells
}

# # Prepare for a plate with 80 samples and 10 internal controls per Olink Explore HT
# assign_fixed_wells_plate(80, 87:96, c(paste0("SC", 1:2), paste0("NC", 1:3), paste0("PC", 1:5)), "plate 1")
#
# # Prepare for a plate with 80 samples and 10 internal controls per Alamar NULISAseq
# assign_fixed_wells_plate(80, (3:12)*8, paste0("IC", 1:10), "plate 1")
#
# # Randomize empty wells among samples, rather than grouping them into fixed wells
# assign_fixed_wells_plate(80, 87:96, c(paste0("SC", 1:2), paste0("NC", 1:3), paste0("PC", 1:5)),
#                          "plate 1", randomize_empties = TRUE)
assign_fixed_wells_plate <- function(n_samples, ic_idcs, ic_labs, plate, plate_col = "plate",
                                     plate_size = 96,
                                     fill_rowwise = FALSE, fill_from_bottom = FALSE,
                                     randomize_empties = FALSE) {
  if (length(ic_idcs) != length(ic_labs)) {
    stop("ic_idcs and ic_labs must be the same length")
  }
  if (any(ic_idcs > plate_size)) {
    stop("ic_idcs cannot be higher than plate_size")
  }

  if (plate_size == 96) {
    all_wells <- paste0(rep(LETTERS[1:8], times = 12), rep(1:12, each = 8))
  } else {
    stop("Plate size must be 96.")
  }

  ic_wells = all_wells[ic_idcs]
  # Fill available wells with samples
  nonic_wells <- all_wells[!(all_wells %in% ic_wells)]
  if (fill_rowwise) {
    nonic_df <- dplyr::tibble(
      lets = substr(nonic_wells, 1, 1),
      nums = substr(nonic_wells, 2, 3)
    )
    if (!fill_from_bottom) {
      nonic_df <- nonic_df |> dplyr::arrange(lets)
    } else if (fill_from_bottom) {
      nonic_df <- nonic_df |> dplyr::arrange(dplyr::desc(lets))
    }
    nonic_wells <- paste0(nonic_df$lets, nonic_df$nums)
  }
  sample_wells <- nonic_wells[1:n_samples]

  if (!randomize_empties) {
    # Allocate remaining wells as "empty"
    empty_wells <- nonic_wells[!(nonic_wells %in% sample_wells)]
    if (length(empty_wells > 0)) {
      empty_labs <- paste0("Empty_", empty_wells)
    } else {
      empty_labs <- NULL
    }
    # Get empty indices
    empty_idcs <- match(empty_wells, all_wells)
  } else {
    empty_idcs <- empty_wells <- empty_labs <- NULL
  }

  # Create dataframe with indices, well codes and labels for more straightforward input to easyplater
  fixed_df <- dplyr::tibble(
    "{plate_col}" := plate,
    idc = c(ic_idcs, empty_idcs) |> as.integer(),
    well = c(ic_wells, empty_wells),
    lab = c(ic_labs, empty_labs)
  )

  fixed_df
}

#' Create  sample plate locations to existing fixed wells dataframe
#'
#' Given a data frame of samples and a data frame of fixed wells, pre-allocate samples to wells. These are not the final locations, they are just initialized locations to use at the start of randomization.
#'
#' @param sample_df Data frame where each sample is represented in a row. Cannot have more rows than `plate_size`.
#' @param fixed_wells Wells that are reserved for non-samples (i.e. internal controls or empty wells).
#' @param plate_size Scalar numeric. Number of wells on plate (default: 96).
#'
#' @returns `sample_df` with three additional columns: `well`, `row` and `column`.
#' @export
#'
#' @examples
#' # Say we have 86 samples we want to allocate to wells on a plate with
#' # 10 internal controls in the bottom row
#' sample_df <- input_manifest[1:86,] # first 86 samples without starting wells
#' fixed_wells <- paste0("H", 3:12)
#' add_sample_wells(sample_df, fixed_wells)
add_sample_wells <- function(sample_df, fixed_wells, plate_size = 96) {
  # SampleID shouldn't be numeric or factor
  sample_df <- sample_df |> dplyr::mutate(SampleID = as.character(SampleID))
  # Throw away any location columns if they exist
  sample_df <- sample_df |> dplyr::select(-dplyr::any_of(c("column", "row", "well")))

  if (plate_size == 96) {
    all_wells <- paste0(rep(LETTERS[1:8], times = 12), rep(1:12, each = 8))
  } else {
    stop("Plate size must be 96.")
  }
  # If empty wells were not assigned to fixed wells, warn that they will be randomized.
  if ((nrow(sample_df) + length(fixed_wells)) != plate_size) {
    message("Empty wells will be randomized among samples. Use `fixed_wells` argument to avoid this behavior.")
  }

  nonfixed_wells <- all_wells[!(all_wells %in% fixed_wells)]
  sample_wells <- nonfixed_wells[1:nrow(sample_df)]

  # Allocate remaining wells as "empty"
  empty_wells <- nonfixed_wells[!(nonfixed_wells %in% sample_wells)]
  if (length(empty_wells > 0)) {
    empty_labs <- paste0("Empty_", empty_wells)
  } else {
    empty_labs <- NULL
  }

  # Add sample well (but not fixed well) locations to sample_df to be used as input for easyplater
  df_to_join <- dplyr::tibble(
      SampleID = c(sample_df$SampleID, empty_labs),
      well = c(sample_wells, empty_wells)
    ) |>
    dplyr::mutate(
      row = substr(well, 1, 1),
      column = paste0("Column ", substr(well, 2, 3))
      ) |>
    # To align with historical test/example objects
    dplyr::select(dplyr::all_of(c("SampleID", "column", "row", "well")))

  sample_df |>
    dplyr::full_join(df_to_join, by = c("SampleID"))
}


#' Add imbalance fixer column
#'
#' This is a deprecated functionality, kept for historical reasons but not exported to the user.
#'
#' @param plate_df Data frame that includes a column matching the second element of the imbalance_fixer argument.
#' @param imbalance_fixer List of 4 elements. First is logical. Second is the column name to adjust. Third is a list of factor levels from that group to adjust. Fourth is a weight to apply in `make_ss_matrices()`.
#'
#' @returns plate_df with new "imbalanceFix_vec" column.
#'
#' @examples
#' #
#' imbalance_fixer <- list(TRUE,"Group",list("D1","HC1","D7","D8"),3)
#' sample_df <- input_manifest[1:81,] # manifest for first plate
#' easyplater:::add_imbalance_fixer(sample_df, imbalance_fixer)
add_imbalance_fixer <- function(plate_df, imbalance_fixer) {
  imbfix_col <- imbalance_fixer[[2]]
  imbfix_levels <- imbalance_fixer[[3]] |> unlist()
  plate_df$imbalanceFix_vec <- plate_df$SampleID
  samps_these_levels <- plate_df[[imbfix_col]] %in% imbfix_levels
  plate_df$imbalanceFix_vec[!samps_these_levels] <- "1"

  # Note: imbalanceFix_vec should be made NA for any fixed wells (empty or ic).
  # Should do this in make_easyplater_design

  plate_df
}


