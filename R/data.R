#' Example manifest
#'
#' An example manifest in a format compatible with the `easyplater`'s well randomization algorithm.
#'
#' @format A data frame with 423 rows and 8 columns.
"example_manifest"

#' Example pre-processed plate data frame
#'
#' An example pre-processed plate data frame, ready for input to internal functions such as `calc_pds()`.
#'
#' @format A data frame with 96 rows and 6 columns.
#' The contents are the same as `example_manifest` except that:
#'
#' - all each column is a character vector
#' - `Age` has been replaced with `AgeGroup` using `ggplot::cut_interval()`
#' - `imbalanceFix_vec` has been added *TO DO: Avi, briefly describe purpose of `imbalanceFix`*
#'
"example_plate_df"
