#' Example input manifest
#'
#' An example manifest in a format compatible with the `easyplater`'s well randomization algorithm.
#'
#' @format A data frame with 423 rows and 8 columns.
"input_manifest"

#' Example output manifest
#'
#' An example manifest created by running `easyplater::make_easyplater_design`.
#'
#' @format A data frame with 480 rows and 8 columns.
"output_manifest"

#' Example pre-processed plate data frame
#'
#' An example pre-processed plate data frame, ready for input to internal functions such as `calc_pds()`.
#'
#' @format A data frame with 96 rows and 6 columns.
#' The contents are the same as `example_manifest` except that:
#'
#' - all each column is a character vector
#' - `Age` has been replaced with `AgeGroup` using `ggplot::cut_interval()`
#' - `imbalanceFix_vec` has been added. Length 4 list (note that default value is FALSE if not set). Use imbalance_fixer to create a new column (variable) designed to ameliorate a known category imbalance in an existing column which is being scored. In particular, we are interested in addressing the situation where there are several minority categories of a column which individually are assigned to only a small proportion of samples, but together total a more substantial proportion of samples. See User Guide for further details. First element is logical (indicating whether or not to use an imbalance_fixer), second element is character string of the column name with imbalance, third element is a list of minority category values in the column, and fourth element is a numeric scalar weighting for the imbalance_fixer column that will be created.
#'
"example_plate_df"
