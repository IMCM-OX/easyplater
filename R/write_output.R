# Avoid “no visible binding” R CMD CHECK note:
well <- let <- num <- NULL

#' @export
#' @importFrom OlinkAnalyze olink_displayPlateLayout
OlinkAnalyze::olink_displayPlateLayout

#' Write plate manifest to Excel spreadsheet
#'
#' Write the plate manifest output by [easyplater::make_easyplater_design] to an excel spreadsheet.
#'
#' @param manifest_df A data frame or tibble to write to disk.
#' @param file String. File to write to.
#' @param plate_col String. Name of column indicating the plate that samples belong to.
#' @param display_col String. Column to draw labels for plate layout from.
#' @param plate_size Numeric. Size of plate. Currently, anything other than 96 will return error.
#'
#' @returns Returns input `manifest_df` invisibly.
#'
#' @section Output:
#' The first sheet in the output is a tabular manifest with the same contents as x. Additional sheets contain a plate layout matrix for each plate specified by the column controlled by the `plate_col` argument.
#'
#' @export
#'
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' # If a filename is given without a path, write_manifest_excel() will write
#' # the file to the current working directory.
#' write_manifest_excel(output_manifest, "output_manifest.xlsx")
#'
#' \dontshow{
#' file.remove("output_manifest.xlsx")
#' setwd(.old_wd)
#' }
write_manifest_excel <- function(manifest_df, file,
                                 plate_col = "plate",
                                 display_col = "SampleID",
                                 plate_size = 96) {

  # Check that plate size is 96
  if (plate_size == 96) {
    plate_num_rows <- 8
    plate_num_cols <- 12
  } else {
    stop("plate_size (", plate_size, ") != 96: write_manifest_excel() is currently only implemented for 96-well plates")
  }

  # Check that total number of input wells are a multiple of plate size
  if (nrow(manifest_df) %% plate_size != 0) {
    stop("nrow(manifest_df) (", nrow(manifest_df),") must be a multiple of plate_size (", plate_size, ")")
  }

  # Check that wells are in A1 or A01 format
  A1_wells <- paste0(rep(LETTERS[1:plate_num_rows], times = plate_num_cols),
                     rep(1:plate_num_cols, each = plate_num_rows))

  A01_wells <- paste0(rep(LETTERS[1:plate_num_rows], times = plate_num_cols),
                      rep(stringr::str_pad(1:plate_num_cols, 2, pad = "0"), each = plate_num_rows))

  tryCatch({
    manifest_wells <- manifest_df |> dplyr::distinct(well) |>
      tidyr::separate_wider_regex(well, patterns = c('let' = '^[A-Z]*', 'num' = '[0-9]*$'), cols_remove = FALSE) |>
      dplyr::arrange(as.numeric(num), let) |>
      dplyr::pull(well)
  }, error = function(msg) {
    message("Error: `well` does not start with capital A-Z and end with 0-9.")
  })

  wells_in_A1 <- sum(manifest_wells %in% A1_wells) == length(A1_wells)
  wells_in_A01 <- sum(manifest_wells %in% A01_wells) == length(A01_wells)

  if (!(wells_in_A1 | wells_in_A01)) {
    stop("'well' column contains unexpected well IDs. Well IDs must be in 'A1' or 'A01' format.")
  }

  # Construct plate layouts
  plate_layouts <- split(manifest_df, manifest_df[[plate_col]]) |>
    lapply(\(plate_df) {
      # Reorder well ids so that they fill the plate layout matrix by column (not by row)
      if (wells_in_A1) {
        plate_df <- plate_df[match(A1_wells, plate_df$well),]
      } else if (wells_in_A01) {
        plate_df <- plate_df[match(A01_wells, plate_df$well),]
      }
      plate_layout <- matrix(plate_df[[display_col]], nrow = 8, ncol = 12, byrow = FALSE)
      colnames(plate_layout) <- 1:12
      plate_layout <- data.frame("." = LETTERS[1:8]) |> cbind(plate_layout)

      return(plate_layout)
    })

  c(
    list(Manifest = manifest_df),
    plate_layouts
  ) |>
    writexl::write_xlsx(path = file.path(file))

  invisible(manifest_df)
}

write_plate_layout_html <- function(manifest_df, html_filepath = "plate_layouts.html",
                                    plate_col = "plate",
                                    display_cols = NULL,
                                    plate_size = 96,
                                    include_label = TRUE,
                                    html_title = "Plate layouts") {

  if (is.null(display_cols)) {
    display_cols <- setdiff(colnames(output_manifest), c("plate", "column", "row", "well"))
  }

  plate_list <- split(manifest_df, manifest_df[[plate_col]])

  html_dir <- dirname(html_filepath)
  html_file <- basename(html_filepath)

  rmarkdown::render(input = fs::path_package("rmd", "plate_layouts-format.Rmd", package = "easyplater"),
                    output_dir = html_dir,
                    output_file = html_file)
}
