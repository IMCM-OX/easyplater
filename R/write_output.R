# Avoid “no visible binding” R CMD CHECK note:
well <- let <- num <- NULL

#' @export
#' @importFrom OlinkAnalyze olink_displayPlateLayout
OlinkAnalyze::olink_displayPlateLayout

#' Write plate manifest to Excel spreadsheet
#'
#' Write the plate manifest output by [easyplater::make_easyplater_design] to an excel spreadsheet.
#'
#' @inheritParams manifest2layouts
#' @param file String. File to write to.
#' @param rowwise Logical. Arrange manifest as if filling plates rowwise, rather than columnwise.
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
                                 plate_size = 96,
                                 rowwise = FALSE) {

  plate_layouts <- manifest2layouts(manifest_df)

  # Arrange wells in manifest rowwise for some platforms (e.g. NULISA)
  if (rowwise) {
    manifest_df <- split(manifest_df, manifest_df[[plate_col]]) |>
      lapply(\(plate_df) {
        ordered_wells <- gtools::mixedsort(plate_df$well)
        plate_df <- plate_df[match(ordered_wells, plate_df$well),]
        return(plate_df)
      }) |> dplyr::bind_rows(.id = plate_col)
  }

  # Arrange by plates in correct order, not alphabetically
  manifest_df <- manifest_df |>
    dplyr::slice(gtools::mixedorder(.data[[plate_col]]))

  c(
    list(Manifest = manifest_df),
    plate_layouts
  ) |>
    writexl::write_xlsx(path = file.path(file))

  invisible(manifest_df)
}


#' Write plate layouts to html report
#'
#' Writes plate layouts from an easyplater output manifest to an html report. Each plate is in a separate section, and each variable is organized into tabs.
#'
#' ## Customizing the html output
#' The default R Markdown template wrapping [OlinkAnalyze::olink_displayPlateLayout()] and specifying how to format the report can be found by running `fs::path_package("extdata", "plate_layouts-format.Rmd", package = "easyplater")`. To customize the output html format, you can copy this file and edit it as desired and pass it to `write_plate_layout_html()` with the `rmd_template` argument.
#'
#' @param manifest_df Data frame (or tibble). Output of [easyplater::make_easyplater_design()].
#' @param html_filepath String. Path to output file.
#' @param plate_col String. Name of column indicating the plate ID.
#' @param color_by String. Names of columns containing variables to be displayed in plate wells. Defaults to all columns except "plate", "column", "row", "well", and the one specified by the `plate_col` argument.
#' @param plate_size Numeric. Note: This function is currently only implemented for 96-well plates.
#' @param include_label Character vector. Names of variables to be labeled in plate wells. Defaults to be identical to `color_by` argument. It can be handy to leave out columms with variables that are too large to display.
#' @param include_legend Character vector. Names of variables for which to plot a legend. Defaults to be identical to `color_by` argument, except "SampleID". It can be handy to leave out columms with too many unique values to display in a legend.
#' @param html_title String. Main title of html report.
#' @param fig_height Numeric. Figure height in inches.
#' @param fig_width Numeric. Figure width in inches.
#' @param rmd_template The input R Markdown file to be rendered. See `Customizing the html output` below for details.
#'
#' @returns The compiled document is written into the output file, and the path of the output file is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' write_plate_layout_html(output_manifest)
#' }
write_plate_layout_html <- function(manifest_df,
                                    html_filepath = "plate_layouts.html",
                                    plate_col = "plate",
                                    color_by = NULL,
                                    plate_size = 96,
                                    include_label = NULL,
                                    include_legend = NULL,
                                    html_title = "Plate layouts",
                                    fig_height = 8,
                                    fig_width = 10,
                                    rmd_template = NULL) {

  rlang::check_installed(c("rmarkdown"), reason = "to use `write_plate_layout_html()`")

  # Check that plate size is 96
  if (plate_size != 96) {
    stop("plate_size (", plate_size, ") != 96: write_plate_layout_html() is currently only implemented for 96-well plates")
  }

  if (is.null(rmd_template)) {
    rmd_template <- fs::path_package("extdata", "plate_layouts-format.Rmd", package = "easyplater")
  }

  if (is.null(color_by)) {
    color_by <- setdiff(colnames(manifest_df), c(plate_col, "plate", "column", "row", "well"))
  }

  if (is.null(include_label)) {
    include_label <- color_by
  }

  if (is.null(include_legend)) {
    include_legend <- color_by[!color_by %in% "SampleID"]
  }

  plate_list <- split(manifest_df, manifest_df[[plate_col]])
  # Arrange by plates in correct order, not alphabetically
  plate_list <- plate_list[gtools::mixedorder(names(plate_list))]

  html_dir <- dirname(html_filepath)
  html_file <- basename(html_filepath)

  rmarkdown::render(input = rmd_template,
                    output_dir = html_dir,
                    output_file = html_file)
}

#' Convert manifest into an 8 x 12 plate layout matrices
#'
#' Takes output manifest from [easyplater::make_easyplater_design] and generates a 8 x 12 layout matrix for each plate.
#'
#' @param manifest_df A data frame or tibble in a format matching the output of make_easyplater_design().
#' @param plate_col String. Name of column indicating the plate that samples belong to.
#' @param display_col String. Column to draw labels for plate layout from.
#' @param plate_size Numeric. Size of plate. Currently, anything other than 96 will return error.
#'
#' @returns List of plate layout matrices, named by plate.
#'
#' @export
#'
#' @examples
#' ## We can use the built-in output_manifest loaded with easyplater
#' manifest2layouts(output_manifest)
#' #> $`plate 1`
#' #> .  1  2  3  4    5    6    7    8    9 10  11  12
#' #> 1 A 13  8 14  2   42   16    9   18   38 54  35 NC1
#' #> 2 B 36 80 75 46   62 <NA>    5 <NA>   53 12  28 NC2
#' #> 3 C 64 33 43 76 <NA>   10   15   30   77 26  48 NC3
#' #> 4 D 63 22 45 79   37   51   24    3   32 57  67 PC1
#' #> 5 E 58 17 20 34   69   70 <NA>   44    4  7  78 PC2
#' #> 6 F 66 71 39 29   19   41   23   59 <NA> 50  52 PC3
#' #> 7 G 74 73 25 56   60   31   55   68   49 40 SC1 PC4
#' #> 8 H 47 65 21 72   11    6    1   61   27  0 SC2 PC5
#' #
#' #> $`plate 2`
#' #> .    1    2    3    4    5    6    7   8    9   10   11  12
#' #> 1 A  995  967  984  981  999  988  972 950 1009  947  955 NC1
#' #> 2 B  952 1018  962 1022  949  961  993 944  991 1021  941 NC2
#' #> 3 C  960  974 1004  945 1013 1005 1012 954  957  965 1014 NC3
#' #> 4 D 1002 1017 1008  987 1025  994  951 982 1020  979  983 PC1
#' #> 5 E  953  946 1001 1010  959  956 1011 942 1026  973  977 PC2
#' #> 6 F  968 1015 1023 1000 1006  970 1003 976  980  969  966 PC3
#' #> 7 G  998  963  996 1016 1019  958  990 989  997  964  SC1 PC4
#' #> 8 H 1024  992  986  978  975 1007  971 943  985  948  SC2 PC5
manifest2layouts <- function(manifest_df,
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
  plate_layouts <- plate_layouts[gtools::mixedorder(names(plate_layouts))]

  plate_layouts
}
