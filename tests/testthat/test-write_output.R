test_that("write_manifest_excel() errors when each plate doesn't have exactly 96 samples", {
  expect_error(
    object = {
      output_manifest_missing_row <- output_manifest[2:nrow(output_manfest),]
      write_dir = tempdir()
      write_manifest_excel(output_manifest_missing_row, file.path(write_dir, "output_manifest.xlsx"))
      file.remove(file.path(write_dir, "output_manifest.xlsx"))
    },
    class = "simpleError"
  )
})

test_that("write_manifest_excel() writes expected layouts", {
  expect_identical(
    object = {
      write_dir = tempdir()
      write_manifest_excel(output_manifest, file.path(write_dir, "output_manifest.xlsx"))
      layout1 <- readxl::read_xlsx(file.path(write_dir, "output_manifest.xlsx"), sheet = 2)
      file.remove(file.path(write_dir, "output_manifest.xlsx"))
      layout1[1:2,1:3]
    },
    expected = {
      dplyr::tibble("." = c("A", "B"), "1" = c("13", "36"), "2" = c("8", "80"))
    }
  )
})

test_that("write_manifest_excel() reorders rows into the correct order for mapping to layout matrix", {
  expect_identical(
    object = {
      output_manifest_shuffled_rows <- output_manifest[sample(1:nrow(output_manifest), size = nrow(output_manifest)),]
      write_dir = tempdir()
      write_manifest_excel(output_manifest_shuffled_rows, file.path(write_dir, "output_manifest.xlsx"))
      layout1 <- readxl::read_xlsx(file.path(write_dir, "output_manifest.xlsx"), sheet = 2)
      file.remove(file.path(write_dir, "output_manifest.xlsx"))
      layout1[1:2,1:3]
    },
    expected = {
      dplyr::tibble("." = c("A", "B"), "1" = c("13", "36"), "2" = c("8", "80"))
    }
  )
})

test_that("write_manifest_excel() errors when given well ids that deviate from the complete 96-well complete set", {
  expect_error(
    object = {
      output_manifest_well_ids_deviate <- output_manifest
      output_manifest_well_ids_deviate$well[1] <- "a1"
      write_dir = tempdir()
      write_manifest_excel(output_manifest_well_ids_deviate, file.path(write_dir, "output_manifest.xlsx"))
      file.remove(file.path(write_dir, "output_manifest.xlsx"))
    },
    class = "simpleError"
  )
})

test_that("write_plate_layout_html() works on example output manifest", {
  expect_message(
    object = {
      write_dir = tempdir()
      write_plate_layout_html(output_manifest, html_filepath = file.path(write_dir, "plate_layouts.html"))
      file.remove(file.path(write_dir, "plate_layouts.html"))
    },
    regexp = "Output created"
  )
})

test_that("write_plate_layout_html() errors if asked to render pdf document", {
  expect_error(
    object = {
      write_dir = tempdir()
      write_plate_layout_html(output_manifest, html_filepath = file.path(write_dir, "plate_layouts.html"), output_format = "pdf_document")
      file.remove(file.path(write_dir, "plate_layouts.html"))
    },
    class = "simpleError"
  )
})
