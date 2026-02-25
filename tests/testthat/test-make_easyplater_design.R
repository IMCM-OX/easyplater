test_that("example_manifest.csv can be read with readr::read_csv() and is identical to the builtin example_manifest", {
  expect_identical(
    object = {
      readr::read_csv(fs::path_package("extdata", "example_manifest.csv", package = "easyplater"),
                      show_col_types = FALSE)
    },
    expected = input_manifest
  )
})

test_that("example_manifest.csv can be read with readr::read_csv() and is identical to the manifest_df test fixture", {
  expect_identical(
    object = {
      readr::read_csv(fs::path_package("extdata", "example_manifest.csv", package = "easyplater"),
                      show_col_types = FALSE)
    },
    expected = readRDS(test_path("fixtures", "manifest_df.rds"))
  )
})

test_that("example_manifest.csv can be read with utils::read.csv() and the contents are identical to the builtin example_manifest (except for attributes)", {
  expect_identical(
    object = {
      readr::read_csv(fs::path_package("extdata", "example_manifest.csv", package = "easyplater"),
                      show_col_types = FALSE)
    },
    expected = input_manifest,
    ignore_attr = TRUE
  )
})

test_that("make_easyplater_design() returns the same single plate as the original example", {
  expect_identical(
    object = {
      manifest_df <- readr::read_csv(fs::path_package("extdata", "example_manifest.csv", package = "easyplater"),
                                     show_col_types = FALSE)
      # Prep masks
      well_pair_distances_df <- easyplater:::make_well_distance_df(96)
      well_distances_matrix <- easyplater:::make_well_distances_matrix(96)
      full_mask <- easyplater:::make_full_mask(well_distances_matrix)
      scoring_mask <- easyplater:::make_scoring_mask(well_distances_matrix)
      # Run easyplater
      make_easyplater_design(
        manifest_df = manifest_df,
        plateID = "plate 1",
        columns_for_scoring = c("Cohort","Group","Sex","AgeGroup"),
        column_weights = c(5, 5, 10, 4),
        cols_to_categorize = list(c("Age", 10, NULL, "AgeGroup")),
        plate_size = 96,
        internal_control_well_indices = 86:95,
        internal_control_ids = c(paste0("SC", 1:2), paste0("NC", 1:3), paste0("PC", 1:5)),
        full_mask = full_mask,
        scoring_mask = scoring_mask,
        well_pair_distances_df = well_pair_distances_df,
        splitting_ss_thresh = 0.5,
        splitting_wd_thresh = 1,
        replacing_ss_thresh = 0.5,
        replacing_wd_thresh = 6,
        max_depth = 2,
        wins_required = 10,
        max_attempts = 100
      )
    },
    expected = readRDS(test_path("fixtures", "easy_plate_df.rds"))
  )
})

test_that("make_easyplater_design() returns the same single plate as the original example with minimal params", {
  expect_identical(
    object = {
      manifest_df <- readr::read_csv(fs::path_package("extdata", "example_manifest.csv", package = "easyplater"),
                                     show_col_types = FALSE)
      make_easyplater_design(
        manifest_df = manifest_df,
        plateID = "plate 1",
        columns_for_scoring = c("Cohort","Group","Sex","AgeGroup"),
        column_weights = c(5, 5, 10, 4),
        cols_to_categorize = list(c("Age", 10, NULL, "AgeGroup")),
        plate_size = 96
      )
    },
    expected = readRDS(test_path("fixtures", "easy_plate_df.rds"))
  )
})

test_that("make_easyplater_design() returns the same single plate as the original example with imbalance_fixer", {
  expect_identical(
    object = {
      manifest_df <- readr::read_csv(fs::path_package("extdata", "example_manifest.csv", package = "easyplater"),
                                     show_col_types = FALSE)
      make_easyplater_design(
        manifest_df = manifest_df,
        plateID = "plate 1",
        columns_for_scoring = c("Cohort","Group","Sex","AgeGroup"),
        column_weights = c(5, 5, 10, 4),
        cols_to_categorize = list(c("Age", 10, NULL, "AgeGroup")),
        imbalance_fixer = list(T, "Group", list("D1", "HC1", "D7", "D8"), 3),
        plate_size = 96
      )
    },
    expected = readRDS(test_path("fixtures", "easy_plate_df_imbalance_fixer.rds"))
  )
})

test_that("make_easyplater_design() exits with error if plate_size != 96", {
  expect_error(
    object = {
      manifest_df <- readr::read_csv(fs::path_package("extdata", "example_manifest.csv", package = "easyplater"),
                                     show_col_types = FALSE)
      # Make a list of the plates:
      plates <- stringr::str_sort(unique(manifest_df$plate), numeric = TRUE)
      make_easyplater_design(
        manifest_df = manifest_df,
        plateIDs = plates[1],
        columns_for_scoring = c("Cohort","Group","Sex","AgeGroup"),
        column_weights = c(5, 5, 10, 4),
        cols_to_categorize = list(c("Age", 10, NULL, "AgeGroup")),
        imbalance_fixer = list(T, "Group", list("D1", "HC1", "D7", "D8"), 3),
        plate_size = 48
      )
    },
    class = "simpleError"
  )
})

test_that("make_easyplater_design() returns the same multi-plate manifest as the original example", {
  expect_identical(
    object = {
      manifest_df <- readr::read_csv(fs::path_package("extdata", "example_manifest.csv", package = "easyplater"),
                                     show_col_types = FALSE)
      make_easyplater_design(
        manifest_df = manifest_df,
        plateID = NULL,
        columns_for_scoring = c("Cohort","Group","Sex","AgeGroup"),
        column_weights = c(5, 5, 10, 4),
        cols_to_categorize = list(c("Age", 10, NULL, "AgeGroup")),
        plate_size = 96
      )
    },
    expected = readRDS(test_path("fixtures", "easy_multiplate_df.rds"))
  )
})
