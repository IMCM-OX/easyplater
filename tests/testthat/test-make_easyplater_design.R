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

#### Using "fixed_wells", not "internal_control_well_indices" ####

test_that("make_easyplater_design() returns the expected output using `fixed_wells`", {
  expect_identical(
    object = {
      # Must cut Age into discrete groups, as `cols_to_categorize` is no longer supported
      input_manifest_cut <- input_manifest |>
        dplyr::mutate(AgeGroup = ggplot2::cut_interval(Age, 10) |> as.numeric(),
                      .by = "plate")
      # Decide which wells to keep fixed (not randomized), such as those for internal
      # controls and deliberately empty wells:
      n_samples_plate1 <- sum(input_manifest$plate == "plate 1") # 81
      olink_ht_ic_labels <- c(paste0("SC", 1:2), paste0("NC", 1:3), paste0("PC", 1:5))
      fixed_wells <- assign_fixed_wells(n_samples_plate1, 87:96, olink_ht_ic_labels)
      # Run easyplater
      make_easyplater_design(
        manifest_df = input_manifest_cut,
        plateID = "plate 1",
        columns_for_scoring = c("Cohort","Group","Sex","AgeGroup"),
        column_weights = c(5, 5, 10, 4),
        plate_size = 96,
        fixed_wells = fixed_wells
      )
    },
    expected = readRDS(test_path("fixtures", "easy_plate_df-fixed_wells.rds"))
  )
})

#### Original example, using "internal_control_well_indices" not "fixed_wells" ####

test_that("make_easyplater_design() returns the same single plate as the original example", {
  expect_identical(
    object = {
      # Must cut Age into discrete groups, as `cols_to_categorize` is no longer supported
      input_manifest_cut <- input_manifest |>
        dplyr::mutate(AgeGroup = ggplot2::cut_interval(Age, 10) |> as.numeric(),
                      .by = "plate")
      # Run easyplater
      make_easyplater_design(
        manifest_df = input_manifest_cut,
        plateID = "plate 1",
        columns_for_scoring = c("Cohort","Group","Sex","AgeGroup"),
        column_weights = c(5, 5, 10, 4),
        plate_size = 96,
        internal_control_well_indices = 86:95,
        internal_control_ids = c(paste0("SC", 1:2), paste0("NC", 1:3), paste0("PC", 1:5))
      )
    },
    expected = readRDS(test_path("fixtures", "easy_plate_df.rds"))
  )
})

test_that("make_easyplater_design() returns the same single plate as the original example with imbalance_fixer", {
  expect_identical(
    object = {
      # Must cut Age into discrete groups, as `cols_to_categorize` is no longer supported
      input_manifest_cut <- input_manifest |>
        dplyr::mutate(AgeGroup = ggplot2::cut_interval(Age, 10) |> as.numeric(),
                      .by = "plate")

      make_easyplater_design(
        manifest_df = input_manifest_cut,
        plateID = "plate 1",
        columns_for_scoring = c("Cohort","Group","Sex","AgeGroup"),
        column_weights = c(5, 5, 10, 4),
        imbalance_fixer = list(T, "Group", list("D1", "HC1", "D7", "D8"), 3),
        internal_control_well_indices = 86:95,
        internal_control_ids = c(paste0("SC", 1:2), paste0("NC", 1:3), paste0("PC", 1:5)),
        plate_size = 96
      )
    },
    expected = readRDS(test_path("fixtures", "easy_plate_df_imbalance_fixer.rds"))
  )
})

test_that("make_easyplater_design() exits with error if plate_size != 96", {
  expect_error(
    object = {
      # Must cut Age into discrete groups, as `cols_to_categorize` is no longer supported
      input_manifest_cut <- input_manifest |>
        dplyr::mutate(AgeGroup = ggplot2::cut_interval(Age, 10) |> as.numeric(),
                      .by = "plate")

      # Make a list of the plates:
      plates <- stringr::str_sort(unique(input_manifest_cut$plate), numeric = TRUE)
      make_easyplater_design(
        manifest_df = input_manifest_cut,
        plateIDs = plates[1],
        columns_for_scoring = c("Cohort","Group","Sex","AgeGroup"),
        column_weights = c(5, 5, 10, 4),
        imbalance_fixer = list(T, "Group", list("D1", "HC1", "D7", "D8"), 3),
        internal_control_well_indices = 86:95,
        internal_control_ids = c(paste0("SC", 1:2), paste0("NC", 1:3), paste0("PC", 1:5)),
        plate_size = 48
      )
    },
    class = "simpleError"
  )
})

test_that("make_easyplater_design() returns the same multi-plate manifest as the original example", {
  expect_identical(
    object = {
      # Must cut Age into discrete groups, as `cols_to_categorize` is no longer supported
      input_manifest_cut <- input_manifest |>
        dplyr::mutate(AgeGroup = ggplot2::cut_interval(Age, 10) |> as.numeric(),
                      .by = "plate")

      make_easyplater_design(
        manifest_df = input_manifest_cut,
        plateID = NULL,
        columns_for_scoring = c("Cohort","Group","Sex","AgeGroup"),
        column_weights = c(5, 5, 10, 4),
        internal_control_well_indices = 86:95,
        internal_control_ids = c(paste0("SC", 1:2), paste0("NC", 1:3), paste0("PC", 1:5)),
        plate_size = 96
      )
    },
    expected = readRDS(test_path("fixtures", "easy_multiplate_df.rds"))
  )
})
