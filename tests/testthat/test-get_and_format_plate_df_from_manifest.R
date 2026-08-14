test_that("get_and_format_plate_df_from_manifest() returns the original example plate_df dataframe", {
  expect_identical(
    object = {
      manifest_df <- readRDS(test_path("fixtures", "manifest_df.rds"))
      get_and_format_plate_df_from_manifest(
        manifest_df = manifest_df,
        plateID = "plate 1",
        columns_for_scoring = c("Cohort", "Group", "Sex", "AgeGroup"),
        cols_to_categorize = list(c("Age", "10", "AgeGroup")),
        plate_size = 96,
        plate_wells = paste0(rep(LETTERS[1:8], times = 12), rep(1:12, each = 8)),
        internal_control_well_indices = 86:95,
        internal_control_ids = c(paste0("SC", 1:2), paste0("NC", 1:3), paste0("PC", 1:5))
      )[[1]]
    },
    expected = readRDS(test_path("fixtures", "plate_df.rds"))
  )
})

test_that("get_and_format_plate_df_from_manifest() returns the original example plate_df_aux dataframe", {
  expect_equal(
    object = {
      manifest_df <- readRDS(test_path("fixtures", "manifest_df.rds"))
      get_and_format_plate_df_from_manifest(
        manifest_df = manifest_df,
        plateID = "plate 1",
        columns_for_scoring = c("Cohort", "Group", "Sex", "AgeGroup"),
        cols_to_categorize = list(c("Age", "10", "AgeGroup")),
        plate_size = 96,
        plate_wells = paste0(rep(LETTERS[1:8], times = 12), rep(1:12, each = 8)),
        internal_control_well_indices = 86:95,
        internal_control_ids = c(paste0("SC", 1:2), paste0("NC", 1:3), paste0("PC", 1:5))
      )[[2]]
    },
    expected = readRDS(test_path("fixtures", "plate_df_aux.rds"))
    )
})

test_that("add_imbalance_fixer() returns the same imbalanceFix_vec as the deprecated get_and_format_plate_df_from_manifest() implementation", {
  expect_equal(
    object = {
      imbalance_fixer <- list(T,"Group",list("D1","HC1","D7","D8"),3)
      sample_df <- input_manifest[1:81,] # manifest for first plate
      add_imbalance_fixer(sample_df, imbalance_fixer)$imbalanceFix_vec
    },
    expected = {
      plate_df_imbfix <- readRDS(test_path("fixtures", "plate_df_imbfix.rds"))
      plate_df_imbfix <- plate_df_imbfix[1:81,] # first plate
      plate_df_imbfix$imbalanceFix_vec
    }
  )
})


