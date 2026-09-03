test_that("make_plate_df() returns the original example plate_df_aux dataframe", {
  expect_equal(
    object = {
      manifest_df <- readRDS(test_path("fixtures", "manifest_df.rds"))

      # plate_col <- "plate"
      imbalance_fixer <- FALSE
      plate_num_rows <- 8
      plate_num_cols <- 12
      plate_wells <- paste0(rep(LETTERS[1:plate_num_rows], times = plate_num_cols),
                            rep(1:plate_num_cols, each = plate_num_rows))

      n_samples_plate1 <- sum(manifest_df$plate == "plate 1") # 81
      olink_ht_ic_labels <- c(paste0("SC", 1:2), paste0("NC", 1:3), paste0("PC", 1:5))
      fixed_wells <- assign_fixed_wells(n_samples_plate1, 87:96, olink_ht_ic_labels)

      manifest_df_cut <- manifest_df |>
        dplyr::mutate(AgeGroup = ggplot2::cut_interval(Age, 10) |> as.numeric(),
                      .by = "plate") |>
        dplyr::relocate(AgeGroup, .after = "Age")

      sample_df <- manifest_df_cut |> dplyr::filter(.data[["plate"]] == "plate 1")
      # Compare to original plate_df_aux
      make_plate_df(sample_df, fixed_wells, imbalance_fixer, plate_wells) |>
        dplyr::select(-Age)
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


