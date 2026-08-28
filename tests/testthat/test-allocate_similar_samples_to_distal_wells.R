test_that("find_sample_communities() works", {
  expect_equal(
    object = {
      col_weights <- c(5, 5, 10, 4)
      imbalance_fixer <- list(T,"Group",list("D1","HC1","D7","D8"),3)
      ss_mat <- make_ss_matrix(example_plate_df, col_weights, imbalance_fixer)
      find_sample_communities(ss_mat) |> igraph::modularity()
    },
    expected = 0.429774949 # imprecise, but avoids typing out the full communities object
  )
})

test_that("find_n_wells() works", {
  expect_equal(
    withr::with_seed(
      seed = 4444,
      code = {
        plate_size <- 96
        full_mask <- plate_size |>
          make_well_distances_matrix() |>
          make_full_mask()
        find_n_wells(full_mask, 17L, 86:95, plate_size)
      }),
    expected = c(5, 71, 30, 52, 83, 10, 72, 33, 6, 76, 63, 81, 11, 37, 42, 16, 51)
    )
})
test_that("find_n_wells() errors when given n_wells > n available wells", {
  expect_error(
    withr::with_seed(
      seed = 4444,
      code = {
        plate_size <- 96
        full_mask <- plate_size |>
          make_well_distances_matrix() |>
          make_full_mask()
        find_n_wells(full_mask, 87L, 86:95, plate_size)
      }),
    class = "simpleError"
  )
})
