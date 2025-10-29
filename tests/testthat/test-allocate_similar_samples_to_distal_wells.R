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
