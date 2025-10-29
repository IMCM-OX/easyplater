test_that("min sas edge calculation works", {
  expect_equal(calc_sas_edge_min(33L, 10, 8), 351)
})

test_that("max sas edge calculation works", {
  expect_equal(calc_sas_edge_max(33L, 10, 8), 437)
})

test_that("sas edge range calculation works", {
  expect_equal(calc_sas_edge_range(33L, 10, 8), 86)
})

test_that("calc_pds_global() calculation works when given valid input", {
  expect_equal(
    calc_pds_global(example_plate_df,
                    names(example_plate_df)[2:5],
                    c(5, 5, 10, 4),
                    c(86:95)),
    expected = 20.1845486)
})

test_that("calc_pds_global() errors when given non-96-well plate", {
  expect_error(
    calc_pds_global(example_plate_df[49:96,],
                    names(example_plate_df)[2:5],
                    c(5, 5, 10, 4),
                    c(86:95)),
    class = "simpleError")
})

test_that("calc_row_column_score() calculation works", {
  expect_equal(
    calc_row_column_score(example_plate_df,
                    names(example_plate_df)[2:5],
                    c(5, 5, 10, 4)),
    expected = 470)
})
