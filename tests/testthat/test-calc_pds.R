test_that("min sas edge calculation works", {
  expect_equal(calc_sas_edge_min(33L, 10, 8), 351)
})

test_that("max sas edge calculation works", {
  expect_equal(calc_sas_edge_max(33L, 10, 8), 437)
})
