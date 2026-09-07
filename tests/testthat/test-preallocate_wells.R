test_that("assign_fixed_wells() returns the expected Olink Explore HT ICs", {
  expect_identical(
    object = {
      input_plate <- input_manifest[input_manifest$plate == "plate 1",]
      fixed_idcs <- 87:96
      fixed_labs <- c(paste0("SC", 1:2), paste0("NC", 1:3), paste0("PC", 1:5))
      assign_fixed_wells(input_plate, fixed_idcs, fixed_labs)
    },
    expected = readRDS(test_path("fixtures", "fixed_wells_olink.rds"))
  )
})

test_that("assign_fixed_wells() returns the Olink ICs but with randomized empties", {
  expect_identical(
    object = {
      input_plate <- input_manifest[input_manifest$plate == "plate 1",]
      fixed_idcs <- 87:96
      fixed_labs <- c(paste0("SC", 1:2), paste0("NC", 1:3), paste0("PC", 1:5))
      # Randomize empty wells among samples, rather than grouping them into fixed wells
      assign_fixed_wells(input_plate, fixed_idcs, fixed_labs, randomize_empties = TRUE)
    },
    expected = readRDS(test_path("fixtures", "fixed_wells_olink_random_empties.rds"))
  )
})

test_that("assign_fixed_wells() returns the Olink ICs across plates with same fixed wells", {
  expect_identical(
    object = {
      # To assign different fixed wells across plates, pass a list of indices and labels
      input_plates <- input_manifest[input_manifest$plate %in% c("plate 1", "plate 2"),]
      fixed_idcs <- 87:96
      fixed_labs <- c(paste0("SC", 1:2), paste0("NC", 1:3), paste0("PC", 1:5))
      assign_fixed_wells(input_plates, fixed_idcs, fixed_labs)
    },
    expected = readRDS(test_path("fixtures", "fixed_wells_olink_multiplate.rds"))
  )
})

test_that("assign_fixed_wells() returns the Olink ICs across plates with varying fixed wells", {
  expect_identical(
    object = {
      # To assign different fixed wells across plates, pass a list of indices and labels
      input_plates <- input_manifest[input_manifest$plate %in% c("plate 1", "plate 2"),]
      fixed_idcs <- list(
        "plate 1" = 87:96,
        "plate 2" = 85:96
      )
      fixed_labs <- list(
        "plate 1" = c(paste0("SC", 1:2), paste0("NC", 1:3), paste0("PC", 1:5)),
        "plate 2" = c("BR1", "BR2", paste0("SC", 1:2), paste0("NC", 1:3), paste0("PC", 1:5))
      )
      assign_fixed_wells(input_plates, fixed_idcs, fixed_labs)
    },
    expected = readRDS(test_path("fixtures", "fixed_wells_olink_multiplate_varies.rds"))
  )
})

test_that("assign_fixed_wells() returns the NULISA ICs", {
  expect_identical(
    object = {
      input_plate <- input_manifest[input_manifest$plate == "plate 1",]
      fixed_idcs <- (3:12)*8
      fixed_labs <- paste0("IC", 1:10)
      assign_fixed_wells(input_plate, fixed_idcs, fixed_labs, fill_rowwise = TRUE)
    },
    expected = readRDS(test_path("fixtures", "fixed_wells_nulisa.rds"))
  )
})

test_that("add_sample_wells() works with Olink Explore HT ICs", {
  expect_identical(
    object = {
      input_plate <- input_manifest[input_manifest$plate == "plate 1",]
      fixed_idcs <- 87:96
      fixed_labs <- c(paste0("SC", 1:2), paste0("NC", 1:3), paste0("PC", 1:5))
      fixed_wells <- assign_fixed_wells(input_plate, fixed_idcs, fixed_labs)
      add_sample_wells(input_plate, fixed_wells$well)
    },
    expected = readRDS(test_path("fixtures", "sample_wells_olink.rds"))
  )
})

test_that("add_sample_wells() works with NULISA ICs", {
  expect_identical(
    object = {
      input_plate <- input_manifest[input_manifest$plate == "plate 1",]
      fixed_idcs <- (3:12)*8
      fixed_labs <- paste0("IC", 1:10)
      fixed_wells <- assign_fixed_wells(input_plate, fixed_idcs, fixed_labs, fill_rowwise = TRUE)
      add_sample_wells(input_plate, fixed_wells$well)
    },
    expected = readRDS(test_path("fixtures", "sample_wells_nulisa.rds"))
  )
})

