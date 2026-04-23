preallocate_wells <- function(plate_df, ic_idcs, ic_labs,
                              fill_rowwise = FALSE, fill_from_bottom = FALSE) {
  all_wells <- paste0(rep(LETTERS[1:8], times = 12), rep(1:12, each = 8))
  ic_wells = all_wells[ic_idcs]
  # Fill available wells with samples
  nonic_wells <- all_wells[!(all_wells %in% ic_wells)]
  if (fill_rowwise) {
    nonic_df <- tibble(
      lets = substr(nonic_wells, 1, 1),
      nums = substr(nonic_wells, 2, 3)
    )
    if (!fill_from_bottom) {
      nonic_df <- nonic_df |> arrange(lets)
    } else if (fill_from_bottom) {
      nonic_df <- nonic_df |> arrange(desc(lets))
    }
    nonic_wells <- paste0(nonic_df$lets, nonic_df$nums)
  }
  sample_wells <- nonic_wells[1:nrow(plate_df)]

  # Add sample well (but not ic well) locations to plate_df to be used as input for easyplater
  plate_df$well <- sample_wells
  plate_df$row <- substr(sample_wells, 1, 1)
  plate_df$column <- paste0("Column ", substr(sample_wells, 2, 3))

  # Allocate remaining wells as "empty"
  empty_wells <- nonic_wells[!(nonic_wells %in% sample_wells)]
  if (length(empty_wells > 0)) {
    empty_labs <- paste0("Empty_", empty_wells)
  } else {
    empty_labs <- NULL
  }
  # Get indices
  empty_idcs <- match(empty_wells, all_wells)
  # Create dataframe with indices, well codes and labels for more straightforward input to easyplater
  fixed_df <- tibble(
    idcs = c(ic_idcs, empty_idcs),
    wells = c(ic_wells, empty_wells),
    labs = c(ic_labs, empty_labs)
  )

  list(plate_df = plate_df, fixed_df = fixed_df)
}
