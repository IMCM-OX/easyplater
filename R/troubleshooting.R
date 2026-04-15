# input_manifest_plate1 <- input_manifest |> dplyr::filter(plate == "plate 1")
#
# easyplater_design <- make_easyplater_design(
#   manifest_df = input_manifest_plate1,
#   internal_control_well_indices = c(((3:12)*8)-1, 93:94),
#   internal_control_ids = c(paste0("NULISA_control", 1:10), paste0("Bridging",1:2)),
#   plateID = "plate 1",
#   columns_for_scoring = c("Cohort","Group","Sex","AgeGroup"),
#   column_weights = c(5, 5, 10, 4),
#   cols_to_categorize = list(c("Age", 10, NULL, "AgeGroup")),
#   plate_size = 96
# )
#
# ## Scenario 1: NULISA controls, bridging samples, ignoring 3 empty wells
# easyplater_design <- make_easyplater_design(
#   manifest_df = input_manifest_plate1,
#   internal_control_well_indices = c(((3:12)*8)-1, 93:94),
#   internal_control_ids = c(paste0("NULISA_control", 1:10), paste0("Bridging",1:2)),
#   plateID = "plate 1",
#   columns_for_scoring = c("Cohort","Group","Sex","AgeGroup"),
#   column_weights = c(5, 5, 10, 4),
#   cols_to_categorize = list(c("Age", 10, NULL, "AgeGroup")),
#   plate_size = 96
# )
#
# easyplater_design |>
#   OlinkAnalyze::olink_displayPlateLayout(fill.color = "SampleID", include.label = TRUE) +
#   ggplot2::theme(legend.position = "none")
#
# ## Scenario 2: NULISA controls, bridging samples, forcing location of 3 empty wells
# easyplater_design <- make_easyplater_design(
#   manifest_df = input_manifest_plate1,
#   internal_control_well_indices = c(((3:12)*8)-1,
#                                     (94:95)-1,
#                                     (91:93)-1),
#   internal_control_ids = c(paste0("NULISA_control", 1:10),
#                            paste0("Bridging",1:2),
#                            paste0("Empty_", 91:93)),
#   plateID = "plate 1",
#   columns_for_scoring = c("Cohort","Group","Sex","AgeGroup"),
#   column_weights = c(5, 5, 10, 4),
#   cols_to_categorize = list(c("Age", 10, NULL, "AgeGroup")),
#   plate_size = 96
# )
#
# easyplater_design |>
#   OlinkAnalyze::olink_displayPlateLayout(fill.color = "SampleID", include.label = TRUE) +
#   ggplot2::theme(legend.position = "none")
#
# ## Scenario 3: Default input_manifest, to test that I'm reproducing the original behavior
# easyplater_design <- make_easyplater_design(
#   manifest_df = input_manifest_plate1,
#   plateID = "plate 1",
#   columns_for_scoring = c("Cohort","Group","Sex","AgeGroup"),
#   column_weights = c(5, 5, 10, 4),
#   cols_to_categorize = list(c("Age", 10, NULL, "AgeGroup")),
#   plate_size = 96
# )
#
# easyplater_design |>
#   OlinkAnalyze::olink_displayPlateLayout(fill.color = "SampleID", include.label = TRUE) +
#   ggplot2::theme(legend.position = "none")
#
#
