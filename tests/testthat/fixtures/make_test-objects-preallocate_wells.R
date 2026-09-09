#### fixed_wells_olink ####
# Checkout commit 60abccd1 and run the corresponding test code.
# To get an easy-to-copy-and-paste format to test against, run this:
# x |>
#   dplyr::mutate(dplyr::across(dplyr::everything(), ~ifelse(!is.na(.x), paste0("\"", .x, "\""), .x))) |>
#   dplyr::mutate(dplyr::across(dplyr::everything(), ~ paste0(.x, ","))) |>
#   as.data.frame() |>
#   print(row.names = FALSE)
fixed_wells_olink <- tibble::tribble(
  ~plate,   ~idc,   ~well,    ~lab,
  "plate 1", "87", "G11",       "SC1",
  "plate 1", "88", "H11",       "SC2",
  "plate 1", "89", "A12",       "NC1",
  "plate 1", "90", "B12",       "NC2",
  "plate 1", "91", "C12",       "NC3",
  "plate 1", "92", "D12",       "PC1",
  "plate 1", "93", "E12",       "PC2",
  "plate 1", "94", "F12",       "PC3",
  "plate 1", "95", "G12",       "PC4",
  "plate 1", "96", "H12",       "PC5",
  "plate 1", "82", "B11", "Empty_B11",
  "plate 1", "83", "C11", "Empty_C11",
  "plate 1", "84", "D11", "Empty_D11",
  "plate 1", "85", "E11", "Empty_E11",
  "plate 1", "86", "F11", "Empty_F11"
) |>
  dplyr::mutate(idc = as.integer(idc))
saveRDS(fixed_wells_olink, test_path("fixtures", "fixed_wells_olink.rds"))

#### fixed_wells_olink_random_empties ####
# Checkout commit 60abccd1 and run the corresponding test code.
# To get an easy-to-copy-and-paste format to test against, run this:
# x |>
#   dplyr::mutate(dplyr::across(dplyr::everything(), ~ifelse(!is.na(.x), paste0("\"", .x, "\""), .x))) |>
#   dplyr::mutate(dplyr::across(dplyr::everything(), ~ paste0(.x, ","))) |>
#   as.data.frame() |>
#   print(row.names = FALSE)
fixed_wells_olink_random_empties <- tibble::tribble(
  ~plate,   ~idc,   ~well,    ~lab,
  "plate 1", "87", "G11", "SC1",
  "plate 1", "88", "H11", "SC2",
  "plate 1", "89", "A12", "NC1",
  "plate 1", "90", "B12", "NC2",
  "plate 1", "91", "C12", "NC3",
  "plate 1", "92", "D12", "PC1",
  "plate 1", "93", "E12", "PC2",
  "plate 1", "94", "F12", "PC3",
  "plate 1", "95", "G12", "PC4",
  "plate 1", "96", "H12", "PC5"
) |>
  dplyr::mutate(idc = as.integer(idc))
saveRDS(fixed_wells_olink_random_empties, test_path("fixtures", "fixed_wells_olink_random_empties.rds"))


#### fixed_wells_olink_multiplate ####
# Checkout commit 60abccd1 and run the corresponding test code.
# To get an easy-to-copy-and-paste format to test against, run this:
# x |>
#   dplyr::mutate(dplyr::across(dplyr::everything(), ~ifelse(!is.na(.x), paste0("\"", .x, "\""), .x))) |>
#   dplyr::mutate(dplyr::across(dplyr::everything(), ~ paste0(.x, ","))) |>
#   as.data.frame() |>
#   print(row.names = FALSE)
fixed_wells_olink_multiplate <- tibble::tribble(
  ~plate,   ~idc,   ~well,    ~lab,
  "plate 1", "87", "G11",       "SC1",
  "plate 1", "88", "H11",       "SC2",
  "plate 1", "89", "A12",       "NC1",
  "plate 1", "90", "B12",       "NC2",
  "plate 1", "91", "C12",       "NC3",
  "plate 1", "92", "D12",       "PC1",
  "plate 1", "93", "E12",       "PC2",
  "plate 1", "94", "F12",       "PC3",
  "plate 1", "95", "G12",       "PC4",
  "plate 1", "96", "H12",       "PC5",
  "plate 1", "82", "B11", "Empty_B11",
  "plate 1", "83", "C11", "Empty_C11",
  "plate 1", "84", "D11", "Empty_D11",
  "plate 1", "85", "E11", "Empty_E11",
  "plate 1", "86", "F11", "Empty_F11",
  "plate 2", "87", "G11",       "SC1",
  "plate 2", "88", "H11",       "SC2",
  "plate 2", "89", "A12",       "NC1",
  "plate 2", "90", "B12",       "NC2",
  "plate 2", "91", "C12",       "NC3",
  "plate 2", "92", "D12",       "PC1",
  "plate 2", "93", "E12",       "PC2",
  "plate 2", "94", "F12",       "PC3",
  "plate 2", "95", "G12",       "PC4",
  "plate 2", "96", "H12",       "PC5"
) |>
  dplyr::mutate(idc = as.integer(idc))
saveRDS(fixed_wells_olink_multiplate, test_path("fixtures", "fixed_wells_olink_multiplate.rds"))

#### fixed_wells_olink_multiplate_varies ####
# Checkout commit 60abccd1 and run the corresponding test code.
# To get an easy-to-copy-and-paste format to test against, run this:
# x |>
#   dplyr::mutate(dplyr::across(dplyr::everything(), ~ifelse(!is.na(.x), paste0("\"", .x, "\""), .x))) |>
#   dplyr::mutate(dplyr::across(dplyr::everything(), ~ paste0(.x, ","))) |>
#   as.data.frame() |>
#   print(row.names = FALSE)
fixed_wells_olink_multiplate_varies <- tibble::tribble(
  ~plate,   ~idc,   ~well,    ~lab,
  "plate 1", "87", "G11",       "SC1",
  "plate 1", "88", "H11",       "SC2",
  "plate 1", "89", "A12",       "NC1",
  "plate 1", "90", "B12",       "NC2",
  "plate 1", "91", "C12",       "NC3",
  "plate 1", "92", "D12",       "PC1",
  "plate 1", "93", "E12",       "PC2",
  "plate 1", "94", "F12",       "PC3",
  "plate 1", "95", "G12",       "PC4",
  "plate 1", "96", "H12",       "PC5",
  "plate 1", "82", "B11", "Empty_B11",
  "plate 1", "83", "C11", "Empty_C11",
  "plate 1", "84", "D11", "Empty_D11",
  "plate 1", "85", "E11", "Empty_E11",
  "plate 1", "86", "F11", "Empty_F11",
  "plate 2", "85", "E11",       "BR1",
  "plate 2", "86", "F11",       "BR2",
  "plate 2", "87", "G11",       "SC1",
  "plate 2", "88", "H11",       "SC2",
  "plate 2", "89", "A12",       "NC1",
  "plate 2", "90", "B12",       "NC2",
  "plate 2", "91", "C12",       "NC3",
  "plate 2", "92", "D12",       "PC1",
  "plate 2", "93", "E12",       "PC2",
  "plate 2", "94", "F12",       "PC3",
  "plate 2", "95", "G12",       "PC4",
  "plate 2", "96", "H12",       "PC5"
) |>
  dplyr::mutate(idc = as.integer(idc))
saveRDS(fixed_wells_olink_multiplate_varies, test_path("fixtures", "fixed_wells_olink_multiplate_varies.rds"))

#### fixed_wells_nulisa ####
# Checkout commit 60abccd1 and run the corresponding test code.
# To get an easy-to-copy-and-paste format to test against, run this:
# x |>
#   dplyr::mutate(dplyr::across(dplyr::everything(), ~ifelse(!is.na(.x), paste0("\"", .x, "\""), .x))) |>
#   dplyr::mutate(dplyr::across(dplyr::everything(), ~ paste0(.x, ","))) |>
#   as.data.frame() |>
#   print(row.names = FALSE)
fixed_wells_nulisa <- tibble::tribble(
  ~plate,   ~idc,   ~well,    ~lab,
  "plate 1", "24",  "H3",       "IC1",
  "plate 1", "32",  "H4",       "IC2",
  "plate 1", "40",  "H5",       "IC3",
  "plate 1", "48",  "H6",       "IC4",
  "plate 1", "56",  "H7",       "IC5",
  "plate 1", "64",  "H8",       "IC6",
  "plate 1", "72",  "H9",       "IC7",
  "plate 1", "80", "H10",       "IC8",
  "plate 1", "88", "H11",       "IC9",
  "plate 1", "96", "H12",      "IC10",
  "plate 1", "79", "G10", "Empty_G10",
  "plate 1", "87", "G11", "Empty_G11",
  "plate 1", "95", "G12", "Empty_G12",
  "plate 1",  "8",  "H1",  "Empty_H1",
  "plate 1", "16",  "H2",  "Empty_H2"
) |>
  dplyr::mutate(idc = as.integer(idc))
saveRDS(fixed_wells_nulisa, test_path("fixtures", "fixed_wells_nulisa.rds"))

#### sample_wells_olink ####
# Checkout commit 60abccd1 and run the corresponding test code.
# To get an easy-to-copy-and-paste format to test against, run this:
# x |>
#   dplyr::mutate(dplyr::across(dplyr::everything(), ~ifelse(!is.na(.x), paste0("\"", .x, "\""), .x))) |>
#   dplyr::mutate(dplyr::across(dplyr::everything(), ~ paste0(.x, ","))) |>
#   as.data.frame() |>
#   print(row.names = FALSE)
sample_wells_olink <- tibble::tribble(
  ~SampleID, ~Cohort, ~Group, ~Sex, ~Age, ~plate, ~column, ~row, ~well,
  "0",  "C2",  "D5", "2", "43", "plate 1",  "Column 1", "A",  "A1",
  "1",  "C1",  "D7", "1", "66", "plate 1",  "Column 1", "B",  "B1",
  "2",  "C1",  "D7", "1", "68", "plate 1",  "Column 1", "C",  "C1",
  "3",  "C2",  "D7", "2", "77", "plate 1",  "Column 1", "D",  "D1",
  "4",  "C1",  "D1", "2", "54", "plate 1",  "Column 1", "E",  "E1",
  "5",  "C1",  "D7", "2", "75", "plate 1",  "Column 1", "F",  "F1",
  "6",  "C2",  "D1", "2", "65", "plate 1",  "Column 1", "G",  "G1",
  "7",  "C2",  "D7", "1", "58", "plate 1",  "Column 1", "H",  "H1",
  "8",  "C2",  "D1", "2", "67", "plate 1",  "Column 2", "A",  "A2",
  "9",  "C2",  "D8", "2", "61", "plate 1",  "Column 2", "B",  "B2",
  "10",  "C2",  "D7", "2",   NA, "plate 1",  "Column 2", "C",  "C2",
  "11",  "C2",  "D7", "2", "77", "plate 1",  "Column 2", "D",  "D2",
  "12",  "C1",  "D7", "2", "66", "plate 1",  "Column 2", "E",  "E2",
  "13",  "C1",  "D1", "2", "73", "plate 1",  "Column 2", "F",  "F2",
  "14",  "C1",  "D8", "2", "76", "plate 1",  "Column 2", "G",  "G2",
  "15",  "C1", "HC1", "2", "79", "plate 1",  "Column 2", "H",  "H2",
  "16",  "C2",  "D1",  NA, "56", "plate 1",  "Column 3", "A",  "A3",
  "17",  "C1",  "D1", "1", "71", "plate 1",  "Column 3", "B",  "B3",
  "18",  "C2",  "D7", "1", "69", "plate 1",  "Column 3", "C",  "C3",
  "19",  "C1",  "D1", "2",   NA, "plate 1",  "Column 3", "D",  "D3",
  "20",  "C2",  "D3", "1", "49", "plate 1",  "Column 3", "E",  "E3",
  "21",  "C1",  "D1", "1", "68", "plate 1",  "Column 3", "F",  "F3",
  "22",  "C1", "HC1", "2", "76", "plate 1",  "Column 3", "G",  "G3",
  "23",  "C2",  "D1", "2", "71", "plate 1",  "Column 3", "H",  "H3",
  "24",  "C2",  "D7", "1", "32", "plate 1",  "Column 4", "A",  "A4",
  "25",  "C2",  "D7", "1", "57", "plate 1",  "Column 4", "B",  "B4",
  "26",  "C1",  "D7", "1", "60", "plate 1",  "Column 4", "C",  "C4",
  "27",  "C1",  "D1", "2", "66", "plate 1",  "Column 4", "D",  "D4",
  "28",  "C1",  "D7", "2", "49", "plate 1",  "Column 4", "E",  "E4",
  "29",  "C1",  "D1", "1", "66", "plate 1",  "Column 4", "F",  "F4",
  "30",  "C1",  "D1", "2", "56", "plate 1",  "Column 4", "G",  "G4",
  "31",  "C1",  "D1", "1", "74", "plate 1",  "Column 4", "H",  "H4",
  "32",  "C1", "HC1", "1", "62", "plate 1",  "Column 5", "A",  "A5",
  "33",  "C1",  "D7", "2", "51", "plate 1",  "Column 5", "B",  "B5",
  "34",  "C2",  "D7", "2", "68", "plate 1",  "Column 5", "C",  "C5",
  "35",  "C1", "HC2", "2", "71", "plate 1",  "Column 5", "D",  "D5",
  "36",  "C1",  "D8", "1", "77", "plate 1",  "Column 5", "E",  "E5",
  "37",  "C1",  "D7", "1", "68", "plate 1",  "Column 5", "F",  "F5",
  "38",  "C1",  "D7", "1",   NA, "plate 1",  "Column 5", "G",  "G5",
  "39",  "C1",  "D1", "2", "64", "plate 1",  "Column 5", "H",  "H5",
  "40",  "C1",  "D7", "1", "70", "plate 1",  "Column 6", "A",  "A6",
  "41",  "C1",  "D1", "2", "73", "plate 1",  "Column 6", "B",  "B6",
  "42",  "C2",  "D1", "1", "56", "plate 1",  "Column 6", "C",  "C6",
  "43",  "C2",  "D5",  NA, "65", "plate 1",  "Column 6", "D",  "D6",
  "44",  "C2",  "D7", "1", "61", "plate 1",  "Column 6", "E",  "E6",
  "45",  "C1",  "D7", "2", "60", "plate 1",  "Column 6", "F",  "F6",
  "46",  "C1", "HC1", "1", "70", "plate 1",  "Column 6", "G",  "G6",
  "47",  "C1", "HC1", "1", "58", "plate 1",  "Column 6", "H",  "H6",
  "48",  "C2",  "D3", "2", "50", "plate 1",  "Column 7", "A",  "A7",
  "49",  "C2",  "D8", "2", "56", "plate 1",  "Column 7", "B",  "B7",
  "50",  "C1",  "D7", "2", "80", "plate 1",  "Column 7", "C",  "C7",
  "51",  "C1",  "D1", "2", "67", "plate 1",  "Column 7", "D",  "D7",
  "52",  "C1",  "D3", "1",   NA, "plate 1",  "Column 7", "E",  "E7",
  "53",  "C2",  "D7", "2", "66", "plate 1",  "Column 7", "F",  "F7",
  "54",  "C1", "HC1", "2", "72", "plate 1",  "Column 7", "G",  "G7",
  "55",  "C2", "HC1", "1", "69", "plate 1",  "Column 7", "H",  "H7",
  "56",  "C2",  "D7", "2", "66", "plate 1",  "Column 8", "A",  "A8",
  "57",  "C1",  "D7", "2", "73", "plate 1",  "Column 8", "B",  "B8",
  "58",  "C1",  "D7", "2", "75", "plate 1",  "Column 8", "C",  "C8",
  "59",  "C2",  "D8", "1", "56", "plate 1",  "Column 8", "D",  "D8",
  "60",  "C2",  "D1", "1", "38", "plate 1",  "Column 8", "E",  "E8",
  "61",  "C1",  "D7", "2", "73", "plate 1",  "Column 8", "F",  "F8",
  "62",  "C2",  "D8", "1", "66", "plate 1",  "Column 8", "G",  "G8",
  "63",  "C2",  "D7", "2", "70", "plate 1",  "Column 8", "H",  "H8",
  "64",  "C1", "HC2", "1", "73", "plate 1",  "Column 9", "A",  "A9",
  "65",  "C1",  "D1", "2", "62", "plate 1",  "Column 9", "B",  "B9",
  "66",  "C2",  "D8", "2", "74", "plate 1",  "Column 9", "C",  "C9",
  "67",  "C1",  "D8", "1", "76", "plate 1",  "Column 9", "D",  "D9",
  "68",  "C1",    NA, "2", "72", "plate 1",  "Column 9", "E",  "E9",
  "69",  "C1",  "D5", "2", "59", "plate 1",  "Column 9", "F",  "F9",
  "70",  "C1",  "D1", "1", "61", "plate 1",  "Column 9", "G",  "G9",
  "71",  "C1",  "D1", "1", "57", "plate 1",  "Column 9", "H",  "H9",
  "72",  "C1",  "D7", "2", "52", "plate 1", "Column 10", "A", "A10",
  "73",  "C1",  "D3", "2", "82", "plate 1", "Column 10", "B", "B10",
  "74",  "C1",  "D1", "2", "67", "plate 1", "Column 10", "C", "C10",
  "75",  "C2",  "D7", "1", "46", "plate 1", "Column 10", "D", "D10",
  "76",  "C1",  "D7", "2", "52", "plate 1", "Column 10", "E", "E10",
  "77",  "C2", "HC1", "1", "47", "plate 1", "Column 10", "F", "F10",
  "78",  "C2",  "D1", "2", "78", "plate 1", "Column 10", "G", "G10",
  "79",  "C2",  "D7", "1",   NA, "plate 1", "Column 10", "H", "H10",
  "80",  "C2", "HC1", "2", "59", "plate 1", "Column 11", "A", "A11"
) |>
  dplyr::mutate(dplyr::across(Sex:Age, ~as.numeric(.x)))
saveRDS(sample_wells_olink, test_path("fixtures", "sample_wells_olink.rds"))


#### sample_wells_nulisa ####
# Checkout commit 60abccd1 and run the corresponding test code.
# To get an easy-to-copy-and-paste format to test against, run this:
# x |>
#   dplyr::mutate(dplyr::across(dplyr::everything(), ~ifelse(!is.na(.x), paste0("\"", .x, "\""), .x))) |>
#   dplyr::mutate(dplyr::across(dplyr::everything(), ~ paste0(.x, ","))) |>
#   as.data.frame() |>
#   print(row.names = FALSE)
sample_wells_nulisa <- tibble::tribble(
  ~SampleID, ~Cohort, ~Group, ~Sex, ~Age, ~plate, ~column, ~row, ~well,
  "0",  "C2",  "D5", "2", "43", "plate 1",  "Column 1", "A",  "A1",
  "1",  "C1",  "D7", "1", "66", "plate 1",  "Column 1", "B",  "B1",
  "2",  "C1",  "D7", "1", "68", "plate 1",  "Column 1", "C",  "C1",
  "3",  "C2",  "D7", "2", "77", "plate 1",  "Column 1", "D",  "D1",
  "4",  "C1",  "D1", "2", "54", "plate 1",  "Column 1", "E",  "E1",
  "5",  "C1",  "D7", "2", "75", "plate 1",  "Column 1", "F",  "F1",
  "6",  "C2",  "D1", "2", "65", "plate 1",  "Column 1", "G",  "G1",
  "7",  "C2",  "D7", "1", "58", "plate 1",  "Column 2", "A",  "A2",
  "8",  "C2",  "D1", "2", "67", "plate 1",  "Column 2", "B",  "B2",
  "9",  "C2",  "D8", "2", "61", "plate 1",  "Column 2", "C",  "C2",
  "10",  "C2",  "D7", "2",   NA, "plate 1",  "Column 2", "D",  "D2",
  "11",  "C2",  "D7", "2", "77", "plate 1",  "Column 2", "E",  "E2",
  "12",  "C1",  "D7", "2", "66", "plate 1",  "Column 2", "F",  "F2",
  "13",  "C1",  "D1", "2", "73", "plate 1",  "Column 2", "G",  "G2",
  "14",  "C1",  "D8", "2", "76", "plate 1",  "Column 3", "A",  "A3",
  "15",  "C1", "HC1", "2", "79", "plate 1",  "Column 3", "B",  "B3",
  "16",  "C2",  "D1",  NA, "56", "plate 1",  "Column 3", "C",  "C3",
  "17",  "C1",  "D1", "1", "71", "plate 1",  "Column 3", "D",  "D3",
  "18",  "C2",  "D7", "1", "69", "plate 1",  "Column 3", "E",  "E3",
  "19",  "C1",  "D1", "2",   NA, "plate 1",  "Column 3", "F",  "F3",
  "20",  "C2",  "D3", "1", "49", "plate 1",  "Column 3", "G",  "G3",
  "21",  "C1",  "D1", "1", "68", "plate 1",  "Column 4", "A",  "A4",
  "22",  "C1", "HC1", "2", "76", "plate 1",  "Column 4", "B",  "B4",
  "23",  "C2",  "D1", "2", "71", "plate 1",  "Column 4", "C",  "C4",
  "24",  "C2",  "D7", "1", "32", "plate 1",  "Column 4", "D",  "D4",
  "25",  "C2",  "D7", "1", "57", "plate 1",  "Column 4", "E",  "E4",
  "26",  "C1",  "D7", "1", "60", "plate 1",  "Column 4", "F",  "F4",
  "27",  "C1",  "D1", "2", "66", "plate 1",  "Column 4", "G",  "G4",
  "28",  "C1",  "D7", "2", "49", "plate 1",  "Column 5", "A",  "A5",
  "29",  "C1",  "D1", "1", "66", "plate 1",  "Column 5", "B",  "B5",
  "30",  "C1",  "D1", "2", "56", "plate 1",  "Column 5", "C",  "C5",
  "31",  "C1",  "D1", "1", "74", "plate 1",  "Column 5", "D",  "D5",
  "32",  "C1", "HC1", "1", "62", "plate 1",  "Column 5", "E",  "E5",
  "33",  "C1",  "D7", "2", "51", "plate 1",  "Column 5", "F",  "F5",
  "34",  "C2",  "D7", "2", "68", "plate 1",  "Column 5", "G",  "G5",
  "35",  "C1", "HC2", "2", "71", "plate 1",  "Column 6", "A",  "A6",
  "36",  "C1",  "D8", "1", "77", "plate 1",  "Column 6", "B",  "B6",
  "37",  "C1",  "D7", "1", "68", "plate 1",  "Column 6", "C",  "C6",
  "38",  "C1",  "D7", "1",   NA, "plate 1",  "Column 6", "D",  "D6",
  "39",  "C1",  "D1", "2", "64", "plate 1",  "Column 6", "E",  "E6",
  "40",  "C1",  "D7", "1", "70", "plate 1",  "Column 6", "F",  "F6",
  "41",  "C1",  "D1", "2", "73", "plate 1",  "Column 6", "G",  "G6",
  "42",  "C2",  "D1", "1", "56", "plate 1",  "Column 7", "A",  "A7",
  "43",  "C2",  "D5",  NA, "65", "plate 1",  "Column 7", "B",  "B7",
  "44",  "C2",  "D7", "1", "61", "plate 1",  "Column 7", "C",  "C7",
  "45",  "C1",  "D7", "2", "60", "plate 1",  "Column 7", "D",  "D7",
  "46",  "C1", "HC1", "1", "70", "plate 1",  "Column 7", "E",  "E7",
  "47",  "C1", "HC1", "1", "58", "plate 1",  "Column 7", "F",  "F7",
  "48",  "C2",  "D3", "2", "50", "plate 1",  "Column 7", "G",  "G7",
  "49",  "C2",  "D8", "2", "56", "plate 1",  "Column 8", "A",  "A8",
  "50",  "C1",  "D7", "2", "80", "plate 1",  "Column 8", "B",  "B8",
  "51",  "C1",  "D1", "2", "67", "plate 1",  "Column 8", "C",  "C8",
  "52",  "C1",  "D3", "1",   NA, "plate 1",  "Column 8", "D",  "D8",
  "53",  "C2",  "D7", "2", "66", "plate 1",  "Column 8", "E",  "E8",
  "54",  "C1", "HC1", "2", "72", "plate 1",  "Column 8", "F",  "F8",
  "55",  "C2", "HC1", "1", "69", "plate 1",  "Column 8", "G",  "G8",
  "56",  "C2",  "D7", "2", "66", "plate 1",  "Column 9", "A",  "A9",
  "57",  "C1",  "D7", "2", "73", "plate 1",  "Column 9", "B",  "B9",
  "58",  "C1",  "D7", "2", "75", "plate 1",  "Column 9", "C",  "C9",
  "59",  "C2",  "D8", "1", "56", "plate 1",  "Column 9", "D",  "D9",
  "60",  "C2",  "D1", "1", "38", "plate 1",  "Column 9", "E",  "E9",
  "61",  "C1",  "D7", "2", "73", "plate 1",  "Column 9", "F",  "F9",
  "62",  "C2",  "D8", "1", "66", "plate 1",  "Column 9", "G",  "G9",
  "63",  "C2",  "D7", "2", "70", "plate 1", "Column 10", "A", "A10",
  "64",  "C1", "HC2", "1", "73", "plate 1", "Column 10", "B", "B10",
  "65",  "C1",  "D1", "2", "62", "plate 1", "Column 10", "C", "C10",
  "66",  "C2",  "D8", "2", "74", "plate 1", "Column 10", "D", "D10",
  "67",  "C1",  "D8", "1", "76", "plate 1", "Column 10", "E", "E10",
  "68",  "C1",    NA, "2", "72", "plate 1", "Column 10", "F", "F10",
  "69",  "C1",  "D5", "2", "59", "plate 1", "Column 11", "A", "A11",
  "70",  "C1",  "D1", "1", "61", "plate 1", "Column 11", "B", "B11",
  "71",  "C1",  "D1", "1", "57", "plate 1", "Column 11", "C", "C11",
  "72",  "C1",  "D7", "2", "52", "plate 1", "Column 11", "D", "D11",
  "73",  "C1",  "D3", "2", "82", "plate 1", "Column 11", "E", "E11",
  "74",  "C1",  "D1", "2", "67", "plate 1", "Column 11", "F", "F11",
  "75",  "C2",  "D7", "1", "46", "plate 1", "Column 12", "A", "A12",
  "76",  "C1",  "D7", "2", "52", "plate 1", "Column 12", "B", "B12",
  "77",  "C2", "HC1", "1", "47", "plate 1", "Column 12", "C", "C12",
  "78",  "C2",  "D1", "2", "78", "plate 1", "Column 12", "D", "D12",
  "79",  "C2",  "D7", "1",   NA, "plate 1", "Column 12", "E", "E12",
  "80",  "C2", "HC1", "2", "59", "plate 1", "Column 12", "F", "F12"
) |>
  dplyr::mutate(dplyr::across(Sex:Age, ~as.numeric(.x)))
saveRDS(sample_wells_nulisa, test_path("fixtures", "sample_wells_nulisa.rds"))
