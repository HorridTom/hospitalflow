context("Generic import standardise and bind")
library(hospitalflow)

source("make_generic_standardise_test_data.R")

test_that("Example data are correctly imported, standardised, and bound", {

  # Make test data and save as csv files
  test_data_list <- make_generic_standardise_test_data()
  test_data_names <- names(test_data_list)
  lapply(test_data_names, function(x) {
    readr::write_csv(test_data_list[[x]], file = file.path("testdata", "generic_standardise", paste(x, ".csv", sep = "")))
  })

  # Set up import list
  test_import_list <- make_generic_standardise_test_import_list()

  # Set up correct result
  correct_result_list <- make_generic_standardise_test_correct_result()
  ed_correct_result <- correct_result_list$ED
  ip_correct_result <- correct_result_list$IP

  # Import data using import_standardise_bind
  result <- import_standardise_bind(test_import_list)

  ed_result <- result$ED %>% dplyr::select(-episode_id)
  ip_result <- result$IP %>% dplyr::select(-episode_id)

  expect_equal(ed_result, ed_correct_result)
  expect_equal(ip_result, ip_correct_result)


})
