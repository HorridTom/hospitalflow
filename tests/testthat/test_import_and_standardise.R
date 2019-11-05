context("import_and_standardise")

tmp_data_1 <- paste0(tempfile(),".csv")
tmp_data_2 <- paste0(tempfile(),".csv")
tmp_config_1 <- file.path(tempdir(),"conf1")
tmp_config_2 <- file.path(tempdir(),"conf2")

setup({
  raw_data_1 <- tibble::tibble(person_code = c("0001", "0001", "0002", "0002"),
                               date1 = c("01/01/2019 12:00:00", "01/02/2019 13:00:00",
                                         "01/01/2019 14:23:00", "01/01/2019 14:23:00"),
                               date2 = c("01/01/2019 15:00:00", "01/02/2019 15:12:00",
                                         "01/01/2019 20:01:00", "01/01/2019 20:01:00"),
                               sex = c("M", "M", "F", "F"),
                               other = c("ab", "bc", "ef", "gt"))
  raw_data_2 <- tibble::tibble(person_code = c("0001", "0002", "0002", "0003"),
                               date_st = c("2019-02-01 13:10:00", "2019-02-01 15:18:00",
                                           "2019-02-01 15:18:00", "2019-02-03 11:35:00"),
                               date_en = c("2019-02-07 09:37:00", "2019-02-03 17:48:00",
                                           "2019-02-03 17:48:00", "2019-02-20 12:19:00"),
                               gend = c("Ma", "Fe", "Fe", "Fe"),
                               misc = c("rf", "pq", "cv", "nb"))
  col_mapping_1 <- tibble::tibble(provided = c("person_code", "date1", "date2", "sex"),
                                  standard = c("pseudo_id", "start_datetime", "end_datetime",
                                               "gender"))
  gender_levels_1 <- tibble::tibble(provided = c("M", "F"),
                                    standard = c("Male", "Female"))
  datetime_formats_1 <- tibble::tibble(column_name = c("date1", "date2"),
                                       datetime_format = c("%d/%m/%Y %H:%M:%S",
                                                           "%d/%m/%Y %H:%M:%S"))
  col_mapping_2 <- tibble::tibble(provided = c("person_code", "date_st", "date_en", "gend"),
                                  standard = c("pseudo_id", "start_datetime", "end_datetime",
                                               "gender"))
  gender_levels_2 <- tibble::tibble(provided = c("Ma", "Fe"),
                                    standard = c("Male", "Female"))
  datetime_formats_2 <- tibble::tibble(column_name = c("date_st", "date_en"),
                                       datetime_format = c("%Y-%m-%d %H:%M:%S",
                                                           "%Y-%m-%d %H:%M:%S"))

  readr::write_csv(raw_data_1, tmp_data_1)
  readr::write_csv(raw_data_2, tmp_data_2)

  dir.create(tmp_config_1)
  dir.create(tmp_config_2)

  saveRDS(col_mapping_1, file = file.path(tmp_config_1, "column_mapping.rds"))
  saveRDS(gender_levels_1, file = file.path(tmp_config_1, "gender_levels.rds"))
  saveRDS(datetime_formats_1, file = file.path(tmp_config_1, "datetime_formats.rds"))
  saveRDS(col_mapping_2, file = file.path(tmp_config_2, "column_mapping.rds"))
  saveRDS(gender_levels_2, file = file.path(tmp_config_2, "gender_levels.rds"))
  saveRDS(datetime_formats_2, file = file.path(tmp_config_2, "datetime_formats.rds"))
})

teardown({
  unlink(c(tmp_data_1, tmp_data_2))
  unlink(c(tmp_config_1, tmp_config_2), recursive = TRUE)
})

test_that("import_and_standardise correctly brings in data, no dupe removal",{
  test_import_list <- list(list(data_path = tmp_data_1, config_path = tmp_config_1),
                           list(data_path = tmp_data_2, config_path = tmp_config_2))

  data_list <- import_and_standardise(test_import_list, remove_duplicates = FALSE)

  standardised_data_1 <- tibble::tibble(pseudo_id = c("0001", "0001", "0002", "0002"),
                                        start_datetime = as.POSIXct(c("01/01/2019 12:00:00",
                                                                      "01/02/2019 13:00:00",
                                                                      "01/01/2019 14:23:00",
                                                                      "01/01/2019 14:23:00"),
                                                                    tz = "Europe/London",
                                                                    format = "%d/%m/%Y %H:%M:%S"),
                                        end_datetime = as.POSIXct(c("01/01/2019 15:00:00",
                                                                    "01/02/2019 15:12:00",
                                                                    "01/01/2019 20:01:00",
                                                                    "01/01/2019 20:01:00"),
                                                                  tz = "Europe/London",
                                                                  format = "%d/%m/%Y %H:%M:%S"),
                                        gender = factor(c("Male", "Male", "Female", "Female")))

  standardised_data_2 <- tibble::tibble(pseudo_id = c("0001", "0002", "0002", "0003"),
                                        start_datetime = as.POSIXct(c("2019-02-01 13:10:00",
                                                                      "2019-02-01 15:18:00",
                                                                      "2019-02-01 15:18:00",
                                                                      "2019-02-03 11:35:00"),
                                                                    tz = "Europe/London",
                                                                    format = "%Y-%m-%d %H:%M:%S"),
                                        end_datetime = as.POSIXct(c("2019-02-07 09:37:00",
                                                                    "2019-02-03 17:48:00",
                                                                    "2019-02-03 17:48:00",
                                                                    "2019-02-20 12:19:00"),
                                                                  tz = "Europe/London",
                                                                  format = "%Y-%m-%d %H:%M:%S"),
                                        gender = factor(c("Male", "Female", "Female", "Female")))

  # Check two data files are imported
  expect_equal(length(data_list), 2)

  # Check both data files imported and standardised have 4 rows and 5 columns
  expect_equal(nrow(data_list[[1]]), 4)
  expect_equal(nrow(data_list[[2]]), 4)
  expect_equal(ncol(data_list[[1]]), 5)
  expect_equal(ncol(data_list[[2]]), 5)

  # Check both datasets imported and standardised correctly
  expect_equal(data_list[[1]] %>% dplyr::select(-episode_id), standardised_data_1)
  expect_equal(data_list[[2]] %>% dplyr::select(-episode_id), standardised_data_2)
})

test_that("import_and_standardise correctly brings in data, with dupe removal",{
  test_import_list <- list(list(data_path = tmp_data_1, config_path = tmp_config_1),
                           list(data_path = tmp_data_2, config_path = tmp_config_2))

  data_list <- import_and_standardise(test_import_list, remove_duplicates = TRUE)

  standardised_data_1 <- tibble::tibble(pseudo_id = c("0001", "0001", "0002"),
                                        start_datetime = as.POSIXct(c("01/01/2019 12:00:00",
                                                                      "01/02/2019 13:00:00",
                                                                      "01/01/2019 14:23:00"),
                                                                    tz = "Europe/London",
                                                                    format = "%d/%m/%Y %H:%M:%S"),
                                        end_datetime = as.POSIXct(c("01/01/2019 15:00:00",
                                                                    "01/02/2019 15:12:00",
                                                                    "01/01/2019 20:01:00"),
                                                                  tz = "Europe/London",
                                                                  format = "%d/%m/%Y %H:%M:%S"),
                                        gender = factor(c("Male", "Male", "Female")))

  standardised_data_2 <- tibble::tibble(pseudo_id = c("0001", "0002", "0003"),
                                        start_datetime = as.POSIXct(c("2019-02-01 13:10:00",
                                                                      "2019-02-01 15:18:00",
                                                                      "2019-02-03 11:35:00"),
                                                                    tz = "Europe/London",
                                                                    format = "%Y-%m-%d %H:%M:%S"),
                                        end_datetime = as.POSIXct(c("2019-02-07 09:37:00",
                                                                    "2019-02-03 17:48:00",
                                                                    "2019-02-20 12:19:00"),
                                                                  tz = "Europe/London",
                                                                  format = "%Y-%m-%d %H:%M:%S"),
                                        gender = factor(c("Male", "Female", "Female")))

  # Check two data files are imported
  expect_equal(length(data_list), 2)

  # Check both data files imported and standardised have 3 rows and 5 columns
  expect_equal(nrow(data_list[[1]]), 3)
  expect_equal(nrow(data_list[[2]]), 3)
  expect_equal(ncol(data_list[[1]]), 5)
  expect_equal(ncol(data_list[[2]]), 5)

  # Check both datasets imported and standardised correctly
  expect_equal(data_list[[1]] %>% dplyr::select(-episode_id), standardised_data_1)
  expect_equal(data_list[[2]] %>% dplyr::select(-episode_id), standardised_data_2)
})
