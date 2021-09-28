# Construct test data for the generic_standardise workflow

make_generic_standardise_test_data <- function() {
  ed_test_data_1 <- dplyr::tibble(PAT_CODE = c("1", "2"),
                                  SEX = c("F", "Not Known"),
                                  START_DATETIME = c("01/01/2019 13:01", "02/01/2019 22:02"),
                                  END_DATETIME = c("01/01/2019 16:30", "02/01/2019 04:05"))

  ed_test_data_2 <- dplyr::tibble(PAT_CODE = c("2", "3"),
                                  SEX = c("Not Known", "M"),
                                  START_DATETIME = c("03/01/2019 15:47", "02/01/2019 11:54"),
                                  END_DATETIME = c("03/01/2019 19:32", "02/01/2019 12:58"))

  ip_test_data_1 <- dplyr::tibble(PAT_CODE = c("5", "6"),
                                  START_DATETIME = c("2019-01-02 14:53:00", "2019-01-01 10:11:00"),
                                  END_DATETIME = c("2019-01-09 12:02:00", "2019-01-03 09:33:00"),
                                  DIAGNOSIS_STRING = c("COPD", "Asthma"),
                                  SITE = c("A", "A"))

  ip_test_data_2 <- dplyr::tibble(PAT_CODE = c("2", "4"),
                                  START_DATETIME = c("2019-01-03 19:59:00", "2019-01-01 11:23:00"),
                                  END_DATETIME = c("2019-01-07 15:42:00", "2019-01-06 10:49:00"),
                                  DIAGNOSIS_STRING = c("Heart Failure", "Diabetes"),
                                  SITE = c("B", "B"))

  list(ed1 = ed_test_data_1, ed2 = ed_test_data_2, ip1 = ip_test_data_1, ip2 = ip_test_data_2)
}

make_generic_standardise_test_correct_result <- function() {
  ed_test_result_str <- dplyr::tibble(pseudo_id = c("1", "2", "2", "3"),
                                  gender = c("Female", "Other", "Other", "Male"),
                                  start_datetime = c("01/01/2019 13:01", "02/01/2019 22:02", "03/01/2019 15:47", "02/01/2019 11:54"),
                                  end_datetime = c("01/01/2019 16:30", "02/01/2019 04:05", "03/01/2019 19:32", "02/01/2019 12:58"),
                                  site = c("A", "A", "B", "B"))

  ip_test_result_str <- dplyr::tibble(pseudo_id = c("5", "6", "2", "4"),
                                  start_datetime = c("2019-01-02 14:53:00", "2019-01-01 10:11:00", "2019-01-03 19:59:00", "2019-01-01 11:23:00"),
                                  end_datetime = c("2019-01-09 12:02:00", "2019-01-03 09:33:00", "2019-01-07 15:42:00", "2019-01-06 10:49:00"),
                                  diagnosis_code = c("COPD", "Asthma", "Heart Failure", "Diabetes"),
                                  site = c("A", "A", "B", "B"))

  ed_test_result <- ed_test_result_str %>% dplyr::mutate(gender = factor(gender, levels = c("Female", "Male", "Other")),
                                                  start_datetime = as.POSIXct(start_datetime, format = "%d/%m/%Y %H:%M", tz = "Europe/London"),
                                                  end_datetime = as.POSIXct(end_datetime, format = "%d/%m/%Y %H:%M", tz = "Europe/London"))
  ip_test_result <- ip_test_result_str %>% dplyr::mutate(start_datetime = as.POSIXct(start_datetime, format = "%Y-%m-%d %H:%M:%S", tz = "Europe/London"),
                                                         end_datetime = as.POSIXct(end_datetime, format = "%Y-%m-%d %H:%M:%S", tz = "Europe/London"))

  list(ED = ed_test_result, IP = ip_test_result)
}

make_generic_standardise_test_import_list <- function() {
  list(list(data_path = "testdata/generic_standardise/ed1.csv",
            config_path = "testdata/generic_standardise/ed_config/",
            site = "A",
            facility = "ED",
            time_zone = "Europe/London"),
       list(data_path = "testdata/generic_standardise/ed2.csv",
            config_path = "testdata/generic_standardise/ed_config/",
            site = "B",
            facility = "ED",
            time_zone = "Europe/London"),
       list(data_path = "testdata/generic_standardise/ip1.csv",
            config_path = "testdata/generic_standardise/ip_config/",
            site = NA,
            facility = "IP",
            time_zone = "Europe/London"),
       list(data_path = "testdata/generic_standardise/ip2.csv",
            config_path = "testdata/generic_standardise/ip_config/",
            site = NA,
            facility = "IP",
            time_zone = "Europe/London")
  )
}
