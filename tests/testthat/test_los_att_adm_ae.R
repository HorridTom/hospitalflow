context("Length of stay for attendances and admissions")
library(hospitalflow)


test_that("length of stay is correctly analysed for attendances and admissions", {
  test_los_att_adm_ae <- readRDS("testdata/los_att_adm_tests/constructed_spells_los_att_adm.rds")

  correct_answers <- tibble::tibble(
    Time_binned = as.character(c("2:00", "7:00", "7:00", "3:00")),
    Variable = as.character(c("Hospital_admissions", "Direct_discharge", "Hospital_admissions", "Direct_discharge")),
    Value = as.integer(c(1, 1, 1, 1))
  )

  result <- hospitalflow::los_att_adm_ae(
    start_date = as.Date("2019-01-01", tz = "Europe/London"),
    end_date = as.Date("2019-01-06", tz = "Europe/London"),
    data = test_los_att_adm_ae, plot_chart = FALSE, hospital_name = "Hospital_name"
  )

  # Test results are correct
  expect_equal(result %>% dplyr::arrange(Time_binned, Variable), correct_answers %>% dplyr::arrange(Time_binned, Variable))
})


test_that("length of stay is correctly analysed for attendances and admissions for realistic synthetic data", {
  spell_table <- readr::read_rds("testdata/los_att_adm_tests/synthetic_spelltable_sample.rds")
  correct_answers <- readr::read_rds("testdata/los_att_adm_tests/correct_answers_spelltable_data_los_att_adm.rds")
  correct_answers <- tibble::as_tibble(correct_answers)

  result <- hospitalflow::los_att_adm_ae(
    start_date = as.Date("2016-01-01", tz = "Europe/London"),
    end_date = as.Date("2016-01-08", tz = "Europe/London"),
    data = spell_table, plot_chart = FALSE, hospital_name = "Hospital_name"
  )

  result <- result %>% dplyr::arrange(Variable)
  result$Value <- as.numeric(result$Value)

  # Test results are correct
  expect_equal(result %>% dplyr::arrange(Time_binned, Variable), correct_answers %>% dplyr::arrange(Time_binned, Variable))
})
