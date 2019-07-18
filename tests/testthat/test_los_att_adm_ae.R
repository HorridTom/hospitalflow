context("Length of stay for attendances and admissions")
library(hospitalflow)


test_that("length of stay is correctly analysed for attendances and admissions",{

  test_los_att_adm_ae <- readRDS("testdata/los_att_adm_tests/test_los_att_adm_ae_spells.rds")

  correct_answers <- tibble::tibble(

    Time_binned = as.character(c("2:00", "7:00" , "7:00", "3:00")),
    Variable = as.character(c("Hospital_admissions", "Direct_discharge", "Hospital_admissions", "Direct_discharge")),
    Value = as.integer(c(1, 1, 1, 1))
  )


  result <- hospitalflow::los_att_adm_ae(start_date = as.Date("2019-01-01", tz = "Europe/London"),
                           end_date = as.Date("2019-01-06", tz = "Europe/London"),
                           data = test_los_att_adm_ae, plot_chart = FALSE, hospital_name = "Hospital_name")


  #Test results are correct
  expect_equal(result, correct_answers)


})


test_that("length of stay is correctly analysed for attendances and admissions for real data",{

  load("D:/Rprojects/hospitalflow/tests/testthat/testdata/los_att_adm_tests/sample_test_ed_los_att_adm.rda")
  correct_answers <- readr::read_csv("D:/Rprojects/hospitalflow/tests/testthat/testdata/los_att_adm_tests/correct_answers.csv")


  result <- hospitalflow::los_att_adm_ae(start_date = as.Date("2016-12-14", tz = "Europe/London"),
                                         end_date = as.Date("2017-02-28", tz = "Europe/London"),
                                         data = df_recode, plot_chart = FALSE, hospital_name = "Hospital_name")


  result <- result %>% dplyr::arrange(Variable)
  result$Value <- as.numeric(result$Value)
  #Test results are correct
  expect_equal(result, correct_answers)

})
