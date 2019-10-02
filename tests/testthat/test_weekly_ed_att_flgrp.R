context("Weekly ed attendances by flow groups for real random data")
library(hospitalflow)

test_that("Weekly ed attandances by flow groups",{


 weekly_ed_att_flgrp <- readRDS("D:/Rprojects/hospitalflow/tests/testthat/testdata/weekly_ed_att_flgrps/weekly_ed_flgrp.rds")

 correct_answers <- readRDS("testdata/weekly_ed_att_flgrps/correct_answers.rds")


 result <- weekly_ed_att_flgrp(start_date = as.Date("2016-06-01",tz = "Europe/London"), end_date = as.Date("2016-06-30",tz = "Europe/London"),
                                data = weekly_ed_att_flgrp, time_unit = "weekly", plot_chart = FALSE, hospital_name = hospital_name, restrict_plot_range = TRUE)


  #Test results are correct
  expect_equal(correct_answers, result)

})
