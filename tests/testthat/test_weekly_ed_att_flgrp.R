context("Weekly ed attandances by flow groups for real random data")
library(hospitalflow)

test_that("Weekly ed attandances by flow groups",{


 weekly_ed_att_flgrp <- readRDS("testdata/weekly_ed_att_flgrps/weekly_ed_flgrps.rds")

 correct_answers <- readRDS("testdata/weekly_ed_att_flgrps/correct_answers.rds")


  result <- hospitalflow::weekly_ed_att_flgrp(start_date = as.Date("2016-01-01",tz = "Europe/London"), end_date = as.Date("2016-08-01",tz = "Europe/London"),
                                                                   data = weekly_ed_att_flgrp, time_unit = "weekly", plot_chart = FALSE, hospital_name = hospital_name)

  result <- result %>%
    dplyr::filter(flow_groups != "Flow A")

  #Test results are correct
  expect_equal(correct_answers, result)

})
