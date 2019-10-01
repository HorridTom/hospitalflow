context("ED length of stay for flow groups for real random data")
library(hospitalflow)

test_that("Weekly ed attandances by flow groups",{

  correct_answers <- readRDS("testdata/ed_los_flgrps/correct_answers.rds")

  ed_los_flow_groups <- readRDS("testdata/ed_los_flgrps/ed_los_flgrps.rds")


  result <- ed_los_flow_grps(start_date = as.Date("2016-06-01",tz = "Europe/London"),
                             end_date = as.Date("2016-06-30",tz = "Europe/London"),
                             data = ed_los_flow_groups, plot_chart = FALSE, hospital_name = hospital_name)


  #Test results are correct
  expect_equal(correct_answers, result)

})
