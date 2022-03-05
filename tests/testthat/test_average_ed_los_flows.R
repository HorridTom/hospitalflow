context("Average ED los for flow groups, for real random data")

test_that("averaged ed los for flow groups are calculated correctly", {
  correct_answers <- readRDS("testdata/average_ed_los_flows/correct_answers.rds")

  average_ed_los_flows <- readRDS("testdata/average_ed_los_flows/average_ed_los_flows.rds")

  result <- average_ed_los_flows(
    start_date = as.POSIXct("2016-06-01 00:00:00", tz = "Europe/London"),
    end_date = as.POSIXct("2016-06-03 00:00:00", tz = "Europe/London"),
    data = average_ed_los_flows, plot_chart = FALSE, hospital_name = "CLAHRC"
  )

  # Test results are correct
  expect_equal(correct_answers, result)
})
