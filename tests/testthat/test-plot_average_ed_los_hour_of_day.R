
test_that("averaged lengths of stay in emergency department for flow groups are
          calculated correctly", {
  correct_answers <- readRDS("testdata/plot_average_ed_los_hour_of_day/correct_answers.rds")

  plot_average_ed_los_hour_of_day <- readRDS("testdata/plot_average_ed_los_hour_of_day/plot_average_ed_los_hour_of_day.rds")

  result <- plot_average_ed_los_hour_of_day(
    data = plot_average_ed_los_hour_of_day,
    startDate = as.POSIXct("2016-06-01 00:00:00", tz = "Europe/London"),
    endDate = as.POSIXct("2016-06-03 00:00:00", tz = "Europe/London"),
    returnPlot = FALSE,
    hospitalName = "{hospital_name}"
  )

  # Test results are correct
  expect_equal(correct_answers, result)
  }
)
