
test_that("Weekly ed attandances by flow groups", {
  weekly_ed_att_flgrp <- readRDS("testdata/plot_ed_attendances_timeseries_flow/weekly_ed_flgrp.rds")

  correct_answers <- readRDS("testdata/plot_ed_attendances_timeseries_flow/correct_answers.rds")
  correct_answers <- correct_answers %>%
    tibble::as_tibble()

  result <- plot_ed_attendances_timeseries_flow(
    data = weekly_ed_att_flgrp,
    startDate = as.Date("2016-06-01", tz = "Europe/London"),
    endDate = as.Date("2016-06-30", tz = "Europe/London"),
    timeUnit = "weekly",
    returnPlot = FALSE,
    hospitalName = "hospital_name",
    restrictPlotRange = TRUE
  )

  result <- result %>%
    tibble::as_tibble()

  # Test results are correct
  expect_equal(correct_answers, result)
})
