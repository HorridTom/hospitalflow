context("Arrivals and Occupancy by hour of the day")
library(hospitalflow)

test_that("Average occupancy, interquartile range, range correctly calculated", {
  test_occupancy_hr_day <- readRDS("testdata/occupancy_hr_day/test_occupancy_hr_day.rds")

  # Specify correct results
  correct_answers <- tibble::tibble(
    hour = as.integer(c(11, 11, 11, 11, 11, 11, 11)),
    Weekday = as.factor(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")),
    average_occupancy = as.numeric(c(2.0, 2.5, 1.33, 1, 1.5, 1, 1.5)),
    Q1 = c(`25%` = 1.5, `25%` = 2.25, `25%` = 1, `25%` = 1, `25%` = 1.25, `25%` = 1, `25%` = 1.25),
    Q3 = c(`75%` = 2.5, `75%` = 2.75, `75%` = 1.5, `75%` = 1, `75%` = 1.75, `75%` = 1, `75%` = 1.75),
    Min_n = as.numeric(c(1, 2, 1, 1, 1, 1, 1)),
    Max_n = as.numeric(c(3, 3, 2, 1, 2, 1, 2))
  )

  correct_answers <- correct_answers %>%
    dplyr::mutate(Weekday = factor(Weekday, levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), ordered = FALSE))

  result <- occupancy_weekday_hour(
    start_date = as.POSIXct("2019-01-01 00:00:00", tz = "Europe/London"),
    end_date = as.POSIXct("2019-01-16 00:00:00", tz = "Europe/London"),
    data = test_occupancy_hr_day, plot_chart = FALSE,
    hospital_name = "Hospital Name"
  )

  result <- result %>%
    dplyr::filter(hour == 11)

  result <- result %>%
    dplyr::mutate(Weekday = factor(Weekday, levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), ordered = FALSE)) %>%
    tibble::as_tibble()

  # Test results are correct
  expect_equal(result, correct_answers, tolerance = 0.01)
})
