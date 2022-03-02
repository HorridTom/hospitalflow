context("calculate and plot the Unscheduled ED 'occupancy' by day and hour between startDay and endDay")
library(hospitalflow)

# constructed data
pseudo_id <- c(1, 2, 5, 7, 8, 9, 10)
start_datetime <- c(
  "2013-01-01 00:12:34", "2013-01-01 00:05:35", "2013-01-03 01:07:00", "2013-01-04 01:10:02",
  "2013-01-05 00:10:03", "2013-01-05 02:10:04", "2013-01-13 00:12:05"
)
end_datetime <- c(
  "2013-01-11 23:54:00", "2013-01-03 23:54:01", "2013-01-10 23:45:00", "2013-01-14 23:45:02",
  "2013-01-12 23:55:03", "2013-01-13 23:45:04", "2013-01-14 22:56:05"
)
ED_day_hour_test_data <- tibble::tibble(pseudo_id, start_datetime, end_datetime)


test_that("Occupancy is calculated correctly for constructed data", {
  occupancy_day1 <- occupancy(time_instance = as.POSIXct("2013-01-01 12:00:00"), df = ED_day_hour_test_data, df_type = "ED table")
  occupancy_day6 <- occupancy(time_instance = as.POSIXct("2013-01-06 12:00:00"), df = ED_day_hour_test_data, df_type = "ED table")
  occupancy_day13 <- occupancy(time_instance = as.POSIXct("2013-01-13 12:00:00"), df = ED_day_hour_test_data, df_type = "ED table")

  occupancies <- c(occupancy_day1, occupancy_day6, occupancy_day13)

  testthat::expect_equal(occupancies, c(2, 5, 3))
})


occupancy_df_test <- ED_day_hour("2013-01-01", "2013-01-15", df = ED_day_hour_test_data)


test_that("Average is calculated correctly for constructed data", {

  # averages at 12:00:00 each day
  average_mon <- occupancy_df_test[[13, "average"]]
  average_tues <- occupancy_df_test[[37, "average"]]
  average_wed <- occupancy_df_test[[61, "average"]]
  average_thurs <- occupancy_df_test[[85, "average"]]
  average_fri <- occupancy_df_test[[109, "average"]]
  average_sat <- occupancy_df_test[[133, "average"]]
  average_sun <- occupancy_df_test[[157, "average"]]
  averages <- c(average_mon, average_tues, average_wed, average_thurs, average_fri, average_sat, average_sun)

  testthat::expect_equal(averages, c(3.5, 3.5, 3.5, 4, 3.5, 4, 4))
})


test_that("Q1 is calculated correctly for constructed data", {
  q1_mon <- occupancy_df_test[[13, "Q1"]]
  q1_tues <- occupancy_df_test[[37, "Q1"]]
  q1_wed <- occupancy_df_test[[61, "Q1"]]
  q1_thurs <- occupancy_df_test[[85, "Q1"]]
  q1_fri <- occupancy_df_test[[109, "Q1"]]
  q1_sat <- occupancy_df_test[[133, "Q1"]]
  q1_sun <- occupancy_df_test[[157, "Q1"]]
  q1s <- c(q1_mon, q1_tues, q1_wed, q1_thurs, q1_fri, q1_sat, q1_sun)

  testthat::expect_equal(q1s, c(2.75, 2.75, 2.75, 3.5, 3.25, 3.5, 3.5))
})


test_that("Q3 is calculated correctly for constructed data", {
  q3_mon <- occupancy_df_test[[13, "Q3"]]
  q3_tues <- occupancy_df_test[[37, "Q3"]]
  q3_wed <- occupancy_df_test[[61, "Q3"]]
  q3_thurs <- occupancy_df_test[[85, "Q3"]]
  q3_fri <- occupancy_df_test[[109, "Q3"]]
  q3_sat <- occupancy_df_test[[133, "Q3"]]
  q3_sun <- occupancy_df_test[[157, "Q3"]]
  q3s <- c(q3_mon, q3_tues, q3_wed, q3_thurs, q3_fri, q3_sat, q3_sun)

  testthat::expect_equal(q3s, c(4.25, 4.25, 4.25, 4.5, 3.75, 4.5, 4.5))
})


test_that("Max is calculated correctly for constructed data", {
  max_mon <- occupancy_df_test[[13, "Max"]]
  max_tues <- occupancy_df_test[[37, "Max"]]
  max_wed <- occupancy_df_test[[61, "Max"]]
  max_thurs <- occupancy_df_test[[85, "Max"]]
  max_fri <- occupancy_df_test[[109, "Max"]]
  max_sat <- occupancy_df_test[[133, "Max"]]
  max_sun <- occupancy_df_test[[157, "Max"]]
  maxes <- c(max_mon, max_tues, max_wed, max_thurs, max_fri, max_sat, max_sun)

  testthat::expect_equal(maxes, c(5, 5, 5, 5, 4, 5, 5))
})


test_that("Min is calculated correctly for constructed data", {
  min_mon <- occupancy_df_test[[13, "Min"]]
  min_tues <- occupancy_df_test[[37, "Min"]]
  min_wed <- occupancy_df_test[[61, "Min"]]
  min_thurs <- occupancy_df_test[[85, "Min"]]
  min_fri <- occupancy_df_test[[109, "Min"]]
  min_sat <- occupancy_df_test[[133, "Min"]]
  min_sun <- occupancy_df_test[[157, "Min"]]
  mins <- c(min_mon, min_tues, min_wed, min_thurs, min_fri, min_sat, min_sun)

  testthat::expect_equal(mins, c(2, 2, 2, 3, 3, 3, 3))
})
