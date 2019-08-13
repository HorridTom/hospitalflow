context("calculate and plot the Unscheduled ED 'occupancy' by day and hour between startDay and endDay")
library(hospitalflow)

pseudo_id <- c(1, 2,  5,  7,  8,  9, 10)
start_datetime <- c("2013-01-01 00:12:34", "2013-01-01 00:05:35", "2013-01-03 01:07:00", "2013-01-04 01:10:02",
                    "2013-01-05 00:10:03", "2013-01-05 02:10:04", "2013-01-13 00:12:05")
end_datetime <- c("2013-01-11 23:54:00", "2013-01-03 23:54:01", "2013-01-10 23:45:00", "2013-01-14 23:45:02",
                  "2013-01-12 23:55:03", "2013-01-13 23:45:04", "2013-01-14 22:56:05")
ED_day_hour_test_data <- tibble::tibble(pseudo_id, start_datetime, end_datetime)

testthat::test_that("Occupancy is calculated correctly",{

  occupancy_day1 <- occupancy("2013-01-01 12:00:00", df = ED_day_hour_test_data, start_time = "start_datetime", end_time = "end_datetime")
  occupancy_day6 <- occupancy("2013-01-06 12:00:00", df = ED_day_hour_test_data, start_time = "start_datetime", end_time = "end_datetime")
  occupancy_day13 <- occupancy("2013-01-13 12:00:00", df = ED_day_hour_test_data, start_time = "start_datetime", end_time = "end_datetime")

  testthat::expect_equal(occupancy_day1, 2)
  testthat::expect_equal(occupancy_day6, 5)
  testthat::expect_equal(occupancy_day13, 3)
})

occupancy_df_test <- ED_day_hour("2013-01-01","2013-01-15", df = ED_day_hour_test_data)

testthat::test_that("Average is calculated correctly",{

  average_mon <- occupancy_df_test[[13,"average"]]
  average_tues <- occupancy_df_test[[37,"average"]]
  average_wed <- occupancy_df_test[[61,"average"]]
  average_thurs <- occupancy_df_test[[85,"average"]]
  average_fri <- occupancy_df_test[[109,"average"]]
  average_sat <- occupancy_df_test[[133,"average"]]
  average_sun <- occupancy_df_test[[157,"average"]]

  testthat::expect_equal(average_mon, 3.5)
  testthat::expect_equal(average_tues, 3.5)
  testthat::expect_equal(average_wed, 3.5)
  testthat::expect_equal(average_thurs, 4)
  testthat::expect_equal(average_fri, 3.5)
  testthat::expect_equal(average_sat, 4)
  testthat::expect_equal(average_sun, 4)

})


testthat::test_that("Q1 is calculated correctly",{

  q1_mon <- occupancy_df_test[[13,"Q1"]]
  q1_tues <- occupancy_df_test[[37,"Q1"]]
  q1_wed <- occupancy_df_test[[61,"Q1"]]
  q1_thurs <- occupancy_df_test[[85,"Q1"]]
  q1_fri <- occupancy_df_test[[109,"Q1"]]
  q1_sat <- occupancy_df_test[[133,"Q1"]]
  q1_sun <- occupancy_df_test[[157,"Q1"]]

  testthat::expect_equal(q1_mon, 2.75)
  testthat::expect_equal(q1_tues, 2.75)
  testthat::expect_equal(q1_wed, 2.75)
  testthat::expect_equal(q1_thurs, 3.5)
  testthat::expect_equal(q1_fri, 3.25)
  testthat::expect_equal(q1_sat, 3.5)
  testthat::expect_equal(q1_sun, 3.5)
})

testthat::test_that("Q3 is calculated correctly",{

  q3_mon <- occupancy_df_test[[13,"Q3"]]
  q3_tues <- occupancy_df_test[[37,"Q3"]]
  q3_wed <- occupancy_df_test[[61,"Q3"]]
  q3_thurs <- occupancy_df_test[[85,"Q3"]]
  q3_fri <- occupancy_df_test[[109,"Q3"]]
  q3_sat <- occupancy_df_test[[133,"Q3"]]
  q3_sun <- occupancy_df_test[[157,"Q3"]]

  testthat::expect_equal(q3_mon, 4.25)
  testthat::expect_equal(q3_tues, 4.25)
  testthat::expect_equal(q3_wed, 4.25)
  testthat::expect_equal(q3_thurs, 4.5)
  testthat::expect_equal(q3_fri, 3.75)
  testthat::expect_equal(q3_sat, 4.5)
  testthat::expect_equal(q3_sun, 4.5)
})

testthat::test_that("Max is calculated correctly",{

  max_mon <- occupancy_df_test[[13,"Max"]]
  max_tues <- occupancy_df_test[[37,"Max"]]
  max_wed <- occupancy_df_test[[61,"Max"]]
  max_thurs <- occupancy_df_test[[85,"Max"]]
  max_fri <- occupancy_df_test[[109,"Max"]]
  max_sat <- occupancy_df_test[[133,"Max"]]
  max_sun <- occupancy_df_test[[157,"Max"]]

  testthat::expect_equal(max_mon, 5)
  testthat::expect_equal(max_tues, 5)
  testthat::expect_equal(max_wed, 5)
  testthat::expect_equal(max_thurs, 5)
  testthat::expect_equal(max_fri, 4)
  testthat::expect_equal(max_sat, 5)
  testthat::expect_equal(max_sun, 5)
})

testthat::test_that("Min is calculated correctly",{

  min_mon <- occupancy_df_test[[13,"Min"]]
  min_tues <- occupancy_df_test[[37,"Min"]]
  min_wed <- occupancy_df_test[[61,"Min"]]
  min_thurs <- occupancy_df_test[[85,"Min"]]
  min_fri <- occupancy_df_test[[109,"Min"]]
  min_sat <- occupancy_df_test[[133,"Min"]]
  min_sun <- occupancy_df_test[[157,"Min"]]

  testthat::expect_equal(min_mon, 2)
  testthat::expect_equal(min_tues, 2)
  testthat::expect_equal(min_wed, 2)
  testthat::expect_equal(min_thurs, 3)
  testthat::expect_equal(min_fri, 3)
  testthat::expect_equal(min_sat, 3)
  testthat::expect_equal(min_sun, 3)

})
