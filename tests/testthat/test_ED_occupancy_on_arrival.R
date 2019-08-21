context("Occupancy on arrival analysis")
library(hospitalflow)

# tests with constructed data

pseudo_id <- c(1,  2,  3,  4,  5,  6,  7,  8,  9, 10)
attendance_category <- c("First Attendance", "First Attendance", "First Attendance", "First Attendance",
                         "Follow up - Un-planned", "Follow up - Planned", "First Attendance","First Attendance",
                         "First Attendance", "First Attendance")
start_datetime <- c("2017-05-01 00:00:00 UTC", "2017-05-01 12:00:00 UTC", "2017-05-01 00:00:00 UTC",
                    "2017-05-02 00:00:00 UTC", "2017-05-02 00:00:00 UTC", "2017-05-03 00:00:00 UTC",
                    "2017-05-02 12:00:00 UTC", "2017-05-01 00:00:00 UTC", "2017-05-01 00:00:00 UTC",
                    "2017-05-02 00:00:00 UTC")
end_datetime <- c("2017-05-02 00:00:00 UTC", "2017-05-02 12:00:00 UTC", "2017-05-03 12:00:00 UTC",
                  "2017-05-02 12:00:00 UTC", "2017-05-03 00:00:00 UTC", "2017-05-03 12:00:00 UTC",
                  "2017-05-03 12:00:00 UTC", "2017-05-03 00:00:00 UTC", "2017-05-03 00:00:00 UTC",
                  "2017-05-03 12:00:00 UTC")
initial_assess_datetime <- c("2017-05-01 12:00:00 UTC", "2017-05-02 00:00:00 UTC", "2017-05-01 00:00:00 UTC",
                             "2017-05-02 00:00:00 UTC", "2017-05-03 00:00:00 UTC", "2017-05-03 00:00:00 UTC",
                             "2017-05-03 00:00:00 UTC", "2017-05-01 00:00:00 UTC", "2017-05-02 00:00:00 UTC",
                             "2017-05-02 00:00:00 UTC")
treatment_datetime <- c("2017-05-02 12:00:00 UTC", "2017-05-03 00:00:00 UTC", "2017-05-02 00:00:00 UTC",
                        "2017-05-03 00:00:00 UTC", "2017-05-04 00:00:00 UTC", "2017-05-04 00:00:00 UTC",
                        "2017-05-04 00:00:00 UTC", "2017-05-02 00:00:00 UTC", "2017-05-03 00:00:00 UTC",
                        "2017-05-03 00:00:00 UTC")

constructed_test_data <- data.frame(pseudo_id, attendance_category, start_datetime, end_datetime,
                                    initial_assess_datetime, treatment_datetime)

test_that("occupancies on arrival are correct",{

  correct_answers <- c(4, 5, 4, 8, 8, 7, 8, 4, 4, 8)
  result <- occupancy_on_arrival(constructed_test_data)$occupancy_on_arrival

  expect_equal(result, correct_answers)


})


#tests with adapted* real data sample. *adpadpted because such a small sample did not contain enough/any LoSs over 4hrs

real_test_data <- readRDS("testdata/test2_data_occ_on_arrival_real.rds")

test_that("occupancies on arrival are correct for real data",{

  correct_answers <- c(12, 4 , 10, 10, 2, 1, 6, 11, 5, 10, 3, 5, 8, 14, 14, 13, 4, 7)
  result <- occupancy_on_arrival(real_test_data)$occupancy_on_arrival

  expect_equal(result, correct_answers)


})

real_data_plot_df <- ed_occupancy_on_arrival_plot(real_test_data,
                                                  "2017-05-04 00:00:00", "2017-05-05 00:00:00", plotChart = F)

test_that("n_all is correct for real data",{

  correct_answers <- c(1,1,1,2,2,1,1,1,3,1,1,1,2)

  result <- real_data_plot_df$n_all

  expect_equal(result, correct_answers)

})

test_that("n_LoS_over_4hrs is correct for real data",{

  correct_answers <- c(0,1,0,1,0,0,0,0,0,0,1,0,0)

  result <- real_data_plot_df$n_LoS_over_4hrs

  expect_equal(result, correct_answers)

})

test_that("perc_LoS_over_4hrs is correct for real data",{

  correct_answers <- c(0,100,0,50,0,0,0,0,0,0,100,0,0)

  result <- real_data_plot_df$perc_LoS_over_4hrs

  expect_equal(result, correct_answers)

})

test_that("n_W4T_over_4hrs is correct for real data",{

  correct_answers <- c(0,0,0,0,0,0,0,0,0,0,1,0,0)

  result <- real_data_plot_df$n_W4T_over_4hrs

  expect_equal(result, correct_answers)

})

test_that("perc_W4T_over_4hrs is correct for real data",{

  correct_answers <- c(0,0,0,0,0,0,0,0,0,0,100,0,0)

  result <- real_data_plot_df$perc_W4T_over_4hrs

  expect_equal(result, correct_answers)

})

