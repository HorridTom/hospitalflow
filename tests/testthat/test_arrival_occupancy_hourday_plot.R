context("Occupancy and Admissions by hour of the day")
library(hospitalflow)


test_that("Arrivals_Occupancy_January-March 2015",{

  load("testdata/arrivals_occupancy_jan_march.rda")

  #Specify correct results
  correct_answers_arriv_occ <- tibble::tibble(
           Hour = as.character(c(0,   1,    2,    3,    4, 5, 6,    7,    8,    9,   10, 11,   12,     13,   14,    15,    16, 17, 18,    19,    20,    21,    22,   23)),

    Avg_arrivals = as.numeric(c(3.33, 3.33, 1,    2,    1, 2, 1,    1.66, 5,    5,    6, 6,    9,     9.66,  5.66,  7,     10, 11, 4.33,   5.66, 6.33,   6.33, 3.66, 4.66)),
    Average_Occ = as.numeric(c( 8.66, 7.33, 5.66, 7.66, 3, 4, 2.33, 3,    6.66, 7.33, 8, 9.66, 14.33, 16.33, 12.33, 15.66, 19, 20, 11.33, 14,    12.66, 13.66, 7.33, 11.33)))


  #Run Admission Discharges graph

  result_arriv_occ_jan_jun <- arrival_occupancy(start_date = "2015-01-01 00:00:00", end_date = "2015-04-01 00:00:00", data = arrivals_occupancy_jan_march, plot_chart = TRUE)

  result_ariv_occ_data_jj <- result_arriv_occ_jan_jun$data
  result_ariv_occ_data_jj$Hour <- as.character(result_ariv_occ_data_jj$Hour)


  #Test results are correct
  expect_equal(as.data.frame(result_ariv_occ_data_jj), as.data.frame(correct_answers_arriv_occ), tolerance = 0.01)


})
