context("Occupancy and Admissions by hour of the day")
library(hospitalflow)


test_that("Arrivals_Occupancy_January-March 2015",{

  load("testdata/arrivals_occupancy_jan_march.rda")

  #Specify correct results
  correct_answers_arriv_occ <- tibble::tibble(
           Hour = as.character(c(0, 1, 2, 3, 4, 5, 6, 7, 8,    9,   10, 11,   12,     13,   14,    15,    16, 17, 18,    19,    20,    21,    22,   23)),

    Average_arrivals = as.numeric(c(3.62, 2.66, 2.03, 1.72, 1.55, 1.41, 1.32, 1.27, 2.35, 4.24, 6.98, 7.66,  7.54,   6.8,   7.12,  6.63, 6.9,  6.46,  6.64,  6.55,  6.04,  5.25,  4.55, 3.55)),
         Average_occ = as.numeric(c(9.14, 7.12, 6.12, 5.14, 4.41, 3.76, 3.18, 3.11, 3.48, 5.82, 9.01, 10.87, 12.45, 12.45, 13.76, 13.58, 13.45, 13.73, 13.21, 13.21, 12.53, 11.51, 11.37, 9.36)))


  #Run Admission Discharges graph

  result_arriv_occ_jan_march <- arrival_occupancy(start_date = as.Date("2015-01-01",tz = "Europe/London"), end_date = as.Date("2015-03-31",tz = "Europe/London"),
                                                  data = arrivals_occupancy_jan_march, plot_chart = TRUE, hospital_name = "Chelsea & Westminster")

  result_ariv_occ_data_jm <- result_arriv_occ_jan_march$data
  result_ariv_occ_data_jm$Hour <- as.character(result_ariv_occ_data_jm$Hour)


  #Test results are correct
  expect_equal(as.data.frame(correct_answers_arriv_occ), as.data.frame(result_ariv_occ_data_jm), tolerance = 0.01)


})

test_that("Arrivals_Occupancy_test_2",{

  load("testdata/arrivals_occupancy_test_2.rda")

  #Specify correct results
  correct_answers_arriv_occ <- tibble::tibble(
    Hour = as.character(c(0, 1, 2, 3, 4, 5, 6, 7, 8,    9,   10, 11,   12,     13,   14,    15,    16, 17, 18,    19,    20,    21,    22,   23)),

    Average_arrivals = as.numeric(c(0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0.5, 0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)),
    Average_occ = as.numeric(c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5,  0.5, 0.5, 0, 0.5, 1, 1, 1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,  0.5, 0.5, 0.5,  0.5, 0.5)))


  #Run Admission Discharges graph

  result_arriv_occ_jan_march <- arrival_occupancy(start_date = as.Date("2018-12-10",tz = "Europe/London"), end_date = as.Date("2018-12-11",tz = "Europe/London"),
                                                  data = arrivals_occupancy_test_2, plot_chart = TRUE, hospital_name = "Chelsea & Westminster")

  result_ariv_occ_data_jm <- result_arriv_occ_jan_march$data
  result_ariv_occ_data_jm$Hour <- as.character(result_ariv_occ_data_jm$Hour)


  #Test results are correct
  expect_equal(as.data.frame(correct_answers_arriv_occ), as.data.frame(result_ariv_occ_data_jm), tolerance = 0.01)


})
