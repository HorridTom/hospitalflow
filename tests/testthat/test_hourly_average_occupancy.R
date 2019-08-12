context("Arrivals and Occupancy by hour of the day")
library(hospitalflow)

test_that("arrivals and occupancy by hour of the day is correctly calculated",{

  #Specify correct results
  correct_answers_arriv_occ <- tibble::tibble(
    Hour = as.character(c(0, 1, 2, 3, 4, 5, 6, 7, 8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22,  23)),

    Average_arrivals = as.numeric(c(0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0.5, 0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)),
    Average_occupancy = as.numeric(c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0, 0.5, 0.5, 1, 1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,  0.5, 0.5, 0.5,  0.5, 0.5)))

  #creating a dataset


  dt1 <- c("2018-12-10 09:30:00","2018-12-10 10:15:00",  "2018-12-11 08:05:00")

  dt2 <- c("2018-12-10 12:30:00", "2018-12-11 07:45:00", "2018-12-11 09:05-00")

  spell_class_col <- as.character(c("ed_non_admission", "ed_comp_non_admission", "ed_non_admission"))

  starts_with_ed <- as.character(c(TRUE, TRUE, TRUE))

  spell_start <- as.POSIXct(dt1, tz = "Europe/London")

  initial_ed_end_datetime <- as.POSIXct(dt2,tz = "Europe/London")

  test_arrivals_occupancy <- tibble::tibble(spell_number = 101:103,
                                            spell_start,
                                            initial_ed_end_datetime,
                                            spell_class_col,
                                            starts_with_ed)


  #Run Admission Discharges graph

  result_arriv_occ<- ae_arrival_occupancy(start_date = as.POSIXct("2018-12-10 01:00:00",tz = "Europe/London"),
                                              end_date = as.POSIXct("2018-12-12 00:00:00",tz = "Europe/London"),
                                              data = test_arrivals_occupancy,
                                              plot_chart = FALSE,
                                              hospital_name = "Hospital name")

  #result_ariv_occ_data_jm <- result_arriv_occ_jan_march$data
  result_arriv_occ$Hour <- as.character(result_arriv_occ$Hour)

  #Test results are correct
  expect_equal(as.data.frame(correct_answers_arriv_occ), as.data.frame(result_arriv_occ), tolerance = 0.01)

})


context("Arrivals and Occupancy by hour of the day, for real data")
library(hospitalflow)

test_that("arrivals and occupancy by hour of the day is correctly calculated",{

  occupancy_arrival_test <- readRDS("testdata/arrival_occupancy/occupancy_arrival_test.rds")

  occupancy_arrival_test <- occupancy_arrival_test %>%
    dplyr::select(spell_number, spell_start, initial_ed_end_datetime, spell_class_col, starts_with_ed)

  #Specify correct results
  correct_answers <- tibble::tibble(
    Hour = as.numeric(c(0, 1, 2, 3, 4, 5, 23)),

    Average_arrivals = as.numeric(c(0, 0, 0, 0, 0, 0, 2)),
    Average_occupancy = as.numeric(c(2, 2, 2, 2, 2, 2, 0)))

  result_occ <- ae_arrival_occupancy(start_date = as.POSIXct("2016-05-31 23:00:00",tz = "Europe/London"),
                                         end_date = as.POSIXct("2016-06-01 05:00:00",tz = "Europe/London"),
                                         data = occupancy_arrival_test, plot_chart = FALSE, hospital_name = "Hospital Name")

  #Test results are correct
  expect_equal(as.data.frame(correct_answers), as.data.frame(result_occ), tolerance = 0.01)

})

