context("Arrivals and Occupancy by hour of the day")
library(hospitalflow)

test_that("arrivals and occupancy by hour of the day is correctly calculated",{

  #Specify correct results
  correct_answers_arriv_occ <- tibble::tibble(
    Hour = as.character(c(0, 1, 2, 3, 4, 5, 6, 7, 8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22,  23)),

    Average_arrivals = as.numeric(c(0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0.5, 0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)),
    Average_occupancy = as.numeric(c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5,  0.5, 0.0, 0.5, 0.5, 1, 1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,  0.5, 0.5, 0.5,  0.5, 0.5)))


  str(correct_answers_arriv_occ)
  #creating a dataset


  dt1 <- c("2018-12-10 09:30:00","2018-12-10 10:15:00",  "2018-12-11 08:05:00")

  dt2 <- c("2018-12-10 12:30:00", "2018-12-11 07:45:00", "2018-12-11 09:05-00")

  spell_class_col <- as.character(c("ed_non_admission", "ed_comp_non_admission", "ed_non_admission"))

  spell_start <- as.POSIXct(dt1, tz = "Europe/London")
  spell_end <- as.POSIXct(dt2,tz = "Europe/London")

  test_arrivals_occupancy <- tibble::tibble(spell_number = 101:103,
                                            spell_start,
                                            spell_end,
                                            spell_class_col)


  #Run Admission Discharges graph

  result_arriv_occ<- hospitalflow::ae_arrival_occupancy_fct(start_date = as.Date("2018-12-10",tz = "Europe/London"),
                                                            end_date = as.Date("2018-12-11",tz = "Europe/London"),
                                                            data = test_arrivals_occupancy, plot_chart = FALSE,
                                                            hospital_name = "Hospital name")

  #result_ariv_occ_data_jm <- result_arriv_occ_jan_march$data
  result_arriv_occh$Hour <- as.character(result_arriv_occ$Hour)

  #Test results are correct
  expect_equal(as.data.frame(correct_answers_arriv_occ), as.data.frame(result_arriv_occ), tolerance = 0.01)

})


context("Arrivals and Occupancy by hour of the day")
library(hospitalflow)

test_that("arrivals and occupancy by hour of the day is correctly calculated",{

  load("D:/Rprojects/hospitalflow/tests/testthat/testdata/occupancy/occupancy_test.rda")

  occupancy_test <- occupancy %>%
    dplyr::select(spell_number, spell_start, spell_end)

  #Specify correct results
  correct_answers <- tibble::tibble(
    time_hr = as.POSIXct(c("2016-06-28 18:00:00", "2016-06-28 19:00:00")),

    occupancy = as.numeric(c(1, 1)))

  result_occ <- ae_arrival_occupancy_fct(start_date = as.POSIXct("2016-06-28 18:00:00",tz = "Europe/London"),
                                            end_date = as.POSIXct("2016-06-28 19:00:00",tz = "Europe/London"),
                                            data = occupancy_test)

  #Test results are correct
  expect_equal(as.data.frame(correct_answers), as.data.frame(result_occ), tolerance = 0.01)

})

