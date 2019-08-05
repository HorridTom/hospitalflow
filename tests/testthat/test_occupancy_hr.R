context("Occupancy by hour of a date")
library(hospitalflow)

test_that("Occupancy by hour of a date is generated correctly for two dates",{

  #Specify correct results
  correct_answers <- tibble::tibble(
    time_hr = as.POSIXct(c("2018-12-10 09:00:00", "2018-12-10 10:00:00" )),

    occupancy = as.numeric(c(1, 2)))

  #creating a dataset


  dt1 <- c("2018-12-10 09:30:00","2018-12-10 10:15:00",  "2018-12-11 08:05:00")

  dt2 <- c("2018-12-10 12:30:00", "2018-12-11 07:45:00", "2018-12-11 09:05-00")

  spell_start <- as.POSIXct(dt1, tz = "Europe/London")
  spell_end <- as.POSIXct(dt2,tz = "Europe/London")

  test_arrivals_occupancy <- tibble::tibble(pseudo_id = 101:103,
                                            spell_start,
                                            spell_end)

  result_occ <- hospitalflow::occupancy_fct(start_date = as.POSIXct("2018-12-10 09:00",tz = "Europe/London"),
                                            end_date = as.POSIXct("2018-12-10 10:00",tz = "Europe/London"),
                                            data = test_arrivals_occupancy)

  result_occ <- result_occ %>%
    dplyr::select(time_hr, occupancy)


  #Test results are correct
  expect_equal(as.data.frame(correct_answers), as.data.frame(result_occ), tolerance = 0.01)

})
