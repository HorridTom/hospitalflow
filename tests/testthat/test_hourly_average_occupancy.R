context("Admissions and Occupancy by hour of the day")
library(hospitalflow)

test_that("admissions and occupancy by hour of the day",{

  #Specify correct results
  correct_answers_arriv_occ <- tibble::tibble(
    Hour = as.character(c(0, 1, 2, 3, 4, 5, 6, 7, 8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22,  23)),

    Average_arrivals = as.numeric(c(0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0.5, 0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)),
    Average_occupancy = as.numeric(c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5,  0.5, 0.0, 0.5, 0.5, 1, 1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,  0.5, 0.5, 0.5,  0.5, 0.5)))


  str(correct_answers_arriv_occ)
  #creating a dataset


  dt1 <- c("2018-12-10 09:30:00","2018-12-10 10:15:00",  "2018-12-11 08:05:00")

  dt2 <- c("2018-12-10 12:30:00", "2018-12-11 07:45:00", "2018-12-11 09:05-00")

  start_datetime <- as.POSIXct(dt1, tz = "Europe/London")
  end_datetime <- as.POSIXct(dt2,tz = "Europe/London")

  test_arrivals_occupancy <- tibble::tibble(pseudo_id = 101:103,
                                            start_datetime,
                                            end_datetime)


  #Run Admission Discharges graph

  result_arriv_occ_jan_march <- hospitalflow::arrival_occupancy_hr(start_date = as.POSIXct("2018-12-10 00:00",tz = "Europe/London"),
                                                                   end_date = as.POSIXct("2018-12-11 23:00",tz = "Europe/London"),
                                                  data = test_arrivals_occupancy, plot_chart = TRUE, hospital_name = "Hospital name")

  #result_ariv_occ_data_jm <- result_arriv_occ_jan_march$data
  result_arriv_occ_jan_march$Hour <- as.character(result_arriv_occ_jan_march$Hour)

  #Test results are correct
  expect_equal(as.data.frame(correct_answers_arriv_occ), as.data.frame(result_arriv_occ_jan_march), tolerance = 0.01)

})

