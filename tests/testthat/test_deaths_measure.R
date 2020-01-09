context("Deaths by month")
library(hospitalflow)

test_that("deaths by month is giving the right expected answer",{

  deaths_dt <- readRDS("testdata/deaths_measure_test/deaths_measure_test.rds")

  #Specify correct results
  correct_answers <- tibble::tibble(

    x =  as.Date(c( "2019-01-01", "2019-02-01", "2019-03-01", "2019-04-01",
                    "2019-06-01", "2019-07-01", "2019-10-01", "2019-11-01", "2019-12-01")),

    y = as.numeric(c(66.7, 50, 50, 100, 50, 100, 100, 100,100))
  )


  #Run four_hr_performance
  result <- mortality_timeser(start_date = as.POSIXct("2019-01-01 00:00:00", tz = "Europe/London"),
                            end_date = as.POSIXct("2019-12-31 00:00:00", tz = "Europe/London"),
                            data = deaths_dt, plot_chart = FALSE, hospital_name = "Hospital_name")

  result <- result %>%
    dplyr::select(x, y)

  result$y <- round(result$y, digits = 1)

  #Test results are correct
  expect_equal(result, correct_answers, tolerance = 0.1)

})
