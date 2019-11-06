context("Readmissions for 90 days")
library(hospitalflow)

test_that("readmissions within 90 days is giving the right expected answer",{

  readmissions_dt <- readRDS("testdata/readmission_test/dt_readmission_test.rds")

  #Specify correct results
  correct_answers <- tibble::tibble(

    x =  as.Date(c( "2019-01-01", "2019-02-01", "2019-03-01", "2019-04-01", "2019-06-01")),

    y = as.numeric(c(33.3, 50, 50, 50, 50))
  )


  #Run four_hr_performance
  result <- readmissions_ip(start_date = as.POSIXct("2019-01-01 00:00:00", tz = "Europe/London"),
                          end_date = as.POSIXct("2019-06-02 00:00:00", tz = "Europe/London"),
                          data = readmissions_dt, plot_chart = FALSE, hospital_name = "Hospital_name",
                          readmission_by = 90)

  result <- result %>%
    dplyr::select(x, y)

  result$y <- round(result$y, digits = 1)

  #Test results are correct
  expect_equal(result, correct_answers, tolerance = 0.1)

})
