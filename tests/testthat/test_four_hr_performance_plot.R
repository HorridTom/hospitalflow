context("Four hours performance")
library(hospitalflow)

test_that("four hour performance is giving the right expected answer",{

  four_hr_performance <- readRDS("testdata/four_hr_performance/four_hr_performance.rds")
  four_hr_performance <- four_hr_performance %>%
    dplyr::mutate(start_datetime = as.POSIXct(start_datetime, "%Y/%m/%d %H:%M", tz = "Europe/London"),
                  end_datetime = as.POSIXct(end_datetime, "%Y/%m/%d %H:%M", tz = "Europe/London"))
  #Specify correct results
  correct_answers <- tibble::tibble(

    x =  as.Date(c( "2019-01-01", "2019-02-01", "2019-03-01", "2019-04-01", "2019-06-01")),

    y = as.numeric(c(60,40,60,80,50))
  )


  #Run four_hr_performance
  result <- hospitalflow::four_hrs_perf(start_dt = as.POSIXct("2019-01-01 00:00:00", tz = "Europe/London"),
                          end_dt = as.POSIXct("2019-06-02 00:00:00", tz = "Europe/London"),
                          data = four_hr_performance, time_unit = "month",
                          plot_chart = FALSE, hospital_name = "Hospital_name")

  result <- result %>% dplyr::select(x, y)

  #Test results are correct
  expect_equal(result, correct_answers, tolerance = 0.1)

})
