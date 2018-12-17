context("Admissions and Discharges by Weekday plot")
library(hospitalflow)


test_that("Admission numbers by day of the Week for January-Dec 2015",{

  load("testdata/test_sample_data.rda")

  #Specify correct results
  correct_answers <- tibble::tibble(
    Weekday = as.character(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun",  "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),

    Event =  c("Avg_admissions", "Avg_admissions","Avg_admissions", "Avg_admissions","Avg_admissions","Avg_admissions","Avg_admissions",
              "Avg_discharges","Avg_discharges","Avg_discharges","Avg_discharges","Avg_discharges","Avg_discharges","Avg_discharges",
              "Non_emergency_admissions","Non_emergency_admissions","Non_emergency_admissions","Non_emergency_admissions","Non_emergency_admissions",
              "Non_emergency_admissions","Non_emergency_admissions"),

    Value = c(12.88, 13.19, 12.58, 12.46, 11.52, 9.38, 9.37,
              12.10, 12.13, 12.21, 11.65, 12.65, 10.77, 9.88,
              2.88, 3.85, 3.28, 3.96, 2.62, 1.81, 1.67))

  correct_answers <- correct_answers %>%
    dplyr::arrange(Event, Weekday)

 #Run Admission Discharges graph
  result <- admissions_discharges(start_date = as.Date("2014-01-01 00:00:00", tz = "Europe/London"), end_date = as.Date("2014-12-31 00:00:00",tz = "Europe/London"), data = admission_discharge, plot_chart = TRUE, hospital_name = "Chelsea & Westminster" )

  result_data <- result$data
  result_data$Weekday <- as.character(result_data$Weekday)


  result_data <- result_data %>%
    dplyr::arrange(Event, Weekday)

  #Test results are correct
  expect_equal(as.data.frame(result_data), as.data.frame(correct_answers), tolerance = 0.01)


 })

