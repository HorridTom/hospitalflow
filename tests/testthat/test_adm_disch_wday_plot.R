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

    Value = c(2.82, 3.75, 3.35, 3.92, 2.51, 1.73, 1.57, 2.46, 2.59, 2.94, 3.19, 3.75, 2.71, 2.13, 2.63, 3.42, 2.98, 3.48, 2.34, 1.59, 1.46))

  correct_answers <- correct_answers %>%
    dplyr::arrange(Event, Weekday)

 #Run Admission Discharges graph
  result <- admissions_discharges(start_date = as.Date("2014-01-01 00:00:00", tz = "Europe/London"), end_date = as.Date("2015-01-01 00:00:00",tz = "Europe/London"), data = admission_discharge, plot_chart = TRUE, hospital_name = "Chelsea & Westminster" )

  result_data <- result$data
  result_data$Weekday <- as.character(result_data$Weekday)


  result_data <- result_data %>%
    dplyr::arrange(Event, Weekday)

  #Test results are correct
  expect_equal(as.data.frame(result_data), as.data.frame(correct_answers), tolerance = 0.1)


 })

