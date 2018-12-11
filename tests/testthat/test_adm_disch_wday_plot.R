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

    Value = c(1.61, 1.69, 1.67, 1.69, 1.41, 0.98, 0.96, 1.21, 1.23, 1.51, 1.58, 1.88, 1.26, 1.48, 1.53, 1.53, 1.44, 1.56, 1.28, 0.90, 0.96))

  correct_answers <- correct_answers %>%
    dplyr::arrange(Event, Weekday)

 #Run Admission Discharges graph
  result <- admission_discharges(start_date = "2015-01-01 00:00:00", end_date = "2016-01-01 00:00:00", data = admission_discharge, plot_chart = TRUE, hospital_name = "Chelsea & Westminster" )

  result_data <- result$data
  result_data$Weekday <- as.character(result_data$Weekday)


  result_data <- result_data %>%
    dplyr::arrange(Event, Weekday)

  #Test results are correct
  expect_equal(as.data.frame(result_data), as.data.frame(correct_answers), tolerance = 0.1)


 })

