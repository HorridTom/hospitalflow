context("Admissions and Discharges by Weekday plot")
library(hospitalflow)


test_that("Admission numbers by day of the Week for January 2015",{

  load("testdata/test_sample_data.rda")

  #Specify correct results
  correct_answers <- tibble::tibble(
    Weekday = as.character(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")),

    Event =  c("Avg_admissions", "Avg_admissions","Avg_admissions", "Avg_admissions","Avg_admissions","Avg_admissions","Avg_admissions",
              "Avg_discharges","Avg_discharges","Avg_discharges","Avg_discharges","Avg_discharges","Avg_discharges","Avg_discharges",
              "Non_emergency_admissions","Non_emergency_admissions","Non_emergency_admissions","Non_emergency_admissions","Non_emergency_admissions",
              "Non_emergency_admissions","Non_emergency_admissions"),

    Value = c(1.50, 3.75, 4.00, 2.00, 2.00, 2.00, 2.00, 1.00, 1.00, 2.25, 3.25, 3.20, 3.80, 2.40, 0.00, 0.00, 0.75, 0.00, 0.00, 0.00, 0.20))

  correct_answers <- correct_answers %>%
    dplyr::arrange(Event, Weekday)

 #Run Admission Discharges graph

  result <- admission_discharges(start_date = "2015-01-01 00:00:00", end_date = "2015-02-01 00:00:00", data = adm_disch, plot_chart = TRUE)

  result_data <- result$data
  result_data$Weekday <- as.character(result_data$Weekday)


  result_data <- result_data %>%
    dplyr::arrange(Event, Weekday)

  #Test results are correct
  expect_equal(result_data, correct_answers)


 })


test_that("Admission numbers by day of the Week for January-Jun 2015",{

  load("testdata/test_jan_jun_data.rda")

  #Specify correct results
  correct_answers_jan_jun <- tibble::tibble(
    Weekday = as.character(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")),

    Event =  c("Avg_admissions", "Avg_admissions","Avg_admissions", "Avg_admissions","Avg_admissions","Avg_admissions","Avg_admissions",
               "Avg_discharges","Avg_discharges","Avg_discharges","Avg_discharges","Avg_discharges","Avg_discharges","Avg_discharges",
               "Non_emergency_admissions","Non_emergency_admissions","Non_emergency_admissions","Non_emergency_admissions","Non_emergency_admissions",
               "Non_emergency_admissions","Non_emergency_admissions"),

    Value = as.numeric(c(1.92, 3.23, 3.38, 3.48, 3.46, 2.88, 1.96, 2.96, 2.42, 2.46, 3.16, 3.38, 3.85, 2.54, 0.04, 0.08, 0.19, 0.16, 0.15, 0.08, 0.12)))

  correct_answers_jan_jun <- correct_answers_jan_jun %>%
    dplyr::arrange(Event, Weekday)

  #Run Admission Discharges graph

  result_jan_jun <- admission_discharges(start_date = "2015-01-01 00:00:00", end_date = "2015-07-01 00:00:00", data = adm_disch_jan_jun, plot_chart = TRUE)

  result_data_jj <- result_jan_jun$data
  #result_data_jj$Weekday <- as.character(result_data_jj$Weekday)


  result_jan_jun <- result_data_jj %>%
    dplyr::mutate(Weekday = as.character(Weekday)) %>%
    dplyr::arrange(Event, Weekday)

  #Test results are correct
  expect_equal(as.data.frame(result_jan_jun), as.data.frame(correct_answers_jan_jun), tolerance = 0.01)


})


