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

test_that("Admission numbers by day of the Week for improvised data",{

  load("testdata/test_sample_data.rda")

  #Specify correct results
  correct_answers <- tibble::tibble(
    Weekday = as.character(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun",  "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),

    Event =  c("Avg_admissions", "Avg_admissions","Avg_admissions", "Avg_admissions","Avg_admissions","Avg_admissions","Avg_admissions",
               "Avg_discharges","Avg_discharges","Avg_discharges","Avg_discharges","Avg_discharges","Avg_discharges","Avg_discharges",
               "Non_emergency_admissions","Non_emergency_admissions","Non_emergency_admissions","Non_emergency_admissions","Non_emergency_admissions",
               "Non_emergency_admissions","Non_emergency_admissions"),

    Value = c(0.0, 0.5, 0.5, 0.5, 0.5, 0.5, 1.0,
              2.0, 0.0, 1.5, 1.0, 0.5, 1.0, 1.0,
              0.0, 0.5, 0.5, 0.5, 0.5, 0.0, 0.5))

  correct_answers <- correct_answers %>%
    dplyr::arrange(Event, Weekday)

  adm <- c("2018-12-09 09:30:00", "2018-12-09 09:30:00", "2018-12-09 09:30:00", "2018-12-11 09:30:00", "2018-12-09 09:30:00","2018-12-12 09:30:00","2018-12-09 09:30:00", "2018-12-13 08:00:00", "2018-12-09 09:30:00","2018-12-14 08:00:00", "2018-12-09 09:30:00", "2018-12-09 09:30:00", "2018-12-15 08:00:00", "2018-12-16 08:00:00", "2018-12-16 08:00:00")

  disch <- c("2018-12-12 09:30:00", "2018-12-10 09:30:00", "2018-12-12 09:30:00", "2018-12-12 09:30:00", "2018-12-13 09:30:00", "2018-12-13 09:30:00","2018-12-14 08:00:00", "2018-12-14 08:00:00", "2018-12-15 08:00:00","2018-12-15 08:00:00", "2018-12-16 09:30:00", "2018-12-16 09:30:00", "2018-12-17 08:00:00", "2018-12-17 08:00:00", "2018-12-17 09:00:00")


  PatientType <- c("Emergency", "Elective","Emergency", "Elective", "Emergency",
                   "Elective", "Emergency", "Maternity","Emergency", "Elective",
                   "Emergency", "Maternity", "Emergency", "Emergency", "Other Babies")


  EpisodeNumber <- c(1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1)

  Admissions <- as.POSIXct(adm, tz = "Europe/London")
  Discharges <- as.POSIXct(disch,tz = "Europe/London")

  test_avg_adm_disch <- tibble::tibble(IDcol = 101:115,
                                       Admissions,
                                       Discharges, EpisodeNumber, PatientType)

  #Run Admission Discharges graph
  result <- admissions_discharges(start_date = as.Date("2018-12-10", tz = "Europe/London"),
                                  end_date = as.Date("2018-12-23",tz = "Europe/London"),
                                  data = test_avg_adm_disch, plot_chart = TRUE,
                                  hospital_name = "Chelsea & Westminster")

  result_data <- result$data
  result_data$Weekday <- as.character(result_data$Weekday)


  result_data <- result_data %>%
    dplyr::arrange(Event, Weekday)

  #Test results are correct
  expect_equal(as.data.frame(result_data), as.data.frame(correct_answers), tolerance = 0.01)


})



