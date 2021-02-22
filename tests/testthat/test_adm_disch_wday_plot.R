context("Admissions and Discharges by Weekday plot")
library(hospitalflow)

test_that("Admission numbers by day of the Week for improvised data",{


  #Specify correct results
  correct_answers <- tibble::tibble(
    Weekday = as.character(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun",  "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),

    Event =  c("Avg_admissions", "Avg_admissions","Avg_admissions", "Avg_admissions","Avg_admissions","Avg_admissions","Avg_admissions",
               "Avg_discharges","Avg_discharges","Avg_discharges","Avg_discharges","Avg_discharges","Avg_discharges","Avg_discharges",
               "Non_emergency_admissions","Non_emergency_admissions","Non_emergency_admissions","Non_emergency_admissions","Non_emergency_admissions",
               "Non_emergency_admissions","Non_emergency_admissions"),

     Value = c(0.0, 0.5, 0.5, 0.5, 0.5, 0.5, 1.0, 2.0, 0.0, 1.5, 1.0, 1.0, 1.0, 1.0, 0.0, 0.5, 0.5, 0.5, 0.5, 0.0, 0.5))

  correct_answers <- correct_answers %>%
    dplyr::arrange(Weekday, Event)

  adm <- c("2018-12-09 09:30:00", "2018-12-09 09:30:00", "2018-12-09 09:30:00", "2018-12-11 09:30:00", "2018-12-09 09:30:00","2018-12-12 09:30:00","2018-12-09 09:30:00", "2018-12-13 08:00:00", "2018-12-09 09:30:00","2018-12-14 08:00:00", "2018-12-09 09:30:00", "2018-12-09 09:30:00", "2018-12-15 08:00:00", "2018-12-16 08:00:00", "2018-12-16 08:00:00")

  disch <- c("2018-12-12 09:30:00", "2018-12-10 09:30:00", "2018-12-12 09:30:00", "2018-12-12 09:30:00", "2018-12-13 09:30:00", "2018-12-13 09:30:00","2018-12-14 08:00:00", "2018-12-14 08:00:00", "2018-12-15 08:00:00","2018-12-15 08:00:00", "2018-12-16 09:30:00", "2018-12-16 09:30:00", "2018-12-17 08:00:00", "2018-12-17 08:00:00", "2018-12-17 09:00:00")


  spell_class_col <- c("ed_admission", "direct_admission","ed_admission", "direct_admission", "ed_admission",
                    "direct_comp_admission", "ed_admission", "direct_admission","ed_admission", "direct_admission",
                    "ed_admission", "direct_comp_admission", "ed_admission", "ed_admission", "direct_admission")


  spell_start <- as.POSIXct(adm, tz = "Europe/London")
  spell_end <- as.POSIXct(disch,tz = "Europe/London")

  test_avg_adm_disch <- tibble::tibble(spell_number = 101:115,
                                       spell_start,
                                       spell_end,  spell_class_col)

  #Run Admission Discharges graph
  result <- admissions_discharges(start_date = as.Date("2018-12-10", tz = "Europe/London"),
                                  end_date = as.Date("2018-12-23",tz = "Europe/London"),
                                  data = test_avg_adm_disch, plot_chart = FALSE,
                                  hospital_name = "Hospital Two")


  result$Weekday <- as.character(result$Weekday)


  result <- result %>%
    dplyr::arrange(Weekday, Event)

  #Test results are correct
  expect_equal(tibble::as_tibble(result), tibble::as_tibble(correct_answers), tolerance = 0.01)


})


