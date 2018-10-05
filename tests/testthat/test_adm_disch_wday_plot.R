context("Admissions and Discharges by Weekday plot")
library(hospitalflow)

test_that("Admission numbers by day of the Week",{


  load("testdata/test_data.rda")

  #Specify correct results
  correct_answers <- tibble::tibble(Weekday = 1:7, Av_adm = c(4.5, 4.75, 3.5, 4, 5,2, 0.75))

  #Run Admission Discharges graph

  result <- admission_discharges(test_data, plot_chart = TRUE)

  result_data <- result$data

  Monday_adm <- result_data %>% filter(Week_Day =="Mon", variable == "Total Admissions") %>% pull(value)

  #Test results are correct
  expect_equal(Monday_adm, correct_answers %>% filter(Weekday == 1) %>% pull(Av_adm))

 })
