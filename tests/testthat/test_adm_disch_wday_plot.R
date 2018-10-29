context("Admissions and Discharges by Weekday plot")
library(hospitalflow)


test_that("Admission numbers by day of the Week",{


  load("testdata/test_sample_data.rda")

  #Specify correct results
  #correct_answers_1 <- tibble::tibble(Weekday = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), Av_adm = c(1.50, 3.75, 4.00, 2.00, 2.00, 2.00,2.00),
                                      #Av_Disch = c(1.00, 1.00, 2.25, 3.25, 3.20, 3.80, 1.80), Non_Emerg_Adm = c(0.00, 0.00, 0.75, 0.00, 0.00, 0.00, 0.20))

  correct_answers_2 <- tibble::tibble(Weekday =  "Sun", Av_disch = c(1))
  #correct_answers_3 <- tibble:tibble(Weekday = 1:7, Non_emerg_adm = c(0.00, 0.00, 0.75, 0.00, 0.00, 0.00, 0.20))


  #Run Admission Discharges graph

  result <- admission_discharges(start_date = "2015-01-01 00:00:00", end_date = "2015-01-31 00:00:00", data = adm_disch, plot_chart = TRUE)

  result_data <- result$data

 # Results <- result_data %>% dplyr::filter(Weekday == c("Sun", "Wed"), variable == c("Total Admissions", "Total Discharges", "Non Emergency Admissions")) %>% pull(value)
  Av_disch <- result_data %>% dplyr::filter(Weekday == "Sun", variable == "Total Discharges") %>% dplyr::pull(value)

 # Mon_non_emerg_adm <- result_data %>% filter(Weekday == 1, variable == "Non Emergency Admissions") %>% pull(value)
 # Wed_non_emerg_adm <- result_data %>% filter(Weekday == 3, variable == "Non Emergency Admissions") %>% pull(value)

  #Test results are correct
  #expect_equal(Results, correct_answers_1 %>% dplyr::filter(Weekday == c("Sun", "Wed"))  %>% pull("Total Admissions"))
  expect_equal(Av_disch, correct_answers_2 %>%  dplyr::filter(Weekday == "Sun") %>% dplyr::pull(Av_disch))

 # expect_equal(Mon_non_emerg_adm, correct_answers_3) %>% filter(Weekday == 1) %>% pull("Non Emergency Admissions")
 # expect_equal(Wed_non_emerg_adm, correct_answers_3) %>% filter(Weekday == 3) %>% pull("Non Emergency Admissions")

 })

