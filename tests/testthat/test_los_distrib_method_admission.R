context("Los distribution method for admitted patients, for improvised data")
library(hospitalflow)

test_that("Length of stay for admitted patients, for improvised data",{

  #Specify correct results
  correct_answers <- tibble::tibble(

    losbinned =  as.factor(c("2 d", "1 d","1 d","1 d")),

    admission_method_type = c( "Emergency Admissions", "Elective Admissions","Maternity Admissions" , "Other Admissions"),


    Count = as.numeric(c(4, 1, 1, 1))
  )


  correct_answers <- correct_answers %>% dplyr::arrange(losbinned)

  correct_answers <- correct_answers %>%
    dplyr::mutate(losbinned =  factor(losbinned, levels = c("0hrs", "8hrs", "16hrs",
                                                            "1 d", "1 d 8hrs", "1 d 16hrs",
                                                            "2 d", "2 d 8hrs", "2 d 16hrs",
                                                            "3 d", "3 d 8hrs", "3 d 16hrs",
                                                            "4 d", "4 d 8hrs", "4 d 16hrs",
                                                            "5 d", "5 d 8hrs", "5 d 16hrs",
                                                            "6 d",  "6 d 8hrs","6 d 16hrs",
                                                            "7 - 14 d", "15 - 21 d", "22 - 28 d", "> 28 d")))

  spell_start <- c("2019-01-01 09:00:00",  "2019-01-02 16:00:00",   "2019-01-03 16:00:00", "2019-01-05 17:00:00", "2019-01-02 01:00:00",  "2019-01-05 16:00:00", "2019-01-02 01:00:00", "2019-01-05 16:00:00" ,"2019-01-02 01:00:00", "2019-01-05 16:00:00")

  spell_end <- c("2019-01-03 09:00:00",  "2019-01-04 16:00:00",   "2019-01-05 16:00:00", "2019-01-07 17:00:00", "2019-01-02 09:00:00",  "2019-01-06 16:00:00",  "2019-01-02 09:00:00", "2019-01-06 16:00:00","2019-01-02 09:00:00", "2019-01-06 16:00:00")

  admission_method_type <- c( "Emergency Admissions", "Emergency Admissions","Emergency Admissions", "Emergency Admissions", "Elective Admissions", "Elective Admissions", "Maternity Admissions", "Maternity Admissions",   "Other Admissions",   "Other Admissions")


  spell_start <- as.POSIXct(spell_start, tz = "Europe/London")
  spell_end <- as.POSIXct(spell_end, tz = "Europe/London")

  test_los_admission <- tibble::tibble(spell_number = 101:110,
                                         spell_start,
                                         spell_end,
                                         admission_method_type)



  #Run Admission Discharges graph
  result <- hospitalflow::los_distrib_method_admission(start_date = "2019-01-01 00:00:00", end_date = "2019-01-08 00:00:00", data = test_los_admission, plot_chart = FALSE, hospital_name = "Queh")

  # result$admission_method_type <- as.character(result$admission_method_type)
  result$Count <- as.numeric(result$Count)

  result_data <- result %>% dplyr::select(losbinned, admission_method_type, Count)

  #Test results are correct
  expect_equal(result_data, correct_answers, tolerance = 0.1)


})


test_that("Length of stay for admitted patients, for real data",{

  #Specify correct results

  load("testdata/los_distrib_queh_sample_tests/queh_ip_sample.rda")



  queh_ip_sample$spell_number <- as.character(queh_ip_sample$spell_number)


  correct_answers <- readr::read_csv("testdata/los_distrib_queh_sample_tests/correct_answers.csv")

  correct_answers <- correct_answers %>%
    dplyr::select(losbinned, admission_method_type, Count)


  #Run Admission Discharges graph - with min and max dates chosen based on spell_Start and spell_end
  result <- hospitalflow::los_distrib_method_admission(start_date = as.Date("2015-04-01", tz = "Europe/London"), end_date = as.Date("2018-12-30", tz= "Europe/London"),
                                                       data = queh_ip_sample,
                                                       plot_chart = FALSE, hospital_name = "Queh")

  # result$admission_method_type <- as.character(result$admission_method_type)
  result$Count <- as.numeric(result$Count)
  result$losbinned <- as.character(result$losbinned)


  #Test results are correct
  expect_equal(result, correct_answers, tolerance = 0.1)


})
