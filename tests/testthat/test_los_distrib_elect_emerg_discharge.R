context("Los distribution Elective and Emergency plot")
library(hospitalflow)


test_that("Los distribution Elective and Emergency plot for January-March 2015",{

  load("testdata/test_data_los_bin_emerg_elect.rda")

  #Specify correct results
  correct_answers <- tibble::tibble(

    Spell_type = as.character(c("Elective", "Elective", "Elective", "Elective", "Elective", "Elective", "Elective", "Elective", "Elective", "Elective", "Elective",
                             "Elective", "Elective", "Elective", "Elective",  "Elective", "Elective", "Elective", "Elective", "Elective", "Elective","Elective", "Elective",
                             "Emergency", "Emergency", "Emergency", "Emergency", "Emergency", "Emergency", "Emergency", "Emergency",
                             "Emergency", "Emergency", "Emergency", "Emergency", "Emergency", "Emergency", "Emergency", "Emergency",
                             "Emergency", "Emergency", "Emergency", "Emergency", "Emergency", "Emergency", "Emergency")),

    losbinned =  forcats::fct_relevel(c( "8hrs", "16hrs", "1 d",  "1 d 8hrs", "1 d 16hrs",
                                               "2 d",  "2 d 8hrs", "2 d 16hrs",
                                               "3 d",  "3 d 8hrs", "3 d 16hrs",
                                               "4 d",  "4 d 8hrs", "4 d 16hrs",
                                               "5 d",  "5 d 8hrs", "5 d 16hrs",
                                                       "6 d 8hrs", "6 d 16hrs",
                              "7 - 14 d", "14 - 21 d", "21 - 28 d", "> 28 d",
                          "0hrs", "8hrs", "16hrs", "1 d", "1 d 8hrs", "1 d 16hrs",
                                                   "2 d", "2 d 8hrs", "2 d 16hrs",
                                                   "3 d",              "3 d 16hrs",
                                                   "4 d", "4 d 8hrs",  "4 d 16hrs",
                                                   "5 d", "5 d 8hrs",  "5 d 16hrs",
                                                   "6 d", "6 d 8hrs",
                             "7 - 14 d", "14 - 21 d", "21 - 28 d", "> 28 d")),


    Count = as.numeric(c(13, 36, 64, 104, 13, 18, 38, 5, 12, 28, 3, 6, 11, 4, 2, 4, 1, 14, 3, 40, 11, 3, 3, 1, 8, 22, 17, 7, 9, 5, 4, 4, 4, 9, 6, 3, 5, 2, 1, 2, 1, 2, 12, 2, 4, 4))
    )

  str(correct_answers)




  #Run Admission Discharges graph
  result <- los_distrib_elect_emerg_discharge(start_date = "2015-01-01 00:00:00", end_date = "2015-04-01 00:00:00", data = test_data_los_bin_emerg_elect, plot_chart = TRUE)

  result_data <- result$data
  result_data$Count <- as.numeric(result_data$Count)
  result_data$Spell_type <- as.character(result_data$Spell_type)

  str(result_data)

  #Test results are correct
  expect_equal(as.data.frame(result_data), as.data.frame(correct_answers))


})
