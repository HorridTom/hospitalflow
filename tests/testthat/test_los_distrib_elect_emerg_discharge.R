context("Los distribution Elective and Emergency plot")
library(hospitalflow)



test_that("Los distribution Elective and Emergency for improvised data",{

  #load("testdata/test_data_los_bin_emerg_elect.rda")

  #Specify correct results
  correct_answers <- tibble::tibble(

    Spell_type = as.character(c("Elective", "Elective", "Elective", "Emergency", "Emergency", "Emergency")),

    losbinned =  as.factor(c("2 d", "3 d", "> 28 d", "2 d", "3 d", "> 28 d")),

    Count = as.numeric(c(1,  1,  1, 1, 1, 2))
  )

  correct_answers <- correct_answers %>%
    dplyr::mutate(losbinned = factor(losbinned, levels = c("2 d", "3 d", "> 28 d")))


  correct_answers$losbinned <- forcats::fct_relevel(correct_answers$losbinned)

  # improvise a a fake data

  los1 <- c("2018-10-10 09:30:00", "2018-10-10 09:30:00", "2018-10-10 09:30:00", "2018-10-10 08:00:00", "2018-10-10 08:00:00", "2018-10-10 08:00:00", "2018-10-10 08:00:00", "2018-10-10 08:00:00")

  los2 <- c("2018-12-10 09:30:00", "2018-12-10 09:30:00","2018-12-10 09:30:00" , "2018-10-12 08:00:00", "2018-10-12 08:00:00", "2018-10-13 08:00:00", "2018-10-13 08:00:00", "2018-10-10 09:00:00")


  Admission_type <- c("Elective", "Emergency", "Emergency", "Elective", "Emergency", "Elective", "Emergency", "Emergency")

  Spell_type <- c("Elective", "Emergency", "Emergency", "Elective", "Emergency", "Elective", "Emergency", "Emergency")

  Ward_code <- c("DSN", "DSN", "DNA", "NAS", "BAS", "BAS","BAS","BAS")


  Episode_number <- c(1, 1, 1, 1, 1, 1, 1, 2)

  Admissions <- as.POSIXct(los1, tz = "Europe/London")
  Discharges <- as.POSIXct(los2,tz = "Europe/London")

  test_los <- tibble::tibble(IDcol = 101:108,
                             Admissions,
                             Discharges,
                             Admission_type,
                             Episode_number, Spell_type, Ward_code)




  #Run Admission Discharges graph
  result <- los_distrib_elect_emerg_discharge(start_date = as.Date("2018-10-10",tz = "Europe/London"), end_date = as.Date("2018-12-14", tz = "Europe/London"), data = test_los,
                                              plot_chart = TRUE, hospital_name = "Chelsea & Westminster")

  result_data <- result$data
  result_data$Count <- as.numeric(result_data$Count)
  result_data$Spell_type <- as.character(result_data$Spell_type)

  result_data <- result_data %>%
    dplyr::mutate(losbinned = factor(losbinned, levels = c("2 d", "3 d", "> 28 d")))


  result_data$losbinned <- forcats::fct_relevel(result_data$losbinned)


  str(result_data)

  #Test results are correct
  expect_equal(as.data.frame(correct_answers), as.data.frame(result_data))


})



