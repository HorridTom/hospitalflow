context("Make a spell table function")
library(tidyverse)


test_that("spell_number is correctly generated",{

  correct_answers <- readRDS(file = "testdata/test_make_spell_number.rds") %>%
    dplyr::select(-typ)


  pseudo_id <- as.character(c("112", "113", "114", "115"))

  adm <- c("2019-01-02 17:00", "2019-01-01 17:00", "2019-01-04 14:00", "2019-01-04 14:00")

  disch <- c("2019-01-02 19:00", "2019-01-02 00:00", "2019-01-04 21:00", "2019-01-04 17:00")

  start_datetime <- as.POSIXct(adm, tz = "Europe/London")
  end_datetime <- as.POSIXct(disch,tz = "Europe/London")

  ed_data <- tibble::tibble(pseudo_id,
                            start_datetime,
                            end_datetime)


  pseudo_id <- as.character(c("112", "112", "112", "113", "113", "114", "114"))

  adm <- c( "2019-01-02 19:30", "2019-01-03 08:00" , "2019-01-03 19:00","2019-01-02 08:00","2019-01-03 08:00", "2019-01-05 01:00", "2019-01-05 06:00")

  disch <- c( "2019-01-02 21:30", "2019-01-03 10:00", "2019-01-03 21:00","2019-01-02 10:00", "2019-01-03 11:00", "2019-01-05 03:00", "2019-01-05 13:00")

  start_datetime <- as.POSIXct(adm, tz = "Europe/London")
  end_datetime <- as.POSIXct(disch,tz = "Europe/London")

  inpatient_data <- tibble::tibble(pseudo_id,
                                   start_datetime,
                                   end_datetime)

  result <- make_spell_number(ed_data, inpatient_data, same_type_episode_lag = 1, different_type_episode_lag = 6) %>%
    dplyr::select(pseudo_id, start_datetime, end_datetime, spell_number)

  #Test results are correct
  expect_equal(result, correct_answers)


})


test_that("spell table is created correctly",{

  correct_answers <- tibble::tibble(

    spell_number = as.character(c("1", "2", "3", "4", "5", "6", "7", "8", "9")),
    spell_start = as.POSIXct(c("2019-01-02 17:00", "2019-01-03 08:00", "2019-01-03 19:00", "2019-01-01 17:00", "2019-01-02 08:00", "2019-01-03 08:00", "2019-01-04 14:00", "2019-01-05 06:00", "2019-01-04 14:00")),
    spell_end = as.POSIXct(c("2019-01-02 21:30", "2019-01-03 10:00", "2019-01-03 21:00", "2019-01-02 00:00", "2019-01-02 10:00", "2019-01-03 11:00", "2019-01-05 03:00", "2019-01-05 13:00", "2019-01-04 17:00")),
    number_of_episodes = as.character(c("2", "1", "1", "1", "1", "1", "2", "1", "1"))

  )


  pseudo_id <- as.character(c("112", "113", "114", "115"))

  adm <- c("2019-01-02 17:00", "2019-01-01 17:00", "2019-01-04 14:00", "2019-01-04 14:00")

  disch <- c("2019-01-02 19:00", "2019-01-02 00:00", "2019-01-04 21:00", "2019-01-04 17:00")

  start_datetime <- as.POSIXct(adm, tz = "Europe/London")
  end_datetime <- as.POSIXct(disch,tz = "Europe/London")

  ed_data <- tibble::tibble(pseudo_id,
                            start_datetime,
                            end_datetime) %>%
    dplyr::mutate(episode_id = dplyr::row_number(),
                  gender = "F")



  pseudo_id <- as.character(c("112", "112", "112", "113", "113", "114", "114"))

  adm <- c( "2019-01-02 19:30", "2019-01-03 08:00" , "2019-01-03 19:00","2019-01-02 08:00","2019-01-03 08:00", "2019-01-05 01:00", "2019-01-05 06:00")

  disch <- c( "2019-01-02 21:30", "2019-01-03 10:00", "2019-01-03 21:00","2019-01-02 10:00", "2019-01-03 11:00", "2019-01-05 03:00", "2019-01-05 13:00")

  start_datetime <- as.POSIXct(adm, tz = "Europe/London")
  end_datetime <- as.POSIXct(disch,tz = "Europe/London")

  inpatient_data <- tibble::tibble(pseudo_id,
                                   start_datetime,
                                   end_datetime) %>%
    dplyr::mutate(episode_id = dplyr::row_number(),
                  gender = "F")

  result <- make_spell_table(ed_data, inpatient_data, same_type_episode_lag = 1, different_type_episode_lag = 6) %>%
    dplyr::select(spell_number, spell_start, spell_end, number_of_episodes)

  result$spell_number <- as.character(result$spell_number)
  result$number_of_episodes <- as.character(result$number_of_episodes)

  #Test results are correct
  expect_equal(result, correct_answers)


})
