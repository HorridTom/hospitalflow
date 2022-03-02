context("Calculating the directorate")
library(hospitalflow)


test_that("directorate is calculated correctly", {
  correct_answers <- tibble::tibble(
    spell_number = as.character(c("1", "2", "3", "4", "5", "6", "7", "8", "9")),
    directorate = as.character(c(NA, "Surgical", "Medical", NA, "Medical", "Medical", NA, "Medical", NA))
  )

  # creating the ed_data first
  pseudo_id <- as.character(c("112", "113", "114", "115"))

  episode_id <- as.integer(c(1, 2, 3, 4))

  start_datetime <- c("2019-01-02 17:00", "2019-01-01 17:00", "2019-01-04 14:00", "2019-01-04 14:00")

  end_datetime <- c("2019-01-02 19:00", "2019-01-02 00:00", "2019-01-04 21:00", "2019-01-04 17:00")

  start_datetime <- as.POSIXct(start_datetime, tz = "Europe/London")

  end_datetime <- as.POSIXct(end_datetime, tz = "Europe/London")

  gender <- c("Male", "Female", "Male", "Other")

  age_band_start <- c("1-4 yrs", "5-9", "10-14", "95+")

  attendance_disposal <- c("Discharged", "Admitted", "Transfered to other Health Care Provider", "Referred to A&E clinic")

  referral_source <- c("Emergency Services", "Self Referral", "Health Care Provider", "Emergency Services")

  hrg_code <- c("VB08Z", "VB09Z", "VB01Z", "VB03Z")

  ed_data <- tibble::tibble(
    pseudo_id, episode_id,
    start_datetime,
    end_datetime,
    gender,
    age_band_start,
    attendance_disposal,
    referral_source,
    hrg_code
  )
  # creating the inpatient data

  pseudo_id <- as.character(c("112", "112", "112", "113", "113", "114", "114"))

  episode_id <- as.integer(c("1", "2", "3", "4", "5", "6", "7"))

  start_datetime <- c("2019-01-02 19:30", "2019-01-03 08:00", "2019-01-03 19:00", "2019-01-02 08:00", "2019-01-03 08:00", "2019-01-05 01:00", "2019-01-05 06:00")

  end_datetime <- c("2019-01-02 21:30", "2019-01-03 10:00", "2019-01-03 21:00", "2019-01-02 10:00", "2019-01-03 11:00", "2019-01-05 03:00", "2019-01-05 13:00")

  start_datetime <- as.POSIXct(start_datetime, tz = "Europe/London")
  end_datetime <- as.POSIXct(end_datetime, tz = "Europe/London")

  gender <- c("Male", "Male", "Female", "Male", "Other", "Female", "Female")

  age_band_start <- c("1-4 yrs", "1-4 yrs", "5-9", "5-9", "5-9", "10-14", "10-14")

  admission_method <- c("Accident and emergency", "Accident and emergency", "Booked", "Booked", "Booked", "Birth-this provider", "Birth-this provider")

  main_specialty <- c("OBSTRETICS", "GENERAL SURGERY", "CARDIOLOGY", "GENERAL MEDICINE", "GENERAL MEDICINE", "Accident and Emergency", "GENERAL MEDICINE")

  discharge_method <- c(1, 1, 2, 3, 4, 5, 7)

  inpatient_data <- tibble::tibble(
    pseudo_id, episode_id,
    start_datetime,
    end_datetime,
    gender,
    age_band_start, admission_method,
    main_specialty, discharge_method
  )

  # get the spell table for this data
  result <- make_spell_table(ed_data,
    inpatient_data,
    same_type_episode_lag = 1,
    different_type_episode_lag = 6
  )

  result <- result$spell_table

  # in order to be able to add directorate, first we need to add spell variables
  # which will output an object with main specialty start, prev_disch, days_since_prev_discharges
  # However, this won't be tested but will serve for testing the add_directorate_variable function
  dt <- add_spell_variables(ed_data, inpatient_data, result)

  # Extract only the spell number and directorate
  result <- add_directorate_variable(dt) %>%
    dplyr::select(spell_number, directorate) %>%
    dplyr::arrange(spell_number)

  result$spell_number <- as.character(result$spell_number)
  result$directorate <- as.character(result$directorate)

  # Test results are correct
  expect_equal(result, correct_answers)
})
