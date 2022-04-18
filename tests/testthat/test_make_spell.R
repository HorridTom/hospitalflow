
test_that("spell table is created correctly", {

  # in the following tests in this script this dataframe will be loaded from testthat/testdata/spell_linkage
  correct_answers <- tibble::tibble(
    spell_number = as.character(c("1", "2", "3", "4", "5", "6", "7", "8", "9")),
    spell_start = as.POSIXct(c("2019-01-02 17:00", "2019-01-03 08:00", "2019-01-03 19:00", "2019-01-01 17:00", "2019-01-02 08:00", "2019-01-03 08:00", "2019-01-04 14:00", "2019-01-05 06:00", "2019-01-04 14:00"), tz = "Europe/London"),
    spell_end = as.POSIXct(c("2019-01-02 21:30", "2019-01-03 10:00", "2019-01-03 21:00", "2019-01-02 00:00", "2019-01-02 10:00", "2019-01-03 11:00", "2019-01-05 03:00", "2019-01-05 13:00", "2019-01-04 17:00"), tz = "Europe/London"),
    number_of_episodes = as.character(c("2", "1", "1", "1", "1", "1", "2", "1", "1")),
    admission_method_type = as.character(c("Emergency Admissions", "Emergency Admissions", "Elective Admissions", NA, "Elective Admissions", "Elective Admissions", "Other Admissions", "Other Admissions", NA))
  )

  pseudo_id <- as.character(c("112", "113", "114", "115"))
  episode_id <- as.integer(c(1, 2, 3, 4))
  start_datetime <- c("2019-01-02 17:00", "2019-01-01 17:00", "2019-01-04 14:00", "2019-01-04 14:00")
  start_datetime <- as.POSIXct(start_datetime, tz = "Europe/London")
  end_datetime <- c("2019-01-02 19:00", "2019-01-02 00:00", "2019-01-04 21:00", "2019-01-04 17:00")
  end_datetime <- as.POSIXct(end_datetime, tz = "Europe/London")
  gender <- c("Male", "Female", "Male", "Other")
  age_band_start <- c("1-4 yrs", "5-9", "10-14", "95+")
  attendance_disposal <- c("Discharged", "Admitted", "Transfered to other Health Care Provider", "Referred to A&E clinic")
  referral_source <- c("Emergency Services", "Self Referral", "Health Care Provider", "Emergency Services")
  hrg_code <- c("VB082", "VB083", "VB084", "VB085")

  # in the following tests in this script this dataframe will be loaded from testthat/testdata/spell_linkage
  ed_data_age_sex <- tibble::tibble(
    pseudo_id,
    episode_id,
    start_datetime,
    end_datetime,
    gender,
    age_band_start,
    attendance_disposal,
    referral_source,
    hrg_code
  )

  pseudo_id <- as.character(c("112", "112", "112", "113", "113", "114", "114"))
  episode_id <- as.integer(c("1", "2", "3", "4", "5", "6", "7"))
  start_datetime <- c("2019-01-02 19:30", "2019-01-03 08:00", "2019-01-03 19:00", "2019-01-02 08:00", "2019-01-03 08:00", "2019-01-05 01:00", "2019-01-05 06:00")
  start_datetime <- as.POSIXct(start_datetime, tz = "Europe/London")
  end_datetime <- c("2019-01-02 21:30", "2019-01-03 10:00", "2019-01-03 21:00", "2019-01-02 10:00", "2019-01-03 11:00", "2019-01-05 03:00", "2019-01-05 13:00")
  end_datetime <- as.POSIXct(end_datetime, tz = "Europe/London")
  gender <- c("Male", "Male", "Female", "Male", "Other", "Female", "Female")
  age_band_start <- c("1-4 yrs", "1-4 yrs", "5-9", "5-9", "5-9", "10-14", "10-14")
  admission_method <- c("Accident and emergency", "Accident and emergency", "Booked", "Booked", "Booked", "Birth-this provider", "Birth-this provider")
  discharge_method <- c(1, 2, 3, 3, 4, 5, 7)

  # in the following tests in this script this dataframe will be loaded from testthat/testdata/spell_linkage
  inpatient_data_age_sex <- tibble::tibble(
    pseudo_id,
    episode_id,
    start_datetime,
    end_datetime,
    gender,
    age_band_start,
    admission_method,
    discharge_method
  )

  result <- make_spell_table(
    ed_data_age_sex,
    inpatient_data_age_sex,
    same_type_episode_lag = 1,
    different_type_episode_lag = 6
  )

  result <- result$spell_table

  result <- result %>%
    dplyr::select(
      spell_number, spell_start, spell_end, number_of_episodes, admission_method_type
    )

  result$spell_number <- as.character(result$spell_number)
  result$number_of_episodes <- as.character(result$number_of_episodes)

  # Test results are correct
  expect_equal(result, correct_answers)
})

## Below was used when optimizing make_spell_table function. Now

# test_that("make_spell_number_dtbl returns identical results to make_spell_number", {
#   correct_answers <- readRDS("testdata/spell_linkage/correct_answers.rds")
#   ed_data_age_sex <- readRDS("testdata/spell_linkage/ed_data_age_sex.rds")
#   inpatient_data_age_sex <- readRDS("testdata/spell_linkage/inpatient_data_age_sex.rds")
#
#   make_spell_number_result <- make_spell_number(
#     ed_data_age_sex,
#     inpatient_data_age_sex,
#     1,
#     6
#   )
#
#   make_spell_number_dtbl_result <- make_spell_number_dtbl(
#     ed_data_age_sex,
#     inpatient_data_age_sex,
#     1,
#     6
#   )
#
#   expect_identical(make_spell_number_result, make_spell_number_dtbl_result)
# })
#
#
# test_that("optimized version of spell_variables returns identical constituent ED and IP episodes", {
#   correct_answers <- readRDS("testdata/spell_linkage/correct_answers.rds")
#   ed_data_age_sex <- readRDS("testdata/spell_linkage/ed_data_age_sex.rds")
#   inpatient_data_age_sex <- readRDS("testdata/spell_linkage/inpatient_data_age_sex.rds")
#
#   all_episodes <- make_spell_number(
#     ed_data_age_sex,
#     inpatient_data_age_sex,
#     1,
#     6
#   )
#
#   # copy-pasted from spell_variables function
#   from_spell_variables <- all_episodes %>%
#     dplyr::group_by(spell_number) %>%
#     tidyr::nest() %>%
#     dplyr::mutate(
#       constituent_ed_episodes = purrr::map(
#         data,
#         get_episode_id_list,
#         episode_type_to_list = "ED"
#       ),
#       constituent_ip_episodes = purrr::map(
#         data,
#         get_episode_id_list,
#         episode_type_to_list = "IP")) %>%
#     dplyr::select(-data) %>%
#     dplyr::ungroup()
#
#   from_spell_variables_new <- spell_variables_new(all_episodes)$constituent_ed_episodes
#
#   expect_identical(from_spell_variables, from_spell_variables_new)
# })
#
#
# test_that("optimized version of spell_variables returns identical gender", {
#   correct_answers <- readRDS("testdata/spell_linkage/correct_answers.rds")
#   ed_data_age_sex <- readRDS("testdata/spell_linkage/ed_data_age_sex.rds")
#   inpatient_data_age_sex <- readRDS("testdata/spell_linkage/inpatient_data_age_sex.rds")
#
#   all_episodes <- make_spell_number(
#     ed_data_age_sex,
#     inpatient_data_age_sex,
#     1,
#     6
#   )
#
#   # copy-pasted from spell_variables function
#   from_spell_variables <- all_episodes %>%
#     dplyr::group_by(spell_number) %>%
#     tidyr::nest() %>%
#     dplyr::mutate(gender = purrr::map(data, get_latest_gender)) %>%
#     dplyr::select(-data) %>%
#     dplyr::ungroup()
#
#   from_spell_variables_new <- spell_variables_new(all_episodes)$gender_df
#
#   expect_identical(from_spell_variables, from_spell_variables_new)
# })
#
#
# test_that("optimized version of spell_variables returns identical age bands", {
#   correct_answers <- readRDS("testdata/spell_linkage/correct_answers.rds")
#   ed_data_age_sex <- readRDS("testdata/spell_linkage/ed_data_age_sex.rds")
#   inpatient_data_age_sex <- readRDS("testdata/spell_linkage/inpatient_data_age_sex.rds")
#
#   all_episodes <- make_spell_number(
#     ed_data_age_sex,
#     inpatient_data_age_sex,
#     1,
#     6
#   )
#
#   # copy-pasted from spell_variables function
#   from_spell_variables <- all_episodes %>%
#     dplyr::group_by(spell_number) %>%
#     tidyr::nest() %>%
#     dplyr::mutate(age_band_start = purrr::map(data, get_age_band_start)) %>%
#     dplyr::select(-data) %>%
#     dplyr::ungroup()
#
#   from_spell_variables_new <- spell_variables_new(all_episodes)$age_band_df
#
#   expect_identical(from_spell_variables, from_spell_variables_new)
# })
#
#
# test_that("optimized version of spell_variables returns identical episode class sequence", {
#   correct_answers <- readRDS("testdata/spell_linkage/correct_answers.rds")
#   ed_data_age_sex <- readRDS("testdata/spell_linkage/ed_data_age_sex.rds")
#   inpatient_data_age_sex <- readRDS("testdata/spell_linkage/inpatient_data_age_sex.rds")
#
#   all_episodes <- make_spell_number_dtbl(
#     ed_data_age_sex,
#     inpatient_data_age_sex,
#     1,
#     6
#   )
#
#   # copy-pasted from spell_variables function
#   from_spell_variables <- all_episodes %>%
#     dplyr::group_by(spell_number) %>%
#     tidyr::nest() %>%
#     dplyr::mutate(episode_class_sequence = purrr::map(data, get_episode_class_sequence)) %>%
#     dplyr::select(-data) %>%
#     dplyr::ungroup()
#
#   from_spell_variables_new <- spell_variables_new(all_episodes)$ep_class_seq_df
#
#   expect_identical(from_spell_variables, from_spell_variables_new)
# })
#
#
# test_that("optimized version of spell_variables returns identical admission methods", {
#   correct_answers <- readRDS("testdata/spell_linkage/correct_answers.rds")
#   ed_data_age_sex <- readRDS("testdata/spell_linkage/ed_data_age_sex.rds")
#   inpatient_data_age_sex <- readRDS("testdata/spell_linkage/inpatient_data_age_sex.rds")
#
#   all_episodes <- make_spell_number(
#     ed_data_age_sex,
#     inpatient_data_age_sex,
#     1,
#     6
#   )
#
#   # copy-pasted from spell_variables function
#   from_spell_variables <- all_episodes %>%
#     dplyr::group_by(spell_number) %>%
#     tidyr::nest() %>%
#     dplyr::mutate(admission_method_type = purrr::map(data, admission_method_class)) %>%
#     dplyr::select(-data) %>%
#     dplyr::ungroup()
#
#   from_spell_variables_new <- spell_variables_new(all_episodes)$admission_type_df
#
#   expect_identical(from_spell_variables, from_spell_variables_new)
# })
#
#
# test_that("optimized version of spell_variables returns identical initial ed episode end datetime", {
#   correct_answers <- readRDS("testdata/spell_linkage/correct_answers.rds")
#   ed_data_age_sex <- readRDS("testdata/spell_linkage/ed_data_age_sex.rds")
#   inpatient_data_age_sex <- readRDS("testdata/spell_linkage/inpatient_data_age_sex.rds")
#
#   all_episodes <- make_spell_number(
#     ed_data_age_sex,
#     inpatient_data_age_sex,
#     1,
#     6
#   )
#
#   # copy-pasted from spell_variables function
#   from_spell_variables <- all_episodes %>%
#     dplyr::group_by(spell_number) %>%
#     tidyr::nest() %>%
#     dplyr::mutate(initial_ed_end_datetime = purrr::map(data, get_initial_ed_episode_end_datetime)) %>%
#     dplyr::select(-data) %>%
#     dplyr::ungroup()
#
#   from_spell_variables_new <- spell_variables_new(all_episodes)$datetime_df
#
#   expect_identical(from_spell_variables, from_spell_variables_new)
# })
#
#
# test_that("optimized version of spell_variables returns identical disposal code", {
#   correct_answers <- readRDS("testdata/spell_linkage/correct_answers.rds")
#   ed_data_age_sex <- readRDS("testdata/spell_linkage/ed_data_age_sex.rds")
#   inpatient_data_age_sex <- readRDS("testdata/spell_linkage/inpatient_data_age_sex.rds")
#
#   all_episodes <- make_spell_number(
#     ed_data_age_sex,
#     inpatient_data_age_sex,
#     1,
#     6
#   )
#
#   # copy-pasted from spell_variables function
#   from_spell_variables <- all_episodes %>%
#     dplyr::group_by(spell_number) %>%
#     tidyr::nest() %>%
#     dplyr::mutate(disposal_code = purrr::map(data, get_disposal_code)) %>%
#     dplyr::select(-data) %>%
#     dplyr::ungroup()
#
#   from_spell_variables_new <- spell_variables_new(all_episodes)$disposal_df
#
#   expect_identical(from_spell_variables, from_spell_variables_new)
# })
#
#
# test_that("optimized version of spell_variables returns identical hrg ae code", {
#   correct_answers <- readRDS("testdata/spell_linkage/correct_answers.rds")
#   ed_data_age_sex <- readRDS("testdata/spell_linkage/ed_data_age_sex.rds")
#   inpatient_data_age_sex <- readRDS("testdata/spell_linkage/inpatient_data_age_sex.rds")
#
#   all_episodes <- make_spell_number(
#     ed_data_age_sex,
#     inpatient_data_age_sex,
#     1,
#     6
#   )
#
#   # copy-pasted from spell_variables function
#   from_spell_variables <- all_episodes %>%
#     dplyr::group_by(spell_number) %>%
#     tidyr::nest() %>%
#     dplyr::mutate(hrg_ae_code = purrr::map(data, get_hrg)) %>%
#     dplyr::select(-data) %>%
#     dplyr::ungroup()
#
#   from_spell_variables_new <- spell_variables_new(all_episodes)$hrg_df
#
#   expect_identical(from_spell_variables, from_spell_variables_new)
# })
#
#
# test_that("optimized version of spell_variables returns identical source referral", {
#   correct_answers <- readRDS("testdata/spell_linkage/correct_answers.rds")
#   ed_data_age_sex <- readRDS("testdata/spell_linkage/ed_data_age_sex.rds")
#   inpatient_data_age_sex <- readRDS("testdata/spell_linkage/inpatient_data_age_sex.rds")
#
#   all_episodes <- make_spell_number(
#     ed_data_age_sex,
#     inpatient_data_age_sex,
#     1,
#     6
#   )
#
#   # copy-pasted from spell_variables function
#   from_spell_variables <- all_episodes %>%
#     dplyr::group_by(spell_number) %>%
#     tidyr::nest() %>%
#     dplyr::mutate(source_referral_ae = purrr::map(data, get_source_of_referral)) %>%
#     dplyr::select(-data) %>%
#     dplyr::ungroup()
#
#   from_spell_variables_new <- spell_variables_new(all_episodes)$source_referral_df
#
#   expect_identical(from_spell_variables, from_spell_variables_new)
# })
#
#
# test_that("optimized version of spell_variables returns identical mortality ip", {
#   correct_answers <- readRDS("testdata/spell_linkage/correct_answers.rds")
#   ed_data_age_sex <- readRDS("testdata/spell_linkage/ed_data_age_sex.rds")
#   inpatient_data_age_sex <- readRDS("testdata/spell_linkage/inpatient_data_age_sex.rds")
#
#   all_episodes <- make_spell_number(
#     ed_data_age_sex,
#     inpatient_data_age_sex,
#     1,
#     6
#   )
#
#   # copy-pasted from spell_variables function
#   from_spell_variables <- all_episodes %>%
#     dplyr::group_by(spell_number) %>%
#     tidyr::nest() %>%
#     dplyr::mutate(died_ip = purrr::map(data, get_mortality_ip)) %>%
#     dplyr::select(-data) %>%
#     dplyr::ungroup()
#
#   from_spell_variables_new <- spell_variables_new(all_episodes)$mortality_df
#
#   expect_identical(from_spell_variables, from_spell_variables_new)
# })

