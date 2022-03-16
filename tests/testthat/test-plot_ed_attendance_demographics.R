
test_that("Admission and Attendances by age-Sex, for improvised data", {

  # Specify correct results
  correct_answers <- tibble::tibble(
    gender = as.factor(c("Female", "Female", "Male", "Male", "Female", "Female", "Male", "Male")),
    age_band_start = as.factor(c("1-4 yrs", "95+", "1-4 yrs", "95+", "1-4 yrs", "95+", "1-4 yrs", "95+")),
    value = as.numeric(c(3, 3, 3, 3, 2, 2, 2, 2)),
    group = as.factor(c("Female attendances", "Female attendances", "Male attendances", "Male attendances", "Female admitted", "Female admitted", "Male admitted", "Male admitted"))
  )

  correct_answers_1 <- correct_answers %>%
    dplyr::mutate(age_band_start = factor(age_band_start, levels = c(
      "0 yrs", "1-4 yrs", "5-9 yrs", "10-14 yrs", "15-19 yrs", "20-24 yrs", "25-29 yrs", "30-34 yrs", "35-39 yrs", "40-44 yrs",
      "45-49 yrs", "50-54 yrs", "55-59 yrs", "60-64 yrs", "65-69 yrs", "70-74 yrs", "75-79 yrs", "80-84 yrs", "85-89 yrs", "90-94 yrs", "95+"
    )))

  spell_start <- c("2019-01-01 09:30:00", "2019-01-02 07:30:00", "2019-01-01 09:30:00", "2019-01-01 09:30:00", "2019-01-02 07:30:00", "2019-01-01 09:30:00", "2019-01-02 07:30:00", "2019-01-01 09:30:00", "2019-01-01 09:30:00", "2019-01-02 07:30:00", "2019-01-02 07:30:00", "2019-01-02 07:30:00")

  spell_end <- c("2019-01-05 09:30:00", "2019-01-04 09:30:00", "2019-01-01 12:30:00", "2019-01-05 09:30:00", "2019-01-04 09:30:00", "2019-01-01 12:30:00", "2019-01-04 09:30:00", "2019-01-01 12:30:00", "2019-01-05 09:30:00", "2019-01-04 09:30:00", "2019-01-04 09:30:00", "2019-01-04 09:30:00")

  age_band_start <- c("1-4 yrs", "1-4 yrs", "1-4 yrs", "1-4 yrs", "1-4 yrs", "1-4 yrs", "95+", "95+", "95+", "95+", "95+", "95+")

  gender <- as.factor(c("Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female"))

  spell_class_col <- as.factor(c(
    "ed_non_admission", "ed_comp_admission", "ed_comp_admission", "ed_non_admission", "ed_admission", "ed_comp_admission",
    "ed_non_admission", "ed_comp_admission", "ed_comp_admission", "ed_non_admission", "ed_admission", "ed_comp_admission"
  ))

  spell_start <- as.POSIXct(spell_start, tz = "Europe/London")
  spell_end <- as.POSIXct(spell_end, tz = "Europe/London")

  test_att_adm_age_sex <- tibble::tibble(
    spell_number = 101:112,
    spell_start,
    spell_end,
    gender,
    age_band_start,
    spell_class_col
  )

  test_att_adm_age_sex <- test_att_adm_age_sex %>%
    dplyr::mutate(age_band_start = factor(age_band_start, levels = c(
      "0 yrs", "1-4 yrs", "5-9 yrs", "10-14 yrs", "15-19 yrs", "20-24 yrs", "25-29 yrs", "30-34 yrs", "35-39 yrs", "40-44 yrs",
      "45-49 yrs", "50-54 yrs", "55-59 yrs", "60-64 yrs", "65-69 yrs", "70-74 yrs", "75-79 yrs", "80-84 yrs", "85-89 yrs", "90-94 yrs", "95+"
    )))

  # Run Admission Discharges graph
  result <- plot_ed_attendance_demographics(
    data = test_att_adm_age_sex,
    startDate = "2019-01-01 00:00:00",
    endDate = "2019-01-05 00:00:00",
    returnPlot = FALSE,
    hospitalName = "{hospital_name}"
  )

  result$value <- as.numeric(result$value)
  result$group <- as.factor(result$group)
  result$gender <- as.factor(result$gender)

  # Test results are correct
  expect_equal(result, correct_answers_1, tolerance = 0.1)
})

test_that("5-9 yrs age group is in the correct place", {
  test_data <- readRDS("testdata/plot_ed_attendance_demographics/attend_adm_age_sex_test.rds")
  result <- plot_ed_attendance_demographics(
    data = test_data,
    startDate = "2019-01-01",
    endDate = "2019-01-02",
    returnPlot = TRUE,
    hospitalName = "{hospital_name}"
  )
  # extracting x-tick labels from the plot
  lbls <- ggplot2::ggplot_build(result)$layout$panel_params[[1]]$x$breaks
  expect_equal(which("5-9 yrs" == lbls), 3)
})
