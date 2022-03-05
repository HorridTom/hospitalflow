context("Occupancy at specified time")

test_that("Occupancy at specified time correct for improvised data", {

  # test time
  t0 <- as.POSIXct("2018-12-10 09:00:00", tz = "Europe/London")
  t1 <- as.POSIXct("2018-12-10 09:30:00", tz = "Europe/London")
  t2 <- as.POSIXct("2018-12-10 10:45:00", tz = "Europe/London")

  # Specify correct results
  correct_answer0 <- 0
  correct_answer1 <- 1
  correct_answer2 <- 2

  # creating a dataset
  dt1 <- c("2018-12-10 09:30:00", "2018-12-10 10:15:00", "2018-12-11 08:05:00")
  dt2 <- c("2018-12-10 12:30:00", "2018-12-11 07:45:00", "2018-12-11 09:05:00")

  spell_start <- as.POSIXct(dt1, tz = "Europe/London")
  spell_end <- as.POSIXct(dt2, tz = "Europe/London")

  test_arrivals_occupancy <- tibble::tibble(
    pseudo_id = 101:103,
    spell_start,
    spell_end
  )

  result0 <- occupancy(time_instance = t0, df = test_arrivals_occupancy, df_type = "spell table")
  result1 <- occupancy(time_instance = t1, df = test_arrivals_occupancy, df_type = "spell table")
  result2 <- occupancy(time_instance = t2, df = test_arrivals_occupancy, df_type = "spell table")

  # Test resultsare correct
  expect_equal(result0, correct_answer0)
  expect_equal(result1, correct_answer1)
  expect_equal(result2, correct_answer2)
})


test_that("Occupancy by hour of a date is generated correctly, for real data", {
  occupancy_test <- readRDS("testdata/occupancy/occupancy_test.rds")

  occupancy_test <- occupancy_test %>%
    dplyr::select(spell_number, spell_start, spell_end)

  t0 <- as.POSIXct("2016-05-23 13:45:00", tz = "Europe/London")
  t1 <- as.POSIXct("2016-06-01 06:00:00", tz = "Europe/London")

  # Specify correct results
  correct_answer0 <- 1
  correct_answer1 <- 2

  # Specify correct results
  result0 <- occupancy(time_instance = t0, df = occupancy_test, df_type = "spell table")
  result1 <- occupancy(time_instance = t1, df = occupancy_test, df_type = "spell table")

  # Test results are correct
  expect_equal(result0, correct_answer0)
  expect_equal(result1, correct_answer1)
})
