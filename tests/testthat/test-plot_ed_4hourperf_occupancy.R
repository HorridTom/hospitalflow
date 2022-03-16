
# tests with constructed data

pseudo_id <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
attendance_category <- c(
  "First Attendance", "First Attendance", "First Attendance", "First Attendance",
  "Follow up - Un-planned", "Follow up - Planned", "First Attendance", "First Attendance",
  "First Attendance", "First Attendance"
)
start_datetime <- c(
  "2017-05-01 00:00:00 UTC", "2017-05-01 12:00:00 UTC", "2017-05-01 00:00:00 UTC",
  "2017-05-02 00:00:00 UTC", "2017-05-02 00:00:00 UTC", "2017-05-03 00:00:00 UTC",
  "2017-05-02 12:00:00 UTC", "2017-05-01 00:00:00 UTC", "2017-05-01 00:00:00 UTC",
  "2017-05-02 00:00:00 UTC"
)
end_datetime <- c(
  "2017-05-02 00:00:00 UTC", "2017-05-02 12:00:00 UTC", "2017-05-03 12:00:00 UTC",
  "2017-05-02 12:00:00 UTC", "2017-05-03 00:00:00 UTC", "2017-05-03 12:00:00 UTC",
  "2017-05-03 12:00:00 UTC", "2017-05-03 00:00:00 UTC", "2017-05-03 00:00:00 UTC",
  "2017-05-03 12:00:00 UTC"
)
initial_assess_datetime <- c(
  "2017-05-01 12:00:00 UTC", "2017-05-02 00:00:00 UTC", "2017-05-01 00:00:00 UTC",
  "2017-05-02 00:00:00 UTC", "2017-05-03 00:00:00 UTC", "2017-05-03 00:00:00 UTC",
  "2017-05-03 00:00:00 UTC", "2017-05-01 00:00:00 UTC", "2017-05-02 00:00:00 UTC",
  "2017-05-02 00:00:00 UTC"
)
treatment_datetime <- c(
  "2017-05-02 12:00:00 UTC", "2017-05-03 00:00:00 UTC", "2017-05-02 00:00:00 UTC",
  "2017-05-03 00:00:00 UTC", "2017-05-04 00:00:00 UTC", "2017-05-04 00:00:00 UTC",
  "2017-05-04 00:00:00 UTC", "2017-05-02 00:00:00 UTC", "2017-05-03 00:00:00 UTC",
  "2017-05-03 00:00:00 UTC"
)

constructed_test_data <- data.frame(
  pseudo_id, attendance_category, start_datetime, end_datetime,
  initial_assess_datetime, treatment_datetime
)

test_that("occupancies on arrival are correct", {
  correct_answers <- c(4, 5, 4, 8, 8, 7, 8, 4, 4, 8)
  result <- occupancy_on_arrival(constructed_test_data)$occupancy_on_arrival

  expect_equal(result, correct_answers)
})


# tests with simulated "real" data sample

real_test_data <- structure(
  list(
    pseudo_id = c(
      129L, 528L, 1191L, 1455L, 1482L, 1519L, 1659L,
      1730L, 1862L
    ),
    gender = structure(c(2L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 1L),
      .Label = c("Female", "Male"),
      class = "factor"
    ),
    age_band_start = c(
      "30-34 yrs", "35-39 yrs", "50-54 yrs",
      "40-44 yrs", "80-84 yrs", "0 yrs", "80-84 yrs",
      "25-29 yrs", "5-9 yrs"
    ),
    ethnic_category = c(
      "Not stated", "White - Any other White background",
      "White - British", "Asian or Asian - Pakistani",
      "Not stated", "White - British", "White - British",
      "Asian or Asian - British Indian",
      "Asian or Asian - Any other Asian background"
    ),
    start_datetime = structure(c(
      1551481262.63053, 1551462154.29765,
      1551457481.40517, 1551466480.26945,
      1551454829.22573, 1551423199.38281,
      1551403473.48296, 1551441473.30863,
      1551418272.15184
    ),
    tzone = "", class = c("POSIXct", "POSIXt")
    ),
    end_datetime = structure(c(
      1551481564.61342, 1551462465.80738,
      1551471596.35501, 1551467460.53274,
      1551478520.39928, 1551431533.11254,
      1551405930.33802, 1551457110.66745,
      1551434257.62777
    ),
    tzone = "", class = c("POSIXct", "POSIXt")
    ),
    triage_category = c(
      "Urgent", "Very Urgent", "Standard",
      "Urgent", "Urgent", "Urgent", "Very Urgent",
      "Standard", "Urgent"
    ),
    attendance_category = c(
      "First Attendance", "First Attendance",
      "First Attendance", "First Attendance",
      "First Attendance", "First Attendance",
      "First Attendance", "First Attendance",
      "First Attendance"
    ),
    arrival_mode = c(
      "Ambulance", "Emergency Ambulance", "Emergency Ambulance",
      "Custodial Services", "Walk-in", "Emergency Ambulance",
      "Emergency Ambulance", "Walk-in", "Walk-in"
    ),
    attendance_disposal = c(
      "Transferred to other Health Care Provider",
      "Discharged", "Discharged", "Admitted",
      "Transferred to other Health Care Provider",
      "Discharged", "Discharged",
      "Transferred to other Health Care Provider",
      "Left Department before being seeing for treatment"
    ),
    referral_source = c(
      "Health Care Provider", "Health Care Provider",
      "Emergency Services", "Emergency Services",
      "Emergency Services", "Self Referral",
      "Health Care Provider", "Self Referral",
      "Health Care Provider"
    ),
    initial_assess_datetime = structure(c(
      1551481564.61342, 1551462465.80738,
      1551458544.40517, 1551467460.53274,
      1551424222.38281, 1551404494.48296,
      1551442542.30863, 1551419325.15184,
      1551463312.90687
    ),
    tzone = "",
    class = c("POSIXct", "POSIXt")
    ),
    treatment_datetime = structure(c(
      1551481564.61342, 1551462465.80738,
      1551462812.40517, 1551467460.53274,
      1551460055.22573, 1551428568.38281,
      1551405930.33802, 1551446757.30863,
      1551423533.15184
    ),
    tzone = "",
    class = c("POSIXct", "POSIXt")
    ),
    hrg_code = c(
      "WD22Z", "AA15Z", "AA19Z", "AA18B", "WA24Z", "AA08A",
      "WA23X", "WA20Y", "WA20Y"
    ),
    episode_id = c(
      214L, 875L, 2013L, 2488L, 2544L, 2605L, 2846L, 2972L,
      3178L
    )
  ),
  row.names = c(NA, -9L),
  groups = structure(list(.rows = structure(list(
    1L, 2L, 3L, 4L, 5L, 6L,
    7L, 8L, 9L
  ),
  ptype = integer(0),
  class = c(
    "vctrs_list_of",
    "vctrs_vctr", "list"
  )
  )),
  row.names = c(NA, -9L), class = c(
    "tbl_df", "tbl",
    "data.frame"
  )
  ),
  class = c("rowwise_df", "tbl_df", "tbl", "data.frame")
)

test_that("occupancies on arrival are correct for real data", {
  correct_answers <- c(1, 3, 2, 3, 2, 2, 1, 1, 1)
  result <- occupancy_on_arrival(real_test_data)$occupancy_on_arrival

  expect_equal(result, correct_answers)
})

real_data_plot_df <- plot_ed_4hourperf_occupancy(real_test_data,
  "2019-03-01 00:00:00",
  "2019-03-02 00:00:00",
  returnPlot = F
)

test_that("n_all is correct for real data", {
  correct_answers <- c(4, 3, 2)
  result <- real_data_plot_df$n_all

  expect_equal(result, correct_answers)
})

test_that("n_LoS_over_4hrs is correct for real data", {
  correct_answers <- c(2, 1, 0)
  result <- real_data_plot_df$n_LoS_over_4hrs

  expect_equal(result, correct_answers)
})

test_that("perc_LoS_over_4hrs is correct for real data", {
  correct_answers <- c(50, 33.33, 0)
  result <- real_data_plot_df$perc_LoS_over_4hrs

  expect_equal(result, correct_answers, tolerance = 0.01)
})

test_that("n_W4T_over_4hrs is correct for real data", {
  correct_answers <- c(0, 0, 0)
  result <- real_data_plot_df$n_W4T_over_4hrs

  expect_equal(result, correct_answers)
})

test_that("perc_W4T_over_4hrs is correct for real data", {
  correct_answers <- c(0, 0, 0)
  result <- real_data_plot_df$perc_W4T_over_4hrs

  expect_equal(result, correct_answers)
})
