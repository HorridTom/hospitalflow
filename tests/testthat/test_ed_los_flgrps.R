context("ED length of stay for flow groups for real random data")
library(hospitalflow)

test_that("ed length of stay for flow groups is calculated correctly", {
  correct_answers <- readRDS("testdata/ed_los_flows/correct_answers.rds") %>%
    dplyr::ungroup()

  ed_los_flow_groups <- readRDS("testdata/ed_los_flows/ed_los_flows.rds")

  result <- ed_los_flow_grps(
    start_date = as.Date("2016-06-01", tz = "Europe/London"),
    end_date = as.Date("2016-06-30", tz = "Europe/London"),
    data = ed_los_flow_groups, plot_chart = FALSE, hospital_name = hospital_name
  )

  # Test results are correct up to row order
  expect_equal(
    correct_answers %>% dplyr::arrange(Time_binned, flow_groups),
    result %>% dplyr::arrange(Time_binned, flow_groups)
  )
})
