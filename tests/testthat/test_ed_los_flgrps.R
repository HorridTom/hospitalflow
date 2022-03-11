
test_that("ed length of stay for flow groups is calculated correctly", {
  correct_answers <- readRDS("testdata/plot_ed_los_distribution_flow/correct_answers.rds") %>%
    dplyr::ungroup()

  ed_los_flow_groups <- readRDS("testdata/plot_ed_los_distribution_flow/plot_ed_los_distribution_flow.rds")

  result <- plot_ed_los_distribution_flow(
    data = ed_los_flow_groups,
    startDate = as.Date("2016-06-01", tz = "Europe/London"),
    endDate = as.Date("2016-06-30", tz = "Europe/London"),
    returnPlot = FALSE,
    hospitalName = "Anytown General Hospital"
  )

  # Test results are correct up to row order
  expect_equal(
    correct_answers %>% dplyr::arrange(Time_binned, flow_groups),
    result %>% dplyr::arrange(Time_binned, flow_groups)
  )
})
