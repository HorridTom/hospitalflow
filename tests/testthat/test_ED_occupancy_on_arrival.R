context("Length of stay >4hrs, wait for first assessment and unscheduled ED attendance against registered ED occupancy on arrival")
library(hospitalflow)


test_that("occupancies on arrival are correct",{

  test_ed_occupancy_on_arrival_data <- readRDA("testdata/ed_occupancy_on_arrival/test_ed_occupancy_on_arrival.rda")

  correct_answers <- c(4, 5, 4, 8, 8, 7, 8, 4, 4, 8)


  result <- hospitalflow::ed_occupancy_on_arrival(test_ed_occupancy_on_arrival_data)$occupancy_on_arrival


  #Test results are correct
  expect_equal(result, correct_answers)


})


