
test_that("ward type-specialty relationship is computed correctly", {
  # Load sample data
  sample_ip_data <- readRDS("testdata/boarding/sample_inpatient_data.rds")

  # Specify correct results
  correct_answers <- tibble::tibble(
    "ACCIDENT & EMERGENCY"  = as.integer(c(0,0,1,0,0,0,1,1,0,0)),
    "ANAESTHETICS"          = as.integer(c(0,1,0,0,0,0,0,0,0,0)),
    "CLINICAL HAEMATOLOGY"  = as.integer(c(0,0,0,0,0,0,1,0,0,0)),
    "DERMATOLOGY"           = as.integer(c(0,1,0,0,0,0,0,0,0,0)),
    "GASTROENTEROLOGY"      = as.integer(c(0,0,0,1,0,0,0,0,0,0)),
    "GENERAL MEDICINE"      = as.integer(c(0,0,1,0,1,1,1,0,1,0)),
    "GENERAL SURGERY"       = as.integer(c(1,0,1,2,0,0,0,0,0,0)),
    "PAEDIATRICS"           = as.integer(c(0,0,0,1,0,0,0,0,0,0)),
    "TRAUMA & ORTHOPAEDICS" = as.integer(c(1,0,0,1,0,0,0,0,0,1))
  )

  # Geting function output
  result <- get_ward_specialty_contingency_table(
    ipData = sample_ip_data,
    scaleBy = NA,
    returnPlot = FALSE
  ) %>% tibble::as_tibble()

  # Comparing the output with correct results
  expect_identical(result, correct_answers)
})
