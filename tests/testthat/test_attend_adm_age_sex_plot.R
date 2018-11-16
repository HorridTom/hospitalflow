context("AE Attendances and Admissions, Age-Sex, Jan, 2012 - Jan, 2015")
library(hospitalflow)

test_that("Admission by age-Sex, Jan, 2012 - March, 2015",{

  load("testdata/test_data_age_sex_att_adm.rda")

  #Specify correct results
  correct_answers <- tibble::tibble(
    Gender =  as.factor(c( "Female", "Male", "Female","Male", "Female","Male",
                            "Female","Male", "Female","Male", "Female","Male",
                            "Female","Male", "Female","Male", "Female","Male",
                            "Female","Male", "Female","Male", "Female","Male",
                            "Female","Male", "Female","Male", "Female","Male",
                            "Female","Male", "Female","Male", "Female","Male", "Female","Male" )),

    AgeBand = as.factor(c(   "0 yrs",  "0 yrs",   "1-4 yrs", "1-4 yrs",
                           "10-14 yrs","10-14 yrs",  "15-19 yrs", "15-19 yrs","20-24 yrs","20-24 yrs",
                           "25-29 yrs", "25-29 yrs", "30-34 yrs", "30-34 yrs","35-39 yrs","35-39 yrs",
                           "40-44 yrs", "40-44 yrs","45-49 yrs", "45-49 yrs","5-9 yrs","5-9 yrs","50-54 yrs","50-54 yrs",
                           "55-59 yrs", "55-59 yrs", "60-64 yrs","60-64 yrs", "65-69 yrs","65-69 yrs",
                           "70-74 yrs ","70-74 yrs ", "75-79 yrs","75-79 yrs", "80-84 yrs","80-84 yrs", "85+ yrs", "85+ yrs" )),


                Attend = as.numeric(c( 1242, 1742, 1769, 2587,
                                       602, 873,   705, 572,  1240,  806,
                                      1498, 981,  1342, 956,  1089,  863,
                                       793, 868,   694, 901,  805, 1144, 657,  749,
                                       526, 622,   553, 626,   521,  569,
                                       541, 511,   486, 501,   550,  510, 1140, 591)),

    Attendances_Gender = as.factor(c("Female not admitted", "Male not admitted","Female not admitted", "Male not admitted", "Female not admitted", "Male not admitted",
                                        "Female not admitted", "Male not admitted", "Female not admitted", "Male not admitted","Female not admitted", "Male not admitted",
                                        "Female not admitted", "Male not admitted","Female not admitted",  "Male not admitted","Female not admitted", "Male not admitted",
                                        "Female not admitted", "Male not admitted", "Female not admitted", "Male not admitted", "Female not admitted", "Male not admitted",
                                        "Female not admitted", "Male not admitted","Female not admitted",  "Male not admitted","Female not admitted", "Male not admitted",
                                        "Female not admitted", "Male not admitted","Female not admitted",  "Male not admitted","Female not admitted", "Male not admitted","Female not admitted", "Male not admitted")))


  # correct_answers <- correct_answers %>%
    # dplyr::arrange(Attend)

  #Run Admission Discharges graph
  result <- ae_attendances_admissions_age_sex(start_date = "2012-01-01 00:00:00", end_date = "2015-01-31 00:00:00", data = test_data_age_sex_att_adm)

  result$Attend <- as.numeric(result$Attend)
  result$Attendances_Gender <- as.factor(result$Attendances_Gender)

  result_data <- result %>%
    dplyr::arrange(AgeBand)


  #Test results are correct
  expect_equal(as.data.frame(result_data), as.data.frame(correct_answers), tolerance = 0.1)


})
