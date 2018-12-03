context("AE Attendances and Admissions, Age-Sex, Jan, 2012 - Jan, 2015")
library(hospitalflow)

test_that("Admission by age-Sex, Jan, 2012 - March, 2015",{

  load("testdata/test_data_age_sex_att_adm.rda")

  #Specify correct results
  correct_answers <- tibble::tibble(
    Gender =  as.factor(c( "Female","Female","Female","Female","Female", "Female", "Female","Female","Female","Female","Female","Female","Female","Female","Female","Female","Female","Female","Female",
                             "Male", "Male",  "Male",   "Male","Male",     "Male",  "Male",   "Male", "Male",  "Male",  "Male",  "Male",  "Male",  "Male",  "Male",   "Male", "Male",    "Male","Male",

                           "Female","Female","Female","Female","Female", "Female", "Female","Female","Female","Female","Female","Female","Female","Female","Female","Female","Female","Female","Female",
                           "Male", "Male",  "Male",   "Male","Male",     "Male",  "Male",   "Male", "Male",  "Male",  "Male",  "Male",  "Male",  "Male",  "Male",   "Male", "Male",    "Male","Male")),

    Age_band = as.factor(c( "0 yrs", "1-4 yrs","5-9 yrs","10-14 yrs","15-19 yrs","20-24 yrs","25-29 yrs","30-34 yrs","35-39 yrs","40-44 yrs","45-49 yrs","50-54 yrs","55-59 yrs","60-64 yrs","65-69 yrs","70-74 yrs","75-79 yrs","80-84 yrs", "85+ yrs",
                            "0 yrs", "1-4 yrs","5-9 yrs","10-14 yrs","15-19 yrs","20-24 yrs","25-29 yrs","30-34 yrs","35-39 yrs","40-44 yrs","45-49 yrs","50-54 yrs","55-59 yrs","60-64 yrs","65-69 yrs","70-74 yrs","75-79 yrs","80-84 yrs", "85+ yrs",


                            "0 yrs", "1-4 yrs","5-9 yrs","10-14 yrs","15-19 yrs","20-24 yrs", "25-29 yrs","30-34 yrs","35-39 yrs","40-44 yrs","45-49 yrs","50-54 yrs","55-59 yrs","60-64 yrs","65-69 yrs","70-74 yrs", "75-79 yrs","80-84 yrs", "85+ yrs",
                            "0 yrs", "1-4 yrs","5-9 yrs","10-14 yrs","15-19 yrs","20-24 yrs","25-29 yrs","30-34 yrs","35-39 yrs","40-44 yrs","45-49 yrs", "50-54 yrs","55-59 yrs","60-64 yrs","65-69 yrs","70-74 yrs", "75-79 yrs","80-84 yrs", "85+ yrs")),

    Value = as.numeric(c(363, 541, 221, 236, 381, 423, 428, 326, 234,222, 255, 177, 169, 160, 153,146, 182, 189, 344,
                         517, 832, 241, 149, 227, 303, 265, 262, 244,272, 333, 231, 199, 187, 170,173, 131, 156, 192,

                              85, 107, 50,  77,  116, 112,  138, 93, 70,  73,  47, 62, 58, 78, 82, 86,  117, 127, 255,
                             134, 212, 42,  46,  67,   91,   85, 80, 86,  84,  63, 87, 76, 92, 84, 98,  78,   94, 128)),

    Group = as.factor(c("Female not admitted", "Female not admitted","Female not admitted","Female not admitted","Female not admitted", "Female not admitted","Female not admitted","Female not admitted",
                        "Female not admitted", "Female not admitted","Female not admitted","Female not admitted","Female not admitted", "Female not admitted","Female not admitted","Female not admitted",
                        "Female not admitted", "Female not admitted","Female not admitted",

                        "Male not admitted",   "Male not admitted",  "Male not admitted", "Male not admitted","Male not admitted",   "Male not admitted",  "Male not admitted", "Male not admitted",
                        "Male not admitted",   "Male not admitted",  "Male not admitted", "Male not admitted","Male not admitted",   "Male not admitted",  "Male not admitted", "Male not admitted",
                        "Male not admitted",   "Male not admitted",  "Male not admitted",

                        "Female admitted", "Female admitted","Female admitted","Female admitted","Female admitted", "Female admitted","Female admitted","Female admitted",
                        "Female admitted", "Female admitted","Female admitted","Female admitted","Female admitted", "Female admitted","Female admitted","Female admitted",
                        "Female admitted", "Female admitted","Female admitted",

                        "Male admitted",   "Male admitted",  "Male admitted", "Male admitted","Male admitted",   "Male admitted",  "Male admitted", "Male admitted",
                        "Male admitted",   "Male admitted",  "Male admitted", "Male admitted","Male admitted",   "Male admitted",  "Male admitted", "Male admitted",
                        "Male admitted",   "Male admitted",  "Male admitted")))

   correct_answers_1 <- correct_answers %>%
     dplyr::mutate(Age_band =  factor(Age_band, levels = c("0 yrs", "1-4 yrs","5-9 yrs","10-14 yrs","15-19 yrs","20-24 yrs","25-29 yrs","30-34 yrs","35-39 yrs","40-44 yrs","45-49 yrs",
                                                            "50-54 yrs","55-59 yrs","60-64 yrs","65-69 yrs","70-74 yrs","75-79 yrs","80-84 yrs", "85+ yrs")))


   #correct_answers_1 <- correct_answers_1 %>%
     #dplyr::mutate(Gender = factor(Gender, levels = c("Male","Female", "Not Specified")))

    str(correct_answers_1)


    #Run Admission Discharges graph
    result <- ae_attendances_admissions_age_sex(start_date = "2012-01-01 00:00:00", end_date = "2015-01-01 00:00:00", data = test_data_age_sex_att_adm, plot_chart = TRUE)

    result_data <- result$data
    result_data$Value <- as.numeric(result_data$Value)

    #result_data <- result_data %>%
      #dplyr::mutate(Gender = factor(Gender, levels = Male("Male", "Not Specified"), Female = c("Female")))


    #result_data <- result_data %>%
     # dplyr::mutate(Gender = fct_collapse(fct_inorder(Gender), Male = c("Male", "Not Specified"), Female = c("Female")))


    result_data$Group <- as.factor(result_data$Group)
    result_data$Age_band <- forcats::fct_relevel(result_data$Age_band)



    str(result_data)
    #Test results are correct
    expect_equal(as.data.frame(result_data), as.data.frame(correct_answers_1), tolerance = 0.1)



})
