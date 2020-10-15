
#function to get simulated ed data
get_simulated_ed_data <- function(npat = 1000, start = as.POSIXct("2019-01-01 00:00:00"),
                                  end = as.POSIXct("2019-04-01 00:00:00")) {
  simulated_pat_data <- tibble::tibble(pseudo_id = 1:npat,
                                       gender = factor(sample(c("Male","Female"), npat, replace = TRUE))
  )

  simulated_pat_data <- simulated_pat_data %>%
    rowwise()%>%
    mutate(age_band_start = get_age()) %>%
    mutate(ethnic_category = get_ethnic_cat()) %>%
    ungroup()

  simulated_data <- get_simulated_admission_data(simulated_pat_data, start = start, end = end)
  simulated_data <- simulated_data[[2]]

  simulated_data <- simulated_data %>%
    select(-c(pseudo_id1)) %>%
    rowwise() %>%
    mutate(triage_category = get_traige_cat()) %>%
    mutate(attendance_category = get_attendance_cat()) %>%
    mutate(arrival_mode = get_arrival_mode()) %>%
    mutate(attendance_disposal = get_attendance_disp()) %>%
    mutate(referral_source = get_referral_source()) %>%
    mutate(initial_assess_datetime = as.POSIXct(generate_initial_assess(start_datetime = start_datetime,
                                                                        end_datetime = end_datetime))) %>%
    mutate(treatment_datetime = as.POSIXct(generate_treatment_time(start_datetime = start_datetime,
                                                                   end_datetime = end_datetime))) %>%
    mutate(hrg_code = get_hrg_code())

  simulated_data$episode_id <- 1:nrow(simulated_data)

  simulated_data

}


#function to get simulated inpatient data
get_simulated_ip_data <- function(npat = 800,
                                  start = as.POSIXct("2019-01-01 00:00:00"),
                                  end = as.POSIXct("2019-04-01 00:00:00"),
                                  ed_data = example_ed_data) {

  #get patients admistted from ED
  ed_admissions <- ed_data %>%
    filter(attendance_disposal == "Admitted") %>%
    mutate(start_datetime_ip = end_datetime, admission_method = "Accident and emergency") %>%
    select(pseudo_id, gender, age_band_start, ethnic_category, start_datetime_ip, admission_method) %>%
    rename(start_datetime = start_datetime_ip) %>%
    mutate(end_datetime = as.POSIXct(start_datetime +
                                       as.difftime(rexp(1, rate = 1/(388800)), units = "secs")))

  #simulate new patiens who have not come from ED
  simulated_pat_data <- tibble::tibble(pseudo_id = 1001:(1000 + npat),
                                       gender = factor(sample(c("Male","Female"), npat, replace = TRUE))
                                       )

  #combine new patients and patiens from ED
  simulated_pat_data <- simulated_pat_data %>%
    rowwise()%>%
    mutate(age_band_start = get_age()) %>%
    mutate(ethnic_category = get_ethnic_cat()) %>%
    ungroup()

  simulated_data <- get_simulated_admission_data(simulated_pat_data, start = start, end = end, los_rate = (1/388800))
  simulated_data <- simulated_data[[2]]

  #add extra rows for 14 and 28 day readmissions
  simulated_readmissions_14 <- simulated_data[rep((nrow(simulated_data) - 20):nrow(simulated_data),1),]
  simulated_readmissions_14 <- simulated_readmissions_14 %>%
    mutate(start_datetime = start_datetime + lubridate::days(13), end_datetime = end_datetime + lubridate::days(13))

  simulated_readmissions_28 <- simulated_data[rep((nrow(simulated_data) - 40):(nrow(simulated_data) - 21),1),]
  simulated_readmissions_28 <- simulated_readmissions_28 %>%
    mutate(start_datetime = start_datetime + lubridate::days(27), end_datetime = end_datetime + lubridate::days(27))

  simulated_data <- simulated_data %>%
    rowwise() %>%
    bind_rows(simulated_readmissions_14) %>%
    bind_rows(simulated_readmissions_28) %>%
    mutate(admission_method = get_admission_method()) %>%
    bind_rows(ed_admissions) %>%
    mutate(source_of_admission = get_source_of_admission()) %>%
    mutate(discharge_method = get_discharge_method()) %>%
    mutate(discharge_destination = get_discharge_destination()) %>%
    mutate(patient_classification = get_patient_classification()) %>%
    mutate(main_specialty = get_main_specialty()) %>%
    mutate(local_subspecialty = main_specialty) %>%
    mutate(diagnosis_code = get_diagnosis_code()) %>%
    mutate(ward_category = get_ward_category()) %>%
    mutate(consultant = get_consultant()) %>%
    mutate(hrg_code = get_hrg_code()) %>%
    select(-pseudo_id1)

  simulated_data$episode_id <- 1:nrow(simulated_data)

  simulated_data

}


##########################################################################################
#supporting functions

get_simulated_admission_data <- function(patient_data, fixedPerPatient = F, lambda = 0.69,
                                         start,
                                         end,
                                         los_rate = 1/(12771)) {

  npat <- nrow(patient_data)

  if(fixedPerPatient) {
    patient_data <- patient_data %>% dplyr::mutate(num_admissions = lambda)
  } else {
    patient_data <- patient_data %>% dplyr::mutate(num_admissions = rpois(n = npat, lambda = lambda) +1)
  }



  generate_admissions <- function(...) {
    pseudo_id <- list(...)[["pseudo_id"]]
    num_admissions <- list(...)[["num_admissions"]]
    tibble::tibble(pseudo_id1 = rep(pseudo_id, num_admissions),
                   start_datetime = as.POSIXct(random_datetimes(n = num_admissions, start = start, end = end)),
                   end_datetime = as.POSIXct(start_datetime +
                                               as.difftime(rexp(num_admissions, rate = los_rate), units = "secs"))
    )

  }

  patient_data <- patient_data %>% dplyr::mutate(admissions =
                                                   purrr::pmap(., function(...) generate_admissions(...)))

  list(patients = patient_data %>% dplyr::select(-admissions, -num_admissions),
       admissions = patient_data %>% tidyr::unnest(cols = c(admissions)) %>% dplyr::select(-num_admissions))
}


random_datetimes <- function(n, start, end) {
  interval_duration_seconds <- difftime(end, start, units = "secs")
  random_seconds_from_start <- runif(n, min = 0, max = interval_duration_seconds)
  times_from_start <- as.difftime(random_seconds_from_start, units = "secs")
  start + times_from_start
}

# random_datetimes_normal <- function(n, start, end, mean = 12771, sd = 121037) {
#   interval_duration_seconds <- difftime(end, start, units = "secs")
#   random_seconds_from_start <- runif(n, mean = mean, sd = sd)
#   times_from_start <- as.difftime(random_seconds_from_start, units = "secs")
#   start + times_from_start
# }

########################################################
#function to return an age band according to proportions
get_age <- function(){

  ranNum <- runif(1, 0, 100)

  if(ranNum >= 0 & ranNum < 4.25){
    age_band_start <- "0 yrs"
  }else if(ranNum >= 4.25 & ranNum < 13.47){
    age_band_start <- "1-4 yrs"
  }else if(ranNum >= 13.47 & ranNum < 18.75){
    age_band_start <- "5-9 yrs"
  }else if(ranNum >= 18.75 & ranNum < 23.06){
    age_band_start <- "10-14 yrs"
  }else if(ranNum >= 23.06 & ranNum < 28.22){
    age_band_start <- "15-19 yrs"
  }else if(ranNum >= 28.22 & ranNum < 35.28){
    age_band_start <- "20-24 yrs"
  }else if(ranNum >= 35.28 & ranNum < 43.26){
    age_band_start <- "25-29 yrs"
  }else if(ranNum >= 43.26 & ranNum < 50.82){
    age_band_start <- "30-34 yrs"
  }else if(ranNum >= 50.82 & ranNum < 57.60){
    age_band_start <- "35-39 yrs"
  }else if(ranNum >= 57.60 & ranNum < 63.34){
    age_band_start <- "40-44 yrs"
  }else if(ranNum >= 63.34 & ranNum < 69.04){
    age_band_start <- "45-49 yrs"
  }else if(ranNum >= 69.04 & ranNum < 74.44){
    age_band_start <- "50-54 yrs"
  }else if(ranNum >= 74.44 & ranNum < 78.97){
    age_band_start <- "55-59 yrs"
  }else if(ranNum >= 78.97 & ranNum < 82.62){
    age_band_start <- "60-64 yrs"
  }else if(ranNum >= 82.62 & ranNum < 86.06){
    age_band_start <- "65-69 yrs"
  }else if(ranNum >= 86.06 & ranNum < 89.32){
    age_band_start <- "70-74 yrs"
  }else if(ranNum >= 89.32 & ranNum < 92.62){
    age_band_start <- "75-79 yrs"
  }else if(ranNum >= 92.62 & ranNum < 95.72){
    age_band_start <- "80-84 yrs"
  }else if(ranNum >= 95.72 & ranNum < 98.18){
    age_band_start <- "85-89 yrs"
  }else if(ranNum >= 98.18 & ranNum < 99.52){
    age_band_start <- "90-94 yrs"
  }else{
    age_band_start <- "95+"
  }

  age_band_start
}


########################################################
#function to return ethnic category according to proportions
get_ethnic_cat <- function(){

  ranNum <- runif(1, 0, 100)

  if(ranNum >= 0 & ranNum < 46.69){
    ethnic_category <- "White - British"
  }else if(ranNum >= 46.69 & ranNum < 47.20){
    ethnic_category <- "White - Irish"
  }else if(ranNum >= 47.20 & ranNum < 54.89){
    ethnic_category <- "White - Any other White background"
  }else if(ranNum >= 54.89 & ranNum < 55.22){
    ethnic_category <- "Mixed - White and Black Caribbean"
  }else if(ranNum >= 55.22 & ranNum < 55.55){
    ethnic_category <- "Mixed - White and Black African"
  }else if(ranNum >= 55.55 & ranNum < 55.63){
    ethnic_category <- "Mixed - White and Asian"
  }else if(ranNum >= 55.63 & ranNum < 57.55){
    ethnic_category <- "Mixed - Any other mixed background"
  }else if(ranNum >= 57.55 & ranNum < 60.18){
    ethnic_category <- "Asian or Asian - British Indian"
  }else if(ranNum >= 60.18 & ranNum < 60.61){
    ethnic_category <- "Asian or Asian - Pakistani"
  }else if(ranNum >= 60.61 & ranNum < 60.88){
    ethnic_category <- "Asian or Asian - Bangladeshi"
  }else if(ranNum >= 60.88 & ranNum < 65.39){
    ethnic_category <- "Asian or Asian - Any other Asian background"
  }else if(ranNum >= 65.39 & ranNum < 67.20){
    ethnic_category <- "Black or Black - British Carribean"
  }else if(ranNum >= 67.20 & ranNum < 78.12){
    ethnic_category <- "Black or Black - British African"
  }else if(ranNum >= 78.12 & ranNum < 81.05){
    ethnic_category <- "Black or Black British - Any other Black background"
  }else if(ranNum >= 81.05 & ranNum < 82.08){
    ethnic_category <- "Other Ethnic Groups - Chinese"
  }else if(ranNum >= 82.08 & ranNum < 87.25){
    ethnic_category <- "Other Ethnic Groups - Any other ethnic group"
  }else{
    ethnic_category <- "Not stated"
  }

  ethnic_category

}

########################################################
#function to return triage category according to proportions
get_traige_cat <- function(){

  ranNum <- runif(1, 0, 100)

  if(ranNum >= 0 & ranNum < 0.54){
    triage_category <- "Immediate Resuscitation"
  }else if(ranNum >= 0.54 & ranNum < 20.88){
    triage_category <- "Very Urgent"
  }else if(ranNum >= 20.88 & ranNum < 75.80){
    triage_category <- "Urgent"
  }else if(ranNum >= 75.80 & ranNum < 93.55){
    triage_category <- "Standard"
  }else{
    triage_category <- "Non-urgent"
  }

  triage_category
}


########################################################
#function to return attendance category according to proportions
get_attendance_cat <- function(){

  ranNum <- runif(1, 0, 100)

  if(ranNum >= 0 & ranNum < 98.5256){
    attendance_category <- "First Attendance"
  }else if(ranNum >= 98.5256 & ranNum < 98.8227){
    attendance_category <- "Follow up - Planned"
  }else if(ranNum >= 98.8227 & ranNum < 99.9148){
    attendance_category <- "Follow up - Un-planned"
  }else if(ranNum >= 99.9148 & ranNum < 99.9997){
    attendance_category <- "Follow-up Un-planned"
  }else{
    attendance_category <- "Death on Arrival"
  }

  attendance_category
}

########################################################
#function to return arrival_mode according to proportions
get_arrival_mode <- function(){

  ranNum <- runif(1, 0, 100)

  if(ranNum >= 0 & ranNum < 36.869){
    arrival_mode <- "Walk-in"
  }else if(ranNum >= 36.869 & ranNum < 49.984){
    arrival_mode <- "Ambulance"
  }else if(ranNum >= 49.984 & ranNum < 68.660){
    arrival_mode <- "Emergency Ambulance"
  }else if(ranNum >= 68.668 & ranNum < 99.337){
    arrival_mode <- "Custodial Services"
  }else if(ranNum >= 99.337 & ranNum < 99.441){
    arrival_mode <- "Police"
  }else if(ranNum >= 99.441 & ranNum < 99.778){
    arrival_mode <- "Non-Emergency Ambulance"
  }else if(ranNum >= 99.778 & ranNum < 99.827){
    arrival_mode <- "Air Ambulance"
  }else if(ranNum >= 99.827 & ranNum < 99.975){
    arrival_mode <- "Unknown"
  }else if(ranNum >= 99.975 & ranNum < 99.993){
    arrival_mode <- "Repatriation by Air"
  }else{
    arrival_mode <- "Helicopter"
  }

  arrival_mode
}

########################################################
#function to return attendance disposal according to proportions
get_attendance_disp <- function(){

  ranNum <- runif(1, 0, 100)

  if(ranNum >= 0 & ranNum < 13.01){
    attendance_disposal <- "Admitted"
  }else if(ranNum >= 13.01 & ranNum < 69.61){
    attendance_disposal <- "Discharged"
  }else if(ranNum >= 69.61 & ranNum < 70.19){
    attendance_disposal <- "Referred to A&E clinic"
  }else if(ranNum >= 70.19 & ranNum < 72.81){
    attendance_disposal <- "Referred to Fracture Clinic"
  }else if(ranNum >= 72.81 & ranNum < 74.51){
    attendance_disposal <- "Referred to other Out-Patient Clinic"
  }else if(ranNum >= 74.51 & ranNum < 92.65){
    attendance_disposal <- "Transferred to other Health Care Provider"
  }else if(ranNum >= 92.65 & ranNum < 92.70){
    attendance_disposal <- "Died in Department"
  }else if(ranNum >= 92.70 & ranNum < 94.27){
    attendance_disposal <- "Referred to other health Care Professional"
  }else if(ranNum >= 94.27 & ranNum < 95.9210){
    attendance_disposal <- "Left Department before being seeing for treatment"
  }else if(ranNum >= 95.9210 & ranNum < 95.9216){
    attendance_disposal <- "Left Department having refused treatment"
  }else{
    attendance_disposal <- "Other"
  }

  attendance_disposal
}

########################################################
#function to return referral source according to proportions
get_referral_source <- function(){

  ranNum <- runif(1, 0, 100)

  if(ranNum >= 0 & ranNum < 18.78){
    referral_source <- "Health Care Provider"
  }else if(ranNum >= 18.78 & ranNum < 38.50){
    referral_source <- "Emergency Services"
  }else if(ranNum >= 38.50 & ranNum < 42.917){
    referral_source <- "Other"
  }else if(ranNum >= 42.917 & ranNum < 42.920){
    referral_source <- "Community Dental Service"
  }else if(ranNum >= 42.920 & ranNum < 42.926){
    referral_source <- "General Dental Practitioner"
  }else if(ranNum >= 42.926 & ranNum < 43.01){
    referral_source <- "Educational Establishment"
  }else if(ranNum >= 43.01 & ranNum < 47.86){
    referral_source <- "General Medical Practitioner"
  }else if(ranNum >= 47.86 & ranNum < 47.91){
    referral_source <- "Local Authority Social Services"
  }else if(ranNum >= 47.91 & ranNum < 48.07){
    referral_source <- "Police"
  }else if(ranNum >= 48.07 & ranNum < 99.95){
    referral_source <- "Self Referral"
  }else{
    referral_source <- "Work"
  }

  referral_source
}

########################################################
#function to return randomised fake hrg_code
get_hrg_code <- function(){

  ranNum <- round(runif(1, 1, 31))

  if(ranNum == 1){
    hrg_code <- "AA02Z"
  }else if(ranNum == 2){
    hrg_code <- "AA03B"
  }else if(ranNum == 3){
    hrg_code <- "AA03Z"
  }else if(ranNum == 4){
    hrg_code <- "AA18B"
  }else if(ranNum == 5){
    hrg_code <- "WA21Y"
  }else if(ranNum == 6){
    hrg_code <- "AA15Z"
  }else if(ranNum == 7){
    hrg_code <- "AA16Z"
  }else if(ranNum == 8){
    hrg_code <- "AA18B"
  }else if(ranNum == 9){
    hrg_code <- "WA22X"
  }else if(ranNum == 10){
    hrg_code <- "WD22Z"
  }else if(ranNum == 11){
    hrg_code <- "AA09Z"
  }else if(ranNum == 12){
    hrg_code <- "AA09B"
  }else if(ranNum == 13){
    hrg_code <- "WA22V"
  }else if(ranNum == 14){
    hrg_code <- "AA09A"
  }else if(ranNum == 15){
    hrg_code <- "WA21W"
  }else if(ranNum == 16){
    hrg_code <- "AA08Z"
  }else if(ranNum == 17){
    hrg_code <- "WD11Z"
  }else if(ranNum == 18){
    hrg_code <- "AA20A"
  }else if(ranNum == 19){
    hrg_code <- "AA19Z"
  }else if(ranNum == 20){
    hrg_code <- "WA20Y"
  }else if(ranNum == 21){
    hrg_code <- "WA20W"
  }else if(ranNum == 22){
    hrg_code <- "WA19Y"
  }else if(ranNum == 23){
    hrg_code <- "WA24Z"
  }else if(ranNum == 24){
    hrg_code <- "WA19W"
  }else if(ranNum == 25){
    hrg_code <- "AA08A"
  }else if(ranNum == 26){
    hrg_code <- "AA06Z"
  }else if(ranNum == 27){
    hrg_code <- "WA23X"
  }else if(ranNum == 28){
    hrg_code <- "WA23Y"
  }else if(ranNum == 29){
    hrg_code <- "VB02Z"
  }else if(ranNum == 30){
    hrg_code <- "VB07Z"
  }else if(ranNum == 31){
    hrg_code <- "VB09Z"
  }else{
    hrg_code <- NA
  }

  hrg_code
}


#function to generate initial assessment time
generate_initial_assess <- function(start_datetime, end_datetime) {

  initial_assess_datetime <- start_datetime + as.difftime(rpois(1, 1043), units = "secs")

  if(initial_assess_datetime > end_datetime){
    initial_assess_datetime <- end_datetime
  }else{
    initial_assess_datetime
  }

}

#function to generate initial assessment time
generate_treatment_time <- function(start_datetime, end_datetime) {

  treatment_assess_datetime <- start_datetime + as.difftime(rpois(1, 5244), units = "secs")

  if(treatment_assess_datetime > end_datetime){
    treatment_assess_datetime <- end_datetime
  }else{
    treatment_assess_datetime
  }

}

#site?

##################################################
#funtions for additional columns in inpatient data
#admission_method
get_admission_method <- function(){

  ranNum <- runif(1, 0, 100)

  if(ranNum <= 21.96){
    admission_method <- "Accident and Emergency Department"
  }else if(ranNum <= 21.96){
    admission_method <- "Booked"
  }else if(ranNum <= 34.07){
    admission_method <- "Planned"
  }else if(ranNum <= 75.56){
    admission_method <- "Accident and emergency"
  }else if(ranNum <= 75.92){
    admission_method <- "General Practitioner"
  }else if(ranNum <= 75.95){
    admission_method <- "Bed bureau"
  }else if(ranNum <= 76.44){
    admission_method <- "Consultant Clinic"
  }else if(ranNum <= 76.445){
    admission_method <- "Mental Health Crisis Resolution Team"
  }else if(ranNum <= 80.4){
    admission_method <- "Waiting list"
  }else if(ranNum <= 80.41){
    admission_method <- "Transfer from another Hospital Provider"
  }else if(ranNum <= 80.42){
    admission_method <- "Intended home birth"
  }else if(ranNum <= 82.5){
    admission_method <- "Other emergency admission"
  }else if(ranNum <= 83.11){
    admission_method <- "Other means"
  }else if(ranNum <= 91.58){
    admission_method <- "Admitted ante-partum"
  }else if(ranNum <= 91.84){
    admission_method <- "Admitted post-partum"
  }else if(ranNum <= 99.28){
    admission_method <- "Birth-this provider"
  }else if(ranNum <= 99.42){
    admission_method <- "Birth-outside provider(not intended home)"
  }else if(ranNum <= 99.94){
    admission_method <- "Transfer from other provider(non-emergency)"
  }else{
    admission_method <- NA
  }

  admission_method
}

#source_of_admission
get_source_of_admission <- function(){

  ranNum <- runif(1, 0, 100)

  if(ranNum <= 85.59 ){
    source_of_admission <- "Usual Place of Residence"
  }else if(ranNum <= 92.83){
    source_of_admission <- "Temporary Place of Residence"
  }else if(ranNum <= 99.81){
    source_of_admission <- "NHS other Hospital Provider"
  }else if(ranNum <= 99.98){
    source_of_admission <- "Babies born in or on the way to hospital"
  }else if(ranNum <= 99.99){
    source_of_admission <- "Non-NHS run Hospice"
  # }else if(ranNum == 0){
  #   source_of_admission <- "Penal Establishment"
  # }else if(ranNum == 0){
  #   source_of_admission <- "NHS run Care Home"
  # }else if(ranNum == 0){
  #   source_of_admission <- "Non-NHS run Care Home"
  # }else if(ranNum == 0){
  #   source_of_admission <- "Local Authority Foster Care"
  }else{
    source_of_admission <- NA
  }

  source_of_admission
}

#discharge_method
get_discharge_method <- function(){

  ranNum <- runif(1, 0, 100)

  if(ranNum <= 98.24182633){
    discharge_method <- "Discharged"
  }else if(ranNum <= 99.97155){
    discharge_method <- "Patient died"
  }else{
    discharge_method <- "Stillbirth"
  }

  discharge_method
}


#discharge_destination
get_discharge_destination <- function(){

  ranNum <- runif(1, 0, 100)

  if(ranNum <= 96.12755){
    discharge_destination <- "Own Residence"
  }else if(ranNum <= 97.8723){
    discharge_destination <- "General Ward"
  }else if(ranNum <= 99){
    discharge_destination <- "Patient Died or Still Birth"
  # }else if(ranNum == 4){
  #   discharge_destination <- "Maternity Ward"
  # }else if(ranNum == 5){
  #   discharge_destination <- "Temporary Place of Residence"
  # }else if(ranNum == 6){
  #   discharge_destination <- "NHS run Care Home"
  # }else if(ranNum == 7){
  #   discharge_destination <- "Non-NHS run Hospital"
  # }else if(ranNum == 8){
  #   discharge_destination <- "Non-NHS run Care Home"
  # }else if(ranNum == 9){
  #   discharge_destination <- "NHS Hospital Provider"
  # }else if(ranNum == 10){
  #   discharge_destination <- "Psychiatric Accommodation"
  # }else if(ranNum == 11){
  #   discharge_destination <- "Penal Establishment"
  # }else if(ranNum == 12){
  #   discharge_destination <- "Mentally ill/Learning Disabilities Ward"
  # }else if(ranNum == 13){
  #   discharge_destination <- "Residential Accommodation"
  # }else if(ranNum == 14){
  #   discharge_destination <- "Medium Secure Unit"
  # }else if(ranNum == 15){
  #   discharge_destination <- "Court"
  # }else if(ranNum == 16){
  #   discharge_destination <- "Psychiatric Hospital, Scotland"
  # }else if(ranNum == 17){
  #   discharge_destination <- "Non-NHS run Hospice"
  # }else if(ranNum == 18){
  #   discharge_destination <- "Foster Care"
  }else{
    discharge_destination <- "NHS Hospital Provider"
  }

  discharge_destination
}


#patient_classification
get_patient_classification <- function(){

  ranNum <- runif(1, 0, 100)

  if(ranNum <= 67.940514457){
    patient_classification <- "Ordinary Admission"
  }else if(ranNum <= 93.34966){
    patient_classification <- "Day case admission"
  }else if(ranNum <= 99.99731){
    patient_classification <- "Regular day admission"
  }else{
    patient_classification <- "Regular night admission"
  }

  patient_classification
}


#main_speciality
get_main_specialty <- function(){

  ranNum <- runif(1, 0, 100)

  if(ranNum <= 11.94678){
    main_speciality <- "GENERAL MEDICINE"
  }else if(ranNum <= 25.35643){
    main_speciality <- "PAEDIATRICS"
  }else if(ranNum <= 33.07909){
    main_speciality <- "OBSTETRICS"
  }else if(ranNum <= 35.90671){
    main_speciality <- "GERIATRIC MEDICINE"
  }else if(ranNum <= 39.9172){
    main_speciality <- "DERMATOLOGY"
  }else if(ranNum <= 41.31686){
    main_speciality <- "CARDIOLOGY"
  }else if(ranNum <= 46.6733){
    main_speciality <- "TRAUMA & ORTHOPAEDICS"
  }else if(ranNum <= 53.97897){
    main_speciality <- "GENERAL SURGERY"
  }else if(ranNum <= 64.69563){
    main_speciality <- "GASTROENTEROLOGY"
  }else if(ranNum <= 74.40575){
    main_speciality <- "ACCIDENT & EMERGENCY"
  }else if(ranNum <= 74.98329){
    main_speciality <- "ANAESTHETICS"
  }else if(ranNum <= 77.40323){
    main_speciality <- "CLINICAL HAEMATOLOGY"
  }else if(ranNum <= 79.27994){
    main_speciality <- "UROLOGY"
  }else if(ranNum <= 81.87385){
    main_speciality <- "PLASTIC SURGERY"
  }else if(ranNum <= 84.42874){
    main_speciality <- "ENDOCRINOLOGY"
  }else if(ranNum <= 85.65412){
    main_speciality <- "PAEDIATRIC DENTISTRY"
  }else if(ranNum <= 90.08019){
    main_speciality <- "GYNAECOLOGY"
  }else if(ranNum <= 91.43656){
    main_speciality <- "ENT"
  }else{
    main_speciality <- "RESPIRATORY MEDICINE"
  }

  main_speciality
}


#diagnosis_code
get_diagnosis_code <- function(){

  ranNum <- runif(1, 0, 19)

  if(ranNum <= 1){
    diagnosis_code <- "A409"
  }else if(ranNum <= 2){
    diagnosis_code <-"A492"
  }else if(ranNum <= 3){
    diagnosis_code <- "B179"
  }else if(ranNum <= 4){
    diagnosis_code <- "D643"
  }else if(ranNum <= 5){
    diagnosis_code <- "D821"
  }else if(ranNum <= 6){
    diagnosis_code <- "F458"
  }else if(ranNum <= 7){
    diagnosis_code <- "E880"
  }else if(ranNum <= 8){
    diagnosis_code <- "G030"
  }else if(ranNum <= 9){
    diagnosis_code <- "F681"
  }else if(ranNum <= 10){
    diagnosis_code <- "G048"
  }else if(ranNum <= 11){
    diagnosis_code <- "E209"
  }else if(ranNum <= 12){
    diagnosis_code <- "F500"
  }else if(ranNum <= 13){
    diagnosis_code <- "F801"
  }else if(ranNum <= 14){
    diagnosis_code <- "D093"
  }else if(ranNum <= 15){
    diagnosis_code <- "C717"
  }else if(ranNum <= 16){
    diagnosis_code <- "D222"
  }else if(ranNum <= 17){
    diagnosis_code <- "F99X"
  }else if(ranNum <= 18){
    diagnosis_code <- "G052"
  }else{
    diagnosis_code <- "A041"
  }

  diagnosis_code
}


#ward_category
get_ward_category <- function(){

  ranNum <- runif(1, 0, 100)

  if(ranNum <= 10){
    ward_category <- "Ward 1"
  }else if(ranNum <= 20){
    ward_category <-"Ward 2"
  }else if(ranNum <= 30){
    ward_category <- "Ward 3"
  }else if(ranNum <= 40){
    ward_category <- "Ward 4"
  }else if(ranNum <= 50){
    ward_category <- "Ward 5"
  }else if(ranNum <= 60){
    ward_category <- "Ward 6"
  }else if(ranNum <= 70){
    ward_category <- "Ward 7"
  }else if(ranNum <= 80){
    ward_category <- "Ward 8"
  }else if(ranNum <= 90){
    ward_category <- "Ward 9"
  }else{
    ward_category <- "Ward 10"
  }

  ward_category
}


#consultant pseudo_id
get_consultant <- function(){

  ranNum <- runif(1, 0, 100)

  if(ranNum <= 10){
    consultant <- "Consultant 1"
  }else if(ranNum <= 20){
    consultant <-"Consultant 2"
  }else if(ranNum <= 30){
    consultant <- "Consultant 3"
  }else if(ranNum <= 40){
    consultant <- "Consultant 4"
  }else if(ranNum <= 50){
    consultant <- "Consultant 5"
  }else if(ranNum <= 60){
    consultant <- "Consultant 6"
  }else if(ranNum <= 70){
    consultant <- "Consultant 7"
  }else if(ranNum <= 80){
    consultant <- "Consultant 8"
  }else if(ranNum <= 90){
    consultant <- "Consultant 9"
  }else{
    consultant <- "Consultant 10"
  }

  consultant
}

