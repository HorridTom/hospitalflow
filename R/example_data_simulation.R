
#function to get simulated ed data
get_simulated_ed_data <- function(npat = 1000, start = as.POSIXct("2019-01-01 00:00:00"),
                                              end = as.POSIXct("2019-04-01 00:00:00")) {
  simulated_pat_data <- tibble::tibble(pseudo_id = 1:npat,
                                        gender = factor(sample(c("M","F"), npat, replace = TRUE))
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
    mutate(referral_cource = get_referral_source()) %>%
    mutate(initial_assess_datetime = as.POSIXct(generate_initial_assess(start_datetime = start_datetime,
                                                             end_datetime = end_datetime))) %>%
    mutate(treatment_datetime = as.POSIXct(generate_treatment_time(start_datetime = start_datetime,
                                                             end_datetime = end_datetime)))

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
    tibble::tibble(pseudo_id = rep(pseudo_id, num_admissions),
                   start_datetime = as.POSIXct(random_datetimes(n = num_admissions, start = start, end = end)),
                   end_datetime = as.POSIXct(start_datetime +
                     as.difftime(rexp(num_admissions, rate = los_rate), units = "secs"))
    )

  }

  patient_data <- patient_data %>% dplyr::mutate(admissions =
                                                   purrr::pmap(., function(...) generate_admissions(...)))

  list(patients = patient_data %>% dplyr::select(-admissions, -num_admissions),
       admissions = patient_data %>% tidyr::unnest() %>% dplyr::select(-num_admissions))
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




