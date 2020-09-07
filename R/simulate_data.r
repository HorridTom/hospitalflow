#simulate example ED data frame

#pseudo_id
pseudo_id <- round(runif(n = 1000, min = 0, max = 800))

#gender
gender_fun <- function(n){
  if(n <= 450){
    n <- "female"
  }else if(n > 450 & n <= 900){
    n <- "male"
  }else if(n > 900 & n<= 950){
    n <- "Not Known"
  }else{
    n <- "Not Specified"
  }
}
gender <- sapply(round(runif(n = 1000, min = 1, max = 1000)), gender_fun)

#age_band_start
age_fun <- function(n){
  if(n == 0){
    n <- "0 yrs"
  }else if(n == 1){
    n <- "1-4 yrs"
  }else if(n == 2){
    n <- "5-9 yrs"
  }else if(n == 3){
    n <- "10-14 yrs"
  }else if(n == 4){
    n <- "15-19 yrs"
  }else if(n == 5){
    n <- "20-24 yrs"
  }else if(n == 6){
    n <- "25-29 yrs"
  }else if(n == 7){
    n <- "30-34 yrs"
  }else if(n == 8){
    n <- "35-39 yrs"
  }else if(n == 9){
    n <- "40-44 yrs"
  }else if(n == 10){
    n <- "45-49 yrs"
  }else if(n == 11){
    n <- "50-54 yrs"
  }else if(n == 12){
    n <- "55-59 yrs"
  }else if(n == 13){
    n <- "60-64 yrs"
  }else if(n == 14){
    n <- "65-69 yrs"
  }else if(n == 15){
    n <- "70-74 yrs"
  }else if(n == 16){
    n <- "65-79 yrs"
  }else if(n == 17){
    n <- "80-84 yrs"
  }else if(n == 18){
    n <- "10-14 yrs"
  }else if(n == 19){
    n <- "85-89 yrs"
  }else if(n == 20){
    n <- "90-94 yrs"
  }else{
    n <- "95+"
  }
}
age_band_start <- sapply(round(rnorm(n = 1000, mean = 12, sd = 5)), age_fun)

#ethnic_category
ethnic_cat_fun <- function(n){
  if(n == 0){
    n <- "White - British"
  }else if(n == 1){
    n <- "White - Irish"
  }else if(n == 2){
    n <- "White - Any other White background"
  }else if(n == 3){
    n <- "Mixed - White and Black Caribbean"
  }else if(n == 4){
    n <- "Mixed - White and Black African"
  }else if(n == 5){
    n <- "Mixed - White and Asian"
  }else if(n == 6){
    n <- "Mixed - Any other mixed background"
  }else if(n == 7){
    n <- "Asian or Asian - British Indian"
  }else if(n == 8){
    n <- "Asian or Asian - Pakistani"
  }else if(n == 9){
    n <- "Asian or Asian - Bangladeshi"
  }else if(n == 10){
    n <- "Asian or Asian - Any other Asian background"
  }else if(n == 11){
    n <- "Black or Black - British Carribean"
  }else if(n == 12){
    n <- "Black or Black - British African"
  }else if(n == 13){
    n <- "Black or Black British - Any other Black background"
  }else if(n == 14){
    n <- "Other Ethnic Groups - Chinese"
  }else if(n == 15){
    n <- "Other Ethnic Groups - Any other ethnic group"
  }else if(n == 16){
    n <- "Not stated"
  }else{
    n <- NA
  }
}
ethnic_category <- sapply(round(runif(n = 1000, min = 0, max = 16)), ethnic_cat_fun)


#attendance_category
att_cat_fun <- function(n){
  if(n <= 4){
    n <- "First Attendance"
  }else if(n == 5){
    n <- "Planned Follow-up Attendance"
  }else if(n == 6){
    n <- "Unplanned Follow-up Attendance"
  }else{
    n <- NA
  }
}
attendance_category <- sapply(round(runif(n = 1000, min = 0, max = 6)), att_cat_fun)

#arrival_mode
arrival_mode_fun <- function(n){
  if(n <= 9){
    n <- "Other"
  }else if(n == 10){
    n <- "Brought in by Emergency Ambulance"
  }else{
    n <- NA
  }
}
arrival_mode <- sapply(round(runif(n = 1000, min = 0, max = 10)), arrival_mode_fun)


#attendance_disposal
attendance_disposal_fun <- function(n){
  if(n <= 20){
    n <- "Admitted"
  }else if(n > 20 & n <= 81){
    n <- "Discharged"
  }else if(n == 82){
    n <- "Died in Department"
  }else if(n == 83){
    n <- "A&E Clinic referral"
  }else if(n == 84){
    n <- "Referred to Fracture Clinic"
  }else if(n == 85){
    n <- "Left Department before being seen for treatment"
  }else if(n == 86){
    n <- "Left Department having refused treatment"
  }else if(n == 87){
    n <- "Left Dept without completing treatment"
  }else if(n == 88){
    n <- "Referred to Out-Patient Clinic"
  }else if(n == 89){
    n <- "Patient removed from dept"
  }else if(n == 90){
    n <- "Referred to A Clinic"
  }else if(n == 91){
    n <- "Referred to Other Health Care Professional"
  }else if(n == 92){
    n <- "Transferred to another provider"
  }else if(n == 93){
    n <- "Transferred to Burns Unit"
  }else if(n == 94){
    n <- "Other"
  }else{
    n <- NA
  }
}
attendance_disposal <- sapply(round(runif(n = 1000, min = 0, max = 94)), attendance_disposal_fun)

#referral_source
referral_source_fun <- function(n){
  if(n <= 6){
    n <- "Self referral"
  }else if(n == 7){
    n <- "General Medical Practitioner"
  }else if(n == 8){
    n <- "Local Authority Social Services"
  }else if(n == 9){
    n <- "Emergency services"
  }else if(n == 10){
    n <- "Work"
  }else if(n == 11){
    n <- "Educational Establishment"
  }else if(n == 12){
    n <- "Police"
  }else if(n == 13){
    n <- "Health Care Provider: same or other"
  }else if(n == 14){
    n <- "General Dental Practitioner"
  }else if(n == 15){
    n <- "Other"
  }else{
    n <- NA
  }
}
referral_source <- sapply(round(runif(n = 1000, min = 0, max = 15)), referral_source_fun)


#triage_category
triage_category_fun <- function(n){
  if(n <= 3){
    n <- "Standard"
  }else if(n == 4){
    n <- "Very Urgent"
  }else if(n == 5){
    n <- "Urgent"
  }else if(n == 6){
    n <- "Immediate Resuscitation"
  }else if(n >= 7){
    n <- "Non-urgent"
  }else{
    n <- NA
  }
}
triage_category <- sapply(round(runif(n = 1000, min = 0, max = 9)), triage_category_fun)


#start_datetime
#start_datetime <- sample(seq(as.POSIXct('2015-01-01 00:00:00'), as.POSIXct('2015-04-01 00:00:00'), by="day"), 12)
start_datetime <- musicnotationR::random_datetime(1000, st = '2015-01-01 00:00:00', et = '2015-04-01 00:00:00')

#end_datetime
Los <- lubridate::minutes(abs(round(rnorm(n = 1000, mean = 420, sd = 360))))
#IP_data_sim <- dplyr::mutate(IP_data_sim, end_datetime = start_datetime + Los)

#initial_assess_datetime
wait <- lubridate::minutes(abs(round(rnorm(n = 1000, mean = 30, sd = 10))))
#IP_data_sim <- dplyr::mutate(IP_data_sim, initial_assess_datetime = as.POSIXct(start_datetime + wait))

#treatment_datetime
waitTreat <- lubridate::minutes(abs(round(rnorm(n = 1000, mean = 240, sd = 60))))
#IP_data_sim <- dplyr::mutate(IP_data_sim, treatment_datetime = as.POSIXct(start_datetime + waitTreat))

#hrg_code
hrg_code_fun <- function(n){
  if(n == 1){
    n <- "AA123"
  }else if(n == 2){
    n <- "BB567"
  }else if(n == 3){
    n <- "CC890"
  }else if(n == 4){
    n <- "DD345"
  }else if(n == 5){
    n <- "EE678"
  }else{
    n <- NA
  }
}
hrg_code <- sapply(round(runif(n = 1000, min = 0, max = 5)), hrg_code_fun)

#site
site_fun <- function(n){
  if(n == 0){
    n <- "SITE1"
  }else{
    n <- "SITE2"
  }
}
site <- sapply(round(runif(n = 1000, min = 0, max = 1)), site_fun)


#admission_method
admission_method_fun <- function(n){
  if(n <= 10){
    n <- "Accident and Emergency Department"
  }else if(n == 11){
    n <- "Booked"
  }else if(n == 12){
    n <- "Planned"
  }else if(n == 13){
    n <- "Accident and emergency"
  }else if(n == 14){
    n <- "General Practitioner"
  }else if(n == 15){
    n <- "Bed bureau"
  }else if(n == 16){
    n <- "Consultant Clinic"
  }else if(n == 17){
    n <- "Mental Health Crisis Resolution Team"
  }else if(n == 18){
    n <- "Waiting list"
  }else if(n == 19){
    n <- "Transfer from another Hospital Provider"
  }else if(n == 20){
    n <- "Intended home birth"
  }else if(n == 21){
    n <- "Other emergency admission"
  }else if(n == 22){
    n <- "Other means"
  }else if(n == 23){
    n <- "Admitted ante-partum"
  }else if(n == 24){
    n <- "Other"
  }else if(n == 25){
    n <- "Admitted post-partum"
  }else if(n == 26){
    n <- "Birth-this provider"
  }else if(n == 27){
    n <- "Birth-outside provider(not intended home)"
  }else if(n == 28){
    n <- "Transfer from other provider(non-emergency)"
  }else{
    n <- NA
  }
}
admission_method <- sapply(round(runif(n = 1000, min = 0, max = 94)), admission_method_fun)


#source_of_admission
source_of_admission_fun <- function(n){
  if(n == 1){
    n <- "Babies born in or on the way to hospital"
  }else if(n == 2){
    n <- "Usual Place of Residence"
  }else if(n == 3){
    n <- "NHS other Hospital Provider"
  }else if(n == 4){
    n <- "Temporary Place of Residence"
  }else if(n == 5){
    n <- "Non-NHS run Hospice"
  }else if(n == 6){
    n <- "Penal Establishment"
  }else if(n == 7){
    n <- "NHS run Care Home"
  }else if(n == 8){
    n <- "Non-NHS run Care Home"
  }else if(n == 9){
    n <- "Local Authority Foster Care"
  }else{
    n <- NA
  }
}
source_of_admission <- sapply(round(runif(n = 1000, min = 0, max = 15)), source_of_admission_fun)

#discharge_method
discharge_method_fun <- function(n){
  if(n <= 20){
    n <- "Discharged"
  }else if(n == 21){
    n <- "Patient died"
  }else if(n == 22){
    n <- "Stillbirth"
  }else{
    n <- NA
  }
}
discharge_method <- sapply(round(runif(n = 1000, min = 0, max = 25)), discharge_method_fun)


#discharge_destination
discharge_destination_fun <- function(n){
  if(n == 1){
    n <- "Own Residence"
  }else if(n == 2){
    n <- "Patient Died or Still Birth"
  }else if(n == 3){
    n <- "NHS Hospital Provider"
  }else if(n == 4){
    n <- "Maternity Ward"
  }else if(n == 5){
    n <- "Temporary Place of Residence"
  }else if(n == 6){
    n <- "NHS run Care Home"
  }else if(n == 7){
    n <- "Non-NHS run Hospital"
  }else if(n == 8){
    n <- "Non-NHS run Care Home"
  }else if(n == 9){
    n <- "General Ward"
  }else if(n == 10){
    n <- "Psychiatric Accommodation"
  }else if(n == 11){
    n <- "Penal Establishment"
  }else if(n == 12){
    n <- "Mentally ill/Learning Disabilities Ward"
  }else if(n == 13){
    n <- "Residential Accommodation"
  }else if(n == 14){
    n <- "Medium Secure Unit"
  }else if(n == 15){
    n <- "Court"
  }else if(n == 16){
    n <- "Psychiatric Hospital, Scotland"
  }else if(n == 17){
    n <- "Non-NHS run Hospice"
  }else if(n == 18){
    n <- "Foster Care"
  }else{
    n <- NA
  }
}
discharge_destination <- sapply(round(runif(n = 1000, min = 0, max = 20)), discharge_destination_fun)


#patient_classification
patient_classification_fun <- function(n){
  if(n <= 20){
    n <- "Ordinary Admission"
  }else if(n == 21){
    n <- "Day case admission"
  }else if(n == 22){
    n <- "Regular day admission"
  }else if(n == 23){
    n <- "Regular night admission"
  }else{
    n <- NA
  }
}
patient_classification <- sapply(round(runif(n = 1000, min = 0, max = 25)), patient_classification_fun)


#main_speciality
main_speciality_fun <- function(n){
  if(n <= 20){
    n <- "GENERAL MEDICINE"
  }else if(n == 21){
    n <- "ENT"
  }else if(n == 22){
    n <- "OPHTHALMOLOGY"
  }else if(n == 23){
    n <- "ORAL SURGERY"
  }else if(n == 24){
    n <- "PERIODONTICS"
  }else if(n == 25){
    n <- "PROSTHODONTICS"
  }else if(n == 26){
    n <- "NEUROSURGERY"
  }else if(n == 27){
    n <- "PLASTIC SURGERY"
  }else if(n == 28){
    n <- "CARDIOTHORACIC SURGERY"
  }else if(n == 29){
    n <- "ACCIDENT & EMERGENCY"
  }else if(n == 30){
    n <- "ANAESTHETICS"
  }else if(n == 31){
    n <- "CRITICAL CARE MEDICINE"
  }else if(n == 32){
    n <- "GASTROENTEROLOGY"
  }else if(n == 33){
    n <- "TRAUMA & ORTHOPAEDICS"
  }else if(n == 34){
    n <- "ENDOCRINOLOGY"
  }else if(n == 35){
    n <- "IMMUNOPATHOLOGY"
  }else if(n == 36){
    n <- "MEDICAL VIROLOGY"
  }else{
    n <- NA
  }
}
main_speciality <- sapply(round(runif(n = 1000, min = 0, max = 40)), main_speciality_fun)


