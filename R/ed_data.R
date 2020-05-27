#' Standard format for ED data
#'
#' Datasets containing ED data from any hospital with columns corresponding
#' to the columns of this dataset can be standardised to match this format
#' via config files. Original hospital datasets which do not have all of the
#' columns listed can still be standardised but may me that not all analyses
#' in the HospitalFlow package can be performed.
#'
#' @format A data frame with upto 14 variables depending on available hospital data.
#' Each observation corresponds to a single episode within a patient spell.
#'
#' \describe{
#'   \item{pseudo_id}{Unique ID number of the patient. Original mapping back to
#'   hospital number should remain behind the hospital's NHS firewall}
#'   \item{gender}{The recorded gender of the patient coded using the NHS data dictionary
#'   coding system (e.g. "Male","Female","Not Known","Not Specified")
#'   \url{https://www.datadictionary.nhs.uk/data_dictionary/attributes/p/person/person_gender_code_de.asp?shownav=1?query=gender&rank=80&shownav=1}}
#'   \item{age_band_start}{The age band that the patient was in at time of admission (e.g. "0 yrs", "1-4 yrs"..."95+")}
#'   \item{ethnic_category}{The ethnicity of the patient recorded using the NHS data
#'   dictionary coding system (e.g. "British", "Irish", "Bangladeshi"...)
#'   \url{http://www.datadictionary.nhs.uk/data_dictionary/attributes/e/end/ethnic_category_code_de.asp}}
#'   \item{attendance_category}{The attendance category coded using the NHS data
#'   dictionary coding system (e.g. "First Attendance", "Planned Follow-up Attendance", "Unplanned Follow-up Attendance")
#'   \url{https://www.datadictionary.nhs.uk/data_dictionary/attributes/a/a_and_e_attendance_category_de.asp?shownav=1?query=attendance&rank=25&shownav=1}}
#'   \item{arrival_mode}{The arrival mode coded using the NHS data
#'   dictionary coding system (e.g. "Brought in by Emergency Ambulance", "Other")
#'   \url{https://www.datadictionary.nhs.uk/data_dictionary/attributes/a/acc/accident_and_emergency_arrival_mode_de.asp?shownav=1?query=arrival+mode&rank=100&shownav=1}}
#'   \item{attendance_disposal}{The attendance disposal coded using the NHS data
#'   dictionary coding system (e.g. "A&E Clininc referral", "Admitted", "Discharged"...)
#'   \url{https://www.datadictionary.nhs.uk/data_dictionary/attributes/a/acc/accident_and_emergency_attendance_disposal_de.asp?shownav=1?query=attendance+disposal&rank=100&shownav=1}}
#'   \item{referral_source}{The referral source coded using the NHS data
#'   dictionary coding system (e.g. "General Medical Practitioner", "Self referral", "Emergency Services"...)
#'   \url{https://www.datadictionary.nhs.uk/data_dictionary/attributes/s/smo/source_of_referral_for_community_de.asp?shownav=1?query=referral+source&rank=29.45948&shownav=1}}
#'   \item{start_datetime}{The datetime at which this episode began}
#'   \item{end_datetime}{The datetime at which this episode ended}
#'   \item{triage_category}{The triage category coded using the NHS data
#'   dictionary coding system (e.g. "Non-urgent","Standard","Urgent"...)
#'   \url{https://www.datadictionary.nhs.uk/data_dictionary/attributes/a/a_and_e_initial_assessment_triage_category_de.asp?shownav=1?query=triage+category&rank=100&shownav=1}}
#'   \item{initial_assess_datetime}{The datetime at which the initial assessment was performed}
#'   \item{treatment_datetime}{The datetime at which the treatment was performed}
#'   \item{hrg_code}{Healthcare Resource Group code which refers to standard groupings of clinically similar treatments which use common levels of healthcare resource which are currently used as a means of determining fair and equitable reimbursement for care services}
#'
#'
#'
#'
#' }
"ed_data"
