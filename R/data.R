#' Episode data for Emergency Department
#'
#' A dataset containing data ... episodes for Emergency Department (ED)
#'
#'
#' @format A tibble containing observations of 16 variables. Each observation corresponds to an episode of
#' emergency department care.
#' \describe{
#'   \item{pseudo_id}{Variable type: character; Unique ID number of the patient/ Original mapping back to hospital number remains
#'   behind hospital/NHS Trust firewall}
#'   \item{start_datetime}{Variable type; as.POSIXct; The date that the patient attended the ED department for the spell at the hospital that
#'   this episode of care is part of}
#'   \item{end_datetime}{Variable type: as.POSIXct; The time the patient was discharged from ED department}
#'   \item{gender}{Variable type: factor; 3 levels: "Female", "Male", "Other"}
#'   \item{age_band_start}{Variable type: factor; 22 levels. Levels formed are within five years age band into which the patient
#'   falls at the start at the ED episode; e.g. 0, 1-4, 5-9 ... 90-94, 95+ ; Some hospitals may have different levels,
#'   depending on the size of the hospital}
#'   \item{facility}{Variable type: factor; levels are localy determined; organizational or geographical units that fulfill the role fo ED deparment}
#'   \item{ethnic_category}{Variable type: factor; The ethnicity of the patient recorded as per NHS data dictionary
#'   coding system using the initials for each ethicity; see link:
#'   \url{https://www.datadictionary.nhs.uk/data_dictionary/attributes/e/end/ethnic_category_code_de.asp}}
#'   \item{attendance_category}{Variable type: factor; levels: "First Attendance"; "Follow-up Planned"; "Follow-up unplanned"}
#'   \item{triage_category}{Variable type: factor; recorded as per NHS data dictionary coding system; see link:
#'   \url{https://www.datadictionary.nhs.uk/data_dictionary/attributes/a/a_and_e_initial_assessment_triage_category_de.asp?shownav=1}}
#'   \item{diagnosis_string}{Variable type: character; coding system as per NHS data dictionary; see link:
#'   \url{https://www.datadictionary.nhs.uk/web_site_content/supporting_information/clinical_coding/accident_and_emergency_diagnosis_tables.asp?shownav=1}}
#'   \item{hrg_string}{Variable type: character; string of HRG codes associated with this ED episode; see link:
#'   \url{https://www.gov.uk/government/publications/nhs-national-tariff-payment-system-201617}}
#'   \item{postcode_sector}{Variable type: character; The postcode of the patient's home, up to sector level}
#'   \item{arrival_mode}{Variable type: factor; The means of arrival of the patient at the ED department}
#'   \item{attendance_disposal}{Variable type: factor; Attendance disposal describes what happened at patient after Episode treatment;
#'   recorded as per NHS data dictionary, see link:
#'   \url{https://www.datadictionary.nhs.uk/data_dictionary/attributes/a/acc/accident_and_emergency_attendance_disposal_de.asp?shownav=1}}
#'   \item{referal_source}{Varaible type: factor; The source of referral; recorded as per NHS data dictionary coding system; see link:
#'   \url{https://www.datadictionary.nhs.uk/data_dictionary/attributes/s/smo/source_of_referral_for_a_and_e_de.asp?shownav=1}}
#'   \item{specialty}{Variable type: character; Specialty of the consultant.Recorded as per NHS data dictionary; see link:
#'   \url{https://www.datadictionary.nhs.uk/data_dictionary/attributes/m/main_specialty_code_de.asp}}
#'   \item{episode_id}{Variable type: interger; Unique identifier for the ED episode within this dataset, Equivalent to a
#'   database primary key for the table}
#'
#'
#'
#'   }
"ed_data_example"

