#' Standard format for inpatient data
#'
#' Datasets containing inpatient data, with each row corresponding to an episode
#' of care and columns corresponding to the columns of this dataset, can be
#' standardised to match this format via config files. Datasets which do not
#' have all of the columns listed can still be standardised but it may be that not
#' all analyses in the hospitalflow package can be performed.
#'
#' @format A data frame with up to 22 variables, depending on available hospital data.
#' Each observation corresponds to a single episode within a patient spell.
#'
#' \describe{
#'   \item{pseudo_id}{Unique ID number of the patient. Original mapping back to
#'   hospital number should be destroyed or adequately protected according to
#'   all relevant data governance legislation and protocols}
#'   \item{gender}{The recorded gender of the patient coded using the NHS data
#'   dictionary coding system (e.g. "Male","Female","Not Known","Not Specified")
#'   \url{https://www.datadictionary.nhs.uk/data_dictionary/attributes/p/person/person_gender_code_de.asp?shownav=1?query=gender&rank=80&shownav=1}}
#'   \item{age_band_start}{The age band that the patient was in at time of
#'   admission (e.g. "0 yrs", "1-4 yrs"..."95+")}
#'   \item{ethnic_category}{The ethnicity of the patient recorded using the NHS
#'   data dictionary coding system (e.g. "British", "Irish", "Bangladeshi"...)
#'   \url{http://www.datadictionary.nhs.uk/data_dictionary/attributes/e/end/ethnic_category_code_de.asp}}
#'   \item{start_datetime}{The datetime at which this episode began, as a
#'   POSIXct datetime}
#'   \item{end_datetime}{The datetime at which this episode ended, as a POSIXct
#'   datetime}
#'   \item{spell_number}{Optional index for the spell this episode is part of}
#'   \item{episode_number}{Optional index of this episode within its containing
#'   spell, ordered by start_datetime}
#'   \item{admission_method}{The admission method coded using the NHS data
#'   dictionary coding system (e.g. "Waiting list", "Booked", "Accident and
#'   Emergency"...)
#'   \url{https://www.datadictionary.nhs.uk/data_dictionary/attributes/a/add/admission_method_de.asp?shownav=1?query=admission+method&rank=100&shownav=1}}
#'   \item{source_of_admission}{The source of admission coded using the NHS data
#'   dictionary coding system (e.g. "Usual Place of Residence", "NHS other
#'   Hospital Provider", "NHS run Care Home"...)
#'   \url{https://www.datadictionary.nhs.uk/data_dictionary/attributes/s/smo/source_of_admission_de.asp?shownav=1?query=source+of+admission&rank=87.19749&shownav=1}}
#'   \item{discharge_method}{The discharge method coded using the NHS data
#'   dictionary coding system (e.g. "Patient died", "Discharged", "Stillbirth")
#'   \url{https://www.datadictionary.nhs.uk/data_dictionary/attributes/d/disc/discharge_method_de.asp?shownav=1?query=discharge+method&rank=100&shownav=1}}
#'   \item{discharge_destination}{The discharge destination coded using the NHS
#'   data dictionary coding system (e.g. "Own Residence", "Patient Died or Still
#'   Birth", "NHS Hospital Provider"...)
#'   \url{https://www.datadictionary.nhs.uk/data_dictionary/attributes/d/disc/discharge_destination_de.asp?shownav=1?query=discharge+destination&rank=100&shownav=1}}
#'   \item{patient_classification}{The patient classification coded using the
#'   NHS data dictionary coding system (e.g. "Ordinary Admission", "Day case
#'   admission", "Regular Day Admission"...)
#'   \url{https://www.datadictionary.nhs.uk/data_dictionary/attributes/p/pati/patient_classification_de.asp?shownav=1?query=patient+classification&rank=34.61654&shownav=1}}
#'   \item{provider}{The provider at which the episode took place (e.g. Lewisham
#'   and Greenwich Trust)}
#'   \item{hospital_site}{The hospital site at which the episode took place
#'   (e.g. Lewisham University Hospital)}
#'   \item{main_specialty}{The main speciality that the episode falls into coded
#'   using the NHS data (e.g. "TRAUMA & ORTHOPAEDICS", "ENT",
#'   "OPHTHALMOLOGY"...) dictionary coding system
#'   \url{https://www.datadictionary.nhs.uk/web_site_content/supporting_information/main_specialty_and_treatment_function_codes_table.asp?shownav=1?query=main+category&rank=100&shownav=1}}
#'   \item{local_subspecialty}{Local subspeciality that the episode falls into
#'   coded using the same data dictionary coding as Main Speciality}
#'   \item{ward_category}{The type of ward that the patient is on during the
#'   episode (e.g. Acute Medical Unit)}
#'   \item{ward_stay_number}{The number of wards that the patient has been on
#'   during their stay upto this specific episode}
#'   \item{consultant}{The name or pseudo-identifier of the consultant looking
#'   after the patient}
#'   \item{diagnosis_code}{A string formed by concatenating the ICD diagnosis
#'   codes for that patient stay in the format of a single column with an
#'   identifiable separator}
#'   \item{hrg}{Healthcare Resource Group code. Standard groupings of clinically
#'   similar treatments which use common levels of healthcare resource, and
#'   which are currently used as a means of determining fair and equitable
#'   reimbursement for care services}
#'
#'
#'
#'
#'
#'
#' }
"inpatient_data"
