#' Simulated spell table corresponding to ed_data and inpatient_data.
#'
#' A dataset containing the output of make_spell_table() and add_spell_vars() when applied to ed_data and inpatient_data.
#'
#' @format A data frame with 32 variables. Each observation corresponds to an entire patient spell made up of their
#' episodes from the corresponding ED and inpatient tables.
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
#'   \item{spell_start}{The datetime at which the first episode in this spell
#'   began, as a POSIXct datetime}
#'   \item{spell_end}{The datetime at which the last episode in this spell ended,
#'    as a POSIXct datetime}
#'   \item{number_of_episodes}{Total number of episode that make up this spell}
#'   \item{constituent_ed_episodes}{list of the episode IDs of the episodes which
#'   took place in ED for this spell}
#'   \item{constituent_ip_episodes}{list of the episode IDs of the inpatient episodes
#'   for this spell}
#'   \item{episode_class_sequence}{A string made up of "E"s and "I"s denoting the sequence
#'    of ED and Inpatient episodes within the spell respectively}
#'   \item{spell_number}{Distinct ID number for each spell}
#'   \item{admission_method_type}{The type of admission if applicable (e.g Maternity
#'   Admission, Emergency Admission, Elective Admission)}
#'   \item{initial_ed_end_datetime}{The end datetime of the first ED episode in the
#'   spell, if the spell started with an ED episode}
#'   \item{disposal_code}{Code denoting how the spell ended e.g.(Discharged, Transferred
#'   to other Health Care Provider, Admitted, ...)}
#'   \item{hrg_ae_code}{Healthcare Resource Group code. Standard groupings of
#'   clinically similar treatments which use common levels of healthcare
#'   resource, and which are currently used as a means of determining fair and
#'   equitable reimbursement for care services}
#'   \item{source_referral_ae}{The referral source to ED, if applicable, coded using
#'   the NHS data dictionary coding system (e.g. "General Medical Practitioner", "Self
#'   referral", "Emergency Services", ...)}
#'   \item{died_ip}{A boolean of whether the patient died during the spell}
#'   \item{starts_with_ed}{A boolean of whether the spell starts with an ED episode}
#'   \item{ed_non_adm}{A boolean of whether the spell was entirely in ED with no admission}
#'   \item{ed_comp_non_adm}{A boolean of whether the spell is a made up of more than one
#'    ED episode with no admission}
#'   \item{ed_admission}{A boolean of whether the spell involves an admission from ED}
#'   \item{ed_comp_adm}{A boolean of whether the spell is a composite ED admission.
#'   I.e. The spell starts with an ED episode followed by any number of inpatient episodes. }
#'   \item{direct_comp_adm}{A boolean of whether the spell is made up of any number of
#'   inpatient episodes that did not start in ED}
#'   \item{direct_admission}{A boolean of whether the spell involves a direct admission}
#'   \item{spell_class_col}{The type of admission/non-admission was involved, derived from
#'   the booleans in the previous columns}
#'   \item{main_specialty_start}{The main speciality of the first ward that the spell
#'   started on}
#'   \item{diagnosis_codes}{String of diagnoses codes from the episodes in the spell
#'   separated by "#"}
#'   \item{discharge_destination}{The discharge destination coded using the NHS
#'   data dictionary coding system (e.g. "Own Residence", "Patient Died or Still
#'   Birth", "NHS Hospital Provider"...)}
#'   \item{prev_disch}{The datetime of the most recent previous discharge if applicable}
#'   \item{prev_disch_dest}{The discharge destination of the previous discharge (e.g. Own Residence)}
#'   \item{prev_disch_diagnoses}{A string of diagnoses from previous spell relating to
#'   that patient, separted by "#"}
#'   \item{all_prev_diagnoses}{A string of all previous diagnoses from all spells
#'   relating to that patient, separted by "#"}
#'   \item{days_since_prev_disch}{Number of days since the previous discharge}
#'
#'
#'
#'
#'
#'
#' }
"spell_table"
