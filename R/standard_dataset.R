
scratch_data_standardis <- function(data) {

  #set.seed(22)

  #test_data <- sample_n(data[c("PAT_CODE", "WARD_CODE", "START_DATETIME", "END_DATETIME",
  # "episode.order", "spell.type", "AGE_BAND", "SEX")], size = 30000, replace = FALSE)

  #subseting data set#
  #colname_ct <- function(data, colname){
  #data[,colname] = as.POSIXct(data[,colname])
  #data
  #}


  #df_date_ct <- test_data %>%
  #colname_ct("START_DATETIME") %>%
  #colname_ct("END_DATETIME")

  #df_rename <- df_date_ct %>%
  #dplyr::select(PAT_CODE, START_DATETIME, END_DATETIME, WARD_CODE, episode.order, spell.type, AGE_BAND, SEX) %>%
  #dplyr::rename(IDcol = PAT_CODE, Admissions = START_DATETIME,
  # Discharges = END_DATETIME, EpisodeNumber = episode.order, PatientType = spell.type,
  #WardCode = WARD_CODE, PatientType = spell.type)

  #start_date <- as.POSIXct("2015-01-01 00:00:00", tz = "Europe/London")
  #end_date <- as.POSIXct( "2015-01-31 00:00:00", tz = "Europe/London")
  #df_date_ct <- df_rename %>%
  # colname_ct("Admissions") %>%
  # colname_ct("Discharges")

  #start_date <- as.POSIXct("2015-01-01 00:00:00", tz = "Europe/London")
  #end_date <- as.POSIXct( "2016-01-31 00:00:00", tz = "Europe/London")
}


#' lgt_col_mapping
#'
#' @return the mapping table as a tibble
#' @export
#'
#' @examples
#' lgt_col_mapping()
#'
lgt_col_mapping <- function() {
  lgt_col_mapping <- tibble::tibble(provided = c('PAT_CODE', 'START_DATETIME', 'END_DATETIME', 'SEX', 'agegroup'),
                                    standard = c('pseudo_id', 'start_datetime', 'end_datetime', 'gender', 'age_band_start'))
  lgt_col_mapping
}


#' make_adm_disch_cols
#'
#' @param data episode data, including columns called
#' 'Spell Number', 'START_DATETIME', 'END_DATETIME' and 'Admission.Type'
#'
#' @return data, with columns for admission and discharge datetime, and spell-level admission type (PatientType), added
#' @export
#'
#' @examples
#' \dontrun{
#' new_dataset <- make_adm_disch_cols(dataset)
#' }
make_adm_disch_cols <- function(data) {
  spell_data <- data %>% group_by(`Spell Number`) %>%
    summarise(Admissions = min(START_DATETIME),
              Discharges = max(END_DATETIME),
              PatientType = spell_type(Admission.Type))
  data %>% left_join(spell_data, by = 'Spell Number')
}


#' make_adm_type_col
#'
#' @param data episode data including ADMISSION_FROM_CODE column
#' @param adm_method_mapping mapping from Admission Method to type of admission. e.g. 11 = Elective.
#' Must have columns Admission.Method and Admission.Type, Admission.Method values should be distinct i.e.
#' each value of Admission.Method should have one and only one corresponding Admission.Type.
#'
#' @return data, with additional column Admission.Type mapped from ADMISSION_FROM_CODE using adm_method_mapping
#' @export
#'
#' @examples
#' \dontrun{
#' new_dataset <- make_adm_type_col(dataset, adm_method_mapping = mapping)
#' }
make_adm_type_col <- function(data, adm_method_mapping) {
  data <- data %>% left_join(adm_method_mapping, by = c("ADMISSION_FROM_CODE" = "Admission.Method"))

}

#' spell_type
#'
#' @param spell_episode_types a vector containing the episode types for episodes
#' within one spell in hospital.
#'
#' @return the type of the spell - if all episode types are equal, this unique type is the
#' type of the spell, otherwise the type of the spell is 'Mixed'.
#' @export
#'
spell_type <- function(spell_episode_types) {
  if(length(spell_episode_types)==0) {
    spell_typ <- NA
  } else {
    spell_typ <- if_else(
      length(unique(spell_episode_types)) == 1,
      as.character(spell_episode_types[1]),
      "Mixed"
    )
  }
  spell_typ
}

#' standardise_colum_names
#'
#' @param data episode data whose column names are to be standardised
#' @param colname_mapping a dataframe with two columns, dataCol - the old names,
#' and analysisCol - their new equivalents
#'
#' @return data, with column names standardised according to colname_mapping
#' @export
#'
#' @examples
#' \dontrun{
#' standardise_column_names(data = dataset, colname_mapping = lgt_col_mapping())
#' }
standardise_column_names <- function(data, colname_mapping) {
  oldnames <- colname_mapping %>% dplyr::pull(provided)
  newnames <- colname_mapping %>% dplyr::pull(standard)
  data %>% dplyr::rename_at(dplyr::vars(tidyselect::all_of(oldnames)), ~ newnames) # rename_at superseded by rename_with + select. Try to avoid triple exclamation marks. Also, tidyselect is installed!
}

# Process used to put LGT A&E data into C&W format 2018-12-17
# lgt_ae_age_gender_data <- lgt_ae %>% select(SEX, agegroup, START_DATETIME, END_DATETIME)
# lgt_ae_age_gender_data$Ward <- rep('A&E',nrow(lgt_ae_age_gender_data))
# lgt_ae_age_gender_data$LastWard <- rep('ED only',nrow(lgt_ae_age_gender_data))
# lgt_ae_age_gender_data$SEX <- recode(lgt_ae_age_gender_data$SEX, F = "Female", M = "Male", N = "Not Specified", U = "Not Specified")
# lgt_ae_age_gender_data <- lgt_ae_age_gender_data %>% rename(Gender = SEX)
# lgt_ae_age_gender_data <- lgt_ae_age_gender_data %>% rename(Age_band = agegroup)
# lgt_ae_age_gender_data <- lgt_ae_age_gender_data %>% mutate(START_DATETIME = as.POSIXct(START_DATETIME, tz = "Europe/London", format = '%d/%m/%Y %H:%M'))
# lgt_ae_age_gender_data <- lgt_ae_age_gender_data %>% mutate(END_DATETIME = as.POSIXct(END_DATETIME, tz = "Europe/London", format = '%d/%m/%Y %H:%M'))
# lgt_ae_a_g_data_Q22018 <- lgt_ae_age_gender_data %>% filter(START_DATETIME <= as.Date('2018-07-31', tz = "Europe/London"), START_DATETIME >= as.Date('2018-04-01', tz = "Europe_London"))
# lgt_ae_a_g_data_Q22018$EpisodeNumber <- rep(1,nrow(lgt_ae_a_g_data_Q22018))
# lgt_ae_a_g_data_Q22018$PatientType <- rep('Emergency',nrow(lgt_ae_a_g_data_Q22018))
# lgt_ae_a_g_data_Q22018_fact <- lgt_ae_a_g_data_Q22018 %>% mutate(Gender = as.factor(Gender), Age_band = as.factor(Age_band), Ward = as.factor(Ward), LastWard = as.factor(LastWard), PatientType = as.factor(PatientType))


#' spell_intersections
#'
#' @param data1 a dataset with a column called 'Spell Number'
#' @param data2 a dataset with a column called 'Spell Number'
#'
#' @return list of three vectors giving distinct values of spell number present in both, and only in each,
#' of data1 and data2
#' @export
#'
spell_intersections <- function(data1, data2) {
  spell_nos1 <- data1 %>% distinct(`Spell Number`)
  spell_nos2 <- data2 %>% distinct(`Spell Number`)
  spell_nos1 <- spell_nos1 %>% mutate(inData2 = `Spell Number` %in% (spell_nos2 %>% pull(`Spell Number`)))
  spell_nos2 <- spell_nos2 %>% mutate(inData1 = `Spell Number` %in% (spell_nos1 %>% pull(`Spell Number`)))
  in_1_only <- spell_nos1 %>% filter(inData2 == FALSE) %>% pull(`Spell Number`)
  in_2_only <- spell_nos2 %>% filter(inData1 == FALSE) %>% pull(`Spell Number`)
  in_both <- spell_nos1 %>% filter(inData2 == TRUE) %>% pull(`Spell Number`)
  return(list('intersect'=in_both, '1 only'=in_1_only, '2 only'=in_2_only))
}
