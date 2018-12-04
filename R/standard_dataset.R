
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
  lgt_col_mapping <- tibble::tibble(dataCol = c('PAT_CODE', 'Admissions', 'Discharges', 'PatientType', 'Episode Number', 'WARD_CODE', 'Column1', 'SEX'),
                                    analysisCol = c('IDcol', 'Admissions', 'Discharges', 'PatientType', 'EpisodeNumber', 'WardCode', 'AGE_BAND', 'SEX'))
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
  oldnames <- colname_mapping %>% pull(dataCol)
  newnames <- colname_mapping %>% pull(analysisCol)
  data %>% rename_at(vars(oldnames), ~ newnames)
}
