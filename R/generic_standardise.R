
#' recode_factors
#'
#' @param provided_data a tibble containing the imported dataset included all standard columns parsed
#' as the correct type
#' @param config_path path to the directory containing the config files for this dataset
#'
#' @return copy of provided_data with factor variables recoded to standard levels as
#' per the config files
#' @export
#'
#' @examples
recode_factors <- function(provided_data, config_path) {
  # TO BE IMPLEMENTED!

}

#' get_import_col_types
#'
#' @param config_path path to the config files for a given dataset
#'
#' @return a column specification to be used for parsing the dataset, as a named vector of
#' expressions. The expressions are readr collectors, and the names specify the columns they
#' will parse.
#' @export
#'
#' @examples
get_import_col_types <- function(config_path) {

  # Specify the standard ordering of the standard column names
  ordered_standard_column_names <- tibble::tibble(standard = c(
    "pseudo_id",
    "gender",
    "age_band_start",
    "ethnic_category",
    "attendance_category",
    "arrival_mode",
    "attendance_disposal",
    "triage_category",
    "referral_source",
    "start_datetime",
    "end_datetime"

  ))

  # Read in the mapping from provided column names to standard column names...
  column_mapping <- readRDS(file.path(config_path, "column_mapping.rds"))
  # ...and make a vector of the provided names ordered as per the standard order above
  colImportNames <- ordered_standard_column_names %>%
    dplyr::left_join(column_mapping, by="standard") %>%
    dplyr::pull(provided)

  # Set up the provided levels of any factor variables
  gender_levels <- readRDS(file.path(config_path, "gender_levels.rds"))
  provided_gender_levels <- gender_levels %>% dplyr::pull(provided)

  age_band_start_levels <- readRDS(file.path(config_path, "age_band_levels.rds"))
  provided_age_band_start_levels <- age_band_start_levels %>% dplyr::pull(provided)

  ethnic_category_levels <- readRDS(file.path(config_path, "ethnic_category_levels.rds"))
  provided_ethnic_category_levels <- ethnic_category_levels %>% dplyr::pull(provided)

  attendance_category_levels <- readRDS(file.path(config_path, "attendance_category_levels.rds"))
  provided_attendance_category_levels <- attendance_category_levels %>% dplyr::pull(provided)

  arrival_mode_levels <- readRDS(file.path(config_path, "arrival_mode_levels.rds"))
  provided_arrival_mode_levels <- arrival_mode_levels %>% dplyr::pull(provided)

  attendance_disposal_levels <- readRDS(file.path(config_path, "attendance_disposal_levels.rds"))
  provided_attendance_disposal_levels <- attendance_disposal_levels %>% dplyr::pull(provided)

  triage_category_levels <- readRDS(file.path(config_path, "triage_category_levels.rds"))
  provided_triage_category_levels <- triage_category_levels %>% dplyr::pull(provided)

  referral_source_levels <- readRDS(file.path(config_path, "referral_source_levels.rds"))
  provided_referral_source_levels <- referral_source_levels %>% dplyr::pull(provided)

  # Set up datetime formats
  datetime_formats <- readRDS(file.path(config_path, "datetime_formats.rds"))

  # extract the format of the provided start_datetime data
  # from the config files
  start_datetime_provided_colname <- column_mapping %>%
    dplyr::filter(standard == "start_datetime") %>%
    dplyr::pull(provided)
  start_datetime_provided_format <- datetime_formats %>%
    dplyr::filter(column_name == start_datetime_provided_colname) %>%
    dplyr::pull(datetime_format)

  # extract the format of the provided end_datetime data
  # from the config files
  end_datetime_provided_colname <- column_mapping %>%
    dplyr::filter(standard == "end_datetime") %>%
    dplyr::pull(provided)
  end_datetime_provided_format <- datetime_formats %>%
    dplyr::filter(column_name == end_datetime_provided_colname) %>%
    dplyr::pull(datetime_format)


  # Set up the column types, in the standard order as per above
  # NOTE: the order of column types here must be precisely
  # as per the standard order specified above
  colImportTypes <- rlang::exprs(
    readr::col_character(), #pseudo_id
    readr::col_factor(levels = !!eval(rlang::expr(provided_gender_levels))), # gender
    readr::col_factor(levels = !!eval(rlang::expr(provided_age_band_start_levels))), # age_band_start
    readr::col_factor(levels = !!eval(rlang::expr(provided_ethnic_category_levels))), #ethnic_category
    readr::col_factor(levels = !!eval(rlang::expr(provided_attendance_category_levels))), # attendance_category
    readr::col_factor(levels = !!eval(rlang::expr(provided_arrival_mode_levels))), #arrival_mode_levels
    readr::col_factor(levels = !!eval(rlang::expr(provided_attendance_disposal_levels))),  #attendance_disposal_levels
    readr::col_factor(levels = !!eval(rlang::expr(provided_triage_category_levels))), # triage_category
    readr::col_factor(levels = !!eval(rlang::expr(provided_referral_source_levels))), #referal_source
    readr::col_datetime(format = !!eval(rlang::expr(start_datetime_provided_format))), #start_datetime
    readr::col_datetime(format = !!eval(rlang::expr(end_datetime_provided_format))) #end_datetime
    )

  # Label the types with the respective column names and return this as a named
  # vector
  colImportTypes <- rlang::set_names(colImportTypes, colImportNames)
  colImportTypes
}

#' get_import_col_types_inpatient
#'
#' @param config_path path to the config files for a given dataset
#'
#' @return a column specification to be used for parsing the dataset, as a named vector of
#' expressions. The expressions are readr collectors, and the names specify the columns they
#' will parse.
#' @export
#'
#' @examples
get_import_col_types_inpatient <- function(config_path) {

  # Specify the standard ordering of the standard column names
  ordered_standard_column_names <- tibble::tibble(standard = c(
    "pseudo_id",
    "gender",
    "age_band_start",
    "ethnic_category",
    "main_specialty",
    "discharge_method",
    "discharge_destination",
    "admission_method",
    "source_of_admission",
    "patient_classification",
    "hospital_site",
    #"start_datetime",
    "end_datetime",
    "spell_number",
    "hrg",
    "consultant",
    "local_subspecialty",
    "ward_stay_number",
    "episode_number",
    "provider",
    "diagnosis_code",
    "diagnosis"

  ))

  # Read in the mapping from provided column names to standard column names...
  column_mapping <- readRDS(file.path(config_path, "column_mapping.rds"))
  # ...and make a vector of the provided names ordered as per the standard order above
  colImportNames <- ordered_standard_column_names %>%
    dplyr::left_join(column_mapping, by="standard") %>%
    dplyr::pull(provided)

  # Set up the provided levels of any factor variables
  gender_levels <- readRDS(file.path(config_path, "gender_levels.rds"))
  provided_gender_levels <- gender_levels %>% dplyr::pull(provided)

  age_band_start_levels <- readRDS(file.path(config_path, "age_band_start_levels.rds"))
  provided_age_band_start_levels <- age_band_start_levels %>% dplyr::pull(provided)

  ethnic_category_levels <- readRDS(file.path(config_path, "ethnic_category_levels.rds"))
  provided_ethnic_category_levels <- ethnic_category_levels %>% dplyr::pull(provided)

  main_specialty_levels <- readRDS(file.path(config_path, "main_specialty_levels.rds"))
  provided_main_specialty_levels <- main_specialty_levels %>% dplyr::pull(provided)

  discharge_method_levels <- readRDS(file.path(config_path, "discharge_method_levels.rds"))
  provided_discharge_method_levels <- discharge_method_levels %>% dplyr::pull(provided)

  discharge_destination_levels <- readRDS(file.path(config_path, "discharge_destination_levels.rds"))
  provided_discharge_destination_levels <- discharge_destination_levels %>% dplyr::pull(provided)

  admission_method_levels <- readRDS(file.path(config_path, "admission_method_levels.rds"))
  provided_admission_method_levels <- admission_method_levels %>% dplyr::pull(provided)

  source_of_admission_levels <- readRDS(file.path(config_path, "source_of_admission_levels.rds"))
  provided_source__of_admission_levels <- source_of_admission_levels %>% dplyr::pull(provided)

  patient_classification_levels <- readRDS(file.path(config_path, "patient_classification_levels.rds"))
  provided_patient_classification_levels <- patient_classification_levels %>% dplyr::pull(provided)

  hospital_site_levels <- readRDS(file.path(config_path, "hospital_site_levels.rds"))
  provided_hospital_site_levels <- hospital_site_levels %>% dplyr::pull(provided)

  # Set up datetime formats
  datetime_formats <- readRDS(file.path(config_path, "datetime_formats.rds"))

  # extract the format of the provided start_datetime data
  # from the config files
  # start_datetime_provided_colname <- column_mapping %>%
  #   dplyr::filter(standard == "start_datetime") %>%
  #   dplyr::pull(provided)
  # start_datetime_provided_format <- datetime_formats %>%
  #   dplyr::filter(column_name == start_datetime_provided_colname) %>%
  #   dplyr::pull(datetime_format)

  #extract the format of the provided end_datetime data
  #from the config files
  end_datetime_provided_colname <- column_mapping %>%
    dplyr::filter(standard == "end_datetime") %>%
    dplyr::pull(provided)

  end_datetime_provided_format <- datetime_formats %>%
    dplyr::filter(column_name == end_datetime_provided_colname) %>%
    dplyr::pull(datetime_format)

  # Set up the column types, in the standard order as per above
  # NOTE: the order of column types here must be precisely
  # as per the standard order specified above
  colImportTypes <- rlang::exprs(
    readr::col_character(), #pseudo_id
    readr::col_factor(levels = !!eval(rlang::expr(provided_gender_levels))), # gender
    readr::col_factor(levels = !!eval(rlang::expr(provided_age_band_start_levels))), # age_band_start
    readr::col_factor(levels = !!eval(rlang::expr(provided_ethnic_category_levels))), #, #ethnic_category
    readr::col_factor(levels = !!eval(rlang::expr(provided_main_specialty_levels))), # main_specialty
    readr::col_factor(levels = !!eval(rlang::expr(provided_discharge_method_levels))), # discharge_method
    readr::col_factor(levels = !!eval(rlang::expr(provided_discharge_destination_levels))), # discharge_destination
    readr::col_factor(levels = !!eval(rlang::expr(provided_admission_method_levels))), # admission_method
    readr::col_factor(levels = !!eval(rlang::expr(provided_source__of_admission_levels))), # source_of_admission
    readr::col_factor(levels = !!eval(rlang::expr(provided_patient_classification_levels))), # patient_classification
    readr::col_factor(levels = !!eval(rlang::expr(provided_hospital_site_levels))), #hospital_site_levels
    #readr::col_datetime(format = !!eval(rlang::expr(start_datetime_provided_format))), #start_datetime
    readr::col_datetime(format = !!eval(rlang::expr(end_datetime_provided_format))),
    readr::col_character(), #spell_number
    readr::col_character(), # hrg
    readr::col_character(), #consultant
    readr::col_character(), #local_sub_specialty
    readr::col_integer(), # ward_stay_number
    readr::col_integer(), #episode_number
    readr::col_character(), #provider
    readr::col_character(), #diagnosis_code
    readr::col_character() # diagnosis
  )

  # Label the types with the respective column names and return this as a named
  # vector
  colImportTypes <- rlang::set_names(colImportTypes, colImportNames)
  colImportTypes
}

#' get_colname_mapping
#'
#' @param config_path path to the config files for a given dataset
#'
#' @return character vector specifying column renaming, of the form
#' c(new_name_1 = "old_name_1", new_name_2 = "old_name_2")
#' @export
#'
#' @examples
get_colname_mapping <- function(config_path) {
  # Read in the mapping from provided column names to standard column names...
  column_mapping <- readRDS(file.path(config_path, "column_mapping.rds"))

  column_mapping
}

#' import_and_standardise
#'
#' @param data_import_list list of named lists, each named list has two elements called
#' data_path and config_path, whose values are character strings specifying the paths
#' to i) a csv file to import data from and ii) a folder containing hospitalflow config
#' files.
#'
#' @return list of tibbles, each containing standardised import of one data file from data_paths
#' @export
#'
#' @examples
import_and_standardise <- function(data_import_list) {

  # Take the data_import_list and for each element x, load the data located at data_path
  # using configuration specified by the files at config_path.
  # In particular, use the column specification generated by
  # get_import_col_types from the config files.
  data_config_list <- lapply(data_import_list,
                             function(x) {list(data = readr::read_csv(x$data_path,
                                                                      locale = readr::locale(tz = 'Europe/London'),
                                                                      col_types = eval(rlang::call2(readr::cols,
                                                                                                    !!!get_import_col_types(x$config_path),
                                                                                                    .default = readr::col_skip()))),
                                               config_path = x$config_path)
    })

  # Rename the columns in each imported dataset, using the name mapping onto standard hospitalflow
  # variable names specified in the config files at the specified config_path.
  data_config_list <- lapply(data_config_list,
                             function(x) {
                               list(data = standardise_column_names(x$data,
                                                                    get_colname_mapping(x$config_path) %>%
                                                                      dplyr::filter(provided %in% colnames(x$data))
                                                                    ),
                                    config_path = x$config_path)
                               })

  # HERE NEED TO DEAL WITH FORMAT CONVERSIONS: DATETIMES, FACTORS, ETC.

  # Extract the data as a tibble for each imported file, and return as a list of these tibbles.
  lapply(data_config_list, function(x) x$data)
}


#' import_and_standardise_inpatient
#'
#' @param data_import_list list of named lists, each named list has two elements called
#' data_path and config_path, whose values are character strings specifying the paths
#' to i) a csv file to import data from and ii) a folder containing hospitalflow config
#' files.
#'
#' @return list of tibbles, each containing standardised import of one data file from data_paths
#' @export
#'
#' @examples
import_and_standardise_inpatient <- function(data_import_list) {

  # Take the data_import_list and for each element x, load the data located at data_path
  # using configuration specified by the files at config_path.
  # In particular, use the column specification generated by
  # get_import_col_types from the config files.
  data_config_list <- lapply(data_import_list,
                             function(x) {list(data = readr::read_csv(x$data_path,
                                                                      locale = readr::locale(tz = 'Europe/London'),
                                                                      col_types = eval(rlang::call2(readr::cols,
                                                                                                    !!!get_import_col_types_inpatient(x$config_path),
                                                                                                    .default = readr::col_skip()))),
                                               config_path = x$config_path)
                             })

  # Rename the columns in each imported dataset, using the name mapping onto standard hospitalflow
  # variable names specified in the config files at the specified config_path.
  data_config_list <- lapply(data_config_list,
                             function(x) {
                               list(data = standardise_column_names(x$data,
                                                                    get_colname_mapping(x$config_path) %>%
                                                                      dplyr::filter(provided %in% colnames(x$data))
                               ),
                               config_path = x$config_path)
                             })

  # HERE NEED TO DEAL WITH FORMAT CONVERSIONS: DATETIMES, FACTORS, ETC.

  # Extract the data as a tibble for each imported file, and return as a list of these tibbles.
  lapply(data_config_list, function(x) x$data)
}

#example_data_import_list <- list(list(data_path = "../lgt-data/data-extract-201901/CLAHRCExtractToSend_QEH_20190107_ED.csv",
                                     #config_path = "lgt-config/"),
                                 #list(data_path = "../lgt-data/data-extract-201901/CLAHRCExtractToSend_UHL_20190104_ED.csv",m                                    #config_path = "lgt-config/"))

#cw_data_import_list <- list(list(data_path = "../cw-data/cw_ae_anonim.csv", config_path = "../cw_config_files/ed/"))

#cw_data_import_list_inpatient <- list(list(data_path = "../cw-data/cw_ip_anonim.csv", config_path = "../cw_config_files/inpatient/"))