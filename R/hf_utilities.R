#' convert_rds_cols_to_char
#'
#' @param path
#'
#' @return list of NULL (as per saveRDS)
#'
#' Loads in all RDS files in path, assumes all are dataframes. Converts all columns
#' to character, and re-saves as same filenames.
#'
#' Use with care!
#'
#' @export
#'
#' @examples
convert_rds_cols_to_char <- function(path) {
  config_file_list <- Sys.glob(paste0(path, "*.rds"))
  lapply(config_file_list, function(x) {
    config_tibble <- readRDS(x)
    config_tibble <- config_tibble %>% dplyr::mutate_all(as.character)
    saveRDS(config_tibble, x)
  })
}


#' add_directorate_variable
#'
#' @param spell_table
#'
#' @return
#' @export
#'
#' @examples
add_directorate_variable <- function(spell_table){

  df <- dplyr::left_join(spell_table, specialty_mapping, by = c("main_specialty_start" = "spec_name"))

  df

}

