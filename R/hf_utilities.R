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
#' @param table a dataframe with a column containing specialties
#' @param spec_column string containing column name containing specialties
#'
#' @return table with an additional column containing the directorate associated
#' with the specialty for that row.
#' @export
#'
#' @examples
add_directorate_variable <- function(table, spec_column = "main_specialty_start"){

  df <- dplyr::left_join(table, specialty_mapping, by = setNames("spec_name", spec_column))

  df

}


# , .sep = " ", .collapse = NULL
cumulative_paste = function(x, ...)
  Reduce(function(x1, x2) paste(x1, x2, ...), x, accumulate = TRUE)
