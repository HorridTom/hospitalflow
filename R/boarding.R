

#' Contingency table between ward category and main specialty
#'
#' @param spell_table
#'
#' @return
#' @export
#'
#' @examples
#'
get_ward_specialty_contingency_table <- function(ip_data){
  # Perform check if the necessary column exist
  if (!("ward_category" %in% colnames(ip_data))) {
    stop("The 'ward_category' column not found in the provided dataframe.")
  }

  if (!("main_specialty" %in% colnames(ip_data))) {
    stop("The 'main_specialty' column not found in the provided dataframe.")
  }

  tbl <- as.data.frame.matrix(
    table(
      spell_table$ward_category, spell_table$main_specialty
      )
    )
  return (tbl)
}
