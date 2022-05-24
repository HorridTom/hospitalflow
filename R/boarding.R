

#' Contingency table between ward category and main specialty
#'
#' @description
#' \code{get_ward_specialty_contingency_table} returns a table showing the relationship
#' between ward types and main specialties. Used in patient boarding analysis to infer
#' what type of patients a given ward is designed to accommodate.
#'
#' @param ip_data
#' \itemize{Hospital inpatient episode data with at least the following fields:
#'   \item \code{ward_category} - category of the ward;
#'   \item \code{main_specialty} - main specialty of the clinician responsible for the episode;
#' }
#'
#' @return A contingency table showing the relationship between ward type and
#' main specialty.
#' @export
#'
#' @examples
#' \dontrun{
#' get_ward_specialty_contingency_table(
#'   ip_data = inpatient_data
#' )
#' }
#'
#' @export
get_ward_specialty_contingency_table <- function(ipData, scaleBy, returnPlot = FALSE){
  # Perform check if the necessary column exist
  if (!("ward_category" %in% colnames(ip_data))) {
    stop("The 'ward_category' column not found in the provided dataframe.")
  }

  if (!("main_specialty" %in% colnames(ip_data))) {
    stop("The 'main_specialty' column not found in the provided dataframe.")
  }

  tbl <- as.data.frame.matrix(
    table(
      ip_data$ward_category, ip_data$main_specialty
      )
    )

  tbl <- tbl[gtools::mixedsort(row.names(tbl)),]

  return (tbl)
}
