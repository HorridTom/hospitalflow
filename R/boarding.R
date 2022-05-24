

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
  if (!("ward_category" %in% colnames(ipData))) {
    stop("The 'ward_category' column not found in the provided dataframe.")
  }

  if (!("main_specialty" %in% colnames(ipData))) {
    stop("The 'main_specialty' column not found in the provided dataframe.")
  }

  tbl <- as.data.frame.matrix(
    table(
      ipData$ward_category, ipData$main_specialty
      )
    )

  tbl <- tbl[gtools::mixedsort(row.names(tbl)),]


  if (returnPlot == TRUE) {
    # will be needed for the correct order of wards on X axis
    ward_levels <- row.names(tbl)

    # transforming to dataframe to long format
    cont_tbl_long <- tbl %>%
      tibble::rownames_to_column() %>%
      tidyr::gather(colname, value, -rowname) %>%
      dplyr::mutate(rowname = factor(rowname, levels = ward_levels))

    # creating a plot
    cont_tbl_plot <- ggplot2::ggplot(
      cont_tbl_long,
      ggplot2::aes(x = rowname, y = colname, fill = value)
      ) +
      ggplot2::geom_tile(ggplot2::aes(width=0.95, height=0.95)) + # creates heatmap and adds whitespace around tiles
      ggplot2::ggtitle("Relationship between Ward Type and Main Specialty") + # title of the plot
      ggplot2::xlab("\nWard Type") + # adds x axis label
      ggplot2::ylab("Specialty\n") + # adds y axis label
      ggplot2::scale_fill_continuous(name = "# of episodes") + # changes legend title
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5), # adds title
        axis.text.x = ggplot2::element_text(angle=90, vjust = 0.5), # rotates X tick labels and aligns them to tick marks

      )

    return(cont_tbl_plot)

  } else {

    if (is.na(scaleBy)) {
      return (tbl)
    } else if (scaleBy == "rowsum") {

    }


  }
}
