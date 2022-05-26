

#' Contingency table between ward category and main specialty/disease categories.
#'
#' @description
#' \code{get_ward_specialty_contingency_table} returns a table showing the relationship
#' between ward types and main specialties or disease groups. Used in patient boarding analysis to infer
#' what type of patients a given ward is designed to accommodate.
#'
#' @param ip_data
#' \itemize{Hospital inpatient episode data with at least the following fields:
#'   \item \code{ward_category} - category of the ward;
#'   \item \code{main_specialty} - main specialty of the clinician responsible for the episode;
#'   \item \code{ccsr_category_description} - disease group that ICD-10 code of the patient belongs to.
#' }
#' @param vars Variables of interest - either \code{"ws"} (ward type - main specialty) or
#' \code{"ws"} (ward type - CCSR category).
#' @param scaleBy
#' @param returnPlot
#'
#' @return A contingency table (default) or a heatmap showing the relationship between ward type and
#' main specialty or CCSR categories.
#' @export
#'
#' @examples
#' \dontrun{
#' get_contingency_table(
#'   ip_data = inpatient_data,
#'   vars = "ws",
#'   scaleBy = "none",
#'   returnPlot = FALSE
#' )
#' }
#'
#' @export
get_contingency_table <- function(ipData, vars, scaleBy = "none", returnPlot = FALSE) {
  # Perform check if the necessary column exist
  if (!("ward_category" %in% colnames(ipData))) {
    stop("The 'ward_category' column not found in the provided dataframe.")
  }

  if (!("main_specialty" %in% colnames(ipData))) {
    stop("The 'main_specialty' column not found in the provided dataframe.")
  }

  if (!("ccsr_category_description" %in% colnames(ipData))) {
    stop("The 'ccsr_category_description' column not found in the provided dataframe.")
  }

  # Check if vars argument makes sense
  if (!(vars %in% c("ws", "wd"))) {
    stop("Unrecognized value provided to 'vars'. Please provide on of: 'ws', 'wd'.")
  }

  # Getting the contingency table
  if (vars == "ws") {
    tbl <- as.data.frame.matrix(
    table(
      ipData$ward_category, ipData$main_specialty
      )
    )
    plot_name <- "Relationship between Ward Type and Main Specialty"
    x_axis_label <- "Specialty"
    y_axis_label <- "Ward Type"

  } else if (vars == "wd") {
    tbl <- as.data.frame.matrix(
      table(
        ipData$ward_category, ipData$ccsr_category_description
      )
    )
    plot_name <- "Relationship between Ward Type and Disease Groups (CCSR Categories)"
    x_axis_label <- "CCSR Category Description"
    y_axis_label <- "Ward Type"
  }

  tbl <- tbl[gtools::mixedsort(row.names(tbl)),]

  # scaling (if needed)
  if (scaleBy == "none") {
    tbl <- tbl
    legend_name <- "# of episodes"
  } else if (scaleBy == "rowsum") {
    tbl <- t(apply(tbl, 1, function(x){x/sum(x)})) %>% as.data.frame() %>% round(2) # need to transpose
    legend_name <- paste0("Proportion of episodes (by ", y_axis_label, ")")
  } else if (scaleBy == "colsum") {
    tbl <- apply(tbl, 2, function(x){x/sum(x)}) %>% as.data.frame() %>% round(2)
    legend_name <- paste0("Proportion of episodes (by ", x_axis_label, ")")
  } else if (scaleBy == "total") {
    tbl <- (tbl/sum(tbl)) %>% as.data.frame() %>% round(2)
    legend_name <- "Proportion of episodes (out of total)"
  }

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
      ggplot2::aes(x = colname, y = rowname, fill = value)
      ) +
      ggplot2::geom_tile(ggplot2::aes(width=0.95, height=0.95)) + # creates heatmap and adds whitespace around tiles
      ggplot2::geom_text(ggplot2::aes(label = value), color = "red") + # adds labels to tiles
      ggplot2::ggtitle(plot_name) + # title of the plot
      ggplot2::xlab(x_axis_label) + # adds x axis label
      ggplot2::ylab(y_axis_label) + # adds y axis label
      ggplot2::scale_fill_continuous(name = legend_name) + # changes legend title
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5), # adds title
        axis.text.x = ggplot2::element_text(angle=90, vjust = 0.5, hjust = 1), # rotates X tick labels and aligns them to tick marks

      )

    return(cont_tbl_plot)

  } else {

    return(tbl)

  }

}
