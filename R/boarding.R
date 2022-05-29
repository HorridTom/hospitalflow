

#' Contingency table between ward category and main specialty/disease categories.
#'
#' @description
#' \code{get_ward_specialty_contingency_table} returns a table showing the relationship
#' between ward types and main specialties or disease groups. Used in patient boarding analysis to infer
#' what type of patients a given ward is designed to accommodate.
#'
#' @param ipData
#' \itemize{Hospital inpatient episode data with at least the following fields:
#'   \item \code{ward_category} - category of the ward;
#'   \item \code{main_specialty} - main specialty of the clinician responsible for the episode;
#'   \item \code{ccsr_category_description} - disease group that ICD-10 code of the patient belongs to.
#' }
#' @param vars Variables of interest - either \code{"ws"} (ward type - main specialty) or
#' \code{"wd"} (ward type - CCSR disease category).
#' @param scaleBy
#' \itemize{Whether and how entries of contingency table should be scaled.
#'   \item \code{"none"} - no scaling, presents absolute episode counts;
#'   \item \code{"rowsum"} - normalizes by the total number of episodes in a given ward. Useful when investigating the distribution of specialties/diseases among wards.
#'   \item \code{"colsum"} - normalizes by the total number of episodes in a given specialty/disease group. Useful when investigating the distribution of wards among specialties/diseases.
#'   \item \code{"total"} - normalizes by the total number of episodes.
#' }
#' @param returnPlot A boolean value indicating whether a plot (heatmap) should be return. If \code{FALSE}, a table is returned.
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

  # Check if vars argument makes sense
  if (!(vars %in% c("ws", "wd"))) {
    stop("Unrecognized value provided to 'vars'. Should be one of the following: 'ws', 'wd'.")
  }

  # Check if scaleBy argument makes sense
  if (!(scaleBy %in% c("none", "rowsum", "colsum", "total"))) {
    stop("Unrecognized value provided to 'scaleBy'. Should be one of the following: 'none', 'rowsum', 'colsum', 'total'.")
  }

  ipData <- change_icd10_codes(ipData)

  # Getting the contingency table
  if (vars == "ws") {
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
    plot_name <- "Relationship between Ward Type and Main Specialty"
    x_axis_label <- "Specialty"
    y_axis_label <- "Ward Type"

  } else if (vars == "wd") {
    # Load lookup table
    lookup_tbl <- hospitalflow:::icd10_ccsr_mapping

    ipData <- add_ccsr_categories(ipData, lookup_tbl)

    # Perform check if the necessary column exist
    if (!("ward_category" %in% colnames(ipData))) {
      stop("The 'ward_category' column not found in the provided dataframe.")
    }

    if (!("ccsr_category_description" %in% colnames(ipData))) {
      stop("The 'ccsr_category_description' column not found in the provided dataframe.")
    }

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


change_icd10_codes <- function (ipData) {

  # This function takes the simulated inpatient_data and does 2 things with dignosis_code column:
  #    1. Changes all instances of G052 to G053. This is done because there is no G052 code in
  #       CCSR database. Also, there seems to be conflicting information on the internet
  #       because both of the codes correspond to the same disease when looking at different sources.
  #    2. Changes F99X to F99 because they encode the same disease and F99X is not present in
  #       CCSR database.

  ipData$diagnosis_code <- gsub("G052", "G053", ipData$diagnosis_code)
  ipData$diagnosis_code <- gsub("F99X", "F99", ipData$diagnosis_code)

  return(ipData)

}


add_ccsr_categories <- function(ipData, mappingTbl) {
  # Since ICD-10 codes in inpatient_data are specified in the format of X1234,
  # I need to reformat the ICD-10 codes in the icd10_ccsr_mapping. Natively,
  # ICD-10 in the icd10_ccsr_mapping are between 3 and 7 (including) in length.

  mappingTbl$icd10cm_code <- ifelse(
    nchar(mappingTbl$icd10cm_code) == 3,
    mappingTbl$icd10cm_code,
    substr(mappingTbl$icd10cm_code, 1, 4)
  )

  # Truncating the ICD-10 codes results in duplicate values in icd10cm_codes column.
  # Need to remove duplicates. The code below assumes that less-granular versions of
  # ICD-10 codes are mapped to the same categories.

  mappingTbl <- mappingTbl %>%
    dplyr::group_by(icd10cm_code) %>%
    dplyr::slice_head(n=1) %>%
    dplyr::ungroup()


  # At this point, ICD-10 codes in ipData should be a subset of ICD-10 codes in mapping Tbl.
  # Checking this.

  if(!all(ipData$diagnosis_code %in% mappingTbl$icd10cm_code)) {
    stop("There are some ICD-10 codes inside inpatient episode data that cannot be mapped to CCSR categories. Make sure that the format of ICD-10 codes is the same between the 2 tables.")
  } else {
    # Performing the left join - resulting is the inpatient episode data with CCSR categories
    joined_df <- dplyr::left_join(
      ipData,
      mappingTbl,
      by = c("diagnosis_code" = "icd10cm_code")
    )
  }
  return(joined_df)
}


