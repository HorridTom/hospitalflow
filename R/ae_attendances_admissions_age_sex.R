
#' ae_attendances_admissions_age_sex
#'
#' @param start_date date of earliest discharge to be included in the analysis
#' @param end_date date of latest admission to be included in the analysis
#' @param data hospital episode data
#' @param plot_chart if TRUE return chart, otherwise if FALSE return dataframe
#' @param hospital_name this is a string specifying the hospital name for the chart title
#'
#' @return Chart or dataframe showing A&E Attendances and Admissions by Gender and Age
#' @export
#'
#' @examples
#' \dontrun{
#' hospitalflow::ae_attendances_admissions_age_sex(start_date = "2012-01-01 00:00:00",
#' end_date = "2015-01-01 00:00:00",
#' data = test_data_age_sex_att_adm, plot_chart = TRUE)
#' }
ae_attendances_admissions_age_sex <- function(start_date, end_date, data, plot_chart, hospital_name = "Chelsea & Westminster"){

  # finding the number of ae attendances
  # finding the number of ae attendances
  df_ae_attendances <- data %>%
    dplyr::filter(Ward == "A&E" & Gender != "Not Specified") %>%
    dplyr::filter(EpisodeNumber == 1 & PatientType == "Emergency") %>%
    dplyr::group_by(Gender, Age_band) %>%
    dplyr::summarize(Value= n()) %>%
    dplyr::mutate(Group = dplyr::case_when(Gender == "Female" ~ "Female not admitted",
                                    Gender == "Male" ~ "Male not admitted"))

  df_ae_admissions <- data %>%
    dplyr::filter(Ward == "A&E" & LastWard != "ED only" & PatientType == "Emergency") %>%
    dplyr::filter(Gender != "Not Specified" & EpisodeNumber == 1) %>%
    dplyr::group_by(Gender, Age_band) %>%
    dplyr::summarize(Value = n()) %>%
    dplyr::mutate(Group =  dplyr::case_when(Gender == "Female" ~ "Female admitted",
                                            Gender == "Male" ~ "Male admitted"))

  df_numbers_only <- dplyr::full_join(df_ae_attendances, df_ae_admissions, by  = c("Gender", "Age_band", "Value", "Group"))

  # SET CHART TITLE -
  title_stub = " hospital: A&E Attendances and Admissions, "
  chart_title <- paste0(hospital_name, title_stub, start_date," to ", end_date)

  plot_test <- ggplot2::ggplot(df_numbers_only, ggplot2::aes(Age_band, Value, fill = Group)) +
    ggplot2::geom_col(data = dplyr::filter(df_numbers_only, Group %in% c("Male not admitted", "Female not admitted")),
                      position = ggplot2::position_dodge()) +
    ggplot2::geom_col(data = dplyr::filter(df_numbers_only, Group %in% c("Male admitted", "Female admitted")),
                      position = ggplot2::position_dodge(0.9), width = 0.5) +
    ggplot2::scale_fill_manual(name = "",
                               breaks = c("Male admitted", "Male not admitted",
                                          "Female admitted", "Female not admitted"),
                               labels = c("Male Admitted", "Male not admitted",
                                          "Female Admitted", "Female not admitted"),
                               values = c("coral3",  "lightcoral", "steelblue4", "lightblue2")) +
    ggplot2::xlim("0 yrs" , "1-4 yrs", "5-9 yrs", "10-14 yrs", "15-19 yrs", "20-24 yrs", "25-29 yrs",
                  "30-34 yrs", "35-39 yrs", "40-44 yrs", "45-49 yrs", "50-54 yrs", "55-59 yrs",
                  "60-64 yrs", "65-69 yrs", "70-74 yrs", "75-79 yrs", "80-84 yrs", "85+ yrs") +
    ggplot2::xlab("Age Group") +
    ggplot2:: ylab("ED Attendances and Admissions") +
    ggplot2::labs(title = chart_title,
                  subtitle = "ED attendance and hospital admissions from ED, \nNote: (i) planned return/recall attendances have been excluded; (ii) results are intended for management information only",
                  y = "Attendances and Admissions, n", x = "Age Group", caption = "Source: CLAHRC NWL") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 21, b = 0, l = 0)),
                   plot.title = ggplot2::element_text(size = 12, face = "bold"),
                   plot.subtitle = ggplot2::element_text(size = 10),
                  legend.position = "bottom", legend.box = "horizontal") +
    ggplot2::scale_y_continuous(expand = c(0, .5))

  plot_test


  # Things to consider - StartDate and EndDate does not do what it should do -> it plots all the date from 2011-2015

  if(plot_chart == TRUE){

    plot_test

    }else{


     plot_test$data %>% dplyr::select(Gender, Age_band, Value, Group)

  }

}  ######
##################################################################################################
