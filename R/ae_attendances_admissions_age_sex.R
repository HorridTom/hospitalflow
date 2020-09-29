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
ae_attendances_admissions_age_sex <- function(start_date = as.Date("2016-01-01", tz = "Europe/London"),
                                              end_date = as.Date("2017-01-01", tz = "Europe/London"),
                                              data, plot_chart, hospital_name = "Hospital name"){

  dt <- data %>%
    dplyr::filter(spell_start <= end_date & spell_end >= start_date) %>%
    dplyr::select(spell_number, spell_start, spell_end, gender, age_band_start, spell_class_col)


  # finding the number of ae attendances
  df_ae_attendances <- dt %>%
    dplyr::filter(spell_class_col == "ed_non_admission" | spell_class_col == "ed_comp_non_admission" | spell_class_col == "ed_admission" | spell_class_col == "ed_comp_admission") %>%
    dplyr::filter(gender != "Other" ) %>%
    dplyr::group_by(gender, age_band_start) %>%
    dplyr::summarize(value= dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(group = dplyr::case_when(gender == "Female" ~ "Female attendances",
                                    gender == "Male" ~ "Male attendances"))

  df_ae_admissions <- dt %>%
    dplyr::filter(spell_class_col == "ed_admission" | spell_class_col == "ed_comp_admission") %>%
    dplyr::filter(gender != "Other" ) %>%
    dplyr::group_by(gender, age_band_start) %>%
    dplyr::summarize(value = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(group =  dplyr::case_when(gender == "Female" ~ "Female admitted",
                                            gender == "Male" ~ "Male admitted"))

  df_numbers_only <- dplyr::full_join(df_ae_attendances, df_ae_admissions, by  = c("gender", "age_band_start", "value", "group"))


  # Set the title
  title_stub <- ": Attendances and Admissions by Age and Gender,\n"
  #hospital_name <- "Chelsea & Westminster"
  start_date_title <- format(as.Date(start_date), format = "%d %B %Y")
  end_date_title <- format(as.Date(end_date), format = "%d %B %Y")
  chart_title <- paste0(hospital_name, title_stub, start_date_title, " to ", end_date_title)

  plot_test <- ggplot2::ggplot(df_numbers_only, ggplot2::aes(age_band_start, value, fill = group)) +
    ggplot2::geom_col(data = dplyr::filter(df_numbers_only, group %in% c("Male attendances", "Female attendances")),
                      position = ggplot2::position_dodge()) +
    ggplot2::geom_col(data = dplyr::filter(df_numbers_only, group %in% c("Male admitted", "Female admitted")),
                      position = ggplot2::position_dodge(0.9), width = 0.5) +
    ggplot2::scale_fill_manual(name = "",
                               breaks = c("Male admitted", "Male attendances",
                                          "Female admitted", "Female attendances"),
                               labels = c("Male Admitted", "Male attendances",
                                          "Female Admitted", "Female attendances"),
                               values = c("coral3",  "lightcoral", "steelblue4", "lightblue2")) +
    ggplot2::xlab("Age Group") +
    ggplot2:: ylab("ED Attendances and Admissions") +
    ggplot2::labs(title = chart_title,
                  subtitle = "ED attendance and hospital admissions from ED, \nNote: (i) planned return/recall attendances have been excluded; (ii) results are intended for management information only",
                  y = "Attendances and Admissions, n", x = "Age Group", caption = "Source: CLAHRC NWL") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 21, b = 0, l = 0)),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   plot.title = ggplot2::element_text(size = 12, face = "bold"),
                   plot.subtitle = ggplot2::element_text(size = 10),
                   legend.position = "bottom", legend.box = "horizontal") +
    ggplot2::scale_x_discrete(drop = FALSE)

  plot_test



  # Things to consider - StartDate and EndDate does not do what it should do -> it plots all the date from 2011-2015

  if(plot_chart == TRUE){

    plot_test

  }else{


    plot_test$data %>% dplyr::select(gender, age_band_start, value, group)

  }

}
#####################################################################################################################


#' age_sex
#'
#' @return
#' @export
#'
age_sex <- function() {
  ae_attendances_admissions_age_sex(start_date = as.Date("2012-01-01", tz = "Europe/London"),
                                    end_date = as.Date("2015-01-01", tz = "Europe/London"),
                                    data = hospitalflow::cw_disch_201201_201507_782cfa21_stddt_s, plot_chart = TRUE, hospital_name = "Hospital name")
}
