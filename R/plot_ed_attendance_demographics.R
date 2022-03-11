#' Attendance and admissions by age and gender in emergency department (ED)
#'
#' @description
#' \code{plot_ed_attendance_demographics} returns the number of ED attendances
#' and hospitalizations by age (divided into bands) and gender (male and female).
#'
#' @param data Hospital episode data.
#' @inheritParams plot_admissions_discharges_day_of_week
#'
#' @return A plot (default) or a dataframe showing attendance of emergency
#' department and admissions therefrom by age and gender.
#'
#' @examples
#' \dontrun{
#' plot_ed_attendance_demographics(
#'   startDate = "2012-01-01 00:00:00",
#'   endDate = "2015-01-01 00:00:00",
#'   data = test_data_age_sex_att_adm, returnPlot = TRUE
#' )
#' }
#' @export
plot_ed_attendance_demographics <- function(data,
                                            startDate,
                                            endDate,
                                            returnPlot = TRUE,
                                            hospitalName = "Hospital name") {

  # get time zone of data
  time_zone <- attr(data$spell_start, "tzone")

  # set input dates to have the same time zone as the data
  startDate <- as.Date(startDate, tz = time_zone)
  endDate <- as.Date(endDate, tz = time_zone)

  dt <- data %>%
    dplyr::filter(spell_start <= endDate & spell_end >= startDate) %>%
    dplyr::select(spell_number, spell_start, spell_end, gender, age_band_start, spell_class_col)

  if (is.factor(dt$age_band_start)) {
    age_band_levels <- gtools::mixedsort(levels(dt$age_band_start))
  } else {
    age_band_levels <- gtools::mixedsort(unique(dt$age_band_start))
  }

  dt <- dt %>%
    dplyr::mutate(age_band_start = factor(age_band_start, levels = age_band_levels)) # necessary for correct age band ordering on x axis

  # finding the number of ae attendances
  df_ae_attendances <- dt %>%
    dplyr::filter(spell_class_col == "ed_non_admission" | spell_class_col == "ed_comp_non_admission" | spell_class_col == "ed_admission" | spell_class_col == "ed_comp_admission") %>%
    dplyr::filter(gender != "Other") %>%
    dplyr::group_by(gender, age_band_start) %>%
    dplyr::summarize(value = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(group = dplyr::case_when(
      gender == "Female" ~ "Female attendances",
      gender == "Male" ~ "Male attendances"
    ))

  df_ae_admissions <- dt %>%
    dplyr::filter(spell_class_col == "ed_admission" | spell_class_col == "ed_comp_admission") %>%
    dplyr::filter(gender != "Other") %>%
    dplyr::group_by(gender, age_band_start) %>%
    dplyr::summarize(value = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(group = dplyr::case_when(
      gender == "Female" ~ "Female admitted",
      gender == "Male" ~ "Male admitted"
    ))

  df_numbers_only <- dplyr::full_join(df_ae_attendances, df_ae_admissions, by = c("gender", "age_band_start", "value", "group"))


  # Set the title
  title_stub <- ": Attendances and Admissions by Age and Gender,\n"
  start_date_title <- format(as.Date(startDate), format = "%d %B %Y")
  end_date_title <- format(as.Date(endDate), format = "%d %B %Y")
  chart_title <- paste0(hospitalName, title_stub, start_date_title, " to ", end_date_title)

  plot_test <- ggplot2::ggplot(df_numbers_only, ggplot2::aes(age_band_start, value, fill = group)) +
    ggplot2::geom_col(
      data = dplyr::filter(df_numbers_only, group %in% c("Male attendances", "Female attendances")),
      position = ggplot2::position_dodge()
    ) +
    ggplot2::geom_col(
      data = dplyr::filter(df_numbers_only, group %in% c("Male admitted", "Female admitted")),
      position = ggplot2::position_dodge(0.9), width = 0.5
    ) +
    ggplot2::scale_fill_manual(
      name = "",
      breaks = c(
        "Male admitted", "Male attendances",
        "Female admitted", "Female attendances"
      ),
      labels = c(
        "Male Admitted", "Male attendances",
        "Female Admitted", "Female attendances"
      ),
      values = c("coral3", "lightcoral", "steelblue4", "lightblue2")
    ) +
    ggplot2::xlab("Age Group") +
    ggplot2::ylab("ED Attendances and Admissions") +
    ggplot2::labs(
      title = chart_title,
      subtitle = "ED attendance and hospital admissions from ED, \nNote: (i) planned return/recall attendances have been excluded; (ii) results are intended for management information only",
      y = "Attendances and Admissions, n", x = "Age Group", caption = "Source: CLAHRC NWL"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 21, b = 0, l = 0)),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.title = ggplot2::element_text(size = 12, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10),
      legend.position = "bottom", legend.box = "horizontal"
    ) +
    ggplot2::scale_x_discrete(drop = FALSE)

  # Things to consider - StartDate and EndDate does not do what it should do -> it plots all the date from 2011-2015
  if (returnPlot == TRUE) {
    plot_test
  } else {
    plot_test$data %>% dplyr::select(gender, age_band_start, value, group)
  }
}
