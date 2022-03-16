#' Unscheduled emergency department occupancy by weekday and hour
#'
#' @description
#' \code{plot_ed_occupancy_hour_of_week} shows average occupancy in an emergency
#' department throughout weekdays (Mon-Sun).
#'
#' @param data Hospital episode data.
#' @inheritParams plot_admissions_discharges_day_of_week
#'
#' @return A plot (default) or a dataframe showing average emergency department
#' occupancy by weekday and hour.
#'
#' @examples
#' \dontrun{
#' TBD
#' }
#' @export
plot_ed_occupancy_hour_of_week <- function(data,
                                           startDate,
                                           endDate,
                                           returnPlot = TRUE,
                                           hospitalName = "{hospital_name}") {

  # get time zone of data
  time_zone <- attr(data$spell_start, "tzone")

  # set input dates to have the same time zone as the data
  start_date <- as.POSIXct(startDate, tz = time_zone)
  end_date <- as.POSIXct(endDate, tz = time_zone)

  # renaming the variables I am interested in
  df_select <- data %>%
    dplyr::select(spell_number, spell_start, initial_ed_end_datetime, starts_with_ed) %>%
    dplyr::filter(spell_start <= end_date | initial_ed_end_datetime >= start_date) %>%
    dplyr::filter(starts_with_ed == TRUE) %>%
    tidyr::drop_na()


  # date time period chosen

  time_hr <- seq(from = start_date, to = end_date, by = "hour")

  occupancy_vct <- sapply(time_hr, occupancy, df = df_select, df_type = "spell table modified")

  occupancy_df <- tibble::tibble(time_hr, occupancy_vct)

  dt_wday_hour_month <- occupancy_df %>%
    dplyr::mutate(
      month = lubridate::month(time_hr),
      Weekday = lubridate::wday(time_hr, label = TRUE, abbr = TRUE),
      hour = lubridate::hour(time_hr)
    ) %>%
    dplyr::select(time_hr, month, Weekday, hour, occupancy_vct)

  # getting the averages, min, max, interquartile ranges (Q1 and  Q3)
  tbl_avg_occ <- dt_wday_hour_month %>%
    dplyr::group_by(hour, Weekday) %>%
    dplyr::summarize(
      average_occupancy = mean(occupancy_vct),
      Q1 = quantile(occupancy_vct, 0.25),
      Q3 = quantile(occupancy_vct, 0.75),
      Min_n = min(occupancy_vct),
      Max_n = max(occupancy_vct)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(hour, Weekday, average_occupancy, Q1, Q3, Min_n, Max_n)


  # Weekdays <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  # tbl_avg_occ_2 <- dplyr::arrange(transform(tbl_avg_occ, day = factor(Weekday, levels = Weekdays)), Weekday)

  # Set the title
  title_stub <- ": Unscheduled ED Occupancy by day and hour"
  # hospitalName <- "Chelsea & Westminster"
  start_date_title <- format(as.Date(start_date), format = "%d %B %Y")
  end_date_title <- format(as.Date(end_date), format = "%d %B %Y")
  chart_title <- paste0(hospitalName, title_stub, "\n", start_date_title, " to ", end_date_title)

  plt <- tbl_avg_occ %>%
    ggplot2::ggplot(ggplot2::aes(x = hour)) +
    ggplot2::geom_line(ggplot2::aes(y = average_occupancy, color = "Average Occupancy"), size = 1) +
    ggplot2::scale_x_continuous(limits = c(0, 23)) +
    ggplot2::facet_grid(~Weekday) + # nrow = 1
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Q1, ymax = Q3, fill = "interquartile range"), alpha = 0.20) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Min_n, ymax = Max_n, fill = "range"), alpha = 0.30) +
    ggplot2::scale_colour_manual("", values = "red") +
    ggplot2::scale_fill_manual("", values = c("blue", "steel blue")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = chart_title,
      subtitle = "Registered unscheduled A&E 'occupancy', patients; by Hour of the day and day of week. \nNote: results are intended for management information only",
      y = "Average A&E occupancy", x = "Hour of the day", caption = "Source: CLAHRC NWL"
    ) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 21, b = 0, l = 0)),
      plot.title = ggplot2::element_text(size = 12, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10),
      legend.position = "bottom", legend.box = "horizontal"
    )


  if (returnPlot == TRUE) {
    plt
  } else {
    plt$data %>% dplyr::select(hour, Weekday, average_occupancy, Q1, Q3, Min_n, Max_n)
  }
}
