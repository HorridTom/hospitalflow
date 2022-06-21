#' Average length of stay in the emergency department depending on time of the day
#'
#' @description
#' \code{plot_average_ed_los_hour_of_day} allows to visualize the average length
#' of stay in the emergency department over 24 hour period (0-24h). Patients are
#' grouped into 4 batches with respect to the flow group they belong to.
#'
#' @param data Hospital episode data.
#' @inheritParams plot_admissions_discharges_day_of_week
#'
#' @return A plot (default) or a dataframe showing the average length of stay in
#' the emergency department of different patient flow groups.
#'
#' @examples
#' \dontrun{
#' TBD
#' }
#' @export
plot_average_ed_los_hour_of_day <- function(data,
                                            startDate,
                                            endDate,
                                            returnPlot = TRUE,
                                            hospitalName = "Hospital name") {

  # get time zone of data
  time_zone <- attr(data$spell_start, "tzone")

  # set input dates to have the same time zone as the data
  startDate <- as.POSIXct(startDate, tz = time_zone)
  endDate <- as.POSIXct(endDate, tz = time_zone)

  dt_los <- data %>%
    dplyr::select(
      spell_number, spell_start, initial_ed_end_datetime, starts_with_ed, disposal_code,
      ed_non_adm, ed_admission, hrg_ae_code, main_specialty_start, source_referral_ae
    ) %>%
    dplyr::filter(spell_start > startDate | initial_ed_end_datetime < endDate) %>%
    dplyr::filter(starts_with_ed == TRUE | ed_non_adm == TRUE)

  dt_flows <- make_flow_groups(dt_los)

  dt_calc <- dt_flows %>%
    dplyr::mutate(
      Same_day_discharge = dplyr::if_else(as.Date(spell_start) == as.Date(initial_ed_end_datetime), TRUE, FALSE),
      Los = as.numeric(difftime(initial_ed_end_datetime, spell_start, unit = c("min"))),
      Discharged_24hr = dplyr::if_else(Los < 1440, TRUE, FALSE)
    ) %>%
    dplyr::filter(Discharged_24hr == TRUE | Los <= 1440)


  arrivals <- dt_calc %>%
    dplyr::group_by(time_hr = lubridate::floor_date(spell_start, "1 hour")) %>%
    dplyr::select(spell_number, time_hr, flow_groups, Los) %>%
    dplyr::arrange(time_hr) %>%
    tidyr::drop_na() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Hour = lubridate::hour(time_hr)) %>%
    dplyr::group_by(Hour, flow_groups) %>%
    dplyr::summarise(average_arrivals = mean(Los))

  df_time_hr <- tibble::tibble(
    Hour = rep(seq(from = 0, to = 23), 4),
    flow_groups = c(
      rep("Flow 1", 24),
      rep("Flow 2", 24),
      rep("Flow 3", 24),
      rep("Flow 4", 24)
    )
  )

  df_padded <- df_time_hr %>%
    dplyr::left_join(arrivals, by = c("Hour", "flow_groups")) %>%
    tidyr::replace_na(list(average_arrivals = 0)) %>%
    dplyr::ungroup()

  # Set the title
  title_stub <- " hospital: Average ED length of stay, by Hour of the arrival, "
  start_date_title <- format(as.Date(startDate), format = "%d %B %Y")
  end_date_title <- format(as.Date(endDate), format = "%d %B %Y")
  chart_title <- paste0(hospitalName, title_stub, start_date_title, " to ", end_date_title)

  plt_average_flows <- ggplot2::ggplot(data = df_padded, ggplot2::aes(x = as.numeric(Hour), y = average_arrivals, fill = flow_groups)) +
    ggplot2::geom_line(ggplot2::aes(color = flow_groups), size = 0.5) +
    ggplot2::geom_point(ggplot2::aes(shape = flow_groups), size = 1.0) +
    ggplot2::scale_y_continuous(limits = c(0, NA), breaks = seq(0, round(max(df_padded$average_arrivals)), by = 60)) +
    ggplot2::scale_x_continuous(breaks = 0:23, expand = c(0, 0.2)) +
    ggplot2::scale_shape_manual(values = c(7, 6, 5, 4)) +
    ggplot2::scale_color_manual(values = c("blue", "red", "green", "purple")) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = chart_title,
      subtitle = "\nNote: results are intended for management information only",
      y = "Average length of stay (mins)", x = "Hour of the day", caption = "Source: CLAHRC NWL"
    ) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 21, b = 0, l = 0)),
      plot.title = ggplot2::element_text(size = 12, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10),
      legend.position = "bottom", legend.box = "horizontal"
    )

  # Return
  if (returnPlot == TRUE) {
    plt_average_flows
  } else {
    plt_average_flows$data %>% dplyr::select(Hour, flow_groups, average_arrivals)
  }
}
