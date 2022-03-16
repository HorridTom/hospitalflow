#' Weekly attendance of emergency department by flow groups
#'
#' @description
#' \code{plot_ed_attendances_timeseries_flow} shows weekly attendance of emergency
#' department by different flow groups.
#'
#' @param data Hospital episode data.
#' @param startDate The first date of the period for which the analysis will run.
#' @param endDate The last date of the period for which the analysis will run.
#' @param timeUnit TBD
#' @param returnPlot Plots the graph if set to TRUE, returns a dataframe otherwise.
#' @param hospitalName The name of the hospital for which the analysis is being
#' done.
#' @param restrictPlotRange A boolean indicating whether to zoom the plot to only
#' the range specified by \code{startDate} and \code{endDate}.
#'
#' @return A lineplot (default) or a dataframe showing the distribution of ED
#' attendances for different flow groups.
#'
#' @examples
#' \dontrun{
#' TBD
#' }
#' @export

plot_ed_attendances_timeseries_flow <- function(data,
                                                startDate,
                                                endDate,
                                                timeUnit = "week",
                                                returnPlot = TRUE,
                                                hospitalName = "{hospital_name}",
                                                restrictPlotRange = TRUE) {

  # get time zone of data
  time_zone <- attr(data$spell_start, "tzone")

  # set input dates to have the same time zone as the data
  start_date <- as.POSIXct(startDate, tz = time_zone)
  end_date <- as.POSIXct(endDate, tz = time_zone)

  # selecting the date and creating the flow groups
  dt_select <- data %>%
    dplyr::filter(spell_start <= end_date | initial_ed_end_datetime >= start_date) %>%
    dplyr::arrange(spell_start)

  dt_select <- make_flow_groups(dt_select)


  # calculating total time by using difftime ####
  # length of stay for the emergency department #
  ##########################################################
  # creating a new variable - under 4 hrs and above 4 hrs ##

  dt_los <- dt_select %>%
    dplyr::filter(starts_with_ed == TRUE) %>%
    dplyr::arrange(spell_start) %>%
    dplyr::mutate(Time = lubridate::round_date(spell_start, unit = timeUnit)) %>%
    dplyr::select(pseudo_id, Time, directorate, flow_groups, starts_with_ed)

  count_df <- dt_los %>%
    dplyr::group_by(Time, flow_groups) %>%
    dplyr::tally() %>%
    tidyr::drop_na()


  # Set the title
  title_stub <- ": Weekly unscheduled ED attendance trends, "
  start_date_title <- format(as.Date(start_date), format = "%d %B %Y")
  end_date_title <- format(as.Date(end_date), format = "%d %B %Y")
  chart_title <- paste0(hospitalName, title_stub, start_date_title, " to ", end_date_title)


  if (restrictPlotRange) {
    plot_x_lims <- c(
      as.POSIXct(start_date, tz = time_zone),
      as.POSIXct(end_date, tz = time_zone)
    )
  } else {
    plot_x_lims <- c(
      min(count_df %>% dplyr::pull(Time)),
      max(count_df %>% dplyr::pull(Time))
    )
  }

  # plot the admissions and discharges (non-emergency appears as well )
  plot_weekly_att <- ggplot2::ggplot(count_df, ggplot2::aes(x = Time, y = n, group = flow_groups, fill = flow_groups)) + # shape = Event,  colour = Event
    ggplot2::geom_line(ggplot2::aes(color = flow_groups), size = 1.0) +
    ggplot2::geom_point(ggplot2::aes(shape = flow_groups), size = 1.0) +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::scale_x_datetime(
      labels = scales::date_format("%d-%m-%Y"),
      breaks = scales::date_breaks(timeUnit),
      limits = plot_x_lims
    ) +
    ggplot2::scale_shape_manual(values = c(7, 6, 5, 4)) +
    ggplot2::scale_color_manual(values = c("blue", "red", "green", "purple")) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = chart_title,
      subtitle = "Weekly unscheduled ED attendance, by patient flow group, n
                  \nNote: Results are intended for management information only",
      y = "Count", caption = "Source: CLAHRC NWL"
    ) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 21, b = 0, l = 0)),
      plot.title = ggplot2::element_text(size = 10, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 9),
      legend.position = "bottom", legend.box = "horizontal"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = "bottom", legend.box = "horizontal"
    )

  plot_weekly_att

  if (returnPlot == TRUE) {
    plot_weekly_att
  } else {
    plot_weekly_att$data %>% dplyr::select(Time, flow_groups, n)
  }
}
