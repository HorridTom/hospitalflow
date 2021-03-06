#' four hrs perf flow_groups
#'
#' @param start_dt
#' @param end_dt
#' @param data
#' @param time_unit
#' @param plot_chart
#' @param hospital_name
#'
#' @return
#' @export
#'
#' @examples
four_hrs_perf_flow_groups <- function(start_dt, end_dt, data, time_unit = "day",
                                      plot_chart, hospital_name = "Hospital_Name"){

  #get time zone of data
  time_zone <- attr(data$spell_start, "tzone")

  #set input dates to have the same time zone as the data
  start_dt <- as.Date(start_dt, tz = time_zone)
  end_dt <- as.Date(end_dt, tz = time_zone)

  dt_select <- data %>%
    dplyr::filter(spell_start < end_dt | initial_ed_end_datetime > start_dt) %>%
    dplyr::arrange(spell_start)

  dt_flow_grps <- make_flow_groups(dt_select)


  # calculating total time by using difftime ####
  # length of stay for the emergency department #
  ##########################################################
  # creating a new variable - under 4 hrs and above 4 hrs ##

  dt_los <-  dt_flow_grps %>%
    dplyr::filter(starts_with_ed == TRUE) %>%
    dplyr::arrange(spell_start) %>%
    dplyr::mutate(Los = difftime(initial_ed_end_datetime, spell_start, units = c("min")),
                  Hr_perf = dplyr::case_when(
                    Los <= 240 ~ "under_4hrs",
                    Los >= 240 ~ "above_4hrs"),
                  Time = lubridate::round_date(spell_start, unit = time_unit)) %>%
    dplyr::select(pseudo_id, Los, Time, Hr_perf, flow_groups)

  sum_4hrs_perf <- dt_los %>%
    dplyr::group_by(Time, Hr_perf, flow_groups) %>%
    dplyr::summarise(Count = dplyr::n()) %>%
    tidyr::drop_na() %>%
    tidyr::spread(Hr_perf, Count) %>%
    dplyr::mutate(N = under_4hrs + above_4hrs) %>%
    tidyr::drop_na()

  sum_4hrs_perf_flow_1 <- sum_4hrs_perf %>%
    dplyr::filter(flow_groups == "Flow 1")

  sum_4hrs_perf_flow_2 <- sum_4hrs_perf %>%
    dplyr::filter(flow_groups == "Flow 2")

  sum_4hrs_perf_flow_3 <- sum_4hrs_perf %>%
    dplyr::filter(flow_groups == "Flow 3")

  sum_4hrs_perf_flow_4 <- sum_4hrs_perf %>%
    dplyr::filter(flow_groups == "Flow 4")


  # Set the title
  title_stub <- ": daily 4 hr emergency access performance \n"
  start_date_title <- format(as.Date(start_dt), format = "%d %B %Y")
  end_date_title <- format(as.Date(end_dt), format = "%d %B %Y")
  chart_title <- paste0(hospital_name, title_stub, start_date_title, " to ", end_date_title)

  # function to plot the 4 hrs emergency performance
  # Plot all days - see Tom's AE APP

  ## Version 0.6.0 of qicharts2 still appears to suffer from
  ## the issue described here: https://github.com/anhoej/qicharts2/issues/21
  ## although it looks like there is a fix. Once that fix is
  ## integrated into a CRAN release, this code should be removed. ***
  options(qic.linecol   = '#5DA5DA',
          qic.signalcol = '#F15854',
          qic.targetcol = '#059748',
          qic.clshade   = TRUE)
  ## ***

  pct_flow_1 <- qicharts2::qic(Time, under_4hrs, n = N, data = sum_4hrs_perf_flow_1, chart = 'pp', ylab = "percent",
                        show.grid = TRUE, multiply= 100)

  pct_flow_2 <- qicharts2::qic(Time, under_4hrs, n = N, data = sum_4hrs_perf_flow_2, chart = 'pp', ylab = "percent",
                               show.grid = TRUE, multiply= 100)

  pct_flow_3 <- qicharts2::qic(Time, under_4hrs, n = N, data = sum_4hrs_perf_flow_3, chart = 'pp', ylab = "percent",
                        show.grid = TRUE, multiply= 100)

  pct_flow_4 <- qicharts2::qic(Time, under_4hrs, n = N, data = sum_4hrs_perf_flow_4, chart = 'pp', ylab = "percent",
                               show.grid = TRUE, multiply= 100)

  pct_flow_1$data$x <- as.Date(pct_flow_1$data$x, tz = time_zone)
  cht_data_flow_1 <- add_rule_breaks(pct_flow_1$data)
  pct_flow_1 <- ggplot2::ggplot(cht_data_flow_1, ggplot2::aes(x, y, label = x))
  cutoff <- data.frame(yintercept= 95, cutoff=factor(95))

  pct_flow_2$data$x <- as.Date(pct_flow_2$data$x, tz = time_zone)
  cht_data_flow_2 <- add_rule_breaks(pct_flow_2$data)
  pct_flow_2 <- ggplot2::ggplot(cht_data_flow_2, ggplot2::aes(x, y, label = x))
  cutoff <- data.frame(yintercept= 95, cutoff=factor(95))

  pct_flow_3$data$x <- as.Date(pct_flow_3$data$x, tz = time_zone)
  cht_data_flow_3 <- add_rule_breaks(pct_flow_3$data)
  pct_flow_3 <- ggplot2::ggplot(cht_data_flow_3, ggplot2::aes(x, y, label = x))
  cutoff <- data.frame(yintercept= 95, cutoff=factor(95))

  pct_flow_4$data$x <- as.Date(pct_flow_4$data$x, tz = time_zone)
  cht_data_flow_4 <- add_rule_breaks(pct_flow_4$data)
  pct_flow_4 <- ggplot2::ggplot(cht_data_flow_4, ggplot2::aes(x, y, label = x))
  cutoff <- data.frame(yintercept= 95, cutoff=factor(95))

  #ensure passed arguments are dates
  st.dt <- as.Date(start_dt, format = "%Y-%m-%d", tz = time_zone)
  ed.dt <- as.Date(end_dt, format = "%Y-%m-%d", tz = time_zone)
  #q.st.dt <- as.Date(zoo::as.yearqtr(st.dt, format = "%Y-%m-%d"))
  #q.ed.dt <- as.Date(zoo::as.yearqtr(ed.dt, format = "%Y-%m-%d"), frac = 1) + 1

  cht_axis_breaks <- seq(st.dt, ed.dt, by = "quarters")
  ylimlow_flow_1 <- min(min(pct_flow_1$data$y, na.rm = TRUE),min(pct_flow_1$data$lcl, na.rm = TRUE))
  ylimlow_flow_2 <- min(min(pct_flow_2$data$y, na.rm = TRUE),min(pct_flow_2$data$lcl, na.rm = TRUE))
  ylimlow_flow_3 <- min(min(pct_flow_3$data$y, na.rm = TRUE),min(pct_flow_3$data$lcl, na.rm = TRUE))
  ylimlow_flow_4 <- min(min(pct_flow_4$data$y, na.rm = TRUE),min(pct_flow_3$data$lcl, na.rm = TRUE))
  ylimlow <- min(ylimlow_flow_1, ylimlow_flow_2, ylimlow_flow_3, ylimlow_flow_4)

  plot_theme <- list(
    ggplot2::geom_hline(ggplot2::aes(yintercept = yintercept, linetype = cutoff),
                        data = cutoff, colour = "#00BB00", linetype = 1),
    ggplot2::scale_x_date(date_breaks = "1 month", labels = scales::date_format("%Y-%m-%d"),
                          breaks = cht_axis_breaks, limits = c(st.dt, ed.dt)),
    ggplot2::annotate("text", ed.dt - 90, 95, vjust = -2, label = "95% Target", colour = "#00BB00"),
    ggplot2::theme(plot.title = ggplot2::element_text(size = 12, face = "bold")),
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, size = 8),
                   axis.text.y = ggplot2::element_text(size = 8),
                   axis.title = ggplot2::element_text(size=8)),
    ggplot2::labs(x = "Month", y = "Percentage within 4 hours"),
    ggplot2::ylim(ylimlow, 100)
  )

    #+
  #ggplot2::geom_text(ggplot2::aes(label = ifelse(x==max(x), format(x, '%b-%y'),'')), hjust = -0.05, vjust = 2)

  four_hr_plot_flow_1 <- format_control_chart(pct_flow_1, r1_col = "orange", r2_col = "steelblue", main_col = "blue") +
    ggplot2::ggtitle("Flow 1") +
    plot_theme

  four_hr_plot_flow_2 <- format_control_chart(pct_flow_2, r1_col = "orange", r2_col = "steelblue", main_col = "red") +
    ggplot2::ggtitle("Flow 2") +
    plot_theme

  four_hr_plot_flow_3 <- format_control_chart(pct_flow_3, r1_col = "orange", r2_col = "steelblue", main_col = "green") +
    ggplot2::ggtitle("Flow 3") +
    plot_theme

  four_hr_plot_flow_4 <- format_control_chart(pct_flow_4, r1_col = "orange", r2_col = "steelblue", main_col = "purple") +
    ggplot2::ggtitle("Flow 4") +
    plot_theme

  #caption = paste("*Shewart chart rules apply: \nRule 1: Any month outside the control limits\nRule 2: Eight or more consecutive months all above, or all below, the centre line", sep = "")

  #text.p <- ggparagraph(text = caption, face = "italic", size = 11, color = "black")

    flow_groups_plot <- ggpubr::ggarrange(four_hr_plot_flow_1, four_hr_plot_flow_2, four_hr_plot_flow_3, four_hr_plot_flow_4,
                                ncol = 2, nrow = 2, legend = "bottom", common.legend = TRUE)


  if(plot_chart == TRUE){

    flow_groups_plot

  }else{

    sum_4hrs_perf

  }



}



#' format_control_chart
#'
#' @param cht ggplot chart object
#' @param r1_col colour to highlight rule 1 breaks
#' @param r2_col colour to highlight rule 2 breaks
#' @param main_col colour without highlight
#'
#' @return chart with formatting applied
#'
#' @examples
format_control_chart <- function(cht, r1_col, r2_col, main_col = "black") {

  point_colours <- c("Rule 1" = r1_col, "Rule 2" = r2_col, "None" = main_col)
  cht +
    ggplot2::geom_line(colour = main_col, size = 0.5) +
    ggplot2::geom_line(ggplot2::aes(x,cl), size = 0.75) +
    ggplot2::geom_line(ggplot2::aes(x,ucl), size = 0.75, linetype = 2) +
    ggplot2::geom_line(ggplot2::aes(x,lcl), size = 0.75, linetype = 2) +
    ggplot2::geom_point(ggplot2::aes(colour = highlight), size = 1.25) +
    ggplot2::scale_color_manual("Rule triggered", values = point_colours) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(colour = "grey80"),
                   panel.grid.minor = ggplot2::element_blank(), panel.background = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1.0, size = 14),
                   axis.text.y = ggplot2::element_text(size = 14), axis.title = ggplot2::element_text(size = 14),
                   plot.title = ggplot2::element_text(size = 20, hjust = 0),
                   plot.subtitle = ggplot2::element_text(size = 16, face = "italic"),
                   axis.line = ggplot2::element_line(colour = "grey60"),
                   plot.caption = ggplot2::element_text(size = 10, hjust = 0.5))

}
