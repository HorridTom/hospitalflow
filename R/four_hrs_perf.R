#' four hour performance for ED
#'
#' @param start_date
#' @param end_date
#' @param data
#' @param plot_chart
#' @param hospital_name
#'
#' @return
#' @export
#'
#' @examples
four_hrs_perf <- function(start_date = as.Date("2015-01-01", tz = "Europe/London"),
                          end_date = as.Date("2015-01-01", tz = "Europe/London"),
                          data, plot_chart, hospital_name = "Hospital_Name"){

  dt_select <- data %>%
    dplyr::select(pseudo_id, start_datetime, end_datetime) %>%
    dplyr::filter(start_datetime <= end_date & end_datetime >= start_date)


  # calculating total time by using difftime ####
  # length of stay for the emergency department #
  ##########################################################
  # creating a new variable - under 4 hrs and above 4 hrs ##

  dt_los <-  dt_select  %>%
    dplyr::mutate(Los = difftime(end_datetime, start_datetime, units = c("min")),
                  Hr_perf = dplyr::case_when(
                    Los <= 240 ~ "under_4hrs",
                    Los >= 240 ~ "above_4hrs"),
                  Time = as.Date(start_datetime),
                  One_week = lubridate::round_date(Time, "7 days")) %>%
    dplyr::select(pseudo_id, Los, Time, One_week, Hr_perf)

  sum_4hrs_perf <- dt_los %>%
    dplyr::group_by(Time, Hr_perf) %>%
    dplyr::summarise(Count = n()) %>%
    tidyr::drop_na() %>%
    tidyr::spread(Hr_perf, Count) %>%
    dplyr::mutate(N = under_4hrs + above_4hrs) %>%
    tidyr::drop_na()


  # Set the title
  title_stub <- ": Number of A&E attendances, "
  hospital_name <- "Hospital_name"
  start_date_title <- format(as.Date(start_date), format = "%d %B %Y")
  end_date_title <- format(as.Date(end_date), format = "%d %B %Y")
  chart_title <- paste0(hospital_name, title_stub, start_date_title, " to ", end_date_title)


  # function to plot the 4 hrs emergency performance
  # Plot all days - see Tom's AE APP

  pct <- qicharts2::qic(Time, under_4hrs, n = N, data = sum_4hrs_perf, chart = 'p', ylab = "percent",
                        show.grid = TRUE, multiply= 100)

  pct$data$x <- as.Date(pct$data$x, tz = "Europe/London")
  cht_data <- add_rule_breaks(pct$data)
  pct <- ggplot2::ggplot(cht_data, ggplot2::aes(x, y, label = x))
  cutoff <- data.frame(yintercept= 95, cutoff=factor(95))

  #convert arguments to dates and round to nearest quarter
  st.dt <- as.Date(start_date, format = "%Y-%m-%d", tz = "Europe/London")
  ed.dt <- as.Date(end_date, format = "%Y-%m-%d", tz = "Europe/London")
  #q.st.dt <- as.Date(zoo::as.yearqtr(st.dt, format = "%Y-%m-%d"))
  #q.ed.dt <- as.Date(zoo::as.yearqtr(ed.dt, format = "%Y-%m-%d"), frac = 1) + 1
  cht_axis_breaks <- seq(st.dt, ed.dt, by = "quarters")
  ylimlow <- min(min(pct$data$y, na.rm = TRUE),min(pct$data$lcl, na.rm = TRUE))

  four_hr_plot <- format_control_chart(pct, r1_col = "orange", r2_col = "steelblue") +
    ggplot2::geom_hline(ggplot2::aes(yintercept = yintercept, linetype = cutoff),
                        data = cutoff, colour = "#00BB00", linetype = 1) +
    ggplot2::scale_x_date(date_breaks = "1 month", labels = scales::date_format("%Y-%m-%d"),
                          breaks = cht_axis_breaks) + #limits = c(st.dt, ed.dt)
    ggplot2::annotate("text", ed.dt - 90, 95, vjust = -2, label = "95% Target", colour = "#00BB00") +
    ggplot2::ggtitle(chart_title) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 11, face = "bold")) +
    #ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, size = 10))
    ggplot2::labs(x = "Month", y = "Percentage within 4 hours",
         caption = "*Shewart chart rules apply (see Understanding the Analysis tab for more detail) \nRule 1: Any month outside the control limits \nRule 2: Eight or more consecutive months all above, or all below, the centre line", size = 10) +
    ggplot2::ylim(ylimlow, 100) +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(x==max(x), format(x, '%b-%y'),'')), hjust = -0.05, vjust = 2)


  if(plot_chart == TRUE){

    four_hr_plot

  }else{

    four_hr_plot$pct %>% dplyr::select(Time, above_4hrs, under_4hrs, N)

  }

}

format_control_chart <- function(cht, r1_col, r2_col) {

  point_colours <- c("Rule 1" = r1_col, "Rule 2" = r2_col, "None" = "black")
  cht +
    ggplot2::geom_line(colour = "black", size = 0.5) +
    ggplot2::geom_line(ggplot2::aes(x,cl), size = 0.75) +
    ggplot2::geom_line(ggplot2::aes(x,ucl), size = 0.75, linetype = 2) +
    ggplot2::geom_line(ggplot2::aes(x,lcl), size = 0.75, linetype = 2) +
    ggplot2::geom_point(ggplot2::aes(colour = highlight), size = 2) +
    ggplot2::scale_color_manual("Rule triggered*", values = point_colours) +
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
