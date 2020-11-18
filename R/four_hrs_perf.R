#' four hour performance for ED
#'
#' @param start_dt datetime, as a POSIXct, of the earliest attendance to be included in the analysis
#' @param end_date datetime, as a POSIXct, of the latest attendance to be included in the analysis
#' @param data hospital episode data with at least the following fields:
#' Attendances - the attendances date and time;
#' Discharges - the discharge date and time;
#' Pseudo id - the patient pseudo id;
#' @param time_unit the unit of aggregation for 4 hour performance percentages
#' in the same format as lubridate::round_date
#' @param plot_chart if TRUE return chart, otherwise if FALSE return dataframe
#' @param hospital_name the hospital name for which the analysis are undertaken
#'
#' @return Chart or dataframe showing 4 hour performance at the
#' aggregation unit chosen
#' @export
#'
#' @examples
four_hrs_perf <- function(start_dt,
                          end_dt,
                          data, time_unit = "day", plot_chart, hospital_name = "Hospital_Name"){

  dt_select <- data %>%
    dplyr::select(pseudo_id, start_datetime, end_datetime) %>%
    dplyr::filter(start_datetime <= end_dt & start_datetime >= start_dt)


  # calculating total time by using difftime ####
  # length of stay for the emergency department #
  ##########################################################
  # creating a new variable - under 4 hrs and above 4 hrs ##

  dt_los <-  dt_select  %>%
    dplyr::mutate(Los = difftime(end_datetime, start_datetime, units = c("min")),
                  Hr_perf = dplyr::case_when(
                    Los <= 240 ~ "under_4hrs",
                    Los >= 240 ~ "above_4hrs"),
                  Time = lubridate::round_date(start_datetime, unit = time_unit))%>%
    dplyr::select(pseudo_id, Los, Time, Hr_perf)

  sum_4hrs_perf <- dt_los %>%
    dplyr::group_by(Time, Hr_perf) %>%
    dplyr::summarise(Count = dplyr::n()) %>%
    tidyr::drop_na() %>%
    tidyr::spread(Hr_perf, Count) %>%
    dplyr::mutate(N = under_4hrs + above_4hrs) %>%
    tidyr::drop_na()


  # Set the title
  title_stub <- ": daily 4 hr emergency access performance, "
  hospital_name <- "Hospital_name"
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

  pct <- qicharts2::qic(Time, under_4hrs, n = N, data = sum_4hrs_perf, chart = 'pp', ylab = "percent",
                        show.grid = TRUE, multiply= 100)

  pct$data$x <- as.Date(pct$data$x)
  cht_data <- add_rule_breaks(pct$data)
  pct <- ggplot2::ggplot(cht_data, ggplot2::aes(x, y, label = x))
  cutoff <- data.frame(yintercept= 95, cutoff=factor(95))

  #get time zone of data
  time_zone <- attr(data$spell_start, "tzone")

  #convert arguments to dates and round to nearest quarter
  st.dt <- as.Date(start_dt, format = "%Y-%m-%d", tz = time_zone)
  ed.dt <- as.Date(end_dt, format = "%Y-%m-%d", tz = time_zone)
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

  four_hr_plot

  if(plot_chart == TRUE){

    four_hr_plot

  }else{

   four_hr_plot$data %>% tibble::as_tibble()

  }

}
