#' occupancy_weekday_hour for A&E
#'
#' @param data
#' @param plot_chart
#'
#' @return
#' @export
#'
#' @examples
occupancy_weekday_hour <- function(start_date = as.Date("2015-04-01", tz = "Europe/London"),
                                   end_date = as.POSIXct("2016-04-01", tz = "Europe/London"),
                                   data, plot_chart, hospital_name = "Hospital name") {

  # renaming the variables I am interested in
  df_select <- data %>%
    dplyr::select(pseudo_id, episode_id, start_datetime, end_datetime, attendance_disposal) %>%
    dplyr::filter(start_datetime <= end_date, end_datetime >= start_date) %>%
    dplyr::filter(attendance_disposal == "Discharged")


  dt_los <- data %>%
    dplyr::select(pseudo_id, start_datetime, end_datetime) %>%
    dplyr::mutate(Same_day_discharge = as.numeric(difftime(end_datetime, start_datetime, unit = c("min"))))


  dt_calc <- dt_los %>%
    dplyr::mutate(Same_day_discharge = dplyr::if_else(as.Date(start_datetime) == as.Date(end_datetime),  TRUE, FALSE),
                  Los = as.numeric(difftime(start_datetime, end_datetime, unit = c("hour"))),
                  Discharged_24hr = dplyr::if_else(Los < 24, TRUE, FALSE)) %>%
    dplyr::filter(Discharged_24hr == TRUE & Los <= 24) %>%
    dplyr::filter(start_datetime <= end_date & end_datetime >= start_date)


  # using gather function to create a new column with date; and filter only by Emergency Department
  occupancy <-  occupancy_fct(start_date = start_date, end_date = end_date, date


  #subseting data set#

  #################################################################################################################


  dt_wday_hour_month <- occupancy %>%
    dplyr::mutate(month = lubridate::month(time_hr),
           Wday = lubridate::wday(time_hr, label = TRUE, abbr = TRUE),
           hour = lubridate::hour(time_hr)) %>%
    dplyr::select(time_hr, month, Wday, hour, occupancy_var)


  #######################################################################
  #######################################################################
  ########################################################################
  # getting the averages, min, max, interquartile ranges (Q1 and  Q3)
  tbl_avg_occ <- dt_wday_hour_month %>%
    dplyr::group_by(hour, Wday) %>%
    dplyr::summarize(average_occupancy = mean(occupancy_var),
                     Q1 = quantile(occupancy_var, 0.025),
                     Q3 = quantile(occupancy_var, 0.975),
                     Min_n = min(occupancy_var),
                     Max_n = max(occupancy_var)) %>%
    dplyr::ungroup()


  Weekdays <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  tbl_avg_occ_2 <- dplyr::arrange(transform(tbl_avg_occ, day = factor(Wday, levels = Weekdays)), Wday)

  # Set the title
  title_stub <- ": Attendances and Admissions by Age and Gender,\n"
  hospital_name <- "Chelsea & Westminster"
  start_date_title <- format(as.Date(start_date), format = "%d %B %Y")
  end_date_title <- format(as.Date(end_date), format = "%d %B %Y")
  chart_title <- paste0(hospital_name, title_stub, start_date_title, " to ", end_date_title)


  plt <- tbl_avg_occ_2 %>%
    ggplot2::ggplot(ggplot2::aes(x = hour)) +
    ggplot2::geom_line(ggplot2::aes(y = average_occupancy, color = "Average Occupancy"), size = 1) +
    ggplot2::scale_x_continuous(limits = c(0, 23)) +
    ggplot2::facet_grid( ~day ) +  #nrow = 1
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Q1, ymax = Q3, fill = "interquartile range"), alpha = "0.20") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Min_n, ymax = Max_n, fill = "range"), alpha = "0.30")+
    ggplot2::scale_colour_manual("", values = "red") +
    ggplot2::scale_fill_manual("", values = c("blue", "steel blue")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = chart_title,
                  subtitle = "Registered unscheduled A&E 'occupancy', patients; by Hour of the day and day of week. \nNote: results are intended for management information only",
                  y = "Average A&E occupancy", x = "Hour of the day", caption = "Source: CLAHRC NWL") +
    ggplot2::theme(axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 21, b = 0, l = 0)),
                   plot.title = ggplot2::element_text(size = 12, face = "bold"),
                   plot.subtitle = ggplot2::element_text(size = 10),
                   legend.position = "bottom", legend.box = "hoatizontal")

  plt

  if(plot_chart == TRUE){

    plt

  }else{

    plt$data %>% select(hour, Average_occ, Q1, Q3, Min_n, Max_n)

  }
}
