#' Title
#'
#' @param start_date
#' @param end_date
#' @param data
#' @param plot_chart
#' @param hospital_name
#' @param config_path path to the ed config folder
#'
#' @return
#' @export
#'
#' @examples
average_ed_los_flows <- function(start_date = as.POSIXct("2012-01-01 00:00:00"),
         end_date = as.POSIXct("2015-01-01 00:00:00"),
         data, plot_chart, hospital_name = "Hospital name",
         config_path = "lgt-config/ed"){

  #get time zone from config file - assumed that time zone is the same for all time variables
  datetime_formats <- readRDS(file.path(config_path, "datetime_formats.rds"))
  time_zone <- datetime_formats$time_zone[1]

  start_date <- as.POSIXct(start_date, tz = time_zone)
  end_date <- as.POSIXct(end_date, tz = time_zone)

  dt_los <- data %>%
    dplyr::select(spell_number, spell_start, initial_ed_end_datetime, starts_with_ed,disposal_code,
                  ed_non_adm,ed_admission, hrg_ae_code, main_specialty_start, source_referral_ae) %>%
    dplyr::filter(spell_start > start_date | initial_ed_end_datetime < end_date) %>%
    dplyr::filter(starts_with_ed == TRUE | ed_non_adm == TRUE)

  dt_flows <- make_flow_groups(dt_los)

  dt_calc <- dt_flows %>%
    dplyr::mutate(Same_day_discharge = dplyr::if_else(as.Date(spell_start) == as.Date(initial_ed_end_datetime),  TRUE, FALSE),
                  Los = as.numeric(difftime(initial_ed_end_datetime, spell_start, unit = c("min"))),
                  Discharged_24hr = dplyr::if_else(Los < 1440, TRUE, FALSE)) %>%
    dplyr::filter(Discharged_24hr == TRUE | Los <= 1440)


  # using gather function to create a new column with date
  arrivals <-  dt_calc %>%
    dplyr::group_by(time_hr = lubridate::floor_date(spell_start, "1 hour")) %>%
    dplyr::select(spell_number, time_hr, flow_groups, Los) %>%
    dplyr::arrange(time_hr) %>%
    tidyr::drop_na() %>%
    tidyr::spread(flow_groups, Los) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Hour = lubridate::hour(time_hr)) %>%
    dplyr::group_by(Hour) %>%
    dplyr::summarise('Flow 1' = mean(`Flow 1`, na.rm =  TRUE),
                     'Flow 2' = mean(`Flow 2`, na.rm = TRUE),
                     'Flow 3' = mean(`Flow 3`, na.rm = TRUE),
                     'Flow 4' = mean(`Flow 4`, na.rm = TRUE)) %>%
    naniar::replace_with_na(replace = list('Flow 1' = NaN, 'Flow 2' = NaN, 'Flow 3' = NaN, 'Flow 4' = NaN))



  df_time_hr <- tibble::tibble(Hour = seq(from = 0 , to = 23))

  df_padded <- df_time_hr %>%
    dplyr::left_join(arrivals) %>%
    tidyr::gather(key = flow_groups, value = average_arrivals, 'Flow 1', 'Flow 2', 'Flow 3', 'Flow 4') %>%
    tidyr::replace_na(list(average_arrivals = 0)) %>%
    dplyr::ungroup() %>%
    tidyr::drop_na()


  # Set the title
  title_stub <- " hospital: Average ED length of stay, by Hour of the arrival, "
  start_date_title <- format(as.Date(start_date), format = "%d %B %Y")
  end_date_title <- format(as.Date(end_date), format = "%d %B %Y")
  chart_title <- paste0(hospital_name, title_stub, start_date_title, " to ", end_date_title)

  plt_average_flows <- ggplot2::ggplot(data =  df_padded, ggplot2::aes(x = as.numeric(Hour), y = average_arrivals,  fill = flow_groups))+
    ggplot2::geom_line(ggplot2::aes(color = flow_groups), size = 0.5) +
    ggplot2::geom_point(ggplot2::aes(shape = flow_groups), size = 1.0) +
    ggplot2::scale_y_continuous(limits = c(0,NA), breaks=seq(0, round(max(df_padded$average_arrivals)), by = 60)) +
    ggplot2::scale_x_continuous(breaks = 0:23, expand = c(0, 0.2)) +
    ggplot2::scale_shape_manual(values = c(7, 6, 5, 4)) +
    ggplot2::scale_color_manual(values = c("blue", "red", "green", "purple")) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = chart_title,
                  subtitle = "\nNote: results are intended for management information only",
                  y = "Average length of stay (mins)", x = "Hour of the day", caption = "Source: CLAHRC NWL") +
    ggplot2::theme(axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 21, b = 0, l = 0)),
                   plot.title = ggplot2::element_text(size = 12, face = "bold"),
                   plot.subtitle = ggplot2::element_text(size = 10),
                   legend.position = "bottom", legend.box = "horizontal")

  plt_average_flows

  #################################################################################################################


  if(plot_chart == TRUE){

    plt_average_flows

  }else{

    plt_average_flows$data %>% dplyr::select(Hour, flow_groups, average_arrivals)

  }

}
