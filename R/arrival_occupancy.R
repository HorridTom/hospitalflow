#' ae_arrival_occupancy
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
ae_arrival_occupancy <- function(start_date = as.POSIXct("2012-01-01 00:00:00"),
                                     end_date = as.POSIXct("2015-01-01 00:00:00"),
                                     data, plot_chart, hospital_name = "Hospital name",
                                     config_path = "lgt-config/ed"){

  #get time zone from config file - assumed that time zone is the same for all time variables
  datetime_formats <- readRDS(file.path(config_path, "datetime_formats.rds"))
  time_zone <- datetime_formats$time_zone[1]

  start_date <- as.POSIXct(start_date, tz = time_zone)
  end_date <- as.POSIXct(end_date, tz = time_zone)

  dt_los <- data %>%
    dplyr::select(spell_number, spell_start, initial_ed_end_datetime, spell_class_col, starts_with_ed) %>%
    dplyr::mutate(Same_day_discharge = as.numeric(difftime(initial_ed_end_datetime, spell_start, unit = c("min")))) %>%
    dplyr::filter(starts_with_ed == TRUE)

  dt_calc <- dt_los %>%
    dplyr::mutate(Same_day_discharge = dplyr::if_else(as.Date(spell_start) == as.Date(initial_ed_end_datetime),  TRUE, FALSE),
                  Los = as.numeric(difftime(initial_ed_end_datetime, spell_start, unit = c("hour"))),
                  Discharged_24hr = dplyr::if_else(Los < 24, TRUE, FALSE)) %>%
    dplyr::filter(Discharged_24hr == TRUE & Los <= 24) %>%
    dplyr::filter(spell_start > start_date | initial_ed_end_datetime < end_date)


  # using gather function to create a new column with date
  arrivals <-  dt_calc %>%
    tidyr::gather(key = type, time, spell_start:initial_ed_end_datetime) %>%
    dplyr::mutate(change = dplyr::if_else(type == "spell_start", 1, -1)) %>%
    dplyr::group_by(time_hr = lubridate::floor_date(time, "1 hour")) %>%
    dplyr::summarise(
      arrivals = sum(type == "spell_start")) %>%
    padr::pad(start_val = start_date, end_val = end_date) %>%
    tidyr::replace_na(list(arrivals = 0)) %>%
    tidyr::drop_na()


  time_hr <- seq(from = start_date, to = end_date, by = "hour")

  occupancy_vct <- sapply(time_hr, occupancy, df = dt_calc, start_time = "spell_start", end_time = "initial_ed_end_datetime")

  occupancy_df <- tibble::tibble(time_hr, occupancy_vct)

  arrival_occupancy <- dplyr::left_join(arrivals, occupancy_df, by = c("time_hr"))


  avg_arriv_occup <- arrival_occupancy  %>%
    dplyr::mutate(Hour = lubridate::hour(time_hr)) %>%
    dplyr::group_by(Hour) %>%
    dplyr::summarize(Average_arrivals = mean(arrivals),
                     Average_occupancy = mean(occupancy_vct))

  # Set the title
  title_stub <- " hospital: Hourly A&E arrival & occupancy profile, "
  start_date_title <- format(as.Date(start_date), format = "%d %B %Y")
  end_date_title <- format(as.Date(end_date), format = "%d %B %Y")
  chart_title <- paste0(hospital_name, title_stub, start_date_title, " to ", end_date_title)

  plt_occ_percent <- ggplot2::ggplot(data = avg_arriv_occup, ggplot2::aes(x = as.numeric(Hour), y = Average_occupancy, group = Hour))+
    ggplot2::geom_bar(stat = "identity", alpha=0.7, width = 0.40, ggplot2::aes(fill = "Occupancy")) +
    ggplot2::scale_x_continuous(breaks = 0:23, expand = c(0, 0.2)) + #, expand = c(0, 0)
    #ggplot2::scale_y_continuous(expand = c(0, 0)) + # breaks = c(0, 5, 10, 15, 20, 25))
    ggplot2::geom_point(ggplot2::aes(y = Average_arrivals)) +
    ggplot2::geom_line(ggplot2::aes(y = Average_arrivals, group = 1, color = "Arrivals in A&E")) +
    ggplot2::scale_fill_manual("",values="yellow4") +
    ggplot2::scale_color_manual("",values = 1) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = chart_title,
                  subtitle = "Averages - Hourly ED Occupancy, % , by Hour of the day. \nNote: results are intended for management information only",
                  y = "Average Count", x = "Hour of the day", caption = "Source: CLAHRC NWL") +
    ggplot2::theme(axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 21, b = 0, l = 0)),
                   plot.title = ggplot2::element_text(size = 12, face = "bold"),
                   plot.subtitle = ggplot2::element_text(size = 10),
                   legend.position = "bottom", legend.box = "horizontal")

  plt_occ_percent

  #################################################################################################################


  if(plot_chart == TRUE){

    plt_occ_percent

  }else{

    plt_occ_percent$data %>% dplyr::select(Hour, Average_arrivals , Average_occupancy)

  }

}

