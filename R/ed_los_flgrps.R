#' ed_los_flow_grps
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
ed_los_flow_grps <- function(start_date, end_date, data, plot_chart, hospital_name){

  dt_select <- data %>%
    dplyr::filter(spell_start < end_date | initial_ed_end_datetime > start_date)

  dt_flow_grps <- make_flow_groups(dt_select)

  #subseting data set
  dt <-  dt_flow_grps %>%
    dplyr::mutate(
      admitted_date = as.Date(spell_start),
      discharge_date = as.Date(initial_ed_end_datetime),
      same_day_discharge = dplyr::if_else(admitted_date == discharge_date, TRUE, FALSE),
      adm_period = dplyr::if_else(spell_start > start_date, TRUE, FALSE)
    )


  # calculating total time by using difftime - length of stay
  # getting rid of the same day non-emergency

  dt_los <- dt %>%
    dplyr::mutate(length_of_stay = as.numeric(difftime(initial_ed_end_datetime, spell_start, units = c("min")))) %>% # or minutes?
    dplyr::select(spell_number, spell_start, initial_ed_end_datetime, flow_groups, length_of_stay) %>%
    dplyr::mutate(Time_binned = dplyr::case_when(length_of_stay <= 14 ~ '00:00',
                                               length_of_stay == 15 | length_of_stay <= 29 ~ '00:15',
                                               length_of_stay == 30 | length_of_stay <= 44 ~ '00:30',
                                               length_of_stay == 45 | length_of_stay <= 59 ~ '00:45',
                                               length_of_stay == 60 | length_of_stay <= 74 ~ '1:00',
                                               length_of_stay == 75 | length_of_stay <= 89 ~ '1:15',
                                               length_of_stay == 90 | length_of_stay <= 104 ~ '1:30',
                                               length_of_stay == 105 | length_of_stay <= 119 ~ '1:45',
                                               length_of_stay == 120 | length_of_stay <= 134 ~ '2:00',
                                               length_of_stay == 135 | length_of_stay <= 149 ~ '2:15',
                                               length_of_stay == 150 | length_of_stay <= 164 ~ '2:30',
                                               length_of_stay == 165 | length_of_stay <= 179 ~ '2:45',
                                               length_of_stay == 180 | length_of_stay <= 194 ~ '3:00',
                                               length_of_stay == 195 | length_of_stay <= 209 ~ '3:15',
                                               length_of_stay == 210 | length_of_stay <= 224 ~ '3:30',
                                               length_of_stay == 225 | length_of_stay <= 239 ~ '3:45',
                                               length_of_stay == 240 | length_of_stay <= 254 ~ '4:00',
                                               length_of_stay == 255 | length_of_stay <= 269 ~ '4:15',
                                               length_of_stay == 270 | length_of_stay <= 284 ~ '4:30',
                                               length_of_stay == 285 | length_of_stay <= 299 ~ '4:45',
                                               length_of_stay == 300 | length_of_stay <= 314 ~ '5:00',
                                               length_of_stay == 315 | length_of_stay <= 329 ~ '5:15',
                                               length_of_stay == 330 | length_of_stay <= 344 ~ '5:30',
                                               length_of_stay == 345 | length_of_stay <= 359 ~ '5:45',
                                               length_of_stay == 360 | length_of_stay <= 374 ~ '6:00',
                                               length_of_stay == 375 | length_of_stay <= 389 ~ '6:15',
                                               length_of_stay == 390 | length_of_stay <= 404 ~ '6:30',
                                               length_of_stay == 405 | length_of_stay <= 414 ~ '6:45',
                                               length_of_stay == 415 | length_of_stay <= 429 ~ '7:00',
                                               length_of_stay == 430 | length_of_stay <= 444 ~ '7:15',
                                               length_of_stay == 445 | length_of_stay <= 459 ~ '7:30',
                                               length_of_stay == 460 | length_of_stay <= 474 ~ '7:45',
                                               length_of_stay == 475 | length_of_stay <= 720 ~ '8-12 hrs',
                                               length_of_stay >= 721 ~ '> 12 hrs'))

  dt_los_flgrps <- dt_los %>%
    dplyr::group_by(Time_binned, flow_groups) %>%
    dplyr::summarize("Count" = dplyr::n()) %>%
    tidyr::drop_na()


  # # Set the title
  title_stub <- " ED LoS distribution, "
  start_date_title <- format(as.Date(start_date), format = "%d %B %Y")
  end_date_title <- format(as.Date(end_date), format = "%d %B %Y")
  chart_title <- paste0(hospital_name, title_stub, start_date_title, " to ", end_date_title)

  # Plotting the lenght of stay by the ward  - check for dual axis graphs

  plot_ed_los_flgrps <- ggplot2::ggplot(dt_los_flgrps, ggplot2::aes(x = Time_binned, y = Count, group = flow_groups,  fill = flow_groups)) +
    ggplot2::geom_line(ggplot2::aes(color = flow_groups), size = 1.0) +
    ggplot2::geom_point(ggplot2::aes(shape = flow_groups), size = 1.0) +
    ggplot2::scale_y_continuous(limits = c(0,NA)) +
    ggplot2::scale_shape_manual(values = c(7, 6, 5, 4)) +
    ggplot2::scale_color_manual(values=c("blue", "red", "green", "purple")) +
    ggplot2::theme_bw() +
    ggplot2::xlim("00:00", "00:15", "00:30", "00:45",
                  "1:00", "1:15", "1:30", "1:45",
                  "2:00", "2:15", "2:30", "2:45",
                  "3:00", "3:15", "3:30", "3:45",
                  "4:00", "4:15", "4:30", "4:45",
                  "5:00", "5:15", "5:30", "5:45",
                  "6:00", "6:15", "6:30", "6:45",
                  "7:00", "7:15", "7:30", "7:45",
                  "8-12 hrs", "> 12 hrs") +
    ggplot2::labs(title = chart_title,
                  subtitle = "Weekly unscheduled ED attendance, by patient flow group, n
                  \nNote: Results are intended for management information only",
                  y = "ED attendances, n", caption = "Source: CLAHRC NWL") +
    ggplot2::theme(axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 21, b = 0, l = 0)),
                   plot.title = ggplot2::element_text(size = 10, face = "bold"),
                   plot.subtitle = ggplot2::element_text(size = 9),
                   legend.position = "bottom", legend.box = "horizontal") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1),
                   legend.position = "bottom", legend.box = "horizontal" )

  plot_ed_los_flgrps

  if(plot_chart == TRUE){

    plot_ed_los_flgrps


  }else{

    plot_ed_los_flgrps$data %>%
      dplyr::select(Time_binned,flow_groups, Count) %>%
      dplyr::ungroup()

  }

}

