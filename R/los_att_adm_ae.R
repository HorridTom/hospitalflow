#' lenght of stay for attendances and admissions from ED
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
los_att_adm_ae <- function(start_date, end_date, data, plot_chart,
                           hospital_name = "Chelsea & Westminster"){

  df <- data %>%
    dplyr::select(spell_number, spell_start, spell_end, spell_class_col, initial_ed_end_datetime) %>%
    dplyr::filter(spell_start < end_date | spell_end > start_date) %>%
    dplyr::filter(spell_class_col == "ed_non_admission" | spell_class_col == "ed_comp_non_admission" | spell_class_col == "ed_admission") %>%
    tidyr::drop_na()

  df_recode <- df %>%
    dplyr::mutate(Time_spent = as.numeric(difftime(initial_ed_end_datetime, spell_start), unit = c("mins"))) %>%
    dplyr::mutate(Time_binned = dplyr::case_when(Time_spent <= 14 ~ '00:00',
                                                 Time_spent == 15 | Time_spent <= 29 ~ '00:15',
                                                 Time_spent == 30 | Time_spent <= 44 ~ '00:30',
                                                 Time_spent == 45 | Time_spent <= 59 ~ '00:45',
                                                 Time_spent == 60 | Time_spent <= 74 ~ '1:00',
                                                 Time_spent == 75 | Time_spent <= 89 ~ '1:15',
                                                 Time_spent == 90 | Time_spent <= 104 ~ '1:30',
                                                 Time_spent == 105 | Time_spent <= 119 ~ '1:45',
                                                 Time_spent == 120 | Time_spent <= 134 ~ '2:00',
                                                 Time_spent == 135 | Time_spent <= 149 ~ '2:15',
                                                 Time_spent == 150 | Time_spent <= 164 ~ '2:30',
                                                 Time_spent == 165 | Time_spent <= 179 ~ '2:45',
                                                 Time_spent == 180 | Time_spent <= 194 ~ '3:00',
                                                 Time_spent == 195 | Time_spent <= 209 ~ '3:15',
                                                 Time_spent == 210 | Time_spent <= 224 ~ '3:30',
                                                 Time_spent == 225 | Time_spent <= 239 ~ '3:45',
                                                 Time_spent == 240 | Time_spent <= 254 ~ '4:00',
                                                 Time_spent == 255 | Time_spent <= 269 ~ '4:15',
                                                 Time_spent == 270 | Time_spent <= 284 ~ '4:30',
                                                 Time_spent == 285 | Time_spent <= 299 ~ '4:45',
                                                 Time_spent == 300 | Time_spent <= 314 ~ '5:00',
                                                 Time_spent == 315 | Time_spent <= 329 ~ '5:15',
                                                 Time_spent == 330 | Time_spent <= 344 ~ '5:30',
                                                 Time_spent == 345 | Time_spent <= 359 ~ '5:45',
                                                 Time_spent == 360 | Time_spent <= 374 ~ '6:00',
                                                 Time_spent == 375 | Time_spent <= 389 ~ '6:15',
                                                 Time_spent == 390 | Time_spent <= 404 ~ '6:30',
                                                 Time_spent == 405 | Time_spent <= 414 ~ '6:45',
                                                 Time_spent == 415 | Time_spent <= 429 ~ '7:00',
                                                 Time_spent == 430 | Time_spent <= 444 ~ '7:15',
                                                 Time_spent == 445 | Time_spent <= 459 ~ '7:30',
                                                 Time_spent == 460 | Time_spent <= 474 ~ '7:45',
                                                 Time_spent == 475 | Time_spent <= 720 ~ '8-12 hrs',
                                                 Time_spent >= 721 ~ '> 12 hrs'))


  # Calculate Attendances
  dt_attendances <- df_recode  %>%
    dplyr::filter(spell_class_col == "ed_non_admission" | spell_class_col == "ed_comp_non_admission") %>%
    dplyr::group_by(Time_binned) %>%
    dplyr::mutate(Direct_discharge = dplyr::n())


  dt_admissions <- df_recode  %>%
    dplyr::filter(spell_class_col == "ed_admission" | spell_class_col == "ed_comp_admissions") %>%
    dplyr::group_by(Time_binned) %>%
    dplyr::mutate(Hospital_admissions = dplyr::n())


  dt_attend_admiss <- dplyr::bind_rows(dt_attendances, dt_admissions) %>%
    tidyr::gather(key = "Variable", value = "Value", "Direct_discharge", "Hospital_admissions") %>%
    tidyr::drop_na() %>%
    dplyr::select(spell_number, Time_binned, Variable, Value) %>%
    dplyr::group_by(Time_binned, Variable) %>%
    dplyr::summarize("Value" = dplyr::n()) %>%
    dplyr::ungroup()



  # Set the title
  title_stub <- " Hospital LoS distribution for admitted patients, "
  #hospital_name <- "Chelsea & Westminster"
  start_date_title <- format(as.Date(start_date), format = "%d %B %Y")
  end_date_title <- format(as.Date(end_date), format = "%d %B %Y")
  chart_title <- paste0(hospital_name, title_stub, start_date_title, " to ", end_date_title)

  att_plot_t_binned <- ggplot2::ggplot(dt_attend_admiss, ggplot2::aes(Time_binned, Value, group = Variable, fill = Variable)) + #shape = Event,  colour = Event
    ggplot2::geom_bar(stat = "identity", position = "identity" ,  width = 0.5, lwd = 0.3, colour = "white") +
    #geom_line(aes(linetype = variable, color = variable), size = 1.0) +
    #geom_point(aes(shape = variable), size = 1.0) +
    #ggplot2::scale_shape_manual(values = c(7, 6)) +
    #ggplot2::scale_linetype_manual(values = c("twodash", "solid")) +
    #ggplot2::scale_color_manual(values=c("black", "red")) +
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
                  subtitle = "Unscheduled A&E attendances, n; ED LoS 15min bins to 8 hr, 8-12 hr, >12,
Note: (i) results are intended for management information only",
                  y = "Count (n), Direct Discharged and Hospital Admissions", x = "Days of the week", caption = "Source: CLAHRC NWL") +
    ggplot2::theme(axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 21, b = 0, l = 0)),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   plot.title = ggplot2::element_text(size = 10, face = "bold"),
                   plot.subtitle = ggplot2::element_text(size = 8),
                   legend.position = "bottom", legend.box = "horizontal")

  att_plot_t_binned

  if(plot_chart == TRUE){

    att_plot_t_binned

  }else{

    att_plot_t_binned$data %>% dplyr::select(Time_binned, Variable, Value)

  }


}
