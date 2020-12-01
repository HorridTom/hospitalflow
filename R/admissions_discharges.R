#' Admission and Discharges Function
#' data isn't  standardised

#' admission_discharges
#'
#' @param start_date date of earliest discharge to be included in the analysis
#' @param end_date date of latest admission to be included in the analysis
#' @param data hospital episode data with at least the following fields:
#' Admissions - the admission date
#' Discharges - the discharge date
#' IDcol - the patient pseudo id
#' PatientType - the admission type, distinguishing emergency from other admissions
#' EpisodeNumber - the episode id
#' WardCode - the ward for the episode
#' @param plot_chart if TRUE return chart, otherwise if FALSE return dataframe
#'
#' @return Chart or dataframe showing average Admissions and Discharges by the day of the week
#' @export
#' @importFrom magrittr %>%
#' @importFrom lubridate %m+%
#'
#' @examples
#' \dontrun{
#' hospitalflow::admissions_discharges(start_date = "2015-01-01 00:00:00", end_date = "2015-02-01 00:00:00",
#'                                    data = example_data, plot_chart = TRUE)
#' }
###################################
# Admission Discharges ############
#####################################################################################################
admissions_discharges <- function(start_date, end_date, data, plot_chart, hospital_name = "Hospial name"){

  #get time zone of data
  time_zone <- attr(data$spell_start, "tzone")

  #set input dates to have the same time zone as the data
  start_date <- as.Date(start_date, tz = time_zone)
  end_date <- as.Date(end_date, tz = time_zone)

  #selecting the variables needed, with new variables created
  admission_discharge <- data  %>%
    dplyr::select(spell_number, spell_start, spell_end, spell_class_col) %>%
    dplyr::filter(spell_start < end_date, spell_end > start_date) %>%
    dplyr::mutate(adm_period = dplyr::if_else(spell_start >= start_date, TRUE, FALSE),
                  disch_period = dplyr::if_else(spell_end <= end_date, TRUE, FALSE),
                  adm = as.Date(spell_start),
                  disch = as.Date(spell_end)) %>%
    dplyr::mutate(non_emerg = dplyr::if_else(spell_class_col == "direct_admission" | spell_class_col == "direct_comp_admission", TRUE, FALSE),
                  same_day_non_emerg = dplyr::if_else(non_emerg == TRUE & adm == disch, TRUE, FALSE),
                  non_admitted_ed_attend = dplyr::if_else(spell_class_col == "ed_non_admission" | spell_class_col == "ed_comp_non_admission", TRUE, FALSE))


  # calculating the Admissions by Adm date, Weekday and Day
  dt_adm <- admission_discharge  %>%
    dplyr::select(spell_number, spell_start, spell_end,  adm, disch, adm_period, same_day_non_emerg, non_admitted_ed_attend) %>%
    dplyr::filter(same_day_non_emerg == FALSE & adm_period == TRUE & non_admitted_ed_attend == FALSE) %>%
    dplyr::mutate(Day = lubridate::day(adm),
                  Weekday = lubridate::wday(adm, label = TRUE)) %>%
    dplyr::group_by(adm, Weekday, Day) %>%
    dplyr::tally() %>%
    dplyr::rename(num_adms = n)

  # Generating a period of date for calc averages
  df_period <- seq(start_date, end_date, by = 'day')

  # Place the new list, with dates, into a tibble and format it accordingly.
  # DO the same as with the adm column by finding the weekday, and merged with the admission table

  df_adm_date <- tibble::as.tibble(df_period)

  df_adm_wkday <- df_adm_date  %>%
    dplyr::rename(adm  = "value") %>%
    dplyr::mutate(Weekday = lubridate::wday(adm, label = TRUE),
                  Day = lubridate::day(adm)) %>%
    dplyr::select(adm, Weekday, Day)


  #Left join does match the columns I need and leaves out the ones with NA.
  dt_adm_calc <- dplyr::left_join(df_adm_wkday, dt_adm, by = c("adm", "Weekday", "Day"))

  # The new column gets NA - as merging found the missing rows for several weedays, these were droped
  # as filters were used in order to select those patients admitted
  dt_adm_final <- dt_adm_calc %>%
    tidyr::replace_na(list(num_adms = 0))

  # Calculating the averages for Admissions by Day of the week

  avg_adm_total <- dt_adm_final %>%
    dplyr::group_by(Weekday) %>%
    dplyr::summarise( "Avg_admissions" = mean(num_adms))

  # Calculating the Discharges by Discharge Date, Weekday, and Day
  dt_disch <- admission_discharge %>%
    dplyr::select(spell_start, spell_end, spell_class_col, disch, disch_period, same_day_non_emerg, non_admitted_ed_attend) %>%
    dplyr::filter(same_day_non_emerg == FALSE & disch_period == TRUE, non_admitted_ed_attend == FALSE) %>%
    dplyr::mutate(Day = lubridate::day(disch),
                  Weekday = lubridate::wday(disch, label = TRUE)) %>%
    dplyr::group_by(disch, Weekday, Day) %>%
    dplyr::tally() %>%
    dplyr::rename(num_adms = n)


  #creating a period
  df_disch_date <- tibble::as.tibble(df_period)

  # creating new variables Weekday, Day in the period generated above
  df_disch_wkday <- df_disch_date  %>%
    dplyr::rename(disch  = "value") %>%
    dplyr::mutate(Weekday = lubridate::wday(disch, label = TRUE),
                  Day = lubridate::day(disch)) %>%
    dplyr::select(disch, Weekday, Day)


  dt_disch_calc <- dplyr::left_join(df_disch_wkday, dt_disch, by = c("disch", "Weekday", "Day"))

  # replace NA with 0 for the purpose of averaging across weekdays
  dt_disch_final <- dt_disch_calc %>%
    tidyr::replace_na(list(num_adms = 0))

  # Get the Average Discharges by Day of the Week
  avg_disch_total <- dt_disch_final %>%
    dplyr::group_by(Weekday) %>%
    dplyr::summarise( "Avg_discharges" = mean(num_adms))

  # joining the tibbles
  dt_adm_disch_avg <- dplyr::left_join(avg_adm_total, avg_disch_total, by = c("Weekday"))

  # Process followed for non-emergency admissions as well
  non_emergency_adm <- admission_discharge  %>%
    dplyr::select(spell_number, adm,  adm_period, spell_class_col, same_day_non_emerg, non_emerg, non_admitted_ed_attend) %>%
    dplyr::filter(non_emerg == TRUE) %>%
    dplyr::filter(same_day_non_emerg == FALSE & adm_period == TRUE) %>%
    dplyr::mutate(Weekday = lubridate::wday(adm, label = TRUE),
                  Day = lubridate::day(adm)) %>%
    dplyr::group_by(adm, Weekday, Day) %>%
    dplyr::tally() %>%
    dplyr::rename(num_non_emerg_adm = n)

  dt_non_emerg_calc <- dplyr::left_join(df_adm_wkday, non_emergency_adm, by = c("adm", "Weekday", "Day"))

  dt_non_emerg_adm_final <- dt_non_emerg_calc %>%
    tidyr::replace_na(list(num_non_emerg_adm = 0))

  avg_non_emerg <- dt_non_emerg_adm_final %>%
    dplyr::group_by(Weekday) %>%
    dplyr::summarise("Non_emergency_admissions" = mean(num_non_emerg_adm))

  total_adm_disch_non_emerg <- dplyr::left_join(dt_adm_disch_avg, avg_non_emerg, by = c("Weekday"))

  melt_for_plt <- tidyr::gather(total_adm_disch_non_emerg, key = "Event", value = Value, Avg_admissions, Avg_discharges, Non_emergency_admissions) #

  # Set the title
  title_stub <- ": Admissions and Discharges by days of the week,\n"
  start_date_title <- format(as.Date(start_date), format = "%d %B %Y")
  end_date_title <- format(as.Date(end_date), format = "%d %B %Y")
  chart_title <- paste0(hospital_name, title_stub, start_date_title, " to ", end_date_title)

  # plot the admissions and discharges (non-emergency appears as well )
  plot_adm_disc <- ggplot2::ggplot(melt_for_plt,  ggplot2::aes(Weekday, Value, group = Event)) + #shape = Event,  colour = Event
    # ggplot2::geom_bar(stat = "identity", position = "identity" , alpha=0.4, width = 0.5, fill = "slateblue4") +
    ggplot2::geom_line(ggplot2::aes(linetype = Event, color = Event), size = 1.0) +
    ggplot2::geom_point(ggplot2::aes(shape = Event), size = 1.0) +
    ggplot2::scale_y_continuous(limits = c(0,NA)) +
    ggplot2::scale_shape_manual(values = c(7, 6, 5)) +
    ggplot2::scale_linetype_manual(values = c("solid", "solid" , "twodash")) +
    ggplot2::scale_color_manual(values=c("red", "black",  "blue")) +
    ggplot2::theme_bw() +
    ggplot2::xlim("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun") +
    ggplot2::labs(title = chart_title,
                  subtitle = "Averages of daily hospital arrivals and discharges(excluding the same day non-emergency cases, and non-admitted ED attendances) by the day of the week.
                  \nNote: results are intended for management information only",
                  y = "Average", x = "Days of the week", caption = "Source: CLAHRC NWL") +
    ggplot2::theme(axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 21, b = 0, l = 0)),
                   plot.title = ggplot2::element_text(size = 10, face = "bold"),
                   plot.subtitle = ggplot2::element_text(size = 9),
                   legend.position = "bottom", legend.box = "horizontal")
  plot_adm_disc

  if(plot_chart == TRUE){

    plot_adm_disc

  }else{

    plot_adm_disc$data %>% dplyr::select(Weekday, Event, Value)

  }

}



#' adm_disch_day_of_week
#'
#' @return
#' @export
#'
adm_disch_day_of_week <- function() {
  admissions_discharges(start_date, end_date, data, plot_chart = TRUE, hospital_name)
}




