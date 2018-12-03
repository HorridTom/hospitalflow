# Admission and Discharges Function
# data isn't  standardised

#' admission_discharges
#'
#' @param start_date date of earliest discharge to be included in the analysis
#' @param end_date date of latest admission to be included in the analysis
#' @param data hospital episode data
#' @param plot_chart if TRUE return chart, otherwise if FALSE return dataframe
#'
#' @return Chart or dataframe showing average Admissions and Discharges by the day of the week
#' @export
#' @importFrom magrittr %>%
#' @importFrom lubridate %m+%
#'
#' @examples
#' \dontrun{
#' hospitalflow::admission_discharges(start_date = "2015-01-01 00:00:00", end_date = "2015-02-01 00:00:00",
#'                                    data = example_data, plot_chart = TRUE)
#' }
admission_discharges <- function(start_date, end_date, data, plot_chart, hospital_name = "Chelsea & Westminster"){

  # Selecting the variables needed.
  # Creating new variables:
     # - Adm_period - whether or not admissions occured in the month in question;
     # - Discharges, created a new columne Disch_period.
     # - A column with Admissions to the hospital - with Lenght of stay more than a day
     # - Start date and End Date were stripped of the time since I need these later on
  admission_discharges <- data %>%
    dplyr::filter(Admissions < end_date & Discharges > start_date) %>%
    dplyr::select(IDcol, Admissions, Discharges, PatientType, EpisodeNumber, WardCode, AGE_BAND, SEX) %>%
    dplyr::mutate(Los = as.numeric(difftime(Discharges, Admissions, unit = c("min")))) %>%
    dplyr::mutate(Adm_period = dplyr::if_else(Admissions > start_date, "TRUE", "FALSE"),
                  Disch_period = dplyr::if_else(Discharges < end_date, "TRUE", "FALSE" ),
                  Los_binned = dplyr::if_else(Los > 1440, "TRUE", "FALSE")) %>%
    dplyr::mutate(Same_day_non_emerg = dplyr::if_else(PatientType != "Emergency" & Los < "1440", "TRUE", "FALSE"),
                  Start_date = as.Date(Admissions, tz = "Europe/London"),
                  End_date = as.Date(Discharges, tz = "Europe/London"))

  # Retrieve the weekdays and days and COUNT the patients based on the dates, weeday and day
  dt_adm <- admission_discharges %>%
    dplyr::select(IDcol, Admissions, EpisodeNumber, Los_binned, Adm_period, Start_date) %>%
    dplyr::filter(Los_binned == "TRUE" & EpisodeNumber == 1 & Adm_period == "TRUE") %>%
    dplyr::mutate(Day = lubridate::day(Admissions),
                  Weekday = lubridate::wday(Admissions, label = TRUE)) %>%
    dplyr::group_by(Start_date, Weekday, Day) %>%
    dplyr::tally() %>%
    dplyr::mutate(Sum_wkday = n()) %>%
    dplyr::rename(num_adms = n)


  #This format is more flexible than the one I suggested
  df_adm_per <- seq(as.Date(start_date, tz = "Europe/London"), as.Date(end_date, tz = "Europe/London"), by = 'day')

  # Place the new list, with dates, into a tibble and format it accordingly.
  # DO the same as with the adm column by finding the weekday, and merged with the admission table

  df_adm_date <- tibble::as.tibble(df_adm_per)

  df_date_adm_ren <- df_adm_date %>%
    dplyr::rename(Start_d  = "value") %>%
    dplyr::mutate(Start_date = lubridate::date(Start_d))

  df_adm_wkday <- df_date_adm_ren %>%
    dplyr::mutate(Weekday = lubridate::wday(Start_date, label = TRUE),
                  Day = lubridate::day(Start_date)) %>%
    dplyr::select(Start_date, Weekday, Day)


  dt_adm_calc <- dplyr::full_join(dt_adm, df_adm_wkday, by = c("Start_date", "Weekday", "Day"))

  # The new column gets NA - as merging found the missing rows for several weedays, these were droped
  # as filters were used in order to select those patients admitted
  dt_adm_final <- dt_adm_calc %>%
    tidyr::replace_na(list(num_adms = 0))

  # Calculating the averages

  avg_adm_total <- dt_adm_final %>%
    dplyr::group_by(Weekday) %>%
    dplyr::summarise( "Avg_admissions" = mean(num_adms))



  # The same process followed into calculating the mean for discharges
  dt_disch <- admission_discharges  %>%
    dplyr::select(IDcol, Discharges, EpisodeNumber, Los, Disch_period, End_date) %>%
    dplyr::filter(Los > 1440 & EpisodeNumber == 1 & Disch_period == "TRUE") %>%
    dplyr::mutate(Day = lubridate::day(Discharges),
                  Weekday = lubridate::wday(Discharges, label = TRUE)) %>%
    dplyr::group_by(End_date, Weekday, Day) %>%
    dplyr::tally() %>%
    dplyr::rename(num_disch = n)

  # Creating a new columns with dates for the month in question - table the dates
  #
  df_disch_per <- seq(as.Date(start_date, tz = "Europe/London"), as.Date(end_date, tz = "Europe/London"), by = 'day')

  df_disch_date <- tibble::as.tibble(df_disch_per)


  #renamed the dates when merging the new table with the discharges calculated above

  df_disch_ren <- df_disch_date %>%
    dplyr::rename(End_d  = "value") %>%
    dplyr::mutate(End_date = lubridate::date(End_d))

  # finding the days and weekdays from dates that were generated above

  df_disch_wkday <- df_disch_ren %>%
    dplyr::mutate(Weekday = lubridate::wday(End_date, label = TRUE),
                  Day = lubridate::day(End_date )) %>%
    dplyr::select(End_date, Weekday, Day)

  str(df_disch_wkday)

  dt_disch_calc <- dplyr::full_join(dt_disch, df_disch_wkday, by = c("End_date", "Weekday", "Day"))

  # replacing the na's that sneaked in when merging the new table with the discharges ones
  # and calculated the means
  dt_disch_final <- dt_disch_calc %>%
    tidyr::replace_na(list(num_disch = 0))

  avg_disch_total <- dt_disch_final %>%
    dplyr::group_by(Weekday) %>%
    dplyr::summarise( "Avg_discharges" = mean(num_disch))


  dt_disch_avg <- dplyr::left_join(avg_adm_total, avg_disch_total, by = c("Weekday"))

  # Process followed for non-emergency admissions as well
  non_emergency_adm <- admission_discharges  %>%
    dplyr::select(IDcol, Admissions, EpisodeNumber, Los, Adm_period, PatientType, WardCode, Same_day_non_emerg, Start_date) %>%
    dplyr::filter(Same_day_non_emerg == "TRUE" & EpisodeNumber == 1) %>% #LoS > "1,440"
    dplyr::filter(Los > 1440 & Adm_period == "TRUE") %>% #& PatientType != "Emergency" #&  WardCode !="A&E"PatientType != "Emergency" &
    dplyr::mutate(Weekday = lubridate::wday(Admissions, label = TRUE),
                  Day = lubridate::day(Admissions)) %>%
    dplyr::group_by(Start_date, Weekday, Day) %>%
    dplyr::tally() %>%
    dplyr::rename(num_non_emerg_adm = n)

  dt_non_emerg_calc <- dplyr::full_join(non_emergency_adm, df_adm_wkday, by = c("Start_date", "Weekday", "Day"))

  dt_non_emerg_adm_final <- dt_non_emerg_calc %>%
    tidyr::replace_na(list(num_non_emerg_adm = 0))

  avg_non_emerg <- dt_non_emerg_adm_final %>%
    dplyr::group_by(Weekday) %>%
    dplyr::summarise("Non_emergency_admissions" = mean(num_non_emerg_adm))

 # Joining all the tables togheter and rearange the columns using function gather
  adm_disch_join <- dplyr::left_join(avg_adm_total, avg_disch_total, by = c("Weekday"))

  adm_disc_non_emerg_join <- dplyr::left_join(adm_disch_join, avg_non_emerg, by = c("Weekday"))


  melt_for_plt <- tidyr::gather(adm_disc_non_emerg_join, key = "Event", value = Value, Avg_admissions, Avg_discharges, Non_emergency_admissions)

  # sET CHART TITLE -
  title_stub = " hospital: Admissions and Discharges by days of the week, "
  chart_title <- paste0(hospital_name, title_stub, start_date," to ", end_date)

  # plot the admissions and discharges (non-emergency appears as well )
  plot_adm_disc <- ggplot2::ggplot(melt_for_plt,  ggplot2::aes(Weekday, Value, group = Event)) + #shape = Event,  colour = Event
    ggplot2::geom_bar(stat = "identity", position = "identity" , alpha=0.4, width = 0.5, fill = "slateblue4") +
    ggplot2::geom_line(ggplot2::aes(linetype = Event, color = Event), size = 1.0) +
    ggplot2::geom_point(ggplot2::aes(shape = Event), size = 1.0) +
    ggplot2::scale_shape_manual(values = c(7, 6, 5)) +
    ggplot2::scale_linetype_manual(values = c("solid", "solid" , "twodash")) +
    ggplot2::scale_color_manual(values=c("red", "black",  "blue")) +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(expand = c(0, .1)) +
    ggplot2::xlim("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun") +
    ggplot2::labs(title = chart_title,
                  subtitle = "Averages of daily hospital arrivals and discharges(excluding the same day non-emergency cases, and non-admitted ED attendances) by the day of the week.\nNote: results are intended for management information only",
                  y = "Average", x = "Days of the week", caption = "Source: CLAHRC NWL") +
    ggplot2::theme(axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 21, b = 0, l = 0)),
                   plot.title = ggplot2::element_text(size = 10, face = "bold"),
                   plot.subtitle = ggplot2::element_text(size = 9),
                   legend.position = "bottom", legend.box = "horizontal")

  if(plot_chart == TRUE){

    plot_adm_disc

  }else{

    # selecting the data only - needed as the function component requires whether the output shall be ploted
    plot_adm_disc$data %>% dplyr::select(Weekday, Event, Value)

  }

}
################################################################################


