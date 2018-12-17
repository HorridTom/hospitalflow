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
#' hospitalflow::admissions_discharges(start_date = "2015-01-01 00:00:00", end_date = "2015-02-01 00:00:00",
#'                                    data = example_data, plot_chart = TRUE)
#' }
###################################
# Admission Discharges ############
#####################################################################################################
admissions_discharges <- function(start_date = as.Date("2014-01-01", tz = "Europe/London"),       ###
                                  end_date = as.Date("2015-01-01",tz = "Europe/London"),          ###
                                  data, plot_chart, hospital_name = "Chelsea & Westminster"){

  # Create Adm and Disch columns containing admission and discharge
  # dates (as in date only, no time information)
  data <- data %>% dplyr::mutate(Adm = as.Date(Admissions), Disch = as.Date(Discharges))

  #selecting the variables needed, with new variables created
  admission_discharge <- data  %>%
    dplyr::filter(Adm < as.POSIXct(end_date, tz= "Europe/London") & Disch >= as.POSIXct(start_date, tz= "Europe/London")) %>%
    dplyr::select(IDcol, Admissions, Discharges, Adm, Disch, PatientType, EpisodeNumber, WardCode, Los, Adm_disch_same_day) %>%
    dplyr::mutate(Adm_period = dplyr::if_else(Adm >= start_date, TRUE, FALSE),
                  Disch_period = dplyr::if_else(Disch <= end_date, TRUE, FALSE)) %>%
    dplyr::mutate(Same_day_non_emerg = dplyr::if_else(PatientType != "Emergency" & Los < 1440, TRUE, FALSE))


  # calculating the Admissions by Adm date, Weekday and Dayu
  dt_adm <- admission_discharge  %>%
    dplyr::select(IDcol, Adm, EpisodeNumber, Los, Adm_period, Los) %>%
    dplyr::filter(Los > 1440 & EpisodeNumber == 1 & Adm_period == TRUE) %>%
    dplyr::mutate(Day = lubridate::day(Adm),
                  Weekday = lubridate::wday(Adm, label = TRUE)) %>%
    dplyr::group_by(Adm, Weekday, Day) %>%
    dplyr::tally() %>%
    dplyr::rename(num_adms = n)


  # Generating a period of date for calc averages
  df_period <- seq(as.Date(start_date, tz = "Europe/London"), as.Date(end_date, tz = "Europe/London"), by = 'day')

  # Place the new list, with dates, into a tibble and format it accordingly.
  # DO the same as with the adm column by finding the weekday, and merged with the admission table

  df_adm_date <- tibble::as.tibble(df_period)


  df_adm_wkday <- df_adm_date  %>%
    dplyr::rename(Adm  = "value") %>%
    dplyr::mutate(Weekday = lubridate::wday(Adm, label = TRUE),
                  Day = lubridate::day(Adm)) %>%
    dplyr::select(Adm, Weekday, Day)


  #Left join does match the columns I need and leaves out the ones with NA.
  dt_adm_calc <- dplyr::left_join(dt_adm, df_adm_wkday, by = c("Adm", "Weekday", "Day"))

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
    dplyr::select(IDcol, Disch, EpisodeNumber, Los, Disch_period, Los) %>%
    dplyr::filter(Los > 1440 & EpisodeNumber == 1 & Disch_period == TRUE) %>%
    dplyr::mutate(Day = lubridate::day(Disch),
                  Weekday = lubridate::wday(Disch, label = TRUE)) %>%
    dplyr::group_by(Disch, Weekday, Day) %>%
    dplyr::tally() %>%
    dplyr::rename(num_adms = n)


  #creating a period
  df_disch_date <- tibble::as.tibble(df_period)

  # creating new variables Weekday, Day in the period generated above
  df_disch_wkday <- df_disch_date  %>%
    dplyr::rename(Disch  = "value") %>%
    dplyr::mutate(Weekday = lubridate::wday(Disch, label = TRUE),
                  Day = lubridate::day(Disch)) %>%
    dplyr::select(Disch, Weekday, Day)

  # Left join does not give me the days that are missing from the original dataset (these dates shall appear when the sequence
  # of dates generated on the period in question. Yet, the full join does. To be reviewed by Tom.)
  dt_disch_calc <- dplyr::left_join(dt_disch, df_disch_wkday, by = c("Disch", "Weekday", "Day"))

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
    dplyr::select(IDcol, Adm, EpisodeNumber, Los, Adm_period, PatientType, Los, Adm_disch_same_day) %>%
    dplyr::filter(PatientType != "Emergency" & EpisodeNumber == 1) %>% #LoS > "1,440"
    dplyr::filter(Los > 1440 & Adm_period == TRUE & Adm_disch_same_day == FALSE) %>% #& PatientType != "Emergency" #  & Adm_disch_same_day == FALSE WardCode !="A&E"PatientType != "Emergency" &
    dplyr::mutate(Weekday = lubridate::wday(Adm, label = TRUE),
                  Day = lubridate::day(Adm)) %>%
    dplyr::group_by(Adm, Weekday, Day) %>%
    dplyr::tally() %>%
    dplyr::rename(num_non_emerg_adm = n)

  dt_non_emerg_calc <- dplyr::full_join(non_emergency_adm, df_adm_wkday, by = c("Adm", "Weekday", "Day"))

  dt_non_emerg_adm_final <- dt_non_emerg_calc %>%
    tidyr::replace_na(list(num_non_emerg_adm = 0))

  avg_non_emerg <- dt_non_emerg_adm_final %>%
    dplyr::group_by(Weekday) %>%
    dplyr::summarise("Non_emergency_admissions" = mean(num_non_emerg_adm))

  total_adm_disch_non_emerg <- dplyr::left_join(dt_adm_disch_avg, avg_non_emerg, by = c("Weekday"))

  melt_for_plt <- tidyr::gather(total_adm_disch_non_emerg, key = "Event", value = Value, Avg_admissions, Avg_discharges, Non_emergency_admissions)

  # Set the title
  title_stub <- " hospital: Admissions and Discharges by days of the week, "
  hospital_name <- "Chelsea & Westminster"
  start_date_title <- format(as.Date(start_date), format = "%d %B %Y")
  end_date_title <- format(as.Date(end_date), format = "%d %B %Y")
  chart_title <- paste0(hospital_name, title_stub, start_date_title, " to ", end_date_title)

  # plot the admissions and discharges (non-emergency appears as well )
  plot_adm_disc <- ggplot2::ggplot(melt_for_plt,  ggplot2::aes(Weekday, Value, group = Event)) + #shape = Event,  colour = Event
    ggplot2::geom_bar(stat = "identity", position = "identity" , alpha=0.4, width = 0.5, fill = "slateblue4") +
    ggplot2::geom_line(ggplot2::aes(linetype = Event, color = Event), size = 1.0) +
    ggplot2::geom_point(ggplot2::aes(shape = Event), size = 1.0) +
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


  if(plot_chart == TRUE){

    plot_adm_disc

  }else{

    plot_adm_disc$data %>% select(Weekday, Event, Value)

  }

} ###
#####################################################################################################


