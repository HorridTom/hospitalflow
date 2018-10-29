# Admission and Discharges Function
# data isn't pressumed to be standardised

##############################################################################
# LOADING THE LIBRARIES                                       ################
##############################################################################

#library(lubridate)

#library(reshape2)
#library(scales)
#library(lazyeval)
#library(qicharts2)
#library(tidyverse)
################################################################################

################################################################################



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
admission_discharges <- function(start_date, end_date, data, plot_chart){

  #set.seed(22)

  #test_data <- sample_n(data[c("PAT_CODE", "WARD_CODE", "START_DATETIME", "END_DATETIME",
                              # "episode.order", "spell.type", "AGE_BAND", "SEX")], size = 30000, replace = FALSE)

  #subseting data set#
  #colname_ct <- function(data, colname){
    #data[,colname] = as.POSIXct(data[,colname])
    #data
  #}


  #df_date_ct <- test_data %>%
    #colname_ct("START_DATETIME") %>%
    #colname_ct("END_DATETIME")

  #df_rename <- df_date_ct %>%
    #dplyr::select(PAT_CODE, START_DATETIME, END_DATETIME, WARD_CODE, episode.order, spell.type, AGE_BAND, SEX) %>%
    #dplyr::rename(IDcol = PAT_CODE, Admissions = START_DATETIME,
                 # Discharges = END_DATETIME, EpisodeNumber = episode.order, PatientType = spell.type,
                  #WardCode = WARD_CODE, PatientType = spell.type)

  #start_date <- as.POSIXct("2015-01-01 00:00:00", tz = "Europe/London")
  #end_date <- as.POSIXct( "2015-01-31 00:00:00", tz = "Europe/London")
  #df_date_ct <- df_rename %>%
   # colname_ct("Admissions") %>%
   # colname_ct("Discharges")

  adm_disch <- data %>%
    dplyr::filter(Admissions < end_date & Discharges > start_date) %>%
    dplyr::select(IDcol, Admissions, Discharges, PatientType, EpisodeNumber, WardCode, AGE_BAND, SEX) %>%
    dplyr::mutate(Los = as.numeric(difftime(Discharges, Admissions, unit = c("min")))) %>%
    dplyr::mutate(Adm_period = dplyr::if_else(Admissions > start_date, "TRUE", "FALSE"),
                  Disch_period = dplyr::if_else(Discharges < end_date, "TRUE", "FALSE" ),
                  Los_binned = dplyr::if_else(Los > 1440, "TRUE", "FALSE")) %>%
    dplyr::mutate(Same_day_non_emerg = dplyr::if_else(PatientType != "Emergency" & Los < "1440", "TRUE", "FALSE"),
                  Start_date = as.Date(Admissions),
                  End_date = as.Date(Discharges))


  #setwd("C:/Users/gburcea/Desktop/")
  #write.csv(adm_disch, "test_data_last3.csv")

  dt_adm <- adm_disch  %>%
    dplyr::select(IDcol, Admissions, EpisodeNumber, Los, Adm_period, Start_date) %>%
    dplyr::filter(Los > 1440 & EpisodeNumber == 1 & Adm_period == "TRUE") %>%
    dplyr::mutate(Day = lubridate::day(Admissions),
                  Weekday = lubridate::wday(Admissions, label = TRUE)) %>%
    dplyr::group_by(Start_date, Weekday, Day) %>%
    dplyr::tally() %>%
    dplyr::rename(num_adms = n)
  #dplyr::mutate(Sum_wkday = n())


  #####################
  ###################

  # ymd function parses dates in year-month-day format
  start_date_new_col <- as.POSIXct(start_date, format = "%Y-%m-%d", tz = "Europe/London")
  # The %m+% adds months to dates without exceeding the last day
  df_adm_per <- start_date_new_col %m+% lubridate::days(c(0:30))

  df_adm_date <- tibble::as.tibble(df_adm_per)

  df_adm_date$value <- as.POSIXct(df_adm_date$value, format = "%y-%m-%d", format = "Europe/London")

  df_date_adm_ren <- df_adm_date %>%
    dplyr::rename(Start_d  = "value") %>%
    dplyr::mutate(Start_date = lubridate::date(Start_d))

  df_adm_wkday <- df_date_adm_ren %>%
    dplyr::mutate(Weekday = lubridate::wday(Start_date, label = TRUE),
                  Day = lubridate::day(Start_date)) %>%
    dplyr::select(Start_date, Weekday, Day)

  str(df_adm_wkday)

  df_adm_wkday

  dt_adm_calc <- dplyr::full_join(dt_adm, df_adm_wkday, by = c("Start_date", "Weekday", "Day"))

  dt_adm_final <- dt_adm_calc %>%
    tidyr::replace_na(list(num_adms = 0))

  #setwd("C:/Users/gburcea/Desktop/")
  #write.csv(adm_disch, "test_data_last.csv")

  avg_adm_total <- dt_adm_final %>%
    dplyr::group_by(Weekday) %>%
    dplyr::summarise( "Total Admissions" = mean(num_adms))


  dt_disch <- adm_disch  %>%
    dplyr::select(IDcol, Discharges, EpisodeNumber, Los, Disch_period, End_date) %>%
    dplyr::filter(Los > 1440 & EpisodeNumber == 1 & Disch_period == "TRUE") %>%
    dplyr::mutate(Day = lubridate::day(Discharges),
                  Weekday = lubridate::wday(Discharges, label = TRUE)) %>%
    dplyr::group_by(End_date, Weekday, Day) %>%
    dplyr::tally() %>%
    dplyr::rename(num_disch = n)

  # ymd function parses dates in year-month-day format
  end_date_new_col <- as.POSIXct(start_date, format = "%Y-%m-%d", tz = "Europe/London")
  # The %m+% adds months to dates without exceeding the last day
  df_disch_per <- end_date_new_col %m+% lubridate::days(c(0:30))


  df_disch_date <- tibble::as.tibble(df_disch_per)

  df_disch_date$value <- as.POSIXct(df_disch_date$value, format = "%y-%m-%d", format = "Europe/London")

  df_disch_ren <- df_disch_date %>%
    dplyr::rename(End_d  = "value") %>%
    dplyr::mutate(End_date = lubridate::date(End_d))


  df_disch_wkday <- df_disch_ren %>%
    dplyr::mutate(Weekday = lubridate::wday(End_date, label = TRUE),
                  Day = lubridate::day(End_date )) %>%
    dplyr::select(End_date, Weekday, Day)

  str(df_disch_wkday)

  dt_disch_calc <- dplyr::full_join(dt_disch, df_disch_wkday, by = c("End_date", "Weekday", "Day"))

  dt_disch_final <- dt_disch_calc %>%
    tidyr::replace_na(list(num_disch = 0))


  avg_disch_total <- dt_disch_final %>%
    dplyr::group_by(Weekday) %>%
    dplyr::summarise( "Total Discharges" = mean(num_disch))

  dt_disch_avg <- dplyr::left_join(avg_adm_total, avg_disch_total, by = c("Weekday"))


  non_emergency_adm <- adm_disch  %>%
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
    dplyr::summarise("Non Emergency Admissions" = mean(num_non_emerg_adm))


  adm_disch_join <- dplyr::left_join(avg_adm_total, avg_disch_total, by = c("Weekday"))

  adm_disc_non_emerg_join <- dplyr::left_join(adm_disch_join, avg_non_emerg, by = c("Weekday"))

  melt_for_plt <- reshape2::melt(adm_disc_non_emerg_join)

  plot_adm_disc <- ggplot2::ggplot(melt_for_plt,  ggplot2::aes(Weekday, value, group = variable)) + #shape = Event,  colour = Event
    ggplot2::geom_bar(stat = "identity", position = "identity" , alpha=0.4, width = 0.5, fill = "slateblue4") +
    ggplot2::geom_line(ggplot2::aes(linetype = variable, color = variable), size = 1.0) +
    ggplot2::geom_point(ggplot2::aes(shape = variable), size = 1.0) +
    ggplot2::scale_shape_manual(values = c(7, 6, 5)) +
    ggplot2::scale_linetype_manual(values = c("solid", "solid" , "twodash")) +
    ggplot2::scale_color_manual(values=c("red", "black",  "blue")) +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(expand = c(0, .1)) +
    ggplot2::xlim("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun") +
    ggplot2::labs(title = "Chelsea Westminster hospital: Admissions and Discharges by days of the week, 1st of Jan to 31st of March, 2015",
         subtitle = "Averages of daily hospital arrivals and discharges(excluding the same day non-emergency cases, and non-admitted ED attendances) by the day of the week.\nNote: results are intended for management information only",
         y = "Average", x = "Days of the week", caption = "Source: CLAHRC NWL") +
    ggplot2::theme(axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 21, b = 0, l = 0)),
          plot.title = ggplot2::element_text(size = 10, face = "bold"),
          plot.subtitle = ggplot2::element_text(size = 9),
          legend.position = "bottom", legend.box = "horizontal")

  plot_adm_disc

  if(plot_chart == TRUE){

    plot_adm_disc

  }else{

    plot_adm_disc$data %>% dplyr::select(Weekday, variable, value)

  }

}    #####
################################################################################




