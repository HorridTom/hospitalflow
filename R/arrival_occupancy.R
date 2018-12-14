#################################################################################
###3.A&E Arrivals and Occupancy, 3rd of March to 27th of April, 2015 ############
#################################################################################


arrival_occupancy <- function(start_date = as.Date("2012-01-01", tz = "Europe/London"),
                              end_date = as.Date("2015-01-01", tz = "Europe/London"),
                              data, plot_chart, hospital_name = "Chelsea & Westminster"){


  # extracting Month and hour from Time column and
  # count = occupancy.
  # ASK TOM FOR THE AVERAGES - WE GET DIFFERENT AVERAGES DEPENDENT OF THE GROUP BY IN DIFFERENT CASES - GROUP_BY WITH DAY AND HOUR GIVES DIFFERENT AVERAGES
  # using gather function to create a new column with date; and filter only by Emergency Department
  #arrivals_occupancy_jan_march <-  data %>%
    #tidyr::gather(key = Type, Time, Admissions:Discharges) %>%
    #dplyr::filter(PatientType == "Emergency" & EpisodeNumber == 1)


  # extracting year, month, day and hour from Time column and count = occupancy
  dt_sep_occ <- data %>%
    dplyr::mutate(
      Time_rounded = lubridate::ymd_hms(Time_rounded),
      Hour = lubridate::hour(Time_rounded),
      Time = lubridate::date(Time_rounded)) %>%
    dplyr::group_by(Time, Hour) %>%
    dplyr::tally() %>%
    dplyr::rename(Occupancy = n)


  #This format is more flexible than the one I suggested
  df_period <- seq.POSIXt(from = as.POSIXct(start_date, tz = "Europe/London"),
                          to = as.POSIXct(end_date, tz= "Europe/London"),
                          by = "hour")

  #df_period
  #Place the new list, with dates, into a tibble and format it accordingly.
  #DO the same as with the adm column by finding the weekday, and merged with the admission table

  df_adm_date <- tibble::as.tibble(df_period)

  df_date_adm_ren <- df_adm_date %>%
    dplyr::rename(Time_rounded = "value") %>%
    dplyr::mutate(Hour = lubridate::hour(Time_rounded),
                  Time = lubridate::date(Time_rounded)) %>%
    dplyr::select(Time, Hour)


  table_average_occ <- dplyr::left_join(df_date_adm_ren, dt_sep_occ, by = c("Time", "Hour"))

  dt_occ_final <- table_average_occ %>%
    tidyr::replace_na(list(Occupancy  = 0))

  tbl_avg_occ <- dt_occ_final %>%
    dplyr::group_by(Hour) %>%
    dplyr::summarize(Average_occ = round(mean(Occupancy), digits = 2))

  # take out the start-datetime only -> this will be used later to be added to a plot alongside the occupancy
  dt_sep_arrivals <- data  %>%
    dplyr::filter(Type == "Admissions") %>%
    dplyr::select(IDcol, Time_rounded) %>%
    dplyr::mutate(
      Time = lubridate::date(Time_rounded),
      Day = lubridate::day(Time_rounded),
      Hour = lubridate::hour(Time_rounded)) %>%
    dplyr::group_by(Time, Hour) %>%
    dplyr::tally() %>%
    dplyr::rename(Arrivals = n)

  table_average_arrivals <- dplyr::left_join(df_date_adm_ren, dt_sep_arrivals, by = c("Time", "Hour"))

  dt_avg_arrivals <- table_average_arrivals %>%
    tidyr::replace_na(list(Arrivals  = 0))

  tbl_avg_arrivals <- dt_avg_arrivals %>%
    dplyr::group_by(Hour) %>%
    dplyr::summarize(Average_arrivals = round(mean(Arrivals), digits = 2))

  tbl_occ_arriv <- dplyr::left_join(tbl_avg_arrivals, tbl_avg_occ, by= c("Hour"))


  # Set the title
  title_stub <- " hospital: Hourly A&E occupancy and arrival profile, "
  hospital_name <- "Chelsea & Westminster"
  start_date_title <- format(as.Date(start_date), format = "%d %B %Y")
  end_date_title <- format(as.Date(end_date), format = "%d %B %Y")
  chart_title <- paste0(hospital_name, title_stub, start_date_title, " to ", end_date_title)

  plt_occ_percent <- ggplot2::ggplot(tbl_occ_arriv, ggplot2::aes(x = as.numeric(Hour), y = Average_occ, group = Hour)) +
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
                  y = "Average occupancy, n", x = "Hour of the day", caption = "Source: CLAHRC NWL") +
    ggplot2::theme(axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 21, b = 0, l = 0)),
                   plot.title = ggplot2::element_text(size = 12, face = "bold"),
                   plot.subtitle = ggplot2::element_text(size = 10),
                   legend.position = "bottom", legend.box = "horizontal")

  plt_occ_percent

  if(plot_chart == TRUE){

    plt_occ_percent

  }else{

    plt_occ_percent$data %>% select(Hour, Average_arrivals, Average_occ)

  }

}

