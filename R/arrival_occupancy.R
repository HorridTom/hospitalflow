
#################################################################################
###3.A&E Arrivals and Occupancy, 3rd of March to 27th of April, 2015 ############
#################################################################################

#################################################################################
arrival_occupancy <- function(start_date, end_date, data, plot_chart){


  # extracting Month and hour from Time column and
  # count = occupancy.
  # ASK TOM FOR THE AVERAGES - WE GET DIFFERENT AVERAGES DEPENDENT OF THE GROUP BY IN DIFFERENT CASES - GROUP_BY WITH DAY AND HOUR GIVES DIFFERENT AVERAGES
  dt_occ_count <- dt_hrs %>%
    dplyr::mutate(
      Time = ymd_hms(Time),
      Month = month(Time),
      Hour = hour(Time)
    ) %>%
    dplyr::count(Month, Hour)

  # getting the averages
  tbl_avg_occ <- dt_occ_count %>%
    dplyr::group_by(Hour) %>%
    dplyr::summarize(Average_occ = round(mean(n), digits = 2)) %>%
    dplyr::ungroup()

  # take out the start-datetime only -> this will be used later to be added to a plot alongside the occupancy
   # Count the Arrivals in A&E
  dt_arriv_count <- data  %>%
    dplyr::select(IDcol, Admissions) %>%
    dplyr::mutate(
      Month = month(Admissions),
      Hour = hour(Admissions)
    ) %>%
    dplyr::count(Month, Hour) #Month,

  # Get averages
  tbl_avg_arrivals <- dt_sep_arrivals %>%
    dplyr::group_by(Hour) %>%
    dplyr::summarize(Average_arrivals = round(mean(n), digits = 2)) %>%
    dplyr::ungroup()


  # Left join the two tables
  tbl_occ_arriv <- tidyr::left_join(tbl_avg_arrivals, tbl_avg_occ, by = "Hour")


  # Get to plot - Arrivals vs. Occupancy
  plt_arriv_occ <- ggplot2::ggplot(tbl_occ_arriv, ggplot2::aes(x = as.numeric(Hour), y = Average_occ, group = Hour)) +
    ggplot2::geom_bar(stat = "identity", alpha=0.7, width = 0.50, ggplot2::aes(fill = "Occupancy")) +
    ggplot2::scale_x_continuous(breaks = 0:23, expand = c(0, .2)) +
    ggplot2::scale_y_continuous(expand = c(0, .5)) + # breaks = c(0, 5, 10, 15, 20, 25))
    ggplot2::geom_point(aes(y = Average_arrivals)) +
    ggplot2::geom_line(aes(y = Average_arrivals, group = 1, color = "Arrivals in A&E")) +
    ggplot2::scale_fill_manual("",values="yellow4") +
    ggplot2::scale_color_manual("",values = 1) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Chelsea & Westminster: Average Hourly A&E Occupancy vs. Arrivals, 1st of Jan to 31st of March, 2015",
                  subtitle = "Averages - Hourly ED Occupancy, % , by Hour of the day. \nNote: results are intended for management information only",
                  y = "Average occupancy, n", x = "Hour of the day", caption = "Source: CLAHRC NWL") +
    ggplot2::theme(axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 21, b = 0, l = 0)),
                   plot.title = ggplot2::element_text(size = 12, face = "bold"),
                   plot.subtitle = ggplot2::element_text(size = 10),
                   legend.position = "bottom", legend.box = "horizontal")


  plt_arriv_occ

  if(plot_chart == TRUE){

    plt_arriv_occ

  }else{

    plt_arriv_occ$data %>% select(Hour, Average_arrivals, Average_occ)

  }

}
#################################################################################

