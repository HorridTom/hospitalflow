#################################################################################
###3.A&E Arrivals and Occupancy, 3rd of March to 27th of April, 2015 ############
#################################################################################


#' arrival_occupancy
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
####################################################################################################################
###3.A&E Arrivals and Occupancy, 3rd of March to 27th of April, 2015 ###############################################
####################################################################################################################
arrival_occupancy <- function(start_date = as.Date("2012-01-01", tz = "Europe/London"),        ############
                                       end_date = as.Date("2015-01-01", tz = "Europe/London"),          ############
                                       data, plot_chart, hospital_name = "Chelsea & Westminster"){



    dt_date <-  data %>%
    dplyr::filter(Admissions > start_date & Discharges < end_date) %>%
    dplyr::select(IDcol, Admissions, Discharges)

  # using gather function to create a new column with date; and filter only by Emergency Department
  arrivals_occupancy_jan_march <-  dt_date %>%
    tidyr::gather(key = Type, Time, Admissions:Discharges) %>%
    dplyr::mutate(Change = dplyr::if_else(Type == "Admissions", 1, -1)) %>%
    dplyr::group_by(time_hr = lubridate::floor_date(Time, "1 hour")) %>%
    dplyr::summarise(Arrivals = sum(Type == "Admissions"),
              Change = sum(Change)) %>%
    padr::pad(start_val = start_date, end_val = end_date) %>%
    tidyr::replace_na(list(Arrivals = 0, Change = 0)) %>%
    dplyr::mutate(Occupancy = cumsum(Change))


  avg_arriv_occup_jan_march <- arrivals_occupancy_jan_march %>%
    dplyr::mutate(Hour = lubridate::hour(time_hr)) %>%
    dplyr::group_by(Hour) %>%
    dplyr::summarize(Average_arrivals = mean(Arrivals),
              Average_occupancy = mean(Occupancy))


  # Set the title
  title_stub <- " hospital: Hourly A&E occupancy and arrival profile, "
  #hospital_name <- "Chelsea & Westminster"
  start_date_title <- format(as.Date(start_date), format = "%d %B %Y")
  end_date_title <- format(as.Date(end_date), format = "%d %B %Y")
  chart_title <- paste0(hospital_name, title_stub, start_date_title, " to ", end_date_title)

  plt_occ_percent <- ggplot2::ggplot(data = avg_arriv_occup_jan_march, ggplot2::aes(x = as.numeric(Hour), y = Average_occupancy, group = Hour))+
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

  #################################################################################################################


  if(plot_chart == TRUE){

    plt_occ_percent

  }else{

    plt_occ_percent$data %>% select(Hour, Average_arrivals , Average_occupancy)

  }

}  ############
####################################################################################################################
