#' four hour performance for ED
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
four_hrs_perf <- function(start_date = as.Date("2012-01-01", tz = "Europe/London"),
                               end_date = as.Date("2015-01-01", tz = "Europe/London"),
                               data, plot_chart, hospital_name = "Hospital_Name"){

  dt_select <- data %>%
    dplyr::select(pseudo_id, start_datetime, end_datetime) %>%
    dplyr::filter(start_datetime <= end_date & end_datetime >= start_date)


  # calculating total time by using difftime #
  # length of stay for the emergency department #
  ######################################
  # creating a new variable - under 4 hrs and above 4 hrs ##

  dt_los <-  dt_select  %>%
    dplyr::mutate(Los = difftime(end_datetime, start_datetime, units = c("min")),
                  Hr_perf = dplyr::case_when(
                    Los <= 240 ~ "under_4hrs",
                    Los >= 240 ~ "above_4hrs"),
                  Time = as.Date(start_datetime),
                  One_week = lubridate::round_date(Time, "7 days")) %>%
    dplyr::select(pseudo_id, Los, Time, One_week, Hr_perf)


  sum_4hrs_perf <- dt_los %>%
    dplyr::group_by(Time, Hr_perf) %>%
    dplyr::summarise(Count = n()) %>%
    tidyr::spread(Hr_perf, Count) %>%
    dplyr::mutate(N = under_4hrs + above_4hrs)

  # Set the title
  title_stub <- " Hospital LoS distribution for admitted patients, "
  hospital_name <- "Chelsea & Westminster"
  start_date_title <- format(as.Date(start_date), format = "%d %B %Y")
  end_date_title <- format(as.Date(end_date), format = "%d %B %Y")
  chart_title <- paste0(hospital_name, title_stub, start_date_title, " to ", end_date_title)


  # function to plot the 4 hrs emergency performance
  # Plot all days - see Tom's AE APP

  plot <- qicharts2::qic(Time, under_4hrs,
                         n = N,
                         data = sum_4hrs_perf,
                         chart = 'pp',
                         ylab = "percent",
                         show.grid = TRUE,
                         #freeze = 1,
                         #part = 11,
                         #multiply= 100,
                         x.format = "%d-%m-%Y",
                         x.angle = 45,
                         xlab = " Compliance with 4hr emergency care standard",
                         title = chart_title)

  plot

  if(plot_chart == TRUE){

    plot

  }else{

    plot$data %>% select(Time, above_4hrs, under_4hrs, N)

  }

}
