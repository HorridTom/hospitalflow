
#' ED_day_hour
#'
#' @param startDay Date for which the analysis starts
#' @param endDay Date for which analysis ends
#' @param df Dataframe representing ED stays with start_ime and end_time specifying stay start and end of time in ED
#'
#'
#'
#' @return A dataframe with mean, Q1, Q3, max and min occupancy for each hour of the week
#'
#'
#' @examples
ED_day_hour <- function(startDay, endDay, df){

  #removing rows with NA values in start_datetime or end_datetime
  #df <- df[complete.cases(df[,c("start_datettime","end_datetime")]),]

  df <- dplyr::filter(df, start_datetime <= endDay, end_datetime >= startDay)

  dateTime <- seq(as.POSIXlt(startDay), as.POSIXlt(endDay), by="hours")
  hourly_occupancy <- sapply(dateTime, hospitalflow::occupancy, df, "start_datetime", "end_datetime")
  occupancy_df <- data.frame(dateTime, hourly_occupancy)

  occupancy_df <- occupancy_df %>%
    dplyr::mutate(weekDay = weekdays(dateTime)) %>%
    dplyr::mutate(date = as.Date(dateTime), time = format(dateTime,"%H:%M:%S")) %>%
    dplyr::group_by(weekDay, time) %>%
    dplyr::summarise(average = mean(hourly_occupancy), Q1 = quantile(hourly_occupancy,0.25), Q3 = quantile(hourly_occupancy,0.75),
                     Max = max(hourly_occupancy), Min = min(hourly_occupancy)) %>%
    dplyr::ungroup()%>%
    dplyr::mutate(weekDay = ordered(weekDay, levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                 "Friday", "Saturday", "Sunday"))) %>%
    dplyr::arrange(weekDay) %>%
    dplyr::mutate(numberCode = 1:168) %>%
    dplyr::select(-weekDay)

  occupancy_df

}


#' ED_day_hour_plot
#'
#' @param startDay Date for which the analysis starts
#' @param endDay Date for which analysis ends
#' @param df Dataframe representing ED stays with start_ime and end_time specifying stay start and end of time in ED
#' @param hospital_name Name of hospital for which the analysis is being done
#' @param plot_chat Plots chart if set to TRUE, else returns dataframe of plot data
#'
#' @return A plot or dataframe with mean, Q1, Q3, max and min occupancy for each hour of the week
#' @export
#'
#' @examples
ED_day_hour_plot <- function(startDay, endDay, df,
                             hospital_name = "Hospital Name",
                             plot_chart = T){

  occupancy_df <- ED_day_hour(startDay, endDay, df)

  #tick lables for the x axis
  breaksVect <- occupancy_df$numberCode[(occupancy_df$numberCode%%4)-1 ==0]
  labelsVect <- substr(occupancy_df$time,1,2)
  labelsVect <- (labelsVect[breaksVect])

  occupancy_df <- dplyr::select(occupancy_df, -time)
  occupancy_df <- reshape2::melt(occupancy_df, id.vars = c('numberCode',"Q1","Q3", "Max","Min"),
                                 variable.name = 'Analysis')

  p <- ggplot2::ggplot(occupancy_df,
                       ggplot2::aes(x= numberCode, y = value)) +
    ggplot2::geom_line(ggplot2::aes(colour = Analysis),
                                size = 1) +
    ggplot2::scale_x_continuous(breaks= breaksVect,
                                labels= labelsVect) +
    ggplot2::geom_vline(xintercept=(24*(1:6))+1,
                        linetype = "longdash") +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::geom_ribbon(data = occupancy_df,
                         ggplot2::aes(ymin = Q1, ymax = Q3, fill = "interquartile range"),
                         alpha = "0.2") +
    ggplot2::geom_ribbon(data = occupancy_df,
                         ggplot2::aes(ymin = Min, ymax = Max, fill = "range"),
                         alpha = "0.2") +
    ggplot2::scale_colour_manual("",
                                 values = "red") +
    ggplot2::scale_fill_manual("",
                               values = c("blue","steel blue")) +
    # ggplot2::labs(title = "ED 'occupancy' by day and hour",
    #               subtitle = paste(hospital_name,"\nbetween",strftime(startDay, "%d/%b/%Y"),"and",strftime(endDay, "%d/%b/%Y")),
    #               caption = "Source: CLAHRC NWL") +
    ggplot2::labs(title = paste0(hospital_name, ": ED 'occupancy' by day and hour "),
                  subtitle = paste0("From ", strftime(startDay, "%d/%B/%Y")," to ", strftime(endDay, "%d/%B/%Y"),"\nNote: results are intended for management information only"),
                  y = "ED Occupancy",
                  x = "Hour in the day",
                  caption = "Source: CLAHRC NWL") +
    ggplot2::annotate(geom = "text",
                        x = c(12, 12+24, 12+(2*24), 12+(3*24), 12+(4*24), 12+(5*24), 12+(6*24)),
                        y = Inf,
                        label = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"),
                        size = 3.5,
                        vjust = 1.5,
                        fontface = "bold")

  if(plot_chart == T){
    p
  }else{
    occupancy_df
  }

}


