
#' ED_day_hour
#'
#' @param startDay Date for which the analysis starts
#' @param endDay Date for which analysis ends
#' @param df Dataframe representing ED stays with start_ime and end_time specifying stay start and end of time in ED
#' @param start_time Name of column in df containing stay start time in ED
#' @param end_time Name of column in df containing stay end time in ED
#'
#' @return A dataframe with mean, Q1, Q3, max and min occupancy for each hour of the week
#'
#'
#' @examples
ED_day_hour <- function(startDay, endDay, df,
                        start_time = "start_datetime", end_time = "end_datetime"
                        ){

  #getting the dataframe into the right format
  dateTime <- seq(as.POSIXlt(startDay), as.POSIXlt(endDay), by="hours")
  hourly_occupancy <- sapply(dateTime, hospitalflow::occupancy, df, start_time, end_time)
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

}


#' ED_day_hour_plot
#'
#' @param startDay Date for which the analysis starts
#' @param endDay Date for which analysis ends
#' @param df Dataframe representing ED stays with start_ime and end_time specifying stay start and end of time in ED
#' @param start_time Name of column in df containing stay start time in ED
#' @param end_time Name of column in df containing stay end time in ED
#' @param plot_chat Plots chart if set to TRUE, else returns dataframe of plot data
#'
#' @return A plot or dataframe with mean, Q1, Q3, max and min occupancy for each hour of the week
#' @export
#'
#' @examples
ED_day_hour_plot <- function(startDay, endDay, df,
                             start_time = "start_datetime", end_time = "end_datetime",
                             plot_chart = T){

  #text for annotation
  mon <- grid::textGrob("Monday", gp=gpar(fontsize=10, fontface="bold"))
  tues <- grid::textGrob("Tuesday", gp=gpar(fontsize=10, fontface="bold"))
  wed <- grid::textGrob("Wednesday", gp=gpar(fontsize=10, fontface="bold"))
  thurs <- grid::textGrob("Thursday", gp=gpar(fontsize=10, fontface="bold"))
  fri <- grid::textGrob("Friday", gp=gpar(fontsize=10, fontface="bold"))
  sat <- grid::textGrob("Saturday", gp=gpar(fontsize=10, fontface="bold"))
  sun <- grid::textGrob("Sunday", gp=gpar(fontsize=10, fontface="bold"))

  occupancy_df <- ED_day_hour(startDay, endDay, df, start_time, end_time)

  #tick lables for the x axis
  breaksVect <- occupancy_df$numberCode[(occupancy_df$numberCode%%4)-1 ==0]
  labelsVect <- substr(occupancy_df$time,1,2)
  labelsVect <- (labelsVect[breaksVect])

  occupancy_df <- dplyr::select(occupancy_df, -time)
  occupancy_df <- reshape2::melt(occupancy_df, id.vars = c('numberCode',"Q1","Q3", "Max","Min"), variable.name = 'Analysis')

  p <- ggplot2::ggplot(occupancy_df,
                       aes(x= numberCode, y = value)) +
    ggplot2::geom_line(aes(colour = Analysis),
              size = 1) +
    ggplot2::scale_x_continuous(breaks= breaksVect,
                                labels= labelsVect) +
    ggplot2::geom_vline(xintercept=(24*(1:6))+1,
                        linetype = "longdash") +
    ggplot2::xlab("Hour in the day") +
    ggplot2::ylab("ED Occupancy") +
    ggplot2::ggtitle(paste("ED 'occupancy' by day and hour \nbetween",startDay,"and",endDay)) +
    ggplot2::theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    ggplot2::theme(plot.margin = unit(c(1,1,5,1), "lines")) +
    ggplot2::theme(axis.title.x = element_text(vjust=-8)) +
    ggplot2::theme(axis.text.x=element_text(angle=90, vjust=0.5)) +
    ggplot2::geom_ribbon(data=occupancy_df,
                         aes(ymin=Q1,ymax=Q3, fill="interquartile range"),
                         alpha="0.2") +
    ggplot2::geom_ribbon(data=occupancy_df,
                         aes(ymin=Min,ymax=Max, fill="range"),
                         alpha="0.2") +
    ggplot2::scale_colour_manual("",
                                 values="red") +
    ggplot2::scale_fill_manual("",
                               values=c("blue","steel blue"))

  #setting where the day labels are placed in y coordinates
  ymin <- -(ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y.range[2] / 6)
  ymax <- ymin

  p <- p +
    ggplot2::annotation_custom(mon,xmin=0,xmax=24,ymin=ymin,ymax=ymax) +
    ggplot2::annotation_custom(tues,xmin=24,xmax=24*2,ymin=ymin,ymax=ymax) +
    ggplot2::annotation_custom(wed,xmin=24*2,xmax=24*3,ymin=ymin,ymax=ymax) +
    ggplot2::annotation_custom(thurs,xmin=24*3,xmax=24*4,ymin=ymin,ymax=ymax) +
    ggplot2::annotation_custom(fri,xmin=24*4,xmax=24*5,ymin=ymin,ymax=ymax) +
    ggplot2::annotation_custom(sat,xmin=24*5,xmax=24*6,ymin=ymin,ymax=ymax) +
    ggplot2::annotation_custom(sun,xmin=24*6,xmax=24*7,ymin=ymin,ymax=ymax)

  # gt <- ggplot2::ggplot_gtable(ggplot_build(p))
  # gt$layout$clip[gt$layout$name == "panel"] <- "off"
  # grid::grid.draw(gt)

  if(plot_chart == T){
    gt <- ggplot2::ggplot_gtable(ggplot_build(p))
    gt$layout$clip[gt$layout$name == "panel"] <- "off"
    grid::grid.draw(gt)
  }else{
    occupancy_df
  }

}
