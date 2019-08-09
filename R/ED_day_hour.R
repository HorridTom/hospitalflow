library(tidyverse)
library(lubridate)
library(testthat)
library(reshape2)
library(grid)


#function to calculate and plot the Unscheduled ED 'occupancy' by day and hour between startDay and endDay

#function to calculate the ED occupancy at a given datetime
ED_day_hour_occupancy <- function(dateTime,startDay, endDay, df = subset_chel_wes_2011_2015_df,
                                  episodeStartTime = "start_datetime", episodeEndTime = "end_datetime"
                                  ){

  #reduces df to only inlcude A&E columns
  #wardCol <- as.name(wardCol)
  episodeStartTime <- as.name(episodeStartTime)
  episodeEndTime <- as.name(episodeEndTime)


  #point interval for the time that you are calculating the occupancy for
  testInterval <- interval(dateTime, dateTime)

  df <- df %>%
    #filter(!!wardCol == !!AnEWardCode & !!episodeNumberCol == 1) %>%
    filter(!!episodeStartTime > !!startDay & !!episodeEndTime < !!endDay) %>%
    mutate(stayInterval = interval(!!episodeStartTime,!!episodeEndTime)) %>%
    mutate(overLapTest = int_overlaps(stayInterval, testInterval))

  occupancy <- sum(df$overLapTest)
  occupancy

}

#function to calculate and put together the dataframe the Unscheduled ED 'occupancy' by day and hour between startDay and endDay
#put as a separate function from the plot function to make testing easier
ED_day_hour <- function(startDay, endDay, df = subset_chel_wes_2011_2015_df, #wardCol = "WARD_CODE",
                        episodeStartTime = "start_datetime", episodeEndTime = "end_datetime"
                        ){

  #getting the dataframe into the right format
  dateTime <- seq(as.POSIXlt(startDay), as.POSIXlt(endDay), by="hours")
  hourly_occupancy <- sapply(dateTime, ED_day_hour_occupancy, startDay, endDay,df,
                             episodeStartTime, episodeEndTime)
  occupancy_df <- data.frame(dateTime, hourly_occupancy)

  occupancy_df <- occupancy_df %>%
    mutate(weekDay = weekdays(dateTime)) %>%
    mutate(date = as.Date(dateTime), time = format(dateTime,"%H:%M:%S")) %>%
    group_by(weekDay, time) %>%
    summarise(average = mean(hourly_occupancy), Q1 = quantile(hourly_occupancy,0.25), Q3 = quantile(hourly_occupancy,0.75),
              Max = max(hourly_occupancy), Min = min(hourly_occupancy)) %>%
    ungroup()%>%
    mutate(weekDay = ordered(weekDay, levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                 "Friday", "Saturday", "Sunday"))) %>%
    arrange(weekDay) %>%
    mutate(numberCode = 1:168) %>%
    select(-weekDay)

}



ED_day_hour_plot <- function(startDay, endDay, df = subset_chel_wes_2011_2015_df,
                             episodeStartTime = "start_datetime", episodeEndTime = "end_datetime"
                            ){

  #text for annotation
  mon <- textGrob("Monday", gp=gpar(fontsize=10, fontface="bold"))
  tues <- textGrob("Tuesday", gp=gpar(fontsize=10, fontface="bold"))
  wed <- textGrob("Wednesday", gp=gpar(fontsize=10, fontface="bold"))
  thurs <- textGrob("Thursday", gp=gpar(fontsize=10, fontface="bold"))
  fri <- textGrob("Friday", gp=gpar(fontsize=10, fontface="bold"))
  sat <- textGrob("Saturday", gp=gpar(fontsize=10, fontface="bold"))
  sun <- textGrob("Sunday", gp=gpar(fontsize=10, fontface="bold"))

  occupancy_df <- ED_day_hour(startDay, endDay, df, episodeStartTime,
                              episodeEndTime)

  #tick lables for the x axis
  breaksVect <- occupancy_df$numberCode[(occupancy_df$numberCode%%4)-1 ==0]
  labelsVect <- substr(occupancy_df$time,1,2)
  labelsVect <- (labelsVect[breaksVect])

  occupancy_df <- select(occupancy_df, -time)
  occupancy_df <- melt(occupancy_df, id.vars = c('numberCode',"Q1","Q3", "Max","Min"), variable.name = 'Analysis')

  p <- ggplot(occupancy_df, aes(x= numberCode, y = value)) + geom_line(aes(colour = Analysis),size = 1) +
    scale_x_continuous(breaks= breaksVect,labels= labelsVect) +
    geom_vline(xintercept=(24*(1:6))+1, linetype = "longdash") +
    xlab("Hour in the day") + ylab("ED Occupancy") +
    ggtitle(paste("ED 'occupancy' by day and hour \nbetween",startDay,"and",endDay)) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme(plot.margin = unit(c(1,1,5,1), "lines")) +
    theme(axis.title.x = element_text(vjust=-8)) +
    theme(axis.text.x=element_text(angle=90, vjust=0.5)) +
    geom_ribbon(data=occupancy_df,aes(ymin=Q1,ymax=Q3, fill="interquartile range"),alpha="0.2") +
    geom_ribbon(data=occupancy_df,aes(ymin=Min,ymax=Max, fill="range"), alpha="0.2") +
    scale_colour_manual("",values="red") +
    scale_fill_manual("",values=c("blue","steel blue"))

  #setting where the day labels are placed in y coordinates
  ymin <- -(ggplot_build(p)$layout$panel_params[[1]]$y.range[2] / 6)
  ymax <- ymin

  p <- p +
    annotation_custom(mon,xmin=0,xmax=24,ymin=ymin,ymax=ymax) +
    annotation_custom(tues,xmin=24,xmax=24*2,ymin=ymin,ymax=ymax) +
    annotation_custom(wed,xmin=24*2,xmax=24*3,ymin=ymin,ymax=ymax) +
    annotation_custom(thurs,xmin=24*3,xmax=24*4,ymin=ymin,ymax=ymax) +
    annotation_custom(fri,xmin=24*4,xmax=24*5,ymin=ymin,ymax=ymax) +
    annotation_custom(sat,xmin=24*5,xmax=24*6,ymin=ymin,ymax=ymax) +
    annotation_custom(sun,xmin=24*6,xmax=24*7,ymin=ymin,ymax=ymax)

  gt <- ggplot_gtable(ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid.draw(gt)

}
