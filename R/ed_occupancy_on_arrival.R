# need a skeleton

# functions to calculate and plot the registered ED occupancy on arrival against the unscheduled attendances with LoS >4hrs,
# wait for assessment and unscheduled ED attendance


# standard occupancy function
ed_occupancy <- function(occupancyDateTime, df){


  #point interval for the time that you are calculating the occupancy for
  testInterval <- lubridate::interval(occupancyDateTime, occupancyDateTime)

  df <- df %>%
    #this line should make the code faster but currently having problems with the >= for datetimes
    #dplyr::filter(start_datetime <= occupancyDateTime & end_datetime >= occupancyDateTime) %>%
    dplyr::mutate(stayInterval = lubridate::interval(start_datetime,end_datetime)) %>%
    dplyr::mutate(overLapTest = lubridate::int_overlaps(stayInterval, testInterval))


  occupancy <- sum(df$overLapTest)

  occupancy

}


# function to calculate the ED occupancy on arrival which is added as a column to the input dataframe
# will try two methods: calculate each row individually and create a table and use lookup
ed_occupancy_on_arrival <- function(df = test_ed_occupancy_on_arrival_data){

  df <- df %>%
    rowwise %>%
    dplyr::mutate(occupancy_on_arrival = ed_occupancy(start_datetime, df))

  df

}


# #function to calculate dataframe and plot unscheduled ED attendances with ED LoS >4hrs against ED occupancy on arrival
# wait_for_assessment_Vs_occupancy <- function(df, hosp){
#
#   df <- ed_occupancy_on_arrival(df)
#   df <- df %>%
#     dplyr::mutate(wait_for_assess_hrs = difftime(initial_assess_datetime, start_datetime, units = "hours")) %>%
#     dplyr::mutate(wait_for_assess_hrs = as.numeric(wait_for_assess_hrs))
#
#   ggplot2::ggplot(df, aes(x=occupancy_on_arrival, y=wait_for_assess_hrs)) +
#     geom_point(size=2, colour = "red") +
#     geom_smooth() +
#     ggtitle(hosp)
#
#
#   # ggplot2::ggplot(df, aes(x=occupancy_on_arrival, y=wait_for_assess_hrs)) +
#   #   #geom_point(size=2, colour = "red") +
#   #   geom_line(size=1.3, colour="steel blue") +
#   #   ggtitle(hosp)
#
# }


#function to calculate dataframe and plot % of unscheduled ED attendances with ED LoS >4hrs, wait for assessment >4hrs and wait for treatment >4hrs against ED occupancy on arrival
wait_times_over_4hrs_Vs_occupancy <- function(df, startDate, endDate, hospitalName, plotChart = T){

  df <- df %>%
    dplyr::filter(start_datetime >= startDate, end_datetime <= endDate) %>%
    dplyr::filter(attendance_category != "Follow up - Planned")
  df <- ed_occupancy_on_arrival(df)
  df <- df %>%
    dplyr::mutate(LoS_hrs = as.numeric(difftime(end_datetime, start_datetime, units = "hours"))) %>%
    dplyr::mutate(wait_for_assess_hrs = as.numeric(difftime(initial_assess_datetime, start_datetime, units = "hours"))) %>%
    dplyr::mutate(wait_for_treatment_hrs = as.numeric(difftime(treatment_datetime, start_datetime, units = "hours")))

  #LoS analysis
  df_LoS_over_4hrs <- df %>%
    dplyr::filter(LoS_hrs >  4) %>%
    dplyr::count(occupancy_on_arrival) %>%
    dplyr::rename(n_LoS_over_4hrs = n)

  df_LoS_all <- df %>%
    dplyr::count(occupancy_on_arrival) %>%
    dplyr::rename(n_LoS_all = n)

  df_LoS_merged <- dplyr::inner_join(df_LoS_all, df_LoS_over_4hrs)
  df_LoS_merged <- df_LoS_merged %>%
    dplyr::mutate(perc_LoS_over_4hrs = (n_LoS_over_4hrs*100)/n_LoS_all)

  #wait for treatment (W4T) analysis
  df_W4T_over_4hrs <- df %>%
    dplyr::filter(wait_for_treatment_hrs >  4) %>%
    dplyr::count(occupancy_on_arrival) %>%
    dplyr::rename(n_W4T_over_4hrs = n)

  df_W4T_all <- df %>%
    dplyr::count(occupancy_on_arrival) %>%
    dplyr::rename(n_W4T_all = n)

  df_W4T_merged <- dplyr::inner_join(df_W4T_all, df_W4T_over_4hrs)
  df_W4T_merged <- df_W4T_merged %>%
    dplyr::mutate(perc_W4T_over_4hrs = (n_W4T_over_4hrs*100)/n_W4T_all)


  df_to_plot <- dplyr::inner_join(df_LoS_merged, df_W4T_merged)

  plot <- ggplot2::ggplot(df_to_plot, aes(occupancy_on_arrival)) +
    geom_smooth(aes(y=perc_LoS_over_4hrs, colour = "perc_LoS_over_4hrs")) +
    geom_smooth(aes(y=perc_W4T_over_4hrs, colour = "per_W4T_over_4hrs")) +
    theme_bw() +
    theme(legend.title = element_blank(), legend.position = "bottom") +
    labs(title = "Occupancy on Arrival versus Average Percentage of Unscheduled \nED Attendances with LoS and Wait Time for Treatment over 4hrs",
         subtitle = paste(hospitalName,"\nbetween ", strftime(startDate, "%d/%m/%Y"), " and ", strftime(endDate, "%d/%m/%Y")),
         y = "Percentage over 4hrs, %", x = "Occupancy on arrival", caption = "Source: CLAHRC NWL") +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    scale_y_continuous(breaks = scales::pretty_breaks()) +
    scale_color_manual(labels = c("Average % of unscheduled ED attendances \nwith wait time for treatment over 4hrs",
                                  "Average % of unscheduled ED \nattendances with LoS over 4hrs"),
                       values = c("blue", "red")) +
    guides(color = guide_legend(reverse = TRUE))


  if(plotChart == T){
    plot
  }else{
    df_to_plot
  }

 }





# plot_it <- function(df_to_plot, hospitalName, startDate, endDate){
#
#   ggplot2::ggplot(df_to_plot, aes(occupancy_on_arrival)) +
#     geom_smooth(aes(y=perc_LoS_over_4hrs, colour = "perc_LoS_over_4hrs")) +
#     geom_smooth(aes(y=perc_W4T_over_4hrs, colour = "per_W4T_over_4hrs")) +
#     theme_bw() +
#     theme(legend.title = element_blank(), legend.position = "bottom") +
#     labs(title = "Occupancy on Arrival versus Average Percentage of Unscheduled \nED Attendances with LoS and Wait Time for Treatment over 4hrs",
#                   subtitle = paste(hospitalName," between ", strftime(startDate, "%d/%m/%Y"), " and ", strftime(endDate, "%d/%m/%Y")),
#                   y = "Percentage over 4hrs, %", x = "Occupancy on arrival", caption = "Source: CLAHRC NWL") +
#     scale_x_continuous(breaks = scales::pretty_breaks()) +
#     scale_y_continuous(breaks = scales::pretty_breaks()) +
#     scale_color_manual(labels = c("Average % of unscheduled ED attendances \nwith wait time for treatment over 4hrs",
#                                                  "Average % of unscheduled ED \nattendances with LoS over 4hrs"),
#                        values = c("blue", "red")) +
#     guides(color = guide_legend(reverse = TRUE))
#
# }
