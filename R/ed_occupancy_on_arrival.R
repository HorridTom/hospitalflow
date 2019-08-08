# need a skeleton

# functions to calculate and plot the registered ED occupancy on arrival against the unscheduled attendances with LoS >4hrs,
# wait for assessment and unscheduled ED attendance


# standard occupancy function
occupancy <- function(occupancyDateTime, df){


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
    dplyr::mutate(occupancy_on_arrival = occupancy(start_datetime, df))

  df

}


#function to calculate dataframe and plot % of unscheduled ED attendances with ED LoS >4hrs and wait for treatment >4hrs against ED occupancy on arrival
wait_times_over_4hrs_Vs_occupancy <- function(df, startDate, endDate, hospitalName, plotChart = T){


  df <- ed_occupancy_on_arrival(df)
  df <- df %>%
    dplyr::filter(start_datetime >= startDate, end_datetime <= endDate) %>%
    dplyr::filter(attendance_category != "Follow up - Planned") %>%
    dplyr::mutate(LoS_hrs =
                    as.numeric(difftime(end_datetime, start_datetime, units = "hours"))) %>%
    dplyr::mutate(wait_for_assess_hrs =
                    as.numeric(difftime(initial_assess_datetime, start_datetime, units = "hours"))) %>%
    dplyr::mutate(wait_for_treatment_hrs =
                    as.numeric(difftime(treatment_datetime, start_datetime, units = "hours")))

  df

  df_all <- df %>%
    dplyr::count(occupancy_on_arrival) %>%
    dplyr::rename(n_all = n)

  #LoS analysis
  df_LoS_over_4hrs <- df %>%
    dplyr::filter(LoS_hrs >  4) %>%
    dplyr::count(occupancy_on_arrival) %>%
    dplyr::rename(n_LoS_over_4hrs = n)


  df_LoS_merged <- dplyr::inner_join(df_all, df_LoS_over_4hrs)
  df_LoS_merged <- df_LoS_merged %>%
    dplyr::mutate(perc_LoS_over_4hrs = (n_LoS_over_4hrs*100)/n_all)

  #wait for treatment (W4T) analysis
  df_W4T_over_4hrs <- df %>%
    dplyr::filter(wait_for_treatment_hrs >  4) %>%
    dplyr::count(occupancy_on_arrival) %>%
    dplyr::rename(n_W4T_over_4hrs = n)


  df_W4T_merged <- dplyr::inner_join(df_all, df_W4T_over_4hrs)
  df_W4T_merged <- df_W4T_merged %>%
    dplyr::mutate(perc_W4T_over_4hrs = (n_W4T_over_4hrs*100)/n_all)


  df_to_plot <- dplyr::inner_join(df_LoS_merged, df_W4T_merged)

  #generate deciles from attendances profile
  deciles <- quantile(df$occupancy_on_arrival, probs = seq(0, 1, length = 11), type = 5)
  deciles_tib <- tibble(deciles = deciles, labels = paste(seq(0, 100, 10),"%"))
  deciles_tib <- filter(deciles_tib, deciles > df_to_plot$occupancy_on_arrival[1], deciles < df_to_plot$occupancy_on_arrival[nrow(df_to_plot)])


  plot <- ggplot2::ggplot(df_to_plot,
                          aes(x = occupancy_on_arrival)) +
    ggplot2::geom_line(aes(y=n_all, colour = "n_all")) +
    ggplot2::geom_smooth(aes(y=perc_LoS_over_4hrs,
                             colour = "perc_LoS_over_4hrs")) +
    ggplot2::geom_smooth(aes(y=perc_W4T_over_4hrs,
                             colour = "per_W4T_over_4hrs")) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = element_blank(),
                   legend.position = "bottom") +
    ggplot2::labs(title = "Occupancy on Arrival versus Average Percentage of Unscheduled \nED Attendances with LoS and Wait Time for Treatment over 4hrs",
                  subtitle = paste(hospitalName,"\nbetween ", strftime(startDate, "%d/%m/%Y"),
                                   " and ", strftime(endDate, "%d/%m/%Y")),
                  y = "Percentage over 4hrs, %", x = "Occupancy on arrival",
                  caption = "Source: CLAHRC NWL") +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::scale_colour_manual(labels = c("Number of Attendances",
                                  "Average % of unscheduled ED \nattendances with LoS over 4hrs",
                                  "Average % of unscheduled ED attendances \nwith wait time for treatment over 4hrs"),
                                values = c("dodgerblue2", "firebrick3", "seagreen4")) +
    ggplot2::guides(color = guide_legend(reverse = TRUE)) +
    ggplot2::geom_vline(xintercept = deciles_tib$deciles,
                        linetype = "dashed",
                        colour = "azure3",
                        size=0.5) +
    ggplot2::annotate(geom ="text",
                      x = deciles_tib$deciles,
                      y = Inf,
                      label = deciles_tib$labels,
                      size = 3,
                      vjust = 3) #+
    # ggplot2::annotate(geom = "text",
    #                   x = deciles[1],
    #                   y = Inf, label = "deciles:",
    #                   size = 3,
    #                   vjust = 1.5)


  if(plotChart == T){
    plot
  }else{
    df_to_plot
  }

 }





