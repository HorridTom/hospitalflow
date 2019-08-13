
#' ed_occupancy_on_arrival
#'
#' @param df dataframe of ED data with start_datetime and end_datetime representing start and end of a patient's time in ED
#'
#'
#' @return the input df with an extra column containing the corresponding occupancy on arrival for each patient
#'
#'
#' @examples
ed_occupancy_on_arrival <- function(df){

  df <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(occupancy_on_arrival = hospitalflow::occupancy(start_datetime, df,
                                                                 start_time = "start_datetime",
                                                                 end_time = "end_datetime"))

  df

}



#' wait_times_over_4hrs_Vs_occupancy
#'
#' @param df dataframe of ED data with start_datetime and end_datetime representing start and end of a patient's time in ED
#' @param startDate the first date of the period for which the analysis will run
#' @param endDate the last date of the period for which the analysis will run
#' @param hospitalName the name of the hospital for which the analysis is being done
#' @param plotChart plots the graph if set to TRUE, returns dataframe of the plot variables if set to FALSE
#'
#' @return graph or dataframe of % of unscheduled ED attendances with ED LoS >4hrs and wait for treatment >4hrs
#' against ED occupancy on arrival
#' @export
#'
#' @examples
wait_times_over_4hrs_Vs_occupancy <- function(df, startDate, endDate, hospitalName, plotChart = T){

  df <- dplyr::filter(df, start_datetime <= endDate, end_datetime >= startDate)

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
  deciles <- stats::quantile(df$occupancy_on_arrival, probs = seq(0, 1, length = 11), type = 5)
  deciles_tib <- tibble::tibble(deciles = deciles, labels = paste(seq(0, 100, 10),"%"))
  deciles_tib <- dplyr::filter(deciles_tib, deciles > df_to_plot$occupancy_on_arrival[1], deciles < df_to_plot$occupancy_on_arrival[nrow(df_to_plot)])


  plot <- ggplot2::ggplot(df_to_plot,
                          ggplot2::aes(x = occupancy_on_arrival)) +
    ggplot2::geom_line(ggplot2::aes(y=n_all, colour = "n_all")) +
    ggplot2::geom_smooth(ggplot2::aes(y=perc_LoS_over_4hrs,
                             colour = "perc_LoS_over_4hrs")) +
    ggplot2::geom_smooth(ggplot2::aes(y=perc_W4T_over_4hrs,
                             colour = "per_W4T_over_4hrs")) +
    ggplot2::geom_point(ggplot2::aes(y=perc_LoS_over_4hrs,
                             colour = "perc_LoS_over_4hrs")) +
    ggplot2::geom_point(ggplot2::aes(y=perc_W4T_over_4hrs,
                             colour = "per_W4T_over_4hrs")) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.position = "bottom") +
    ggplot2::labs(title = "Occupancy on Arrival versus Percentage of Unscheduled \nED Attendances with LoS and Wait Time for Treatment over 4hrs",
                  subtitle = paste(hospitalName,"\nbetween ", strftime(startDate, "%d/%m/%Y"),
                                   " and ", strftime(endDate, "%d/%m/%Y")),
                  y = "Percentage over 4hrs, %", x = "Occupancy on arrival",
                  caption = "Source: CLAHRC NWL") +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::scale_colour_manual(labels = c("Number of Attendances",
                                  "% of unscheduled ED \nattendances with LoS over 4hrs",
                                  "% of unscheduled ED attendances \nwith wait time for treatment over 4hrs"),
                                values = c("dodgerblue2", "firebrick3", "seagreen4")) +
    ggplot2::guides(color = ggplot2::guide_legend(reverse = TRUE)) +
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





