
#' occupancy_on_arrival
#'
#' @param df dataframe of ED data with start_datetime and end_datetime representing start and end of a patient's time in ED
#'
#'
#' @return the input df with an extra column containing the corresponding occupancy on arrival for each patient
#'
#'
#' @examples
occupancy_on_arrival <- function(df){

  df <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(occupancy_on_arrival = hospitalflow::occupancy(start_datetime, df,
                                                                 start_time = "start_datetime",
                                                                 end_time = "end_datetime"))

  df

}



#' ed_occupancy_on_arrival_plot
#'
#' @param df dataframe of ED data with start_datetime and end_datetime representing start and end of a patient's time in ED
#' @param startDate the first date of the period for which the analysis will run
#' @param endDate the last date of the period for which the analysis will run
#' @param hospital_name the name of the hospital for which the analysis is being done
#' @param plotChart plots the graph if set to TRUE, returns dataframe of the plot variables if set to FALSE
#'
#' @return graph or dataframe of % of unscheduled ED attendances with ED LoS >4hrs and wait for treatment >4hrs
#' against ED occupancy on arrival
#' @export
#'
#' @examples
ed_occupancy_on_arrival_plot <- function(df, startDate, endDate, hospital_name = "Hospital Name", plotChart = T){

  df <- dplyr::filter(df, start_datetime <= endDate, end_datetime >= startDate)

  df <- occupancy_on_arrival(df)
  df <- df %>%
    dplyr::filter(start_datetime >= startDate, end_datetime <= endDate) %>%
    dplyr::filter(attendance_category != "Follow up - Planned") %>%
    dplyr::mutate(LoS_hrs =
                    as.numeric(difftime(end_datetime, start_datetime, units = "hours"))) %>%
    dplyr::mutate(wait_for_assess_hrs =
                    as.numeric(difftime(initial_assess_datetime, start_datetime, units = "hours"))) %>%
    dplyr::mutate(wait_for_treatment_hrs =
                    as.numeric(difftime(treatment_datetime, start_datetime, units = "hours")))


  df_all <- df %>%
    dplyr::count(occupancy_on_arrival) %>%
    dplyr::rename(n_all = n)

  #LoS analysis
  df_LoS_over_4hrs <- df %>%
    dplyr::filter(LoS_hrs >  4) %>%
    dplyr::count(occupancy_on_arrival) %>%
    dplyr::rename(n_LoS_over_4hrs = n)


  df_LoS_merged <- dplyr::left_join(df_all, df_LoS_over_4hrs)
  df_LoS_merged <- df_LoS_merged %>%
    dplyr::mutate(perc_LoS_over_4hrs = (n_LoS_over_4hrs*100)/n_all)

  #wait for treatment (W4T) analysis
  df_W4T_over_4hrs <- df %>%
    dplyr::filter(wait_for_treatment_hrs >  4) %>%
    dplyr::count(occupancy_on_arrival) %>%
    dplyr::rename(n_W4T_over_4hrs = n)


  df_W4T_merged <- dplyr::left_join(df_all, df_W4T_over_4hrs)
  df_W4T_merged <- df_W4T_merged %>%
    dplyr::mutate(perc_W4T_over_4hrs = (n_W4T_over_4hrs*100)/n_all)


  #putting both analyses together in the right form to plot
  df_to_plot <- dplyr::full_join(df_LoS_merged, df_W4T_merged)
  df_to_plot <- df_to_plot %>%
    dplyr::mutate(n_LoS_over_4hrs = plyr::mapvalues(n_LoS_over_4hrs, from = NA, to = 0, warn_missing = F)) %>%
    dplyr::mutate(perc_LoS_over_4hrs = plyr::mapvalues(perc_LoS_over_4hrs, from = NA, to = 0, warn_missing = F)) %>%
    dplyr::mutate(n_W4T_over_4hrs = plyr::mapvalues(n_W4T_over_4hrs, from = NA, to = 0, warn_missing = F)) %>%
    dplyr::mutate(perc_W4T_over_4hrs = plyr::mapvalues(perc_W4T_over_4hrs, from = NA, to = 0, warn_missing = F))


  #generate deciles from attendances profile
  deciles <- stats::quantile(df$occupancy_on_arrival, probs = seq(0, 1, length = 11), type = 5)
  deciles_tib <- tibble::tibble(deciles = deciles, labels = paste(seq(0, 100, 10),"%"))
  deciles_tib <- dplyr::filter(deciles_tib, deciles > df_to_plot$occupancy_on_arrival[1], deciles < df_to_plot$occupancy_on_arrival[nrow(df_to_plot)])


  plot <- ggplot2::ggplot(df_to_plot,
                          ggplot2::aes(x = occupancy_on_arrival)) +
    #ggplot2::geom_line(ggplot2::aes(y=n_all, colour = "n_all")) +
    ggplot2::geom_smooth(ggplot2::aes(y=perc_LoS_over_4hrs,
                                      colour = "perc_LoS_over_4hrs"),
                         se = FALSE) +
    ggplot2::geom_point(ggplot2::aes(y=perc_LoS_over_4hrs,
                                     colour = "perc_LoS_over_4hrs")) +
    ggplot2::geom_smooth(ggplot2::aes(y=perc_W4T_over_4hrs,
                                      colour = "perc_W4T_over_4hrs"),
                         se = FALSE) +
    ggplot2::geom_point(ggplot2::aes(y=perc_W4T_over_4hrs,
                                     colour = "perc_W4T_over_4hrs")) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.position = "bottom") +
    ggplot2::labs(title = paste0(hospital_name, ": ED 'Occupancy' on Arrival versus Percentage of \nUnscheduled ED Attendances with LoS and Wait Time for Treatment over 4hrs"),
                  subtitle = paste0("From ", strftime(startDate, "%d/%B/%Y")," to ", strftime(endDate, "%d/%B/%Y"),"\nNote: results are intended for management information only"),
                  y = "Percentage over 4hrs, %",
                  x = "ED 'Occupancy' on arrival",
                  caption = "Vertical lines represent deciles of number of attendances at given occupancies on arrival                       \nSource: CLAHRC NWL") +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::scale_colour_manual(labels = c(#"Number of Attendances",
                                  "% of unscheduled ED attendances \nwith LoS in ED over 4hrs",
                                  "% of unscheduled ED attendances \nwith wait time for treatment over 4hrs"),
                                values = c(#"seagreen4",
                                           "orangered2", "dodgerblue2")) +
    ggplot2::geom_vline(xintercept = deciles_tib$deciles,
                        linetype = "dashed",
                        colour = "grey54",
                        size=0.5,
                        show.legend = T) +
    ggplot2::annotate(geom ="text",
                      x = deciles_tib$deciles,
                      y = Inf,
                      label = deciles_tib$labels,
                      size = 3,
                      vjust = 3)


  if(plotChart == T){
    plot
  }else{
    df_to_plot
  }

 }





