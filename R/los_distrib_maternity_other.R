# Length of stay for Maternity and Other babies


#' length of stay for maternigy and other babies
#'
#' @param start_date date of earliest discharge to be included in the analysis
#' @param end_date date of latest admission to be included in the analysis
#' @param data hospital episode data
#' @param plot_chart if TRUE return chart, otherwise if FALSE return dataframe
#' @param hospital name string for the hospital under analysis
#'
#' @return Chart or dataframe showing counts for Maternity and Other Babies for the 8 hrs bins to 7d, 7d bins to 28 days, and more than 28 days
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' hospitalflow::los_distrib_maternity_other(start_date = as.Date("2015-01-01", tz = "Europe/London"), end_date = as.Date("2015-03-01", tz= "Europe/London"),
#'                                    data = example_data, plot_chart = TRUE, hospital_name = "Hospital under analysis")
#' }
los_distrib_maternity_other <- function(start_date = as.Date("2012-01-01", tz = "Europe/London"),end_date = as.Date("2015-01-01", tz = "Europe/London"),
                                        data, plot_chart, hospital_name = "Chelsea & Westminster"){

  #
  dt <-  data %>%
    dplyr::filter(Admissions < end_date  &  Discharges > start_date) %>%
    dplyr::filter(Episode_number == 1) %>%
    dplyr::mutate(
      Admitted_date = as.Date(Admissions),
      Discharge_date = as.Date(Discharges),
      Same_day_discharge = dplyr::if_else(Admitted_date == Discharge_date, TRUE, FALSE),
      Adm_period = dplyr::if_else(Admissions > start_date, TRUE, FALSE)
    ) %>%
    dplyr::select(IDcol, Admissions, Discharges, Admission_type, Ward_code, Episode_number, Admitted_date, Discharge_date, Same_day_discharge, Spell_type, Adm_period)



  # calculating total time by using difftime - length of stay
  # getting rid of the same day non-emergency

  dt_los <- dt %>%
    dplyr::filter(Same_day_discharge == FALSE ) %>% #| Spell_type != "Emergency"
    dplyr::mutate(LOS = as.numeric(difftime(Discharges, Admissions, units = c("days")))) %>% # or minutes?
    dplyr::select(IDcol, Admissions, Discharges, Admission_type, LOS, Ward_code, Same_day_discharge, Spell_type, Adm_period) %>%
    na.omit()

  # Selecting bins from the Los
  ###########################
  los <-  dt_los$LOS
  ##########################
  # Need to transform los into numeric
  ###########################
  as.numeric(los)

  ###########################
  # choosing the bins
  ##########################
  breaks <- c(0, 0.80, 0.16, 1.00,
              1.80, 1.16, 2.00,
              2.80, 2.16, 3.00,
              3.80, 3.16, 4.00,
              4.80, 4.16, 5.00,
              5.80, 5.16, 6.00,
              6.80, 6.16, 7.00,
              14.00, 21.00, 28.00, max(los, na.rm = TRUE)) #, , na.rm = FALSE



  ###########################

  dt_los$losbinned <- cut(dt_los$LOS,
                          breaks = breaks,
                          labels = c("0hrs", "8hrs", "16hrs",
                                     "1 d", "1 d 8hrs", "1 d 16hrs",
                                     "2 d", "2 d 8hrs", "2 d 16hrs",
                                     "3 d", "3 d 8hrs", "3 d 16hrs",
                                     "4 d", "4 d 8hrs", "4 d 16hrs",
                                     "5 d", "5 d 8hrs", "5 d 16hrs",
                                     "6 d",  "6 d 8hrs","6 d 16hrs",
                                     "7 - 14 d", "15 - 21 d", "22 - 28 d", "> 28 d"),
                          right = FALSE, include.lowest = TRUE) #


  df_wrd_c <- dt_los %>%
    dplyr::filter(Spell_type == "Maternity" | Spell_type == "Other Babies") %>%
    dplyr::filter(Adm_period == TRUE) %>%
    dplyr::group_by(Spell_type, losbinned) %>%
    dplyr::summarise(Count = n()) %>%
    na.omit()

  # Set the title
  title_stub <- " Hospital LoS distribution for admitted patients, "
  start_date_title <- format(as.Date(start_date), format = "%d %B %Y")
  end_date_title <- format(as.Date(end_date), format = "%d %B %Y")
  chart_title <- paste0(hospital_name, title_stub, start_date_title, " to ", end_date_title)

  # Plotting the lenght of stay by the ward  - check for dual axis graphs

  plot_maternity_otherbabies <- ggplot2::ggplot(df_wrd_c, ggplot2::aes(x = losbinned, y = Count, group = Spell_type,  fill = Spell_type)) +   #alpha= ward.type - this gives a ligther version of the colours I choose   # changing the size of the bars for each categ change the parameter width, and in between the stacked categories the white colour is passed on the color command, with the width of the line 0.5
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(), alpha=0.6) +
    ggplot2::scale_fill_brewer(palette= "Set1") +
    ggplot2::theme_bw() +
    ggplot2::labs(title = chart_title,
         subtitle = "Hospital discharges, excl. same-day non-emergency, 1st of March to 31st of March,2015; cummulative hospital LoS in 8 hr bins to 7 d, 7 d bins to28 d, > 28 d \nNote:(i)LoS calculated in days, incl. trolleyed ED LoS and excl transit areas;(ii) results are intended for management information only",
         y = "Hospital admissions*, n", x = "Hospital LoS, d (8 hr bins to 7d, 7d bins to 28d, >28 d) ", caption = "Source: CLAHRC NWL")+
    ggplot2::theme(axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 21, b = 0, l = 0)),
          plot.title = ggplot2::element_text(size = 11, face = "bold"), # changing the parameters in this box will give a different size of the title, with bold passed as a parameter
          plot.subtitle = ggplot2::element_text(size = 8)) + # changing the parameters in this box will give a different size of the subtitle
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1), #
          legend.position = "bottom", legend.box = "horizontal" ) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = "Discharge from"))

  plot_maternity_otherbabies


  if(plot_chart == TRUE){

    plot_maternity_otherbabies


  }else{


    plot_maternity_otherbabies$data %>% select(Spell_type, losbinned, Count)

  }

}
