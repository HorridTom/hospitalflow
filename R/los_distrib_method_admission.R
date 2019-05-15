#' Length of stay distribution
#' los_distrib_elect_emerg_discharge
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
#'
los_distrib_method_admission <- function(start_date = as.Date("2012-01-01", tz = "Europe/London"), end_date = as.Date("2015-01-01", tz = "Europe/London"),
                                         data, plot_chart, hospital_name = "Chelsea & Westminster"){


  # start_date = as.Date("2015-01-01", tz = "Europe/London")
  # end_date = as.Date("2018-01-01", tz = "Europe/London")

  #subseting data set#

  dt <-  data %>%
    dplyr::filter(spell_start < end_date  &  spell_end > start_date) %>%
    dplyr::mutate(
      admitted_date = as.Date(spell_start),
      discharge_date = as.Date(spell_end),
      same_day_discharge = dplyr::if_else(admitted_date == discharge_date, TRUE, FALSE),
      adm_period = dplyr::if_else(spell_start > start_date, TRUE, FALSE)
    ) %>%
    dplyr::select(spell_number, spell_start, spell_end, admission_method_type, admitted_date, discharge_date, same_day_discharge, adm_period)

  # calculating total time by using difftime - length of stay
  # getting rid of the same day non-emergency

  dt_los <- dt %>%
    dplyr::filter(same_day_discharge == FALSE) %>%
    dplyr::mutate(lOS = as.numeric(difftime(spell_end, spell_start, units = c("days")))) %>% # or minutes?
    dplyr::select(spell_number, spell_start, spell_end, admission_method_type, lOS, same_day_discharge, adm_period) %>%
    na.omit()

  # Selecting bins from the Los
  ###########################
  los <-  dt_los$lOS
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

  dt_los$losbinned <- cut(dt_los$lOS,
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
    #dplyr::filter(admission_method_type == "Elective Admissions" | admission_method_type == "Emergency Admissions") %>%
    dplyr::filter(adm_period == TRUE) %>%
    dplyr::group_by(losbinned, admission_method_type) %>%
    dplyr::summarise(Count = n()) %>%
    na.omit()

  # Set the title
  title_stub <- " Hospital LoS distribution for admitted patients, "
  start_date_title <- format(as.Date(start_date), format = "%d %B %Y")
  end_date_title <- format(as.Date(end_date), format = "%d %B %Y")
  hospital_name <- "Queh"
  chart_title <- paste0(hospital_name, title_stub, start_date_title, " to ", end_date_title)

  # Plotting the lenght of stay by the ward  - check for dual axis graphs

  plot_elect_emerg <- ggplot2::ggplot(df_wrd_c, ggplot2::aes(x = losbinned, y = Count, group = admission_method_type,  fill = admission_method_type)) +   #alpha= ward.type - this gives a ligther version of the colours I choose   # changing the size of the bars for each categ change the parameter width, and in between the stacked categories the white colour is passed on the color command, with the width of the line 0.5
    geom_bar(stat = "identity",width = 0.5,colour= "white", lwd = 0.5 ) +
    scale_fill_brewer(palette= "Oranges") +
    # scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400)) +
    ggplot2::theme_bw() +
    #scale_y_continuous("Cumulative hospital addmissions, %", sec.axis = ) +
    ggplot2::labs(title = chart_title,
                  subtitle = "Hospital discharges, excl. same-day non-emergency, 1st of March to 31st of March,2015; cummulative hospital LoS in 8 hr bins to 7 d, 7 d bins to28 d, > 28 d \nNote:(i)LoS calculated in days, incl. trolleyed ED LoS and excl transit areas;(ii) results are intended for management information only",
                  y = "Hospital admissions*, n", x = "Hospital LoS, d (8 hr bins to 7d, 7d bins to 28d, >28 d) ", caption = "Source: CLAHRC NWL")+
    ggplot2::theme(axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 21, b = 0, l = 0)),
                   plot.title = ggplot2::element_text(size = 11, face = "bold"), # changing the parameters in this box will give a different size of the title, with bold passed as a parameter
                   plot.subtitle = ggplot2::element_text(size = 8)) + # changing the parameters in this box will give a different size of the subtitle
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1), #
                   legend.position = "bottom", legend.box = "horizontal" ) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = "Discharge from"))

  plot_elect_emerg


  if(plot_chart == TRUE){

    plot_elect_emerg


  }else{

    plot_elect_emerg$data %>% select(Spell_type, losbinned, Count)


  }

}
