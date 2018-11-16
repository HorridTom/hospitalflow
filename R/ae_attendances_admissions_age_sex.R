
#################################################################################################
ae_attendances_admissions_age_sex <- function(start_date, end_date, data){

  # finding the number of ae attendances
  df_ae_attendances <- data %>%
    dplyr::filter(Ward== "A&E" & Gender != "Not Specified") %>%
    dplyr::filter(EpisodeNumber == 1 & PatientType == "Emergency") %>%
    dplyr::group_by(Gender, AgeBand) %>%
    dplyr::tally() %>%
    dplyr::rename(Attend = n) %>%
    dplyr::mutate(Attendances_Gender = dplyr::case_when(Gender == "Female" ~ "Female not admitted",
                                                        Gender == "Male" ~ "Male not admitted"))

    #df_ae_admissions <- test_data_age_sex_att_adm %>%
    #dplyr::filter(Ward == "A&E" & LastWard != "ED only" & PatientType == "Emergency") %>%
    #dplyr::filter(Gender != "Not Specified" & EpisodeNumber == 1) %>%
    #dplyr::group_by(Gender, AgeBand) %>%
    #dplyr::summarize(Admitted = n()) %>%
    #dplyr::mutate(Admissions_Gender = dplyr::case_when(Gender == "Female" ~ "Female admitted",
    # Gender == "Male" ~ "Male admitted"))

  #plot the two dataset
  #plot <- ggplot2::ggplot() +
  # ggplot2::geom_bar(data =  df_ae_attendances , ggplot2::aes(x = AgeBand, fill = Attendances_Gender, y = Attend),
  #        stat = 'identity', position = 'dodge') +
  # ggplot2::geom_bar(data = df_ae_admissions, ggplot2::aes(x = AgeBand, fill = Admissions_Gender, y = Admitted),
  #          stat = 'identity', position = position_dodge(0.9), width = 0.6) +
  #  ggplot2::xlim("0 yrs" , "1-4 yrs", "5-9 yrs", "10-14 yrs", "15-19 yrs", "20-24 yrs", "25-29 yrs",
  #      "30-34 yrs", "35-39 yrs", "40-44 yrs", "45-49 yrs", "50-54 yrs", "55-59 yrs",
  #        "60-64 yrs", "65-69 yrs", "70-74 yrs", "75-79 yrs", "80-84 yrs", "85+ yrs") +
  #     ggplot2::scale_y_continuous() +
  #  ggplot2::xlab("Age Group") +
  #  ggplot2::ylab("ED Attendances and Admissions") +
  #   ggplot2::labs(title = "Chelsea & Westminster: A&E Attendances and Admissions, 1st Jan to 31st of March, 2015",
  #        subtitle = "ED attendance and hospital admissions from ED, \nNote: (i) planned return/recall attendances have been excluded; (ii) results are intended for management information only",
  #          y = "Attendances and Admissions, n", x = "Age Group", caption = "Source: CLAHRC NWL") +
  #      ggplot2::theme_bw() +
  #    ggplot2::theme(axis.title.y = element_text(margin = margin(t = 0, r = 21, b = 0, l = 0)),
  #                  plot.title = element_text(size = 12, face = "bold"),
  #                 plot.subtitle = element_text(size = 10),
  #                 legend.position = "bottom", legend.box = "horizontal") +
  #scale_y_continuous(expand = c(0, .5)) +
  #    ggplot2::scale_fill_manual("", values = c("coral3",  "lightcoral", "steelblue4", "lightblue2"))

  # Things to consider - StartDate and EndDate does not do what it should do -> it plots all the date from 2011-2015

  #  if(plot_chart == TRUE){

  #   plot

  #  }else{

  #    plot$data %>% select(AgeBand, AgeBand, Admitted, Admissions_Gender)
  #plot$data %>% select(AgeBand, Attendances_Gender, Admitted)
  #   }


}

##################################################################################################
