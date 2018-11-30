los_distrib_elect_emerg_discharge <- function(start_date, end_date, data, plot_chart){

  df_wrd_c <- data %>%
    dplyr::filter(Spell_type == "Emergency" | Spell_type == "Elective") %>%
    dplyr::filter(Spell_type == "Mixed" | Adm_period == "TRUE") %>%
    dplyr::group_by(Spell_type, losbinned) %>%
    dplyr::summarise(Count = n()) %>%
    na.omit()


  # Plotting the lenght of stay by the ward  - check for dual axis graphs

  plot_elect_emerg <- ggplot2::ggplot(df_wrd_c, ggplot2::aes(x = losbinned, y = Count, group = Spell_type,  fill = Spell_type)) +   #alpha= ward.type - this gives a ligther version of the colours I choose   # changing the size of the bars for each categ change the parameter width, and in between the stacked categories the white colour is passed on the color command, with the width of the line 0.5
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(), alpha=0.6) +
    ggplot2::scale_fill_brewer(palette= "Set1") +
    # scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400)) +
    ggplot2::theme_bw() +
    #scale_y_continuous("Cumulative hospital addmissions, %", sec.axis = ) +
    ggplot2::labs(title ="Hospital LoS distribution for admitted patients, 1st of March to 31st of March, 2015",
                 subtitle = "Hospital discharges, excl. same-day non-emergency, 1st of March to 31st of March,2015; cummulative hospital LoS in 8 hr bins to 7 d, 7 d bins to28 d, > 28 d \nNote:(i)LoS calculated in days, incl. trolleyed ED LoS and excl transit areas;(ii) results are intended for management information only",
                        y = "Hospital admissions*, n", x = "Hospital LoS, d (8 hr bins to 7d, 7d bins to 28d, >28 d) ", caption = "Source: CLAHRC NWL")+
    ggplot2::theme(axis.title.y =  ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 21, b = 0, l = 0)),
                   plot.title =    ggplot2::element_text(size = 11, face = "bold"), # changing the parameters in this box will give a different size of the title, with bold passed as a parameter
                   plot.subtitle = ggplot2::element_text(size = 8)) + # changing the parameters in this box will give a different size of the subtitle
                                   ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position = "bottom", legend.box = "horizontal" ) +
    ggplot2::guides(fill =  ggplot2::guide_legend(title = "Discharge from"))

  plot_elect_emerg


  if(plot_chart == TRUE){

    plot_elect_emerg


  }else{

    plot_data <- plot_elect_emerg$data %>% select(Spell_type, losbinned, Count)


  }

}
