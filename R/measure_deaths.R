#' measure_death
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
measure_death <- function(start_date = as.POSIXct("2016-01-01 00:00:00", tz = "Europe/London"),
         end_date = as.POSIXct("2016-03-31 00:00:00", tz = "Europe/London"),
         data, plot_chart, hospital_name){

  dt_select <- data %>%
    dplyr::select(pseudo_id, spell_number, spell_start, spell_end, ed_admission, disposal_code) %>%
    dplyr::filter(start_date <= spell_end & end_date >= spell_start) #%>%
    #dplyr::filter(admission_method_type == "Emergency Admissions" & ed_admission == TRUE)

  #################################################################################################################
  # first, we create a table that contains the csn of the relevant cases
  # and the days since last admission

  dt_calc <- dt_select %>% # we take our data frame
    dplyr::group_by(pseudo_id) %>%
    dplyr::arrange(spell_start) %>%
    dplyr::mutate(one_month = lubridate::floor_date(spell_end, "1 month", "month"))

  dt_calc_disch <- dt_calc %>%
    dplyr::group_by(one_month) %>%
    dplyr::summarise(N = n())

  dt_calc_deaths <- dt_calc  %>%
    dplyr::filter(disposal_code == "Died in Department") %>%
    dplyr::group_by(one_month) %>%
    dplyr::summarise(Deaths = n())


  dt_deaths_disch <- dplyr::left_join(dt_calc_disch, dt_calc_deaths) %>%
    na.omit()

  # function to plot the 4 hrs emergency performance
  # Plot all days - see Tom's AE APP

  ## Version 0.6.0 of qicharts2 still appears to suffer from
  ## the issue described here: https://github.com/anhoej/qicharts2/issues/21
  ## although it looks like there is a fix. Once that fix is
  ## integrated into a CRAN release, this code should be removed. ***
  options(qic.linecol   = '#5DA5DA',
          qic.signalcol = '#F15854',
          qic.targetcol = '#059748',
          qic.clshade   = TRUE)
  ## ***


  # #######################################################
  pct <- qicharts2::qic(Deaths,
                        n = N,
                        x = one_month,
                        data =  dt_deaths_disch,
                        chart = 'pp',
                        ylab = "percent",
                        show.grid = TRUE,
                        multiply= 100,
                        x.angle = 45)

  pct


  # Set the title
  title_stub <- ": Deaths"
 # hospital_name <- "CW"
  start_date_title <- format(as.Date(start_date), format = "%d %B %Y")
  end_date_title <- format(as.Date(end_date), format = "%d %B %Y")
  Days <- " days "
  chart_title <- paste0(hospital_name, title_stub, start_date_title, " to ", end_date_title)


  pct$data$x <- as.Date(pct$data$x, tz = "Europe/London")
  cht_data <- add_rule_breaks(pct$data)
  pct <- ggplot2::ggplot(cht_data, ggplot2::aes(x, y, label = x))
  #cutoff <- data.frame(yintercept= 95, cutoff=factor(95))

  #convert arguments to dates and round to nearest quarter
  st.dt <- as.Date(start_date, format = "%Y-%m-%d", tz = "Europe/London")
  ed.dt <- as.Date(end_date, format = "%Y-%m-%d", tz = "Europe/London")
  cht_axis_breaks <- seq(st.dt, ed.dt, by = "quarters")
  #ylimlow <- min(min(pct$data$y, na.rm = TRUE),min(pct$data$lcl, na.rm = TRUE))

  death_measure_plot <- format_control_chart(pct, r1_col = "orange", r2_col = "steelblue") +
    ggplot2::scale_x_date(date_breaks = "1 month", labels = scales::date_format("%Y-%m-%d"),
                          breaks = cht_axis_breaks) +
    ggplot2::ggtitle(chart_title) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 11, face = "bold")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, size = 10)) +
    ggplot2::labs(x = "Month", y = "Percentage of deaths",
                  caption = "*Shewart chart rules apply (see Understanding the Analysis tab for more detail) \nRule 1: Any month outside the control limits \nRule 2: Eight or more consecutive months all above, or all below, the centre line", size = 10) +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(x==max(x), format(x, '%b-%y'),'')), hjust = -0.05, vjust = 2)

  death_measure_plot

  if(plot_chart == TRUE){

    death_measure_plot

  }else{

    death_measure_plot$data

  }

}
