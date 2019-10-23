#' readmissions_ip
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
readmissions_ip <- function(start_date = as.POSIXct("2016-01-01 00:00:00", tz = "Europe/London"),
                            end_date = as.POSIXct("2016-03-31 00:00:00", tz = "Europe/London"),
                            data, readmissions_by , plot_chart, hospital_name){

  dt_select <- spell_table %>%
    dplyr::select(spell_number, spell_start, spell_end, ed_admission, admission_method_type) %>%
    dplyr::filter(start_date <= spell_start | end_date >= spell_end) %>%
    dplyr::filter(admission_method_type == "Emergency Admissions" & ed_admission == TRUE)

#################################################################################################################
# first, we create a table that contains the csn of the relevant cases
# and the days since last admission

dt_calc <- dt_select %>% # we take our data frame
  dplyr::mutate(readm_date = as.Date(spell_end, tz = "Europe/London")) %>% # turn the dates into date format
  dplyr::arrange(spell_number, readm_date) %>% # sort them first by mrn and then by admit_date
  dplyr::mutate(readmissions_by = readm_date - lag(readm_date)) %>% # ...get the days since last admit
  dplyr::mutate(readmissions_by = as.integer(readmissions_by)) %>%
  ungroup() %>%
  mutate(one_month = lubridate::round_date(spell_start, "1 month", "month"))

dt_readmissions <- dt_calc %>%
  dplyr::filter(readmissions_by > 0 & readmissions_by <= 90)


dt_count_disch <- dt_calc %>%
  dplyr::group_by(one_month) %>%
  tally()

dt_read_disch <- dplyr::left_join(dt_readmissions, dt_count_disch, by = "one_month")

# counting the Readmissions
sum_readm_7 <- dt_readmissions %>%
  dplyr::group_by(one_month) %>%
  dplyr::filter(readmissions_by <= 7) %>%
  dplyr::summarise("readmissions_7_days" = n())

dt_rdm_disch_7 <- dplyr::left_join(sum_readm_7, dt_count_disch, by = "one_month")


# counting the Readmissions
sum_readm_30 <- dt_readmissions  %>%
  dplyr::group_by(one_month) %>%
  dplyr::filter(readmissions_by <= 30) %>%
  dplyr::summarise("readmissions_30_days" = n())

dt_rdm_disch_30 <- dplyr::left_join(sum_readm_30, dt_count_disch, by = "one_month")


# counting the Readmissions
sum_readm_90 <- dt_readmissions  %>%
  dplyr::group_by(one_month) %>%
  dplyr::filter(readmissions_by <= 90) %>%
  dplyr::summarise("readmissions_90_days" = n())

dt_rdm_disch_90 <- dplyr::left_join(sum_readm_90, dt_count_disch, by = "one_month")




#######################################################
pct_7 <- qicharts2::qic(readmissions_7_days,
                      n = n,
                      x = one_month,
                      data = dt_rdm_disch_7,
                      chart = 'pp',
                      #standardised = TRUE,
                      #multiply= 100,
                      title = "Readmissions within 7 days, counts n*",
                      ylab = "Percent patients",
                      xlab = "Readmissions within 7 days",
                      x.angle = 45)

pct_7



#######################################################
pct_30 <- qicharts2::qic(readmissions_30_days,
                        n = n,
                        x = one_month,
                        data = dt_rdm_disch_30,
                        chart = 'pp',
                        #standardised = TRUE,
                        #multiply= 100,
                        title = "Readmissions within 30 days, counts n*",
                        ylab = "Percent patients",
                        xlab = "Readmissions within 30 days",
                        x.angle = 45)

pct_30


pct_90 <- qicharts2::qic(readmissions_90_days,
                         n = n,
                         x = one_month,
                         data = dt_rdm_disch_90,
                         chart = 'pp',
                         #standardised = TRUE,
                         #multiply= 100,
                         title = "Readmissions within 90 days, counts n*",
                         ylab = "Percent patients",
                         xlab = "Readmissions within 90 days",
                         x.angle = 45)

pct_90




}
