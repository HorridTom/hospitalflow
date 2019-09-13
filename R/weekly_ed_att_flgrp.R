#' weekly_ed_att_flgrp
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
weekly_ed_att_flgrp < function(start_date, end_date, data, plot_chart, hospital_name){

  dt_select <- spell_table %>%
    dplyr::filter(spell_start < end_date | spell_end > start_date) %>%
    dplyr::arrange(spell_start) %>%
    dplyr::mutate(flow_groups = dplyr::case_when(ed_non_adm == TRUE  ~ "Flow A",
                                                 directorate == "Medical" ~ "Flow 3",
                                                 directorate == "Surgical" ~ "Flow 4"))


  # calculating total time by using difftime ####
  # length of stay for the emergency department #
  ##########################################################
  # creating a new variable - under 4 hrs and above 4 hrs ##

  dt_los <-  dt_select  %>%
    dplyr::filter(starts_with_ed == TRUE) %>%
    dplyr::arrange(spell_start) %>%
    dplyr::mutate(Time = lubridate::round_date(spell_start, unit = time_unit)) %>%
    dplyr::select(pseudo_id, Time, directorate, flow_groups, starts_with_ed)

  count_df <- dt_los %>%
    dplyr::group_by(Time, flow_groups) %>%
    dplyr::tally() %>%
    tidyr::drop_na()



}
