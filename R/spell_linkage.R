#' make_spell_table
#'
#' @param ed_data standard ED data
#' @param inpatient_data standard inpatient data
#'
#' @return spell table
#' @export
#'
#' @examples
make_spell_table <- function(ed_data, inpatient_data) {

  ed_episodes <- ed_data %>% dplyr::select(pseudo_id, start_datetime, end_datetime) %>%
    dplyr::mutate(episode_type = "ED")

  ip_episodes <- inpatient_data %>% dplyr::select(pseudo_id, start_datetime, end_datetime) %>%
    dplyr::mutate(episode_type = "IP")

  all_episodes <- dplyr::bind_rows(ed_episodes, ip_episodes) %>%
    dplyr::filter(!is.na(end_datetime)) %>%
    dplyr::arrange(pseudo_id, start_datetime) %>%
    dplyr::group_by(pseudo_id) %>%
    dplyr::mutate(episode_lag = difftime(start_datetime, dplyr::lag(end_datetime), units = "hours")) %>%
    dplyr::mutate(prev_episode_type = dplyr::lag(episode_type)) %>%
    dplyr::mutate(new_spell = dplyr::if_else(is.na(prev_episode_type) |
                                        ((prev_episode_type == episode_type) & episode_lag > 1) |
                                        ((prev_episode_type != episode_type) & episode_lag > 4), TRUE, FALSE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(spell_number = cumsum(new_spell))

  spell_table <- all_episodes %>% dplyr::group_by(spell_number) %>%
    dplyr::summarise(spell_start = min(start_datetime, na.rm = TRUE), spell_end = max(end_datetime, na.rm = TRUE), number_of_episodes = n())



}
