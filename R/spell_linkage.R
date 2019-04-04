#' make_spell_table
#'
#' @param ed_data standard ED data
#' @param inpatient_data standard inpatient data
#'
#' @return spell table
#' @export
#'
#' @examples
make_spell_table <- function(ed_data, inpatient_data, same_type_episode_lag = 1, different_type_episode_lag = 6) {

  ed_episodes <- ed_data %>% dplyr::select(pseudo_id, start_datetime, end_datetime, episode_id, gender) %>%
    dplyr::mutate(episode_type = "ED")

  ip_episodes <- inpatient_data %>% dplyr::select(pseudo_id, start_datetime, end_datetime, episode_id, gender) %>%
    dplyr::mutate(episode_type = "IP")

  all_episodes <- dplyr::bind_rows(ed_episodes, ip_episodes) %>%
    dplyr::filter(!is.na(end_datetime)) %>%
    dplyr::arrange(pseudo_id, start_datetime) %>%
    dplyr::group_by(pseudo_id) %>%
    dplyr::mutate(episode_lag = difftime(start_datetime, dplyr::lag(end_datetime), units = "hours")) %>%
    dplyr::mutate(prev_episode_type = dplyr::lag(episode_type)) %>%
    dplyr::mutate(new_spell = dplyr::if_else(is.na(prev_episode_type) |
                                        ((prev_episode_type == episode_type) & episode_lag > same_type_episode_lag) |
                                        ((prev_episode_type != episode_type) & episode_lag > different_type_episode_lag), TRUE, FALSE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(spell_number = cumsum(new_spell))

  episode_lists <- all_episodes %>%
    dplyr::group_by(spell_number) %>%
    tidyr::nest() %>%
    dplyr::mutate(constituent_ed_episodes = purrr::map(data,
                                                       get_episode_id_list,
                                                       episode_type_to_list = "ED"),
                  constituent_ip_episodes = purrr::map(data,
                                                       get_episode_id_list,
                                                       episode_type_to_list = "IP")) %>%
    dplyr::mutate(gender = purrr::map(data, get_latest_gender)) %>%
    dplyr::select(-data) %>% tidyr::unnest()

  spell_table <- all_episodes %>%
    dplyr::group_by(spell_number) %>%
    dplyr::summarise(spell_start = min(start_datetime, na.rm = TRUE),
                     spell_end = max(end_datetime, na.rm = TRUE),
                     number_of_episodes = n()) %>%
    dplyr::left_join(episode_lists, by = "spell_number")



}


get_episode_id_list <- function(episode_df, episode_type_to_list) {
  ep_id_v <- episode_df %>% dplyr::filter(episode_type == episode_type_to_list) %>% dplyr::pull(episode_id)
  list(as.list(ep_id_v))
}


get_latest_gender <- function(gender_df) {
  ordered_gender_records <- gender_df %>% dplyr::filter(!is.na(gender)) %>%
    dplyr::arrange(dplyr::desc(start_datetime)) %>%
    dplyr::pull(gender)

  ordered_gender_records[1]
}





