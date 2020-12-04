#' make_moves_table
#'
#' @param ed_data standard ED data
#' @param inpatient_data standard inpatient data
#'
#' @return moves table
#' @export
#'
#' @examples
make_moves_table <- function(ed_data = test_ed_data_sample,
                             inpatient_data = test_ip_data_sample){

  all_episodes <- hospitalflow::make_spell_number(ed_data, inpatient_data)
  moves_table <- all_episodes %>%
    dplyr::select(pseudo_id, start_datetime, end_datetime, ward_category,episode_id, episode_type, spell_number) %>%
    dplyr::mutate(ward_category = ifelse(episode_type == "ED", "ED", ward_category)) %>%
    dplyr::group_by(spell_number) %>%
    dplyr::arrange(start_datetime) %>%
    # dplyr::mutate(episode_order = dplyr::row_number()) %>%
    # dplyr::mutate(total_eps_in_spell = dplyr::n()) %>%
    dplyr::group_modify(~ dplyr::add_row(.x, ward_category = "External Outgoing")) %>%
    dplyr::mutate(pseudo_id = ifelse(is.na(pseudo_id), dplyr::lag(pseudo_id), pseudo_id)) %>% #add comment
    dplyr::mutate(start_datetime = dplyr::if_else(is.na(start_datetime), dplyr::lag(end_datetime), start_datetime)) %>% #add comment
    dplyr::rename(move_datetime = start_datetime, move_to = ward_category) %>%
    dplyr::mutate(move_from = dplyr::lag(move_to)) %>%
    dplyr::mutate(move_from = ifelse(is.na(move_from), "External Incoming", move_from)) %>%
    dplyr::select(spell_number, pseudo_id, move_from, move_to, move_datetime) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(move_from_category = ifelse(move_from == "ED", "ED",
                                              ifelse(move_from == "External Incoming", move_from, get_ward_mapping(move_from)))) %>%
    dplyr::mutate(move_to_category = ifelse(move_to == "ED", "ED",
                                              ifelse(move_to == "External Outgoing", move_to, get_ward_mapping(move_to)))) %>%

    dplyr::mutate(move_number = 1:dplyr::n())

}

