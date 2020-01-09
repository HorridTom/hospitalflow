
#' get_flow_matrix
#'
#' @param ed_data
#' @param inpatient_data
#' @param spell_table
#'
#' @return
#' @export
#'
#' @examples
get_flow_matrix <- function(ed_data, inpatient_data, spell_table) {
  state_space <- get_state_space(ed_data, inpatient_data)
  state_space <- tibble::tibble(location = state_space, state = c(1:length(state_space)))
  spell_table <- spell_table %>% dplyr::rowwise() %>% dplyr::mutate(state_sequence = get_state_sequence(episode_location_sequence))
  spell_table
}


#' get_state_space
#'
#' @param ed_data
#' @param inpatient_data
#'
#' @return
#' @export
#'
#' @examples
get_state_space <- function(ed_data, inpatient_data) {
  inpt_state_space <- inpatient_data %>% dplyr::distinct(ward_name) %>% dplyr::pull(ward_name)
  # something like the following once site is included:
  #ed_state_space <- ed_data %>% dplyr::distinct(ward_name) %>% dplyr::pull(ward_name)
  c("Not In Hospital", "ED", inpt_state_space)
}


get_state_sequence <- function(episode_location_sequence) {
  els_vect <- unlist(episode_location_sequence)
  state_seq <- state_space[match(els_vect, state_space$location), ] %>% dplyr::pull(state) %>% paste0(collapse = "#")
  paste0("0#", state_seq, "#0")
}

make_move_frequency_matrix <- function(state_sequence) {

}

