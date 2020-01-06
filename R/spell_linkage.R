#' make_spell_table
#'
#' @param ed_data standard ED data
#' @param inpatient_data standard inpatient data
#' @param same_type_episode_lag the maximum amount of time two episodes of the same type
#' can be separated by and still be classified as part of the same spell.
#' @param different_type_episode_lag the maximum amount of time two episodes of different type
#' can be separated by and still be classified as part of the same spell.
#'
#' @return spell table
#' @export
#'
#' @examples
make_spell_table <- function(ed_data, inpatient_data, same_type_episode_lag = 1, different_type_episode_lag = 6) {
  all_episodes <- make_spell_number(ed_data, inpatient_data, same_type_episode_lag, different_type_episode_lag)
  spell_variables(all_episodes)
}


#' make_spell_number
#'
#' @param ed_data standard ED data
#' @param inpatient_data standard inpatient data
#' @param same_type_episode_lag the maximum amount of time two episodes of the same type
#' can be separated by and still be classified as part of the same spell.
#' @param different_type_episode_lag the maximum amount of time two episodes of different type
#' can be separated by and still be classified as part of the same spell.
#'
#' @return table of all episodes with spell_number
#' @export
#'
#' @examples
make_spell_number <- function(ed_data, inpatient_data, same_type_episode_lag = 1, different_type_episode_lag = 6) {

  ed_episodes <- ed_data %>%
    dplyr::mutate(episode_type = "ED")

  ip_episodes <- inpatient_data %>%
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

  all_episodes

}


#' spell_variables
#'
#' @param all_episodes all ED and Inpatient episodes, with spell_number.
#' This should be the return value of a call to make_spell_number
#'
#' @return the spell table
#' @export
#'
#' @examples
spell_variables <- function(all_episodes) {

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
    dplyr::mutate(age_band_start = purrr::map(data, get_age_band_start)) %>%
    # dplyr::mutate(episode_type = purrr::map(data, get_episode_type)) %>%
    dplyr::mutate(episode_class_sequence = purrr::map(data, get_episode_class_sequence)) %>%
    dplyr::mutate(admission_method_type = purrr::map(data, admission_method_class)) %>%
    dplyr::mutate(initial_ed_end_datetime = purrr::map(data, get_initial_ed_episode_end_datetime)) %>%
    dplyr::mutate(disposal_code = purrr::map(data, get_disposal_code)) %>%
    dplyr::mutate(hrg_ae_code = purrr::map(data, get_hrg)) %>%
    dplyr::mutate(source_referral_ae = purrr::map(data, get_source_of_referral)) %>%
    dplyr::mutate(died_ip = purrr::map(data, get_mortality_ip)) %>%
    dplyr::select(-data) %>%
    tidyr::unnest()



  spell_table <- all_episodes %>%
    dplyr::group_by(spell_number) %>%
    dplyr::summarise(spell_start = min(start_datetime, na.rm = TRUE),
                     spell_end = max(end_datetime, na.rm = TRUE),
                     number_of_episodes = n(),
                     pseudo_id = dplyr::first(pseudo_id)) %>%
    dplyr::left_join(episode_lists, by = "spell_number") %>%
    dplyr::mutate(starts_with_ed = stringr::str_detect(episode_class_sequence, pattern = "^E.*$"),
                  ed_non_adm = stringr::str_detect(episode_class_sequence, pattern = "^E$"),
                  ed_comp_non_adm = stringr::str_detect(episode_class_sequence, pattern = "^EE+$"),
                  ed_admission = stringr::str_detect(episode_class_sequence, pattern = "EI"),
                  ed_comp_adm = stringr::str_detect(episode_class_sequence, pattern = "^EI+$"),
                  direct_comp_adm = stringr::str_detect(episode_class_sequence, pattern = "^II+$")) %>%
    dplyr::mutate(direct_admission = stringr::str_count(episode_class_sequence, pattern = "I") > 0) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(spell_class_col = spell_class(starts_with_ed, ed_non_adm, ed_comp_non_adm, ed_admission, ed_comp_adm, direct_admission, direct_comp_adm)) %>%
    dplyr::ungroup()

  spell_table

}


#' add_spell_variables
#'
#' @param ed_data
#' @param inpatient_data
#' @param spell_table pre-created from ed_data and inpatient_data using make_spell_table
#'
#' @return
#' @export
#'
#' @examples
add_spell_variables <- function(ed_data, inpatient_data, spell_table) {

  inpatient_data <- inpatient_data %>% dplyr::mutate(main_specialty = addNA(main_specialty))

  spell_table <- spell_table %>%
    dplyr::mutate(main_specialty_start = purrr::map(constituent_ip_episodes, function(x) {
      if(length(x) == 0)
      {NA}
      else {
        inpatient_data %>%
          dplyr::filter(episode_id == x[[1]]) %>%
          dplyr::slice(1) %>%
          dplyr::pull(main_specialty)
      }
    })
    )

  if("diagnosis_code" %in% colnames(inpatient_data)) {
    spell_table <- spell_table %>% dplyr::mutate(diagnosis_codes = purrr::map(constituent_ip_episodes, function(x) {
      if(length(x) == 0) {NA} else {
        inpatient_data %>% dplyr::filter(episode_id %in% x) %>% dplyr::pull(diagnosis_code) %>% paste(collapse = "#")
      }
    }))
  }

  if("discharge_destination" %in% colnames(inpatient_data)) {
    spell_table <- spell_table %>% dplyr::mutate(discharge_destination = purrr::map(constituent_ip_episodes, function(x) {
      if(length(x) == 0) {NA} else {
        # Note: this actually just takes the discharge destination of the last episode
        # since either this should be the only episode of the spell with this field
        # populated, or it should be constant across (ip) episodes within the spell
        inpatient_data %>% dplyr::filter(episode_id == x[[length(x)]]) %>% dplyr::slice(1) %>% dplyr::pull(discharge_destination)
      }
    }))
  }

  spell_table <- spell_table %>% tidyr::unnest(main_specialty_start) %>%
    dplyr::group_by(pseudo_id) %>%
    dplyr::arrange(spell_start) %>%
    dplyr::mutate(prev_disch = dplyr::lag(spell_end, order_by = spell_start))

  if("discharge_destination" %in% colnames(spell_table)) {
    spell_table <- spell_table %>%
      dplyr::mutate(prev_disch_dest = dplyr::lag(discharge_destination, order_by = spell_start))
  }

  if("diagnosis_codes" %in% colnames(spell_table)) {
    spell_table <- spell_table %>%
      dplyr::mutate(prev_disch_diagnoses = dplyr::lag(diagnosis_codes, order_by = spell_start),
                    all_prev_diagnoses = cumulative_paste(diagnosis_codes, sep = "##")
      )
  }

  spell_table %>%
    dplyr::ungroup() %>%
    dplyr::mutate(days_since_prev_disch = difftime(spell_start, prev_disch, units = "days"))


}

# get_main_specialty <- function(x) {
#   if(length(x) == 0)
#     {NA}
#   else {
#     inpatient_data %>%
#       dplyr::filter(episode_id == x[[1]]) %>%
#       dplyr::slice(1) %>%
#       dplyr::pull(main_specialty)
#   }
# }



get_episode_id_list <- function(episode_df, episode_type_to_list) {
  ep_id_v <- episode_df %>%
    dplyr::filter(episode_type == episode_type_to_list) %>%
    dplyr::pull(episode_id)
  list(as.list(ep_id_v))
}

get_latest_gender <- function(gender_df) {
  ordered_gender_records <- gender_df %>%
    dplyr::filter(!is.na(gender)) %>%
    dplyr::arrange(dplyr::desc(start_datetime)) %>%
    dplyr::pull(gender)

  ordered_gender_records[1]
}

get_age_band_start <- function(age_band_df){
  ordered_age_band <- age_band_df %>% dplyr::filter(!is.na(age_band_start)) %>%
    dplyr::group_by(pseudo_id) %>%
    dplyr::arrange(start_datetime) %>%
    dplyr::pull(age_band_start)

  ordered_age_band[1]
}

# get_episode_type <- function(episode_type_df) {
#   episode_type <- episode_type_df %>%
#     dplyr::filter(!is.na(episode_type)) %>%
#     dplyr::pull(episode_type)
#
# }


get_episode_class_sequence <- function(episode_df) {
  class_vector <- episode_df %>% dplyr::select(start_datetime, episode_type) %>%
    dplyr::arrange(start_datetime) %>%
    dplyr::mutate(episode_type = stringr::str_sub(episode_type, start = 1, end = 1)) %>%
    dplyr::pull(episode_type)

  paste(class_vector, sep = "", collapse = "")
}


spell_class <- function(starts_with_ed, ed_non_adm, ed_comp_non_adm, ed_admission, ed_comp_adm, direct_admission, direct_comp_adm) {
  if(starts_with_ed & ed_non_adm) {
    "ed_non_admission"
  } else if (starts_with_ed & ed_comp_non_adm) {
    "ed_comp_non_admission"
  } else if (starts_with_ed & ed_admission) {
    "ed_admission"
  } else if (starts_with_ed & ed_comp_adm) {
    "ed_comp_admission"
  } else if  (direct_admission) {
    "direct_admission"
  } else {
    "direct_comp_admission"
  }
}

admission_method_class <- function(admission_method_df) {

  ordered_admission_method <- admission_method_df %>%
    dplyr::select(start_datetime, admission_method) %>%
    dplyr::filter(!is.na(admission_method)) %>%
    dplyr::arrange(start_datetime) %>%
    dplyr::mutate(admission_method_type = dplyr::case_when(
      admission_method == "Waiting list" | admission_method == "Booked" | admission_method == "Planned" ~ "Elective Admissions",
      admission_method == "Accident and emergency" | admission_method == "General Practitioner" | admission_method == "Bed bureau" ~ "Emergency Admissions",
      admission_method == "Consultant Clinic" | admission_method == "Mental Health Crisis Resolution Team" | admission_method == "Accident and Emergency Department" ~ "Emergency Admissions",
      admission_method == "Transfer from another Hospital Provider" | admission_method == "Intended home birth" | admission_method == "Other emergency admission" ~ "Emergency Admissions",
      admission_method == "Other means" ~ "Emergency Admissions",
      admission_method == "Admitted ante-partum" | admission_method == "Admitted post-partum"  ~ "Maternity Admissions",
      admission_method == "Birth-this provider" | admission_method == "Birth-outside provider(not intended home)" | admission_method == "Transfer from other provider(non-emergency)" ~ "Other Admissions")) %>%
    dplyr::pull(admission_method_type)

  ordered_admission_method[1]


}



get_initial_ed_episode_end_datetime <- function(spell_episodes_df) {
  first_episode_of_spell <- spell_episodes_df %>%
    dplyr::arrange(start_datetime) %>%
    dplyr::slice(1)

  if(first_episode_of_spell %>% dplyr::pull(episode_type)=="ED"){
    return(first_episode_of_spell %>% dplyr::pull(end_datetime))

  }else{

    NA
  }

}

get_disposal_code <- function(disposal_code_df){
  disposal_code <- disposal_code_df %>%
    dplyr::group_by(pseudo_id) %>%
    dplyr::arrange(start_datetime) %>%
    dplyr::slice(1) %>%
    dplyr::pull(attendance_disposal)

  disposal_code[1]
}

get_source_of_referral <- function(source_of_referral_df){
  source_referral <- source_of_referral_df %>%
    dplyr::group_by(pseudo_id) %>%
    dplyr::arrange(start_datetime) %>%
    dplyr::slice(1) %>%
    dplyr::pull(referral_source)

  source_referral[1]
}

get_hrg <- function(hrg_df){
  hrg_code <- hrg_df %>%
    dplyr::group_by(pseudo_id) %>%
    dplyr::arrange(start_datetime) %>%
    dplyr::slice(1) %>%
    dplyr::pull(hrg_code)

  hrg_code[1]
}


get_mortality_ip <- function(died_ip_df){

  died_ip <- died_ip_df %>%
    dplyr::group_by(pseudo_id) %>%
    dplyr::arrange(start_datetime) %>%
    dplyr::mutate(died = dplyr::if_else(discharge_method == 4, TRUE, FALSE)) %>%
    dplyr::pull(died)

  died_ip[1]
}

