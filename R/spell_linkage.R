#' make_spell_table
#'
#' @param ed_data standard ED data
#' @param inpatient_data standard inpatient data
#' @param same_type_episode_lag the maximum amount of time two episodes of the same type
#' can be separated by and still be classified as part of the same spell.
#' @param different_type_episode_lag the maximum amount of time two episodes of different type
#' can be separated by and still be classified as part of the same spell.
#'
#' @return named list containing the spell table and the all_episodes table
#' @export
#'
#' @examples
make_spell_table <- function(ed_data, inpatient_data, same_type_episode_lag = 1, different_type_episode_lag = 6) {
  all_episodes <- make_spell_number(ed_data, inpatient_data, same_type_episode_lag, different_type_episode_lag)
  spell_table <- spell_variables(all_episodes)
  spells_episodes_tables <- list(spell_table = spell_table, all_episodes = all_episodes)
}


#' make_moves_table
#'
#' @param ed_data standard ED data
#' @param inpatient_data standard inpatient data
#' @param all_episodes all episodes tables (this is an output from make_spell_table)
#' @param ward_mapping_config_path path to the ward mapping config file
#'
#' @return moves table
#' @export
#'
#' @examples
make_moves_table <- function(ed_data = test_ed_data_sample,
                             inpatient_data = test_ip_data_sample,
                             all_episodes,
                             ward_mapping_config_path){

  moves_table <- all_episodes %>%
    dplyr::select(pseudo_id, start_datetime, end_datetime, ward_category,episode_id, episode_type, spell_number) %>%
    dplyr::mutate(ward_category = dplyr::if_else(episode_type == "ED", "ED", ward_category)) %>%
    dplyr::group_by(spell_number) %>%
    dplyr::arrange(start_datetime) %>%
    dplyr::group_modify(~ dplyr::add_row(.x, ward_category = "External Outgoing")) %>%
    dplyr::mutate(pseudo_id = dplyr::if_else(is.na(pseudo_id), dplyr::lag(pseudo_id), pseudo_id)) %>% #gives the added rows the same pseudo ID as the row above
    dplyr::mutate(start_datetime = dplyr::if_else(is.na(start_datetime), dplyr::lag(end_datetime), start_datetime)) %>% #gives the added rows the same start and end datetimes as the row above
    dplyr::rename(move_datetime = start_datetime, move_to = ward_category) %>%
    dplyr::mutate(move_from = dplyr::lag(move_to)) %>%
    dplyr::mutate(move_from = dplyr::if_else(is.na(move_from), "External Incoming", move_from)) %>%
    dplyr::select(spell_number, pseudo_id, move_from, move_to, move_datetime) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(move_from_category = dplyr::if_else(move_from == "ED", "ED",
                                                      dplyr::if_else(move_from == "External Incoming",
                                                                     move_from,
                                                                     get_ward_mapping(move_from, ward_mapping_config_path)))) %>%
    dplyr::mutate(move_to_category = dplyr::if_else(move_to == "ED", "ED",
                                                    dplyr::if_else(move_to == "External Outgoing",
                                                                   move_to,
                                                                   get_ward_mapping(move_to, ward_mapping_config_path)))) %>%

    dplyr::mutate(move_number = 1:dplyr::n())

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
    data.table::data.table() %>%
    dplyr::group_by(pseudo_id) %>%
    dplyr::mutate(episode_lag = difftime(start_datetime, dplyr::lag(end_datetime), units = "hours")) %>%
    dplyr::mutate(prev_episode_type = dplyr::lag(episode_type)) %>%
    dplyr::mutate(new_spell = dplyr::if_else(is.na(prev_episode_type) |
                                               ((prev_episode_type == episode_type) & episode_lag > same_type_episode_lag) |
                                               ((prev_episode_type != episode_type) & episode_lag > different_type_episode_lag), TRUE, FALSE)) %>%
    dplyr::ungroup() %>%
    tibble::as_tibble() %>%
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

  only_spell_numbers <- data.frame(spell_number = unique(all_episodes$spell_number))

  const_episodes_df <- all_episodes %>%
    dplyr::group_by(spell_number) %>%
    dplyr::summarise(
      constituent_ed_episodes = list(as.list(episode_id[episode_type == "ED"])),
      constituent_ip_episodes = list(as.list(episode_id[episode_type == "IP"]))
    )


  gender_df <- all_episodes %>%
    dplyr::group_by(spell_number) %>%
    dplyr::filter(!is.na(gender)) %>%
    dplyr::arrange(dplyr::desc(start_datetime)) %>%
    dplyr::slice_head(n=1) %>% # is it why the datatypes are different? also it might return both rows if there is a tie.
    dplyr::ungroup() %>%
    dplyr::select(spell_number, gender)
  #gender_df$gender <- as.list(as.character(gender_df$gender)) #needed to make datatypes identical to original


  age_band_df <- all_episodes %>%
    dplyr::group_by(spell_number, pseudo_id) %>%
    dplyr::filter(!is.na(age_band_start)) %>%
    dplyr::arrange(start_datetime) %>%
    dplyr::slice_head(n=1) %>%
    dplyr::ungroup() %>%
    dplyr::select(spell_number, age_band_start)
  #age_band_df$age_band_start <- as.list(age_band_df$age_band_start) #needed to make datatypes identical to original


  ep_class_seq_df <- all_episodes %>%
    dplyr::group_by(spell_number) %>%
    dplyr::arrange(start_datetime) %>%
    dplyr::summarise(
      episode_class_sequence = paste(
        substr(episode_type, start = 1, stop = 1),
        sep = '',
        collapse = ''
      )
    )
  #ep_class_seq_df$episode_class_sequence <- as.list(ep_class_seq_df$episode_class_sequence) #needed to make datatypes identical to original


  admission_type_df <- all_episodes %>%
    dplyr::select(spell_number, start_datetime, admission_method) %>%
    dplyr::group_by(spell_number) %>%
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
    dplyr::slice_head(n=1) %>%
    dplyr::ungroup() %>%
    dplyr::select(spell_number, admission_method_type)
  #admission_type_df$admission_method_type <- as.list(admission_type_df$admission_method_type) #needed to make datatypes identical to original

  admission_type_df <- dplyr::left_join(only_spell_numbers, admission_type_df, by = "spell_number")
  admission_type_df$admission_method_type <- ifelse(
    admission_type_df$admission_method_type == "NULL",
    as.character(NA),
    admission_type_df$admission_method_type
  )
  admission_type_df <- tibble::as_tibble(admission_type_df)


  datetime_df <- all_episodes %>%
    dplyr::select(spell_number, start_datetime, episode_type, end_datetime) %>%
    dplyr::group_by(spell_number) %>%
    dplyr::arrange(start_datetime) %>%
    dplyr::slice_head(n=1) %>%
    dplyr::summarise(
      initial_ed_end_datetime = end_datetime[episode_type == "ED"]
    )
  datetime_df <- dplyr::left_join(only_spell_numbers, datetime_df, by = "spell_number")


  disposal_df <- all_episodes %>%
    dplyr::group_by(spell_number, pseudo_id) %>%
    dplyr::arrange(start_datetime) %>%
    dplyr::slice_head(n=1) %>%
    dplyr::ungroup() %>%
    dplyr::select(spell_number, attendance_disposal) %>%
    dplyr::rename(disposal_code = attendance_disposal)
  #disposal_df$disposal_code <- as.list(disposal_df$disposal_code) #needed to make datatypes identical to original


  hrg_df <- all_episodes %>%
    dplyr::group_by(spell_number, pseudo_id) %>%
    dplyr::arrange(start_datetime) %>%
    dplyr::slice_head(n=1) %>%
    dplyr::ungroup() %>%
    dplyr::select(spell_number, hrg_code) %>%
    dplyr::rename(hrg_ae_code = hrg_code)
  #hrg_df$hrg_ae_code <- as.list(hrg_df$hrg_ae_code) #needed to make datatypes identical to original


  source_referral_df <- all_episodes %>%
    dplyr::group_by(spell_number, pseudo_id) %>%
    dplyr::arrange(start_datetime) %>%
    dplyr::slice_head(n=1) %>%
    dplyr::ungroup() %>%
    dplyr::select(spell_number, referral_source) %>%
    dplyr::rename(source_referral_ae = referral_source)
  #source_referral_df$source_referral_ae <- as.list(source_referral_df$source_referral_ae) #needed to make datatypes identical to original


  mortality_df <- all_episodes %>%
    dplyr::group_by(spell_number, pseudo_id) %>%
    dplyr::arrange(start_datetime) %>%
    dplyr::mutate(died = dplyr::if_else(discharge_method == 4, TRUE, FALSE)) %>%
    dplyr::slice_head(n=1) %>%
    dplyr::ungroup() %>%
    dplyr::select(spell_number, died) %>%
    dplyr::rename(died_ip = died)
  #mortality_df$died_ip <- as.list(mortality_df$died_ip) #needed to make datatypes identical to original


  lst_of_dfs <- list(
    const_episodes_df = const_episodes_df,
    gender_df = gender_df,
    age_band_df = age_band_df,
    ep_class_seq_df = ep_class_seq_df,
    admission_type_df = admission_type_df,
    datetime_df = datetime_df,
    disposal_df = disposal_df,
    hrg_df = hrg_df,
    source_referral_df = source_referral_df,
    mortality_df = mortality_df
  )

  episode_lists <- Reduce(function(...) dplyr::left_join(..., on = "spell_number"), lst_of_dfs)

  spell_table <- all_episodes %>%
    dplyr::group_by(spell_number) %>%
    dplyr::summarise(
      spell_start = min(start_datetime, na.rm = TRUE),
      spell_end = max(end_datetime, na.rm = TRUE),
      number_of_episodes = dplyr::n(),
      pseudo_id = dplyr::first(pseudo_id)
    ) %>%
    dplyr::left_join(episode_lists, by = "spell_number") %>%
    dplyr::mutate(
      starts_with_ed = stringr::str_detect(episode_class_sequence, pattern = "^E.*$"),
      ed_non_adm = stringr::str_detect(episode_class_sequence, pattern = "^E$"),
      ed_comp_non_adm = stringr::str_detect(episode_class_sequence, pattern = "^EE+$"),
      ed_admission = stringr::str_detect(episode_class_sequence, pattern = "EI"),
      ed_comp_adm = stringr::str_detect(episode_class_sequence, pattern = "^EI+$"),
      direct_comp_adm = stringr::str_detect(episode_class_sequence, pattern = "^II+$")
    ) %>%
    dplyr::mutate(direct_admission = stringr::str_count(episode_class_sequence, pattern = "I") > 0) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(spell_class_col = spell_class(starts_with_ed, ed_non_adm, ed_comp_non_adm, ed_admission, ed_comp_adm, direct_admission, direct_comp_adm)) %>%
    dplyr::ungroup()

  return(spell_table)

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
  class_vector <- episode_df %>%
    dplyr::select(start_datetime, episode_type) %>%
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

