
# Improving make_spell_number (1/2)

#' make_spell_number_dtbl
#' @param ed_data
#'
#' @param inpatient_data
#' @param same_type_episode_lag
#' @param different_type_episode_lag
#'
#' @export
make_spell_number_dtbl <- function(ed_data, inpatient_data, same_type_episode_lag = 1, different_type_episode_lag = 6) {

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


#' @export
make_spell_number_multidplyr <- function(ed_data,
                                         inpatient_data,
                                         same_type_episode_lag = 1,
                                         different_type_episode_lag = 6) {

  ed_episodes <- ed_data %>% dplyr::mutate(episode_type = "ED")
  ip_episodes <- inpatient_data %>% dplyr::mutate(episode_type = "IP")

  all_episodes <- dplyr::bind_rows(ed_episodes, ip_episodes) %>%
    dplyr::filter(!is.na(end_datetime)) %>%
    dplyr::arrange(pseudo_id, start_datetime)

  cluster <- multidplyr::new_cluster(4)

  all_episodes <- all_episodes %>%
    multidplyr::partition(cluster) %>%
    dplyr::mutate(episode_lag = difftime(start_datetime, dplyr::lag(end_datetime), units = "hours")) %>%
    dplyr::mutate(prev_episode_type = dplyr::lag(episode_type)) %>%
    dplyr::mutate(new_spell = dplyr::if_else(is.na(prev_episode_type) |
                                               ((prev_episode_type == episode_type) & episode_lag > same_type_episode_lag) |
                                               ((prev_episode_type != episode_type) & episode_lag > different_type_episode_lag), TRUE, FALSE)) %>%
    dplyr::collect()
  #    dplyr::mutate(spell_number = cumsum(new_spell))

  all_episodes
}

#' @export
make_spell_number_furrr <- function(
  ed_data,
  inpatient_data,
  same_type_episode_lag = 1,
  different_type_episode_lag = 6) {

  num_workers <- future::availableCores()-1
  future::plan(future::multisession, workers = num_workers)
  ed_episodes <- ed_data %>%
    dplyr::mutate(episode_type = "ED")

  ip_episodes <- inpatient_data %>%
    dplyr::mutate(episode_type = "IP")

  all_episodes <- dplyr::bind_rows(ed_episodes, ip_episodes) %>%
    dplyr::filter(!is.na(end_datetime)) %>%
    dplyr::arrange(pseudo_id, start_datetime) %>%
    dplyr::group_nest(pseudo_id, .key="grouped_data", keep = F) %>% # dplyr::group_by(pseudo_id) %>%
    dplyr::mutate(worker_id = base::sample(1:num_workers, replace=T, size=nrow(.))) %>%
    dplyr::group_split(worker_id, .keep=F) %>%
    furrr::future_map_dfr( function(.data)
      tidyr::unnest(.data, grouped_data) %>%
        dplyr::group_by(pseudo_id) %>%
        dplyr::mutate(episode_lag = difftime(start_datetime, dplyr::lag(end_datetime), units = "hours")) %>%
        dplyr::mutate(prev_episode_type = dplyr::lag(episode_type)) %>%
        dplyr::mutate(new_spell = dplyr::if_else(
          is.na(prev_episode_type) |
            ((prev_episode_type == episode_type) & episode_lag > same_type_episode_lag) |
            ((prev_episode_type != episode_type) & episode_lag > different_type_episode_lag), TRUE, FALSE)) %>%
        tidyr::unnest()
    ) %>%
    dplyr::arrange(pseudo_id, start_datetime) %>%
    dplyr::mutate(spell_number = cumsum(new_spell))


  all_episodes

}

#' @export
make_spell_number_dtplyr <- function(ed_data, inpatient_data, same_type_episode_lag = 1, different_type_episode_lag = 6) {

  ed_episodes <- ed_data %>%
    dplyr::mutate(episode_type = "ED")

  ip_episodes <- inpatient_data %>%
    dplyr::mutate(episode_type = "IP")

  all_episodes <- dplyr::bind_rows(ed_episodes, ip_episodes) %>%
    dtplyr::lazy_dt() %>%
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


# Improving spell_variables (2/2)


#' @export
spell_variables_new <- function(all_episodes) {
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

  #return(lst_of_dfs)
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


