
# Function to clean C&W data for analysis
standardisation_dt <- function(data){

  flow_patient_dt <- data %>%
    dplyr::rename(IDcol = PAT_CODE, Admissions = START_DATETIME, Discharges = END_DATETIME,
                  EpisodeNumber = episode.order, PatientType = spell.type,
                  Ward = WARD_CODE, Gender = SEX, Age_band = AGE_BAND, LastWard = spell.end.ward)


  flow_patient_dt <- flow_patient_dt %>%
    dplyr::mutate(Age_band =  factor(Age_band, levels = c("0 yrs", "1-4 yrs","5-9 yrs","10-14 yrs","15-19 yrs","20-24 yrs","25-29 yrs","30-34 yrs","35-39 yrs","40-44 yrs",
                                                          "45-49 yrs","50-54 yrs","55-59 yrs","60-64 yrs","65-69 yrs","70-74 yrs","75-79 yrs","80-84 yrs", "85+ yrs")),
                  EpisodeNumber = as.numeric(EpisodeNumber),
                  PatientType = as.character(PatientType),
                  Ward = as.character(Ward),
                  Gender = as.character(Gender),
                  LastWard = as.character(LastWard))

  flow_patient_dt

 }

# Ran the above function on the raw 2012-2015 C&W data, then as follows:
# cw_pt_ids <- cw_disch_201201_201507 %>% distinct(IDcol) %>% dplyr::pull(IDcol)
# pt_id_sample <- sample(cw_pt_ids, 50000, replace = FALSE)
# stringr::str_sub(system("git rev-parse HEAD", intern=TRUE), 1, 8)
# cw_disch_201201_201507_782cfa21_stddt_s <- cw_disch_201201_201507 %>% filter(IDcol %in% pt_id_sample)
# devtools::use_data(cw_disch_201201_201507_782cfa21_stddt_s)
