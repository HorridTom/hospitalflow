
scratch_data_standardis <- function(data) {

  #set.seed(22)

  #test_data <- sample_n(data[c("PAT_CODE", "WARD_CODE", "START_DATETIME", "END_DATETIME",
  # "episode.order", "spell.type", "AGE_BAND", "SEX")], size = 30000, replace = FALSE)

  #subseting data set#
  #colname_ct <- function(data, colname){
  #data[,colname] = as.POSIXct(data[,colname])
  #data
  #}


  #df_date_ct <- test_data %>%
  #colname_ct("START_DATETIME") %>%
  #colname_ct("END_DATETIME")

  #df_rename <- df_date_ct %>%
  #dplyr::select(PAT_CODE, START_DATETIME, END_DATETIME, WARD_CODE, episode.order, spell.type, AGE_BAND, SEX) %>%
  #dplyr::rename(IDcol = PAT_CODE, Admissions = START_DATETIME,
  # Discharges = END_DATETIME, EpisodeNumber = episode.order, PatientType = spell.type,
  #WardCode = WARD_CODE, PatientType = spell.type)

  #start_date <- as.POSIXct("2015-01-01 00:00:00", tz = "Europe/London")
  #end_date <- as.POSIXct( "2015-01-31 00:00:00", tz = "Europe/London")
  #df_date_ct <- df_rename %>%
  # colname_ct("Admissions") %>%
  # colname_ct("Discharges")

  #start_date <- as.POSIXct("2015-01-01 00:00:00", tz = "Europe/London")
  #end_date <- as.POSIXct( "2016-01-31 00:00:00", tz = "Europe/London")
  }

