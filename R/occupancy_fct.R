#' occupancy
#'
#' @param time_instance datetime at which we want to know the occupancy
#' @param df dataframe on which to perform the occupancy function
#' @param df_type whether the df specified is a "spell table" or an "inpatient table" or "ED table"
#'
#' @return the occupancy at date_time
#' @export
#'
#' @examples
occupancy <- function(time_instance, df, df_type = "spell table"){

  #get the correct column name for each data set type
  if(df_type == "spell table"){
    start_time = as.name("spell_start")
    end_time = as.name("spell_end")
  }else if(df_type == "ED table"){
    start_time = as.name("start_datetime")
    end_time = as.name("end_datetime")
  }else{
    start_time = as.name("spell_start")
    end_time = as.name("initial_ed_end_datetime")
  }

  #point interval for the time that you are calculating the occupancy for
  testInterval <- lubridate::interval(time_instance, time_instance)

  df <- df %>%
    dplyr::mutate(stayInterval = lubridate::interval(!!start_time,!!end_time)) %>%
    dplyr::mutate(overLapTest = lubridate::int_overlaps(stayInterval, testInterval))

  occupancy <- sum(df$overLapTest)
  occupancy

}
