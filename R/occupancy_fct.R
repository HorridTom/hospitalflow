#' occupancy
#'
#' @param date_time datetime at which we want to know the occupancy
#' @param df dataframe representing stays, with start_datetime and end_datetime specifying stay start and end
#'
#' @return the occupancy at date_time
#' @export
#'
#' @examples
occupancy <- function(date_time, df, start_time = "spell_start", end_time = "spell_end"){


  start_time <- as.name(start_time)
  end_time <- as.name(end_time)

  #point interval for the time that you are calculating the occupancy for
  testInterval <- lubridate::interval(date_time, date_time)

  df <- df %>%
    dplyr::mutate(stayInterval = lubridate::interval(!!start_time,!!end_time)) %>%
    dplyr::mutate(overLapTest = lubridate::int_overlaps(stayInterval, testInterval))

  occupancy <- sum(df$overLapTest)
  occupancy

}
