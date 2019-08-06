#' occupancy_fct
#'
#' @param start_date
#' @param end_date
#' @param data
#'
#' @return
#' @export
#'
#' @examples
occupancy_fct <- function(start_date = as.POSIXct("2016-06-01 00:00:00", tz = "Europe/London"),
                          end_date = as.POSIXct("2016-06-08 00:00:00", tz = "Europe/London"),
                          data){

  # using gather function to create a new column with date
  occupancy <-  data %>%
    tidyr::gather(key = type, time, spell_start:spell_end) %>%
    dplyr::mutate(change = dplyr::if_else(type == "spell_start", 1, -1)) %>%
    dplyr::group_by(time_hr = lubridate::floor_date(time, "1 hour")) %>%
    dplyr::summarise(change = sum(change)) %>%
    padr::pad(start_val = start_date, end_val = end_date) %>%
    tidyr::replace_na(list(change = 0)) %>%
    dplyr::mutate(occupancy = cumsum(change)) %>%
    dplyr::select(-change) %>%
    tidyr::drop_na()

}
