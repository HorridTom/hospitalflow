# need a skeleton

# functions to calculate and plot the registered ED occupancy on arrival against the unscheduled attendances with LoS >4hrs,
# wait for assessment and unscheduled ED attendance


# standard occupancy function
ed_occupancy <- function(occupancyDateTime, df){


  #point interval for the time that you are calculating the occupancy for
  testInterval <- lubridate::interval(occupancyDateTime, occupancyDateTime)

  df <- df %>%
    #this line should make the code faster but currently having problems with the >= for datetimes
    #dplyr::filter(start_datetime <= occupancyDateTime & end_datetime >= occupancyDateTime) %>%
    dplyr::mutate(stayInterval = lubridate::interval(start_datetime,end_datetime)) %>%
    dplyr::mutate(overLapTest = lubridate::int_overlaps(stayInterval, testInterval))


  occupancy <- sum(df$overLapTest)

  occupancy

}


#
ed_occupancy_sequence <- function(df){


  #just for test dataset at the moment
  times <- c("2017-05-01 00:00:00", "2017-05-01 12:00:00", "2017-05-02 00:00:00", "2017-05-02 12:00:00",
             "2017-05-03 00:00:00", "2017-05-03 12:00:00")

  occupancy_sequence <- sapply(times, ed_occupancy, df = df)
  occupancy_sequence

}





# function to calculate the ED occupancy on arrival
# will try two methods: calculate each row individually and create a table and use lookup
ed_occupancy_on_arrival <- function(df = test_ed_occupancy_on_arrival_data){

  df <- df %>%
    rowwise %>%
    mutate(occupancy_on_arrival = ed_occupancy(start_datetime, df))

  df

}
