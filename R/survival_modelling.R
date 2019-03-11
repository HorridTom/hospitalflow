
#' get_poisson_binomial_characteristic_values
#'
#' @param p vector of N = n + 1 probabilities, where n is number of bernoulli trials
#' summed to form a poisson binomial distribution
#'
#' @return values of the characteristic function of the pbd
#' evaluated at arguments of Nth roots of unity
#'
#' @export
#'
#' @examples
get_poisson_binomial_characteristic_values <- function(p) {
  N <- length(p) + 1
  i <- complex(real = 0, imaginary = 1)
  C <- exp(2*pi*i/N)
  l <- c(0:(N-1))
  x <- sapply(l, function(j) {
    prod(get_product_terms(p, j))
  })
  x
}

get_product_terms <- function(p, l) {
  N <- length(p)
  i <- complex(real = 0, imaginary = 1)
  C <- exp(2*pi*i/(N+1))
  m <- c(1:N)
  terms <- 1 + (C^l - 1)*p[m]
  terms
}

#' get_discharge_probability
#'
#' @param time_in the length of time elapsed so far
#' @param KM
#' @param delta_t time interval
#'
#' @return probability still in after additional delta_t
#' @export
#'
#' @examples
get_discharge_probability <- function(time_in, KM, delta_t) {
  # This is a hack - must be a better way to evaluate S(t) for arbitrary t from KM?
  t0 <- DescTools::Closest(KM$time, time_in)[1]
  t1 <- DescTools::Closest(KM$time, time_in + delta_t)[1]

  St0 <- KM %>% dplyr::select(time, surv) %>% dplyr::filter(time == t0) %>% dplyr::pull(surv)
  St1 <- KM %>% dplyr::select(time, surv) %>% dplyr::filter(time == t1) %>% dplyr::pull(surv)

  p <- St1/St0
  p
}


#' get_inpatient_snapshot
#'
#' @param df spell data including start_datetime and end_datetime
#' @param t time to take snapshot
#'
#' @return df restricted to all spells in hospital at time t
#' @export
#'
#' @examples
get_inpatient_snapshot <- function(df, t) {
  df %>% filter(start_datetime <= t, end_datetime > t) %>%
    mutate(stay_duration = difftime(t, start_datetime, units = "hours"))
}
