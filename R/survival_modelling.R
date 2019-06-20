
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
#' @param S_function the survival function
#' @param delta_t time interval
#'
#' @return probability still in after additional delta_t
#' @export
#'
#' @examples
get_discharge_probability <- function(time_in, S_function, delta_t) {
  S_function(time_in + delta_t)/S_function(time_in)
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


#' make_survival_function
#'
#' @param KM Kaplan-Meier output of survfit
#'
#' @return the Kaplan-Meier estimate as a stepfun
#' @export
#'
#' @examples
make_survival_function <- function(KM) {
  KM0t <- tibble::tibble(time = KM$time,
                 n.risk = KM$n.risk,
                 n.event = KM$n.event,
                 n.censor = KM$n.censor,
                 surv = KM$surv)
  stepfun(x = KM0t$time, y = c(1, KM0t$surv), right = TRUE)
}
