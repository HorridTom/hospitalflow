
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
  p <- S_function(time_in + delta_t)/S_function(time_in)
  p[is.nan(p)] <- 1
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


#' predict_residual_occupancy
#'
#' @param df
#' @param t
#' @param S_function
#' @param delta_t
#'
#' @return
#' @export
#'
#' @examples
predict_residual_occupancy <- function(df, t, S_function, delta_t, coxmodel = NULL) {
  if(is.null(coxmodel)) {
    ipss1 <- hospitalflow::get_inpatient_snapshot(df = df, t = t) %>% filter(!is.na(stay_duration))
    ipss1 <- ipss1 %>% mutate(prob_here_after_dt = hospitalflow::get_discharge_probability(stay_duration, S_function, delta_t))
    ipss1 %>% pull(prob_here_after_dt) %>% sum()
  } else {
    # filter to only available data
    df <- df %>% dplyr::filter(start_datetime <= t)
    CM <- coxph(as.formula(coxmodel), data = df, na.action = na.exclude)
    ipss1 <- hospitalflow::get_inpatient_snapshot(df = df, t = t) %>% dplyr::filter(!is.na(stay_duration))
    newdata <- ipss1 %>%
      dplyr::select(obsT = stay_duration, adm_wd, adm_daytime, adm_calmonth, gender, age_band_start,
                    disch_in_last_week, disch_in_last_month, admitted_before) %>%
      mutate(status = TRUE)
    newdata_d <- newdata %>% mutate(obsT = obsT + delta_t)
    CM_prediction_0 <- predict(CM, newdata = newdata, type = "expected")
    CM_prediction_1 <- predict(CM, newdata = newdata_d, type = "expected")
    prob_here_48h_CM <- exp(-CM_prediction_1)/exp(-CM_prediction_0)
    sum(prob_here_48h_CM)
  }
}

#' get_residual_occupancy
#'
#' @param df
#' @param t
#' @param delta_t
#'
#' @return
#' @export
#'
#' @examples
get_residual_occupancy <- function(df, t, delta_t) {
  ipss1 <- hospitalflow::get_inpatient_snapshot(df = df, t = t) %>% filter(!is.na(stay_duration))
  ipss1_dt <- hospitalflow::get_inpatient_snapshot(df = df, t = t + delta_t*60*60) %>% mutate(residual = spell_number %in% ipss1$spell_number)
  ipss1_dt %>% filter(residual == TRUE) %>% nrow()
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


#' run_S_func_model
#'
#' @param df dataset of spells
#' @param S_func survival function
#' @param date_seq sequence of dates to predict for
#' @param horizon how far ahead to predict (default 48h)
#'
#' @return object containing prediction table and diagnostics
#' @export
#'
#' @examples
run_S_func_model <- function(df, S_func, coxmodel = NULL, date_seq, horizon = 48) {

  pred_res_occ_v <- Vectorize(hospitalflow::predict_residual_occupancy, vectorize.args = "t")
  get_res_occ_v <- Vectorize(hospitalflow::get_residual_occupancy, vectorize.args = "t")

  predictions <- tibble::tibble(dates = date_seq)
  predictions <- predictions %>% dplyr::mutate(predicted = pred_res_occ_v(df = df, dates,
                                                                          S_function = S_function,
                                                                          coxmodel = coxmodel,
                                                                          delta_t = horizon))
  predictions <- predictions %>% dplyr::mutate(actual = get_res_occ_v(df = df, dates, delta_t = horizon)) %>%
    dplyr::mutate(error = predicted - actual, rel_error = error/actual,
                  abs_error = abs(error), abs_rel_error = abs(rel_error))

  prediction_comparison <- predictions %>% select(dates, predicted, actual) %>% gather(key = "type", value = "residual_occ", predicted, actual)
  comp_plot <- ggplot2::ggplot(data = prediction_comparison, mapping = aes(dates, residual_occ)) + ggplot2::geom_point(aes(group = type, colour = type)) + geom_line(aes(group = type, colour = type)) + ggtitle("Residual occupancy: predictions versus actual") +
    xlab("Date of prediction") + ylab("Residual occupancy")

  err_plot <- ggplot2::ggplot(data = predictions, mapping = aes(dates, error)) + ggplot2::geom_point() + geom_line() + ggtitle("Residual occupancy: errors in predictions versus actual") +
    xlab("Date of prediction") + ylab("Error")

  rel_err_plot <- ggplot2::ggplot(data = predictions, mapping = aes(dates, rel_error)) + ggplot2::geom_point() + geom_line() + ggtitle("Residual occupancy: relative errors in predictions versus actual") +
    xlab("Date of prediction") + ylab("Relative Error")

  diagnostics <- list()
  output <- list()

  diagnostics$mean_error <- mean(predictions$error)
  diagnostics$mean_rel_error <- mean(predictions$rel_error)
  diagnostics$mean_abs_err <- mean(predictions$abs_error)
  diagnostics$mean_abs_rel_err <- mean(predictions$abs_rel_error)
  diagnostics$comp_plot <- comp_plot
  diagnostics$err_plot <- err_plot
  diagnostics$rel_err_plot <- rel_err_plot

  output$diagnostics <- diagnostics
  output$predictions <- predictions

  output

}

