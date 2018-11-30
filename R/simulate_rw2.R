#' simulate data from a second order random walk
#'
#' @param sigma the standard deviation of the random walk process
#' @param tau the precision of the random walk process
#' @param length the length of the time series
#' @param start the starting values of the time series
#' @param n_sim the number of simulations
#' @return a data.frame with simulated time series from the second order random walk
#' @export
#' @importFrom assertthat assert_that is.number is.count
#' @importFrom dplyr %>% bind_rows
#' @importFrom stats arima.sim
#' @family priors
simulate_rw2 <- function(sigma = 0.01, tau = NULL, length = 10, start = 1, n_sim = 1e3) {
  if (is.null(sigma)) {
    assert_that(
      is.number(tau),
      tau > 0
    )
    sigma <- tau ^ -0.5
  } else {
    assert_that(
      is.null(tau),
      is.number(sigma),
      sigma > 0
    )
  }
  assert_that(
    is.count(length),
    is.number(start),
    is.count(n_sim)
  )
  x <- seq(start, length = length, by = 1)
  lapply(
    seq_len(n_sim),
    function(i) {
      data.frame(
        x,
        y = head(
          arima.sim(
            model = list(order = c(0, 2, 0)),
            n = length,
            n.start = 1e3,
            sd = sigma
          ),
          length
        ),
        replicate = i
      )
    }
  ) %>%
    bind_rows() -> simulated
  class(simulated) <- c("rw2_sim", class(simulated))
  return(simulated)
}
