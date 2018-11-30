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

#' Plot simulated second order random walks
#' @param x a `rw2_sim` object. Which is the output of  `\link{simulate_rw2}`
#' @param y currently ignored
#' @param ... currently ignored
#' @param type which plot to create. `"all"` displays all simulations. `"divergence"` displays the most divergent simulations.
#' @return a ggplot2 object
#' @family priors
#' @importFrom assertthat assert_that has_name
#' @importFrom ggplot2 ggplot aes_string geom_hline geom_line facet_wrap
#' @importFrom dplyr %>% group_by summarise summarise_at  arrange semi_join
#' @importFrom rlang .data
#' @importFrom utils head
#' @export
plot.rw2_sim <- function(
  x, y, ..., type = c("all", "divergence", "stationary")
) {
  assert_that(
    inherits(x, "data.frame"),
    has_name(x, "x"),
    has_name(x, "y"),
    has_name(x, "replicate")
  )
  type <- match.arg(type)
  switch(
    type,
    divergence = {
      x %>%
        group_by(.data$replicate) %>%
        summarise_at("y", c(min = min, max = max)) %>%
        arrange(pmin(rank(.data$min), rank(-.data$max))) %>%
        head(10) %>%
        semi_join(x = x, by = "replicate") %>%
        ggplot(aes_string(x = "x", y = "y", group = "replicate")) +
          geom_hline(yintercept = 0, linetype = 2, col = "red") +
          geom_line()
    },
    stationary = {
      x %>%
        group_by(.data$replicate) %>%
        summarise(extreme = max(abs(.data$y))) %>%
        arrange(.data$extreme) %>%
        head(9) %>%
        semi_join(x = x, by = "replicate") %>%
        ggplot(aes_string(x = "x", y = "y", group = "replicate")) +
          geom_hline(yintercept = 0, linetype = 2, col = "red") +
          geom_line() +
          facet_wrap(~replicate)
    },
    ggplot(x, aes_string(x = "x", y = "y", group = "replicate")) +
      geom_line(alpha = 0.1) +
      geom_hline(yintercept = 0, linetype = 2, col = "red")
  )
}
