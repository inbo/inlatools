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
#' @param link which link to use for back transformation
#' @return a ggplot2 object
#' @family priors
#' @importFrom assertthat assert_that has_name
#' @importFrom ggplot2 ggplot aes_string geom_hline geom_line facet_wrap scale_y_continuous facet_grid
#' @importFrom scales percent
#' @importFrom dplyr %>% group_by summarise summarise_at  arrange semi_join
#' @importFrom rlang .data
#' @importFrom utils head
#' @importFrom tidyr crossing
#' @importFrom stats qlogis
#' @export
plot.rw2_sim <- function(
  x, y, ...,
  type = c("all", "divergence", "stationary"),
  link = c("identity", "log", "logit")
) {
  assert_that(
    inherits(x, "data.frame"),
    has_name(x, "x"),
    has_name(x, "y"),
    has_name(x, "replicate")
  )
  type <- match.arg(type)
  link <- match.arg(link)
  backtrans <- switch(
    link,
    identity = function(x) {
      x
    },
    log = exp,
    logit = plogis
  )
  reference <- switch(
    link, identity = 0, log = 1, logit = c(0.05, 0.1, 0.25, 0.5)
  )
  scale <- switch(
    link,
    identity = scale_y_continuous("effect"),
    log = scale_y_continuous("relative effect", labels = percent),
    logit = scale_y_continuous("proportion", labels = percent)
  )
  switch(
    type,
    divergence = {
      if (link == "logit") {
        x %>%
          group_by(.data$replicate) %>%
          summarise_at("y", c(min = min, max = max)) %>%
          arrange(pmin(rank(.data$min), rank(-.data$max))) %>%
          head(10) %>%
          semi_join(x = x, by = "replicate") %>%
          crossing(reference) %>%
          mutate(
            y = y + qlogis(reference),
            y = plogis(y),
            facet = factor(
              reference,
              labels = sprintf("base = %2.0f%%", 100 * sort(unique(reference)))
            )
          ) %>%
          ggplot(aes_string(x = "x", y = "y", group = "replicate")) +
            geom_hline(
              aes_string(yintercept = "reference"), linetype = 2, colour = "red"
            ) +
            geom_line() +
            scale_y_continuous("proportion", labels = percent) +
            facet_wrap(~facet, scales = "free_y")
      } else {
        x %>%
          group_by(.data$replicate) %>%
          summarise_at("y", c(min = min, max = max)) %>%
          arrange(pmin(rank(.data$min), rank(-.data$max))) %>%
          head(10) %>%
          semi_join(x = x, by = "replicate") %>%
          mutate(y = backtrans(y)) %>%
          ggplot(aes_string(x = "x", y = "y", group = "replicate")) +
            geom_hline(yintercept = reference, linetype = 2, col = "red") +
            geom_line() +
            scale
      }
    },
    stationary = {
      if (link == "logit") {
        x %>%
          group_by(.data$replicate) %>%
          summarise(extreme = max(abs(.data$y))) %>%
          arrange(.data$extreme) %>%
          head(9) %>%
          semi_join(x = x, by = "replicate") %>%
          crossing(reference) %>%
          mutate(
            y = y + qlogis(reference),
            y = plogis(y),
            facet = factor(
              reference,
              labels = sprintf("base = %2.0f%%", 100 * sort(unique(reference)))
            )
          ) %>%
          ggplot(aes_string(x = "x", y = "y")) +
            geom_hline(
              aes_string(yintercept = "reference"), linetype = 2, colour = "red"
            ) +
            geom_line() +
            scale_y_continuous("proportion", labels = percent) +
            facet_grid(facet ~ replicate, scales = "free_y")
      } else {
        x %>%
          group_by(.data$replicate) %>%
          summarise(extreme = max(abs(.data$y))) %>%
          arrange(.data$extreme) %>%
          head(9) %>%
          semi_join(x = x, by = "replicate") %>%
          mutate(y = backtrans(y)) %>%
          ggplot(aes_string(x = "x", y = "y")) +
            geom_hline(yintercept = reference, linetype = 2, col = "red") +
            geom_line() +
            scale +
            facet_wrap(~replicate)
      }
    },
    all = {
      if (link == "logit") {
        crossing(x, reference) %>%
          mutate(
            y = y + qlogis(reference),
            y = plogis(y),
            facet = factor(
              reference,
              labels = sprintf("base = %2.0f%%", 100 * sort(unique(reference)))
            )
          ) %>%
          ggplot(aes_string(x = "x", y = "y", group = "replicate")) +
          geom_line(alpha = 0.1) +
          geom_hline(
            aes_string(yintercept = "reference"), linetype = 2, colour = "red"
          ) +
          scale_y_continuous("proportion", labels = percent) +
          facet_wrap(~facet, scales = "free_y")
      } else {
        x %>%
          mutate(x, y = backtrans(y)) %>%
          ggplot(aes_string(x = "x", y = "y", group = "replicate")) +
            geom_line(alpha = 0.1) +
            geom_hline(yintercept = reference, linetype = 2, col = "red") +
            scale
      }
    }
  )
}
