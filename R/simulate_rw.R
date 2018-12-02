#' simulate data from a second order random walk
#'
#' @param sigma the standard deviation of the random walk process
#' @param tau the precision of the random walk process
#' @param length the length of the time series
#' @param start the starting values of the time series
#' @param order 1 for first order random walk or 2 for second order random walk
#' @param n_sim the number of simulations
#' @return a data.frame with simulated time series from the random walk
#' @export
#' @importFrom assertthat assert_that is.number is.count
#' @importFrom dplyr %>% bind_rows
#' @importFrom stats arima.sim
#' @family priors
simulate_rw <- function(
  sigma = NULL, tau = NULL, length = 10, start = 1, order = 1, n_sim = 1e3
) {
  if (is.null(sigma)) {
    assert_that(
      is.number(tau),
      tau > 0
    )
    sigma <- tau ^ -0.5
  } else {
    assert_that(
      is.number(sigma),
      sigma > 0
    )
    assert_that(
      is.number(sigma),
      sigma > 0
    )
    assert_that(
      is.null(tau),
      msg = "either 'sigma' or 'tau' must be NULL"
    )
  }
  assert_that(
    is.count(length),
    is.number(start),
    is.count(order),
    order < 3,
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
            model = list(order = c(0, order, 0)),
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
  class(simulated) <- c("sim_rw", class(simulated))
  attr(simulated, "sigma") <- sigma
  return(simulated)
}

#' Plot simulated random walks
#' @param x a `sim_rw` object. Which is the output of  `\link{simulate_rw}`
#' @param y currently ignored
#' @param ... currently ignored
#' @param type which plot to create. `"all"` displays all simulations. `"divergence"` displays the most divergent simulations. `"stationary"` displays the smimulations with the smallest differences for the reference. `"quantile"` displays the enveloppes around the simulations. `"changes"` displays the simulations with the highest number of changes in directions in the random walk.
#' @param link which link to use for back transformation
#' @return a ggplot2 object
#' @family priors
#' @importFrom assertthat assert_that has_name
#' @importFrom ggplot2 ggplot aes_string geom_hline geom_line facet_wrap scale_y_continuous facet_grid
#' @importFrom scales percent
#' @importFrom dplyr %>% group_by summarise summarise_at  arrange semi_join lag desc
#' @importFrom rlang .data
#' @importFrom utils head
#' @importFrom tidyr crossing
#' @importFrom stats qlogis
#' @export
plot.sim_rw <- function(
  x, y, ...,
  type = c("all", "divergence", "stationary", "quantile", "change"),
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
  title <- ggtitle(
    bquote(
      sigma == .(signif(attr(x, "sigma"), 4)) ~
        sigma ^ 2 == .(signif(attr(x, "sigma") ^ 2, 4)) ~
        tau == .(signif(attr(x, "sigma") ^ -2, 4))
    )
  )

  switch(
    type,
    change = {
      if (link == "logit") {
        x %>%
          group_by(.data$replicate) %>%
          mutate(
            direction = sign(.data$y - lag(.data$y))
          ) %>%
          summarise(
            changes = sum(.data$direction != lag(.data$direction), na.rm = TRUE)
          ) %>%
          arrange(desc(.data$changes)) %>%
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
          ggplot(aes_string(x = "x", y = "y", group = "replicate")) +
            geom_hline(
              aes_string(yintercept = "reference"), linetype = 2, colour = "red"
            ) +
            geom_line() +
            scale_y_continuous("proportion", labels = percent) +
            facet_grid(facet ~ replicate, scales = "free_y") +
            title
      } else {
        x %>%
          group_by(.data$replicate) %>%
          mutate(
            direction = sign(.data$y - lag(.data$y))
          ) %>%
          summarise(
            changes = sum(.data$direction != lag(.data$direction), na.rm = TRUE)
          ) %>%
          arrange(desc(.data$changes)) %>%
          head(9) %>%
          semi_join(x = x, by = "replicate") %>%
          mutate(x, y = backtrans(y)) %>%
          ggplot(aes_string(x = "x", y = "y")) +
            geom_hline(yintercept = reference, linetype = 2, col = "red") +
            geom_line() +
            scale +
            facet_wrap(~replicate) +
            title
      }
    },
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
            y = .data$y + qlogis(.data$reference),
            y = plogis(.data$y),
            facet = factor(
              .data$reference,
              labels = sprintf(
                "base = %2.0f%%", 100 * sort(unique(.data$reference))
              )
            )
          ) %>%
          ggplot(aes_string(x = "x", y = "y", group = "replicate")) +
            geom_hline(
              aes_string(yintercept = "reference"), linetype = 2, colour = "red"
            ) +
            geom_line() +
            scale_y_continuous("proportion", labels = percent) +
            facet_wrap(~facet, scales = "free_y") +
            title
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
            scale +
            title
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
            y = y + qlogis(.data$reference),
            y = plogis(.data$y),
            facet = factor(
              .data$reference,
              labels = sprintf(
                "base = %2.0f%%",
                100 * sort(unique(.data$reference))
              )
            )
          ) %>%
          ggplot(aes_string(x = "x", y = "y")) +
            geom_hline(
              aes_string(yintercept = "reference"), linetype = 2, colour = "red"
            ) +
            geom_line() +
            scale_y_continuous("proportion", labels = percent) +
            facet_grid(facet ~ replicate, scales = "free_y") +
            title
      } else {
        x %>%
          group_by(.data$replicate) %>%
          summarise(extreme = max(abs(.data$y))) %>%
          arrange(.data$extreme) %>%
          head(9) %>%
          semi_join(x = x, by = "replicate") %>%
          mutate(y = backtrans(.data$y)) %>%
          ggplot(aes_string(x = "x", y = "y")) +
            geom_hline(yintercept = reference, linetype = 2, col = "red") +
            geom_line() +
            scale +
            facet_wrap(~replicate) +
            title
      }
    },
    quantile = {
      if (link == "logit") {
        x %>%
          group_by(.data$x) %>%
          summarise(
            '2.5%' = quantile(y, 0.025),
            '10%' = quantile(y, 0.1),
            '25%' = quantile(y, 0.25),
            '50%' = quantile(y, 0.5),
            '75%' = quantile(y, 0.75),
            '90%' = quantile(y, 0.9),
            '97.5%' = quantile(y, 0.975)
          ) %>%
          crossing(reference) %>%
          gather("quantile", "y", -x, -reference, factor_key = TRUE) %>%
          mutate(
            y = y + qlogis(.data$reference),
            y = plogis(.data$y),
            facet = factor(
              .data$reference,
              labels = sprintf(
                "base = %2.0f%%",
                100 * sort(unique(.data$reference))
              )
            )
          ) %>%
          ggplot(aes_string(x = "x", y = "y", colour = "quantile")) +
            geom_hline(
              aes_string(yintercept = "reference"), linetype = 2, col = "red"
            ) +
            geom_line() +
            scale_y_continuous("proportion", labels = percent) +
            facet_wrap(~facet, scales = "free_y") +
            title
      } else {
        x %>%
          group_by(.data$x) %>%
          summarise(
            '2.5%' = quantile(y, 0.025),
            '10%' = quantile(y, 0.1),
            '25%' = quantile(y, 0.25),
            '50%' = quantile(y, 0.5),
            '75%' = quantile(y, 0.75),
            '90%' = quantile(y, 0.9),
            '97.5%' = quantile(y, 0.975)
          ) %>%
          gather("quantile", "y", -x, factor_key = TRUE) %>%
          mutate(x, y = backtrans(.data$y)) %>%
          ggplot(aes_string(x = "x", y = "y", colour = "quantile")) +
            geom_hline(yintercept = reference, linetype = 2, col = "red") +
            geom_line() +
            scale +
            title
      }
    },
    all = {
      if (link == "logit") {
        crossing(x, reference) %>%
          mutate(
            y = .data$y + qlogis(.data$reference),
            y = plogis(.data$y),
            facet = factor(
              .data$reference,
              labels = sprintf(
                "base = %2.0f%%",
                100 * sort(unique(.data$reference))
              )
            )
          ) %>%
          ggplot(aes_string(x = "x", y = "y", group = "replicate")) +
          geom_line(alpha = 0.1) +
          geom_hline(
            aes_string(yintercept = "reference"), linetype = 2, colour = "red"
          ) +
          scale_y_continuous("proportion", labels = percent) +
          facet_wrap(~facet, scales = "free_y") +
            title
      } else {
        x %>%
          mutate(x, y = backtrans(y)) %>%
          ggplot(aes_string(x = "x", y = "y", group = "replicate")) +
            geom_line(alpha = 0.1) +
            geom_hline(yintercept = reference, linetype = 2, col = "red") +
            scale +
            title
      }
    }
  )
}
