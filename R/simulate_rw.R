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
#' @importFrom dplyr %>% bind_rows tibble
#' @importFrom stats arima.sim
#' @family priors
#' @examples
#' set.seed(20181202)
#' x <- simulate_rw(sigma = 0.1, start = -10, length = 40)
#' head(x)
#' y <- simulate_rw(sigma = 0.001, start = -10, length = 40, order = 2)
#' head(y)
simulate_rw <- function(
  sigma = NULL, tau = NULL, length = 10, start = 1, order = 1, n_sim = 1e3
) {
  if (is.null(sigma)) {
    assert_that(
      !is.null(tau),
      msg = "either 'sigma' or 'tau' must be specified"
    )
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
      tibble(
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
#' @param coefs the polynomial coefficients
#' @param n the number of simulations to plot when only a subset is shown.
#' @return a ggplot2 object
#' @family priors
#' @importFrom assertthat assert_that has_name is.count noNA
#' @importFrom ggplot2 ggplot aes_string geom_hline geom_line facet_wrap scale_y_continuous facet_grid
#' @importFrom scales percent
#' @importFrom dplyr %>% group_by summarise summarise_at  arrange semi_join lag desc
#' @importFrom rlang .data
#' @importFrom utils head
#' @importFrom tidyr crossing
#' @importFrom stats qlogis
#' @export
#' @examples
#' \donttest{
#' set.seed(20181202)
#' x <- simulate_rw(sigma = 0.05, start = -10, length = 40)
#' plot(x)
#' plot(x, type = "quantile")
#' plot(x, type = "divergence")
#' plot(x, type = "stationary")
#' plot(x, type = "change")
#' plot(x, type = "quantile", link = "log")
#' plot(x, type = "quantile", link = "logit")
#' y <- simulate_rw(sigma = 0.001, start = -10, length = 40, order = 2)
#' plot(y)
#' plot(y, type = "quantile")
#' plot(y, type = "divergence")
#' plot(y, type = "stationary")
#' plot(y, type = "change")
#' plot(y, type = "quantile", link = "log")
#' plot(y, type = "quantile", link = "logit")
#' }
plot.sim_rw <- function(
  x, y, ...,
  type = c("all", "divergence", "stationary", "quantile", "change", "poly"),
  link = c("identity", "log", "logit"),
  n = 10, coefs = c(0, -1)
) {
  assert_that(
    inherits(x, "data.frame"),
    has_name(x, "x"),
    has_name(x, "y"),
    has_name(x, "replicate"),
    is.count(n),
    is.numeric(coefs),
    length(coefs) > 1,
    noNA(coefs)
  )
  type <- match.arg(type)
  link <- match.arg(link)
  coefs <- coefs / sqrt(sum(coefs ^ 2))

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
    poly = {
      selection <- select_poly(x, coefs = coefs, n = n)
      if (link == "logit") {
        selection %>%
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
            facet_wrap(~facet, scales = "free_y") +
            title
      } else {
        selection %>%
          mutate(y = backtrans(y)) %>%
          ggplot(aes_string(x = "x", y = "y", group = "replicate")) +
            geom_hline(yintercept = reference, linetype = 2, col = "red") +
            geom_line() +
            scale +
            title
      }
    },
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
          select_quantile(quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.975)) %>%
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
          ggplot(aes_string(x = "x", y = "y", colour = "replicate")) +
            geom_hline(
              aes_string(yintercept = "reference"), linetype = 2, col = "red"
            ) +
            geom_line() +
            scale_y_continuous("proportion", labels = percent) +
            facet_wrap(~facet, scales = "free_y") +
            title
      } else {
        x %>%
          select_quantile(quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.975)) %>%
          mutate(x, y = backtrans(.data$y)) %>%
          ggplot(aes_string(x = "x", y = "y", colour = "replicate")) +
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

#' Select random walks best matching some polygon coefficients
#'
#' The target coefficients will be rescaled to have norm 1. The coefficients of the simulations will be rescaled by the largest norm over all simulations.
#' @param x a `sim_rw` object
#' @inheritParams plot.sim_rw
#' @noRd
#' @importFrom dplyr %>% group_by transmute ungroup mutate summarise arrange select
#' @importFrom rlang .data
#' @importFrom tidyr nest unnest
#' @importFrom purrr map
#' @importFrom tibble rownames_to_column
#' @importFrom stats as.formula lm poly coefficients
#' @importFrom utils head
select_poly <- function(x, coefs = c(0, -1), n = 10) {
  assert_that(
    inherits(x, "sim_rw"),
    has_name(x, "x"),
    has_name(x, "y"),
    has_name(x, "replicate"),
    is.count(n),
    is.numeric(coefs),
    length(coefs) > 1,
    noNA(coefs),
    all(is.finite(coefs))
  )
  sprintf("y ~ poly(x, %i)", length(coefs)) %>%
    as.formula() -> formula
  coefs <- coefs / sqrt(sum(coefs ^ 2))
  x %>%
    group_by(.data$replicate) %>%
    nest() %>%
    mutate(
      coef = map(.data$data, lm, formula = formula) %>%
        map(summary) %>%
        map(coefficients) %>%
        map(data.frame) %>%
        map(rownames_to_column)
    ) %>%
    select(-.data$data) %>%
    unnest() %>%
    filter(grepl("poly", .data$rowname)) %>%
    group_by(.data$replicate) %>%
    transmute(
      .data$rowname,
      .data$Estimate,
      norm = sqrt(sum(.data$Estimate ^ 2))
    ) %>%
    ungroup() %>%
    mutate(Estimate = .data$Estimate / max(.data$norm)) %>%
    inner_join(
      tibble(
        rowname = sprintf("poly(x, %i)%i", length(coefs), seq_along(coefs)),
        target = coefs
      ),
      by = "rowname"
    ) %>%
    group_by(.data$replicate) %>%
    summarise(delta = sum(
      (.data$target - .data$Estimate) ^ 2)
    ) %>%
    arrange(.data$delta) %>%
    head(n) %>%
    semi_join(x = x, by = "replicate") -> selection
  class(selection) <- c("sim_rw", class(selection))
  attr(selection, "sigma") <- attr(x, "sigma")
  return(selection)
}


#' select the quantiles from an 'sim_rw' object
#' @inheritParams plot.sim_rw
#' @param quantiles a vector of quantiles
#' @noRd
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr %>% group_by mutate tibble
#' @importFrom rlang .data
#' @importFrom tidyr nest
#' @importFrom purrr map
#' @importFrom stats quantile
select_quantile <- function(
  x,
  quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.975)
) {
  assert_that(
    inherits(x, "sim_rw"),
    has_name(x, "x"),
    has_name(x, "y"),
    has_name(x, "replicate")
  )
  x %>%
    group_by(.data$x) %>%
    nest() %>%
    mutate(
      data = map(
        .data$data,
        ~tibble(
          y = quantile(.x$y, quantiles),
          replicate = quantiles
        )
      )
    ) %>%
    unnest() %>%
    mutate(
      replicate = factor(
        replicate,
        levels = sort(quantiles),
        labels = sprintf("%.1f%%", 100 * sort(quantiles))
      )
    ) -> selection
  class(selection) <- c("sim_rw_quant", "sim_rw", class(selection))
  attr(selection, "sigma") <- attr(x, "sigma")
  return(selection)
}
