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
#' @param link which link to use for back transformation
#' @param baseline optional baseline for the time series
#' @param center defines how to center the time series to the baseline. Options are: `start` all time series start at the baseline; `mean` the average of the time series is the baseline; `bottom` the lowest value of the time series equals the baseline; `top` the highest value of the time series equals the baseline
#' @return a `\link[ggplot2]{ggplot}` object
#' @family priors
#' @importFrom assertthat assert_that has_name
#' @importFrom ggplot2 ggplot aes_string geom_hline geom_line facet_wrap labs
#' @importFrom scales percent
#' @importFrom dplyr n_distinct %>% mutate distinct slice
#' @importFrom rlang .data
#' @importFrom utils head
#' @importFrom tidyr crossing
#' @importFrom stats qlogis plogis
#' @export
#' @examples
#' \donttest{
#' set.seed(20181202)
#' x <- simulate_rw(sigma = 0.05, start = -10, length = 40)
#' plot(x)
#' plot(select_quantile(x))
#' plot(select_quantile(x), link = "log")
#' plot(select_quantile(x), link = "logit")
#' x <- simulate_rw(sigma = 0.001, start = -10, length = 40, order = 2)
#' plot(x)
#' plot(select_quantile(x))
#' plot(select_quantile(x), link = "log")
#' plot(select_quantile(x), link = "logit")
#' }
plot.sim_rw <- function(
  x, y, ..., link = c("identity", "log", "logit"), baseline,
  center = c("start", "mean", "bottom", "top")
) {
  assert_that(
    inherits(x, "data.frame"),
    has_name(x, "x"),
    has_name(x, "y"),
    has_name(x, "replicate")
  )
  link <- match.arg(link)
  center <- match.arg(center)
  if (missing(baseline)) {
    baseline <- switch(
      link, identity = 0, log = 1, logit = c(0.05, 0.1, 0.25, 0.5)
    )
  } else {
    assert_that(
      is.numeric(baseline),
      length(baseline) >= 1
    )
  }

  alpha <- sqrt(10) / sqrt(pmax(10, n_distinct(x$replicate)))

  switch(
    center,
    start = x %>%
      group_by(.data$replicate) %>%
      arrange(.data$x) %>%
      slice(1) %>%
      select("replicate", center = "y"),
    mean = x %>%
      group_by(.data$replicate) %>%
      summarise(center = mean(.data$y)),
    bottom = x %>%
      group_by(.data$replicate) %>%
      summarise(center = min(.data$y)),
    top = x %>%
      group_by(.data$replicate) %>%
      summarise(center = max(.data$y))
  ) %>%
    inner_join(x, by = "replicate") %>%
    mutate(y = .data$y - .data$center) %>%
    select(-"center") -> z

  z <- crossing(z, bl = baseline)
  z <- switch(
    link,
    identity = z %>%
      mutate(
        y = .data$y + .data$bl,
        facet = factor(.data$bl)
      ),
    log = z %>%
      mutate(
        y = exp(.data$y + log(.data$bl)),
        facet = factor(.data$bl)
      ),
    logit = z %>%
      mutate(
        y = plogis(.data$y + qlogis(.data$bl)),
        facet = factor(
          .data$bl,
          labels = paste0(100 * baseline, "%")
        )
      )
  )

  p <- ggplot(z, aes_string(x = "x", y = "y", group = "replicate")) +
    labs(
      title = bquote(
        sigma == .(signif(attr(x, "sigma"), 4)) ~
          sigma ^ 2 == .(signif(attr(x, "sigma") ^ 2, 4)) ~
          tau == .(signif(attr(x, "sigma") ^ -2, 4))
      )
    )
  if (inherits(x, "sim_rw_quant")) {
    p <- p +
      geom_line(alpha = alpha, aes_string(colour = "replicate")) +
      labs(colour = "quantile")
  } else {
    p <- p + geom_line(alpha = alpha)
  }
  if (length(baseline) == 1) {
    p <- p +
      geom_hline(yintercept = baseline, colour = "red", linetype = 2)
  } else {
    p <- p +
      geom_hline(
        data = distinct(z, .data$bl, .data$facet),
        aes_string(yintercept = "bl"),
        colour = "red",
        linetype = 2
      ) +
      facet_wrap(~facet, scales = "free_y")
  }
  switch(
    link,
    identity = p + scale_y_continuous("effect"),
    log = if (isTRUE(all.equal(baseline, 1))) {
      p + scale_y_continuous("relative effect", labels = percent)
    } else {
      p + scale_y_continuous("effect")
    },
    logit = p + scale_y_continuous("proportion", labels = percent)
  )
}

#' Select random walks best matching some polygon coefficients
#'
#' The target coefficients will be rescaled to have norm 1. The coefficients of the simulations will be rescaled by the largest norm over all simulations.
#' @inheritParams plot.sim_rw
#' @param coefs the polynomial coefficients
#' @param n the number of simulations to plot when only a subset is shown.
#' @family priors
#' @export
#' @importFrom assertthat assert_that has_name is.count noNA
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
    length(coefs) >= 1,
    noNA(coefs),
    all(is.finite(coefs))
  )
  sprintf("y ~ poly(x, %i)", length(coefs)) %>%
    as.formula() -> formula
  if (length(coefs) > 1) {
    rownames <- sprintf("poly(x, %i)%i", length(coefs), seq_along(coefs))
  } else {
    rownames <- "poly(x, 1)"
  }
  if (any(coefs != 0)) {
    coefs <- coefs / sqrt(sum(coefs ^ 2))
  }
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
        rowname = rownames,
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
#' @family priors
#' @export
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
        levels = rev(sort(quantiles)),
        labels = paste0(100 * rev(sort(quantiles)), "%")
      )
    ) -> selection
  class(selection) <- c("sim_rw_quant", "sim_rw", class(selection))
  attr(selection, "sigma") <- attr(x, "sigma")
  return(selection)
}

#' select fast changing simulations from an 'sim_rw' object
#'
#' This functions count the number of changes in direction in each simulation. It returns the subset with the highest number of direction changes
#' @inheritParams plot.sim_rw
#' @inheritParams select_poly
#' @family priors
#' @export
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr %>% group_by mutate summarise lag semi_join desc
#' @importFrom rlang .data
#' @importFrom utils head
select_change <- function(x, n = 10) {
  assert_that(
    inherits(x, "sim_rw"),
    has_name(x, "x"),
    has_name(x, "y"),
    has_name(x, "replicate"),
    is.count(n)
  )
  x %>%
    group_by(.data$replicate) %>%
    mutate(
      direction = sign(.data$y - lag(.data$y))
    ) %>%
    summarise(
      changes = sum(.data$direction != lag(.data$direction), na.rm = TRUE)
    ) %>%
    arrange(desc(.data$changes)) %>%
    head(n) %>%
    semi_join(x = x, by = "replicate") -> selection
  class(selection) <- c("sim_rw", class(selection))
  attr(selection, "sigma") <- attr(x, "sigma")
  return(selection)
}

#' select diverging simulations from an 'sim_rw' object
#'
#' The selection will contain the most extreme simulations base on either the minimum effect or the maximum effect within the simulation.
#' @inheritParams plot.sim_rw
#' @inheritParams select_poly
#' @family priors
#' @export
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr %>% group_by summarise_at semi_join
#' @importFrom rlang .data
#' @importFrom utils head
select_divergence <- function(x, n = 10) {
  assert_that(
    inherits(x, "sim_rw"),
    has_name(x, "x"),
    has_name(x, "y"),
    has_name(x, "replicate"),
    is.count(n)
  )
  x %>%
    group_by(.data$replicate) %>%
    summarise_at("y", c(min = min, max = max)) %>%
    arrange(pmin(rank(.data$min), rank(-.data$max))) %>%
    head(n) %>%
    semi_join(x = x, by = "replicate") -> selection
  class(selection) <- c("sim_rw", class(selection))
  attr(selection, "sigma") <- attr(x, "sigma")
  return(selection)
}
