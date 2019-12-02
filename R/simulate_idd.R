#' simulate data from a second order random walk
#'
#' @param sigma the standard deviation of the random intercept
#' @param tau the precision of the random intercept
#' @inheritParams simulate_rw
#' @return a data.frame with simulated time series from the random walk
#' @export
#' @importFrom assertthat assert_that is.number is.count
#' @importFrom stats rnorm
#' @family priors
#' @examples
#' set.seed(20181202)
#' x <- simulate_iid(sigma = 0.25)
#' head(x)
simulate_iid <- function(sigma = NULL, tau = NULL, n_sim = 1e3) {
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
      is.null(tau),
      msg = "either 'sigma' or 'tau' must be NULL"
    )
  }
  assert_that(is.count(n_sim))

  simulated <- rnorm(n_sim, mean = 0, sd = sigma)
  class(simulated) <- c("sim_iid", class(simulated))
  attr(simulated, "sigma") <- sigma
  return(simulated)
}

#' Plot Simulated Random Intercepts
#' @param x A `sim_iid` object. Which is the output of `\link{simulate_iid}`.
#' @inheritParams plot.sim_rw
#' @param quantiles Which quantiles are shown on the plot.
#' @return A `\link[ggplot2]{ggplot}` object.
#' @family priors
#' @importFrom assertthat assert_that noNA
#' @importFrom dplyr tibble %>% mutate
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes_string geom_density geom_vline geom_line
#' geom_abline annotate scale_x_log10 scale_x_continuous scale_y_continuous labs
#' @importFrom scales percent
#' @importFrom tidyr crossing
#' @importFrom stats quantile plogis qlogis
#' @export
#' @examples
#' set.seed(20181202)
#' x <- simulate_iid(sigma = 0.25)
#' plot(x)
#' plot(x, link = "log")
#' plot(x, link = "logit")
plot.sim_iid <- function(
  x, y, ..., link = c("identity", "log", "logit"), baseline,
  center = c("mean", "bottom", "top"),
  quantiles = c(0.025, 0.1, 0.5, 0.9, 0.975)
) {
  assert_that(
    is.numeric(x),
    is.numeric(quantiles),
    length(quantiles) > 0,
    noNA(quantiles)
  )
  link <- match.arg(link)
  center <- match.arg(center)
  if (missing(baseline)) {
    baseline <- switch(
      link,
      identity = 0,
      log = 1,
      logit = seq(0, 0.5, length = 21)
    )
  } else {
    assert_that(
      is.numeric(baseline),
      length(baseline) >= 1
    )
  }

  quantiles <- sort(quantiles, decreasing = TRUE)
  quant <- quantile(x, quantiles)

  x_c <- switch(
    center,
    mean = x - mean(x),
    bottom = x - min(quant),
    top = x - max(quant)
  )
  quant_c <- switch(
    center,
    mean = quant - mean(x),
    bottom = quant - min(quant),
    top = quant - max(quant)
  )
  z <- crossing(re = x_c, bl = baseline)
  z <- switch(
    link,
    identity = mutate(z, y = .data$re + .data$bl),
    log = mutate(z, y = exp(.data$re + log(.data$bl))),
    logit = mutate(z, y = plogis(.data$re + qlogis(.data$bl)))
  )
  if (length(baseline) == 1) {
    quant_c <- switch(
      link,
      identity = quant_c + baseline,
      log = exp(quant_c + log(baseline)),
      logit = plogis(quant_c + qlogis(baseline))
    )
    p <- ggplot(z, aes_string(x = "y")) +
      geom_density() +
      geom_vline(xintercept = quant_c, linetype = 3) +
      annotate(
        "text",
        x = quant_c,
        y = Inf,
        label = names(quant),
        vjust = -0.5,
        hjust = 1.5,
        angle = 90
      ) +
      geom_vline(xintercept = baseline, colour = "red", linetype = 2) +
      labs(
        title = bquote(
          sigma == .(signif(attr(x, "sigma"), 4)) ~
            sigma ^ 2 == .(signif(attr(x, "sigma") ^ 2, 4)) ~
            tau == .(signif(attr(x, "sigma") ^ -2, 4))
        )
      )
    p <- switch(
      link,
      identity = p + scale_x_continuous("effect"),
      log = if (isTRUE(all.equal(baseline, 1))) {
        p + scale_x_continuous("relative effect", labels = percent)
      } else {
        p + scale_x_continuous("effect")
      },
      logit = p + scale_x_continuous("proportion", labels = percent)
    )
    return(p)
  }

  alpha <- sqrt(10) / sqrt(pmax(10, length(x)))
  tibble(
    re = quant_c,
    quantile = factor(
      names(quant),
      levels = names(quant)
    )
  ) %>%
    crossing(bl = baseline) -> quant_bl
  quant_bl <- switch(
    link,
    identity = mutate(quant_bl, y = .data$re + .data$bl),
    log = mutate(quant_bl, y = exp(.data$re + log(.data$bl))),
    logit = mutate(quant_bl, y = plogis(.data$re + qlogis(.data$bl)))
  )
  p <- ggplot(z, aes_string(x = "bl", y = "y", group = "re")) +
    geom_line(alpha = alpha) +
    geom_line(data = quant_bl, aes_string(colour = "quantile"), size = 1) +
    geom_abline(linetype = 2, colour = "red") +
    labs(
      title = bquote(
        sigma == .(signif(attr(x, "sigma"), 4)) ~
          sigma ^ 2 == .(signif(attr(x, "sigma") ^ 2, 4)) ~
          tau == .(signif(attr(x, "sigma") ^ -2, 4))
      )
    )

  switch(
    link,
    identity = p +
      scale_x_continuous("baseline") +
      scale_y_continuous("effect"),
    log = p +
      scale_x_continuous("baseline") +
      scale_y_continuous("relative effect"),
    logit = p +
      scale_x_continuous("baseline", labels = percent) +
      scale_y_continuous("proportion", labels = percent)
    )
}
