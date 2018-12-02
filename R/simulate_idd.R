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
  attr(simulated, "tau") <- tau
  return(simulated)
}

#' Plot simulated random intercepts
#' @param x a `sim_iid` object. Which is the output of `\link{simulate_iid}`
#' @inheritParams plot.sim_rw
#' @return a ggplot2 object
#' @family priors
#' @importFrom assertthat assert_that
#' @importFrom dplyr tibble %>% mutate
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes_string geom_density geom_vline geom_line geom_abline annotate scale_x_log10 scale_x_continuous scale_y_continuous
#' @importFrom scales percent
#' @importFrom tidyr crossing
#' @export
#' @examples
#' set.seed(20181202)
#' x <- simulate_iid(sigma = 0.25)
#' plot(x)
#' plot(x, link = "log")
#' plot(x, link = "logit")
plot.sim_iid <- function(x, y, ..., link = c("identity", "log", "logit")) {
  assert_that(is.numeric(x))
  quants <- c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)
  quant <- quantile(x, quants)
  link <- match.arg(link)
  title <- ggtitle(
    bquote(
      sigma == .(signif(attr(x, "sigma"), 4)) ~
        sigma ^ 2 == .(signif(attr(x, "sigma") ^ 2, 4)) ~
        tau == .(signif(attr(x, "sigma") ^ -2, 4))
    )
  )

  switch(
    link,
    identity = {
      tibble(effect = x) %>%
        ggplot(aes_string(x = "effect")) +
        geom_density() +
        geom_vline(xintercept = quant, linetype = 3) +
        geom_vline(xintercept = 0, linetype = 2, colour = "red") +
        annotate(
          "text",
          x = quant,
          y = Inf,
          label = sprintf("%.1f%%", 100 * quants),
          vjust = -0.5,
          hjust = 1.5,
          angle = 90
        ) +
        title
    },
    log = {
      tibble(effect = exp(x)) %>%
        ggplot(aes_string(x = "effect")) +
        geom_density() +
        geom_vline(xintercept = exp(quant), linetype = 3) +
        geom_vline(xintercept = 1, linetype = 2, colour = "red") +
        scale_x_log10("relative effect", labels = percent) +
        annotate(
          "text",
          x = exp(quant),
          y = Inf,
          label = sprintf("%.1f%%", 100 * quants),
          vjust = -0.5,
          hjust = 1.5,
          angle = 90
        ) +
        title
    },
    logit = {
      x_range <- seq(0, 1, length = 41)
      tibble(
        quantile = factor(
          rev(quants),
          labels = sprintf("%.1f%%", 100 * rev(quants))
        ),
        replicate = quant
      ) %>%
        crossing(x = x_range) %>%
        mutate(
          y = .data$replicate + qlogis(.data$x),
          y = plogis(.data$y)
      ) -> quantiles
      tibble(replicate = x) %>%
        crossing(x = x_range) %>%
        mutate(
          y = .data$replicate + qlogis(.data$x),
          y = plogis(.data$y)
        ) %>%
        ggplot(aes_string(x = "x", y = "y", group = "replicate")) +
        geom_line(alpha = 0.1) +
        geom_abline(linetype = 2, colour = "red") +
        geom_line(data = quantiles, aes_string(colour = "quantile"), size = 1) +
        scale_x_continuous("base proportion", labels = percent) +
        scale_y_continuous("proportion", labels = percent) +
        title
    }
  )
}
