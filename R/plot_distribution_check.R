#' Plot the results from a distribution check
#' @param x a distribution_check object. Which is the output of
#' `\link{fast_distribution_check}`
#' @param y currently ignored
#' @param ... currently ignored
#' @param n display the number of observations
#' @inheritParams ggplot2::facet_wrap
#' @return a ggplot2 object
#' @importFrom assertthat assert_that has_name is.flag is.string
#' @importFrom dplyr %>% mutate filter
#' @importFrom ggplot2 ggplot aes_string geom_ribbon geom_line geom_hline
#' geom_text geom_blank scale_y_continuous
#' @importFrom graphics plot
#' @importFrom scales percent
#' @export
#' @family utils
#' @examples
#' library(INLA)
#' set.seed(20181202)
#' model <- inla(
#'   poisson ~ 1,
#'   family = "poisson",
#'   data = data.frame(
#'     poisson = rpois(20, lambda = 10),
#'     base = 1
#'   ),
#'   control.predictor = list(compute = TRUE)
#' )
#' fdc <- fast_distribution_check(model)
#' plot(fdc)
plot.distribution_check <- function(x, y, ..., n = FALSE, scales = "fixed") {
  assert_that(
    inherits(x, "data.frame"),
    has_name(x, "x"),
    has_name(x, "ecdf"),
    has_name(x, "n"),
    has_name(x, "median"),
    has_name(x, "lcl"),
    has_name(x, "ucl"),
    assert_that(is.flag(n)),
    assert_that(is.string(scales))
  )
  p <- x %>%
    filter(.data$lcl <= 0.999, .data$ucl < 1) %>%
    mutate(
      median = .data$ecdf / .data$median,
      lcl = .data$ecdf / .data$lcl,
      ucl = .data$ecdf / .data$ucl
    ) %>%
    ggplot(aes_string(x = "x", y = "median")) +
    geom_blank(data = data.frame(x = min(x$x), median = c(0.95, 1.05))) +
    geom_hline(yintercept = 1, linetype = 2) +
    geom_line(aes_string(y = "lcl"), linetype = 3, alpha = 0.5) +
    geom_line(aes_string(y = "ucl"), linetype = 3, alpha = 0.5) +
    geom_ribbon(alpha = 0.1, aes_string(ymin = "lcl", ymax = "ucl")) +
    geom_line() +
    scale_y_continuous("observed / expected", labels = percent)
  if (isTRUE(n)) {
    p <- p + geom_text(aes_string(label = "n"), angle = 90, hjust = 1.5)
  }
  if (!has_name(x, "model")) {
    return(p)
  }
  p + facet_wrap(~model, scales = scales)
}

#' Plot the results from a dispersion check
#' @param x a dispersion_check object. Which is the output of
#' `\link{dispersion_check}`
#' @param y currently ignored
#' @param ... currently ignored
#' @return a ggplot2 object
#' @importFrom assertthat assert_that has_name is.number
#' @importFrom ggplot2 ggplot aes_string geom_density geom_vline ggtitle
#' @importFrom graphics plot
#' @importFrom scales percent
#' @export
#' @family utils
#' @examples
#' library(INLA)
#' set.seed(20181202)
#' model <- inla(
#'   poisson ~ 1,
#'   family = "poisson",
#'   data = data.frame(
#'     poisson = rpois(20, lambda = 10),
#'     base = 1
#'   ),
#'   control.predictor = list(compute = TRUE)
#' )
#' dc <- dispersion_check(model)
#' plot(dc)
plot.dispersion_check <- function(x, y, ...) {
  assert_that(
    has_name(x, "data"),
    has_name(x, "model"),
    is.number(x$data),
    is.numeric(x$model)
  )
  ggplot(
      data.frame(dispersion = x$model),
      aes_string(x = "dispersion")
    ) +
      geom_density() +
      geom_vline(xintercept = x$data, linetype = 2) +
      ggtitle(
        paste(
          "P(D|data > D|model) =",
          signif(mean(x$data > x$model), 3)
        )
      )
}
