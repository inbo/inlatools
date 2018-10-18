#' Plot the results from a distribution check
#' @param x a distribution_check object. Which is the output of
#' `\link{fast_distribution_check}`
#' @param y currently ignored
#' @param ... currently ignored
#' @return a ggplot2 object
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr %>% mutate filter
#' @importFrom ggplot2 ggplot aes_string geom_ribbon geom_line geom_hline
#' geom_text geom_blank scale_y_continuous
#' @importFrom graphics plot
#' @importFrom scales percent
#' @export
plot.distribution_check <- function(x, y, ...) {
  assert_that(
    inherits(x, "data.frame"),
    has_name(x, "x"),
    has_name(x, "ecdf"),
    has_name(x, "n"),
    has_name(x, "median"),
    has_name(x, "lcl"),
    has_name(x, "ucl")
  )
  x %>%
    filter(.data$lcl <= 0.999) %>%
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
    geom_text(aes_string(label = "n"), angle = 90, hjust = 1.5) +
    scale_y_continuous("observed / expected", labels = percent)
}
