#' Convert the posterior marginal of a precision to a standard deviation
#' @param marg A matrix with columns "y" and "x" where "y" is the marginal of
#' the precision.
#' @param threshold The threshold for the cumulative sum of the marginal to
#' determine the range of the marginal to use.
#' The function ignore the right side of the marginal where the cumulative sum
#' is less than the threshold.
#' This avoids numerical issues with the transformation.
#' Defaults to `5e-4`.
#' @return A `data.frame` with the mean, standard deviation and 2.5%, 25%, 50%,
#' 75% and 97.5% quantiles of the posterior of the standard deviation.
#' @examples
#' stopifnot(require(INLA))
#' model <- inla(Sepal.Length ~ Species, data = iris, family = "gaussian")
#' marg <- model$marginals.hyperpar[["Precision for the Gaussian observations"]]
#' @export
#' @importFrom assertthat assert_that is.number noNA
#' @importFrom INLA inla.tmarginal inla.zmarginal
prec2sd <- function(marg, threshold = 5e-4) {
  assert_that(
    inherits(marg, "matrix"), all(c("x", "y") %in% colnames(marg)), noNA(marg),
    inherits(marg[, "x"], "numeric"), inherits(marg[, "y"], "numeric"),
    all(marg[, "y"] >= 0),
    is.number(threshold), noNA(threshold), threshold > 0, threshold < 0.1
  )
  marg <- marg[rev(cumsum(rev(marg[, "y"])) >= threshold), ]
  inla.tmarginal(
    function(x) {
      x ^ -0.5
    },
    marg
  ) |>
    inla.zmarginal(silent = TRUE) |>
    as.data.frame()
}
