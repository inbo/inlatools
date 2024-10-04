#' Convert the posterior marginal of a precision to a standard deviation
#' @param marg A matrix with columns "y" and "x" where "y" is the marginal of
#' the precision.
#' @return A `data.frame` with the mean, standard deviation and 2.5%, 25%, 50%,
#' 75% and 97.5% quantiles of the posterior of the standard deviation.
#' @examples
#' stopifnot(require(INLA))
#' model <- inla(Sepal.Length ~ Species, data = iris, family = "gaussian")
#' marg <- model$marginals.hyperpar[["Precision for the Gaussian observations"]]
#' prec2sd(marg)
#' @export
#' @importFrom assertthat assert_that is.number noNA
#' @importFrom INLA inla.tmarginal inla.zmarginal
prec2sd <- function(marg) {
  assert_that(
    inherits(marg, "matrix"), all(c("x", "y") %in% colnames(marg)), noNA(marg),
    inherits(marg[, "x"], "numeric"), inherits(marg[, "y"], "numeric"),
    all(marg[, "y"] >= 0)
  )
  tmarg <- try(inla.tmarginal(inv_sqrt, marg), silent = TRUE)
  while (inherits(tmarg, "try-error")) {
    marg <- head(marg, -1)
    tmarg <- try(inla.tmarginal(inv_sqrt, marg), silent = TRUE)
  }
  inla.zmarginal(tmarg, silent = TRUE) |>
    as.data.frame()
}

inv_sqrt <- function(x) {
  x ^ -0.5
}
