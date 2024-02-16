#' Use simulations to check for overdispersion or underdispersion
#' @inheritParams get_observed
#' @param nsim the number of simulation
#' @name dispersion_check
#' @rdname dispersion_check
#' @exportMethod dispersion_check
#' @docType methods
#' @importFrom methods setGeneric
#' @family checks
setGeneric(
  name = "dispersion_check",
  def = function(object, nsim = 1000) {
    standardGeneric("dispersion_check") # nocov
  }
)

#' @rdname dispersion_check
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.flag is.count
#' @importFrom purrr map_dfc
#' @importClassesFrom INLA inla
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
#' str(dc)
setMethod(
  f = "dispersion_check",
  signature = signature(object = "inla"),
  definition = function(object, nsim = 1000) {
    assert_that(is.count(nsim))

    stopifnot(
      "Only single responses are handled" = length(object$.args$family) == 1
    )

    observed <- get_observed(object)
    which_na <- is.na(observed)
    observed <- observed[!which_na]
    size <- switch(
      object$.args$family,
      nbinomial = "size for the nbinomial observations (1/overdispersion)",
      zeroinflatednbinomial0 =
        "size for nbinomial_0 zero-inflated observations",
      zeroinflatednbinomial1 =
        "size for nbinomial_1 zero-inflated observations",
      integer(0)
    )
    size <- object$summary.hyperpar[size, "0.5quant"]
    zero_prob <- switch(
      object$.args$family,
      zeroinflatednbinomial0 =
        "zero-probability parameter for zero-inflated nbinomial_0",
      zeroinflatednbinomial1 =
        "zero-probability parameter for zero-inflated nbinomial_1",
      zeroinflatedpoisson0 =
        "zero-probability parameter for zero-inflated poisson_0",
      zeroinflatedpoisson1 =
        "zero-probability parameter for zero-inflated poisson_1",
      integer(0)
    )
    zero_prob <- object$summary.hyperpar[zero_prob, "0.5quant"]
    eta <- object$summary.linear.predictor[!which_na, "0.5quant"]
    mu <- switch(
      object$.args$family,
      poisson = exp(eta),
      nbinomial = exp(eta),
      zeroinflatednbinomial0 = (1 - zero_prob) * exp(eta) /
        (1 - (size / (exp(eta) + size)) ^ size),
      zeroinflatednbinomial1 = (1 - zero_prob) * exp(eta),
      zeroinflatedpoisson0 = (1 - zero_prob) * exp(exp(eta)) * exp(eta) /
        (exp(exp(eta)) - 1),
      zeroinflatedpoisson1 = (1 - zero_prob) * exp(eta),
      stop(object$.args$family, " is not yet handled")
    )
    variance <- switch(
      object$.args$family,
      poisson = mu,
      nbinomial = mu + mu ^ 2 / size,
      zeroinflatednbinomial0 =
        (
          (1 - zero_prob) *
            exp(eta) *
            ((size / (size + exp(eta))) ^ size) *
            (exp(eta) + exp(eta) * size + size)
        ) /
        (
          size *
            (1 - (size / (size + exp(eta))) ^ size) *
            (1 - (exp(eta) / (size + exp(eta))) ^ size)
        ),
      zeroinflatednbinomial1 = mu * (1 + exp(eta) * (zero_prob + 1 / size)),
      zeroinflatedpoisson0 = mu * (exp(eta) + 1 - mu),
      zeroinflatedpoisson1 = mu * (exp(eta) + 1 - mu)
    )
    dispersion_data <- dispersion(
      observed = observed, fitted = mu, variance = variance
    )
    switch(
      object$.args$family,
      poisson = rpois(nsim * length(mu), lambda = exp(eta)),
      nbinomial = rnbinom(nsim * length(mu), mu = exp(eta), size = size),
      zeroinflatednbinomial0 = rzanbinom(
        n = nsim * length(mu), mu = exp(eta), size = size, prob = zero_prob
      ),
      zeroinflatednbinomial1 = rzinbinom(
        n = nsim * length(mu), mu = exp(eta), size = size, prob = zero_prob
      ),
      zeroinflatedpoisson0 = rzapois(
        n = nsim * length(mu), lambda = exp(eta), prob = zero_prob
      ),
      zeroinflatedpoisson1 = rzipois(
        n = nsim * length(mu), lambda = exp(eta), prob = zero_prob
      )
    ) |>
      matrix(ncol = nsim) |>
      apply(2, dispersion, fitted = mu, variance = variance) -> dispersion_model
    output <- list(data = dispersion_data, model = dispersion_model)
    class(output) <- "dispersion_check"
    return(output)
  }
)

#' Calculate a measure for dispersion
#'
#' The measure is calculated as the average of the squared Pearson residuals
#' @param observed the observed values
#' @param fitted the fitted values
#' @param variance the variance of the fitted values
#' @export
#' @family statistics
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
#' dispersion(
#'   observed = get_observed(model),
#'   fitted = fitted(model),
#'   variance = fitted(model)
#' )
dispersion <- function(observed, fitted, variance) {
  mean((observed - fitted) ^ 2 / variance) # nolint
}
