#' Use simulations to check for overdispersion or underdispersion
#' @inheritParams get_observed
#' @param nsim the number of simulation
#' @name dispersion_check
#' @rdname dispersion_check
#' @exportMethod dispersion_check
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "dispersion_check",
  def = function(object, nsim = 1000){
    standardGeneric("dispersion_check") # nocov
  }
)

#' @rdname dispersion_check
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.flag is.count
#' @importFrom INLA inla.posterior.sample
#' @importFrom purrr map_dfc
#' @include s3_classes.R
setMethod(
  f = "dispersion_check",
  signature = signature(object = "inla"),
  definition = function(object, nsim = 1000) {
    assert_that(is.count(nsim))

    if (length(object$.args$family) > 1) {
      stop("Only single responses are handled")
    }

    observed <- get_observed(object)
    mu <- fitted(object)
    which_na <- is.na(observed)
    mu <- mu[!which_na]
    observed <- observed[!which_na]
    if (object$.args$family == "poisson") {
      dispersion_data <- dispersion(
        observed = observed,
        fitted = mu,
        variance = mu
      )
      dispersion_model <- apply(
        matrix(
          rpois(nsim * length(mu), lambda = mu),
          ncol = nsim
        ),
        2,
        function(x) {
          dispersion(observed = x, fitted = mu, variance = mu)
        }
      )
    } else if (object$.args$family == "nbinomial") {
      size <- object$summary.hyperpar[
        "size for the nbinomial observations (1/overdispersion)",
        "mean"
      ]
      dispersion_data <- dispersion(
        observed = observed,
        fitted = mu,
        variance = mu + mu ^ 2 / size
      )
      dispersion_model <- apply(
        matrix(
          rnbinom(nsim * length(mu), mu = mu, size = size),
          ncol = nsim
        ),
        2,
        function(x) {
          dispersion(observed = x, fitted = mu, variance = mu + mu ^ 2 / size)
        }
      )
    } else if (object$.args$family == "zeroinflatednbinomial1") {
      mu <- exp(object$summary.linear.predictor[, "0.5quant"])
      size <- object$summary.hyperpar[
        "size for nbinomial zero-inflated observations",
        "0.5quant"
      ]
      zi <- object$summary.hyperpar[
        "zero-probability parameter for zero-inflated nbinomial_1",
        "0.5quant"
      ]
      zi_mu <- (1 - zi) * mu
      zi_var <- (1 - zi) * mu * (1 + mu * (zi + 1 / size))
      dispersion_data <- dispersion(
        observed = observed,
        fitted = zi_mu,
        variance = zi_var
      )
      dispersion_model <- apply(
        matrix(
          rbinom(nsim * length(mu), size = 1, prob = 1 - zi) *
            rnbinom(nsim * length(mu), mu = mu, size = size),
          ncol = nsim
        ),
        2,
        function(x) {
          dispersion(observed = x, fitted = zi_mu, variance = zi_var)
        }
      )
    } else if (object$.args$family == "zeroinflatedpoisson1") {
      lambda <- exp(object$summary.linear.predictor[!which_na, "0.5quant"])
      zi <- object$summary.hyperpar[1, "0.5quant"]
      zi_mu  <- (1 - zi) * lambda
      zi_var <- (1 - zi) * (lambda ^ 2 + lambda) - zi_mu ^ 2
      dispersion_data <- dispersion(
        observed = observed,
        fitted = zi_mu,
        variance = zi_var
      )
      dispersion_model <- apply(
        matrix(
          rbinom(nsim * length(lambda), size = 1, prob = 1 - zi) *
            rpois(nsim * length(lambda), lambda = lambda),
          ncol = nsim
        ),
        2,
        function(x) {
          dispersion(observed = x, fitted = zi_mu, variance = zi_var)
        }
      )
    } else {
      stop(object$.args$family, " is not yet handled")
    }

    output <- list(data = dispersion_data, model = dispersion_model)
    class(output) <- "dispersion_check"
    return(output)
  }
)

#' Calculate a measure for dispersion
#' The measure is calculated as the average of the squared Pearson residuals
#' @param observed the observed values
#' @param fitted the fitted values
#' @param variance the variance of the fitted values
#' @export
dispersion <- function(observed, fitted, variance) {
  mean((observed - fitted) ^ 2 / variance)
}
