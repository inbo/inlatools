#' Use simulations to check for overdispersion or underdispersion
#' @inheritParams get_observed
#' @param nsim the number of simulation
#' @param plot display a plot of of the simulated values
#' @name dispersion_check
#' @rdname dispersion_check
#' @exportMethod dispersion_check
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "dispersion_check",
  def = function(object, nsim = 1000, plot = TRUE){
    standardGeneric("dispersion_check") # nocov
  }
)

#' @rdname dispersion_check
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.flag is.count
#' @importFrom INLA inla.posterior.sample
#' @importFrom purrr map_dfc
#' @importFrom ggplot2 ggplot aes_string geom_density geom_vline ggtitle
#' @include s3_classes.R
setMethod(
  f = "dispersion_check",
  signature = signature(object = "inla"),
  definition = function(object, nsim = 1000, plot = TRUE) {
    assert_that(is.count(nsim))
    assert_that(is.flag(plot))

    if (length(object$.args$family) > 1) {
      stop("Only single responses are handled")
    }

    observed <- get_observed(object)
    mu <- fitted(object)
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
    } else if (object$.args$family == "zeroinflatedpoisson1") {
      lambda <- exp(object$summary.linear.predictor[, "0.5quant"])
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

    if (!isTRUE(plot)) {
      return(invisible(list(data = dispersion_data, model = dispersion_model)))
    }

    p <- ggplot(
      data.frame(dispersion = dispersion_model),
      aes_string(x = "dispersion")
    ) +
      geom_density() +
      geom_vline(xintercept = dispersion_data, linetype = 2) +
      ggtitle(
        paste(
          "P(D|data > D|model) =",
          signif(mean(dispersion_data > dispersion_model), 3)
        )
      )
    print(p)
    return(
      invisible(
        list(data = dispersion_data, model = dispersion_model, plot = p)
      )
    )
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
