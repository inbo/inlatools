#' Use simulations to test for overdispersion or underdispersion
#' @inheritParams fitted
#' @param nsim the number of simulation
#' @param plot display a plot of of the simulated values
#' @export
#' @importFrom assertthat assert_that is.flag is.count
#' @importFrom INLA inla.posterior.sample
#' @importFrom purrr map_dfc
#' @importFrom ggplot2 ggplot aes geom_density geom_vline ggtitle
test_dispersion <- function(object, nsim = 1000, plot = TRUE) {
  assert_that(inherits(object, "inla"))
  assert_that(is.count(nsim))
  assert_that(is.flag(plot))

  if (length(object$.args$family) > 1) {
    stop("Only single responses are handled")
  }

  observed <- get_observed(object)
  dispersion_data <- dispersion(
    fitted = fitted(object),
    observed = observed,
    family = object$.args$family
  )
  samples <- inla.posterior.sample(n = nsim, result = object)
  relevant <- grep("^Predictor:", rownames(samples[[1]]$latent))
  samples <- map_dfc(samples, "latent")[relevant, ]
  if (object$.args$family == "poisson") {
    samples <- exp(samples)
  } else {
    stop(object$.args$family, " is not yet handled")
  }
  dispersion_model <- apply(
    samples,
    2,
    dispersion,
    observed = observed,
    family = object$.args$family
  )

  if (!isTRUE(plot)) {
    return(invisible(list(data = dispersion_data, model = dispersion_model)))
  }
  p <- ggplot(data.frame(dispersion = dispersion_model), aes(x = dispersion)) +
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
    invisible(list(data = dispersion_data, model = dispersion_model, plot = p))
  )
}

dispersion <- function(fitted, observed, family = "poisson") {
  family = match.arg(family)
  sum((observed - fitted) / sqrt(fitted))
}
