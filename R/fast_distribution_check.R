#' Use simulations to compare the observed distribution with the modelled distribution
#'
#' This check uses the fitted values and thus ignores the uncertainty on the predictions
#' @inheritParams get_observed
#' @inheritParams dispersion_check
#' @name fast_distribution_check
#' @rdname fast_distribution_check
#' @exportMethod fast_distribution_check
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "fast_distribution_check",
  def = function(object, nsim = 1000){
    standardGeneric("fast_distribution_check") # nocov
  }
)

#' @rdname fast_distribution_check
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.count
#' @importFrom INLA inla.posterior.sample
#' @importFrom purrr map_dfc
#' @importFrom dplyr %>% count mutate_all group_by arrange mutate summarise inner_join
#' @importFrom rlang .data
#' @importFrom tidyr gather complete
#' @importFrom stats quantile rpois rnbinom
#' @include s3_classes.R
setMethod(
  f = "fast_distribution_check",
  signature = signature(object = "inla"),
  definition = function(object, nsim = 1000) {
    assert_that(is.count(nsim))

    if (length(object$.args$family) > 1) {
      stop("Only single responses are handled")
    }

    observed <- get_observed(object)
    mu <- fitted(object)
    n_mu <- length(mu)
    if (object$.args$family == "poisson") {
      data.frame(
        run = rep(seq_len(nsim), each = n_mu),
        x = rpois(n = n_mu * nsim, lambda = mu)
      ) %>%
        count(.data$run, .data$x) -> n_sampled
    } else if (object$.args$family == "nbinomial") {
      relevant <- grep("overdispersion", rownames(object$summary.hyperpar))
      size <- object$summary.hyperpar[relevant, "mean"]
      data.frame(
        run = rep(seq_len(nsim), each = n_mu),
        x = rnbinom(n = n_mu * nsim, mu = mu, size = size)
      ) %>%
        count(.data$run, .data$x) -> n_sampled
    } else if (object$.args$family == "zeroinflatedpoisson1") {
      relevant <- grep("zero-probability", rownames(object$summary.hyperpar))
      zero <- object$summary.hyperpar[relevant, "mean"]
      data.frame(
        run = rep(seq_len(nsim), each = n_mu),
        x = rpois(n = n_mu * nsim, lambda = mu) *
          rbinom(n = n_mu * nsim, size = 1, prob = 1 - zero)
      ) %>%
        count(.data$run, .data$x) -> n_sampled
    } else {
      stop(object$.args$family, " is not yet handled")
    }

    data.frame(x = observed) %>%
      count(.data$x) -> n_observed
    n_count <- unique(c(n_observed$x, n_sampled$x))
    n_sampled %>%
      complete(run = .data$run, x = n_count, fill = list(n = 0)) %>%
      group_by(.data$run) %>%
      arrange(.data$x) %>%
      mutate(ecdf = cumsum(.data$n) / sum(.data$n)) %>%
      group_by(.data$x) %>%
      summarise(
        median = quantile(.data$ecdf, probs = 0.5),
        lcl = quantile(.data$ecdf, probs = 0.025),
        ucl = quantile(.data$ecdf, probs = 0.975)
      ) %>%
      inner_join(
        n_observed %>%
          complete(x = n_count, fill = list(n = 0)) %>%
          arrange(.data$x) %>%
          mutate(ecdf = cumsum(.data$n) / sum(.data$n))
        ,
        by = "x"
      ) -> ecdf
    class(ecdf) <- c("distribution_check", class(ecdf))
    return(ecdf)
  }
)
