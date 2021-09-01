#' Use simulations to compare the observed distribution with the modelled
#' distribution
#' @inheritParams get_observed
#' @inheritParams dispersion_check
#' @inheritParams INLA::inla.posterior.sample
#' @name distribution_check
#' @rdname distribution_check
#' @exportMethod distribution_check
#' @docType methods
#' @importFrom methods setGeneric
#' @family checks
setGeneric(
  name = "distribution_check",
  def = function(object, nsim = 1000, seed = 0L) {
    standardGeneric("distribution_check") # nocov
  }
)

#' @rdname distribution_check
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.flag is.count
#' @importFrom purrr map_dfc
#' @importFrom ggplot2 ggplot aes_string geom_ribbon geom_line geom_hline ylab
#' geom_text
#' @importFrom dplyr %>% count mutate_all group_by arrange mutate summarise
#' inner_join filter
#' @importFrom rlang .data
#' @importFrom tidyr gather complete
#' @importFrom stats quantile rpois rnbinom
#' @examples
#' \donttest{
#' library(INLA)
#' model <- inla(
#'   poisson ~ 1,
#'   family = "poisson",
#'   data = data.frame(
#'     poisson = rpois(20, lambda = 10),
#'     base = 1
#'   ),
#'   control.predictor = list(compute = TRUE),
#'   control.compute = list(config = TRUE)
#' )
#' distribution_check(model, seed = 20181202)
#' }
setMethod(
  f = "distribution_check",
  signature = signature(object = "inla"),
  definition = function(object, nsim = 1000, seed = 0L) {
    assert_that(requireNamespace("INLA", quietly = TRUE))
    assert_that(is.count(nsim))

    if (length(object$.args$family) > 1) {
      stop("Only single responses are handled")
    }

    observed <- get_observed(object)
    samples <- INLA::inla.posterior.sample(
      n = nsim, result = object, seed = seed
    )
    relevant <- grep("^Predictor:", rownames(samples[[1]]$latent))
    eta <- map_dfc(samples, "latent")[relevant, , drop = FALSE]
    n_sampled <- switch(
      object$.args$family,
      poisson = {
        mutate_all(exp(eta), rpois, n = nrow(eta)) %>%
          gather("run", "x") %>%
          count(.data$run, .data$x)
      },
      nbinomial = {
        relevant <- grep(
          "size for the nbinomial observations",
          names(samples[[1]]$hyperpar)
        )
        size <- INLA::inla.hyperpar.sample(n = nsim, result = object)[, relevant] #nolint
        mutate_all(
          exp(eta),
          function(mu) {
            rnbinom(n = length(mu), mu = mu, size = size)
          }
        ) %>%
          gather("run", "x") %>%
          count(.data$run, .data$x)
      },
      gpoisson = {
        relevant <- grep(
          "Overdispersion",
          names(samples[[1]]$hyperpar)
        )
        phi <- INLA::inla.hyperpar.sample(n = nsim, result = object)[
          , relevant, drop = TRUE
        ] #nolint
        mu <- exp(eta)
        sapply(
          seq_len(nsim),
          function(i) {
            rgpoisson(n = 1, mu = as.vector(mu[, i, drop = TRUE]), phi = phi[i])
          }
        ) -> n_sampled
        data.frame(
          run = rep(seq_len(nsim), each = nrow(mu)),
          x = as.vector(n_sampled)
        ) %>%
          count(.data$run, .data$x)
      },
      zeroinflatedpoisson1 = {
        relevant <- grep(
          "zero-inflated poisson_1",
          names(samples[[1]]$hyperpar)
        )
        mutate_all(
          exp(eta),
          function(lambda, prob_zero) {
            n <- length(lambda)
            rbinom(n = n, size = 1, prob = 1 - prob_zero) *
              rpois(n = n, lambda = lambda)
          },
          prob_zero = INLA::inla.hyperpar.sample(
            n = nsim, result = object
          )[, relevant] #nolint
        ) %>%
          gather("run", "x") %>%
          count(.data$run, .data$x)
      },
      stop(object$.args$family, " is not yet handled")
    )

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
