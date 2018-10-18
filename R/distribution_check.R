#' Use simulations to compare the observed distribution with the modelled
#' distribution
#' @inheritParams get_observed
#' @inheritParams dispersion_check
#' @name distribution_check
#' @rdname distribution_check
#' @exportMethod distribution_check
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "distribution_check",
  def = function(object, nsim = 1000){
    standardGeneric("distribution_check") # nocov
  }
)

#' @rdname distribution_check
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.flag is.count
#' @importFrom INLA inla.posterior.sample inla.hyperpar.sample
#' @importFrom purrr map_dfc
#' @importFrom ggplot2 ggplot aes_string geom_ribbon geom_line geom_hline ylab
#' geom_text
#' @importFrom dplyr %>% count mutate_all group_by arrange mutate summarise
#' inner_join filter
#' @importFrom rlang .data
#' @importFrom tidyr gather complete
#' @importFrom stats quantile rpois rnbinom
#' @include s3_classes.R
setMethod(
  f = "distribution_check",
  signature = signature(object = "inla"),
  definition = function(object, nsim = 1000) {
    assert_that(is.count(nsim))

    if (length(object$.args$family) > 1) {
      stop("Only single responses are handled")
    }

    observed <- get_observed(object)
    samples <- inla.posterior.sample(n = nsim, result = object) #nolint
    relevant <- grep("^Predictor:", rownames(samples[[1]]$latent))
    eta <- map_dfc(samples, "latent")[relevant, ]
    if (object$.args$family == "poisson") {
      mutate_all(exp(eta), rpois, n = nrow(eta)) %>%
        gather("run", "x") %>%
        count(.data$run, .data$x) -> n_sampled
    } else if (object$.args$family == "nbinomial") {
      relevant <- grep(
        "size for the nbinomial observations",
        names(samples[[1]]$hyperpar)
      )
      size <- inla.hyperpar.sample(n = nsim, result = object)[, relevant] #nolint
      mutate_all(
        exp(eta),
        function(mu) {
          rnbinom(n = length(mu), mu = mu, size = size)
        }
      ) %>%
        gather("run", "x") %>%
        count(.data$run, .data$x) -> n_sampled
    } else if (object$.args$family == "gpoisson") {
      relevant <- grep(
        "Overdispersion",
        names(samples[[1]]$hyperpar)
      )
      phi <- inla.hyperpar.sample(n = nsim, result = object)[, relevant] #nolint
      mu <- exp(eta)
      sapply(
        seq_len(nsim),
        function(i) {
          rgpoisson(n = 1, mu = mu[, i, drop = TRUE], phi = phi[i])
        }
      ) -> n_sampled
      data.frame(
        run = rep(seq_len(nsim), each = nrow(mu)),
        x = as.vector(n_sampled)
      ) %>%
        count(.data$run, .data$x) -> n_sampled
    } else if (object$.args$family == "zeroinflatedpoisson1") {
      relevant <- grep("zero-inflated poisson_1", names(samples[[1]]$hyperpar))
      mutate_all(
        exp(eta),
        function(lambda, prob_zero) {
          n <- length(lambda)
          rbinom(n = n, size = 1, prob = 1 - prob_zero) *
            rpois(n = n, lambda = lambda)
        },
        prob_zero = inla.hyperpar.sample(n = nsim, result = object)[, relevant] #nolint
      ) %>%
        gather("run", "x") %>%
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
