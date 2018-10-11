#' Use simulations to compare the observed distribution with the modelled distribution
#' @inheritParams get_observed
#' @inheritParams test_dispersion
#' @name test_distribution
#' @rdname test_distribution
#' @exportMethod test_distribution
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "test_distribution",
  def = function(object, nsim = 1000, plot = TRUE){
    standardGeneric("test_distribution") # nocov
  }
)

#' @rdname test_distribution
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.flag is.count
#' @importFrom INLA inla.posterior.sample
#' @importFrom purrr map_dfc
#' @importFrom ggplot2 ggplot aes_string geom_ribbon geom_line geom_hline ylab
#' @importFrom dplyr %>% count mutate_all group_by arrange mutate summarise inner_join
#' @importFrom rlang .data
#' @importFrom tidyr gather complete
#' @importFrom stats quantile rpois
#' @include s3_classes.R
setMethod(
  f = "test_distribution",
  signature = signature(object = "inla"),
  definition = function(object, nsim = 1000, plot = TRUE) {
    assert_that(is.count(nsim))
    assert_that(is.flag(plot))

    if (length(object$.args$family) > 1) {
      stop("Only single responses are handled")
    }

    observed <- get_observed(object)
    samples <- inla.posterior.sample(n = nsim, result = object) #nolint
    relevant <- grep("^Predictor:", rownames(samples[[1]]$latent))
    eta <- map_dfc(samples, "latent")[relevant, ]
    if (object$.args$family == "poisson") {
      data.frame(x = observed) %>%
        count(.data$x) -> n_observed
      mutate_all(exp(eta), rpois, n = nrow(eta)) %>%
        gather("run", "x") %>%
        count(.data$run, .data$x) -> n_sampled
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
          ucl = quantile(.data$ecdf, probs = 0.975),
        ) %>%
        inner_join(
          n_observed %>%
            complete(x = n_count, fill = list(n = 0)) %>%
            arrange(.data$x) %>%
            mutate(ecdf = cumsum(.data$n) / sum(.data$n))
          ,
          by = "x"
        ) -> ecdf
    } else {
      stop(object$.args$family, " is not yet handled")
    }

    if (!isTRUE(plot)) {
      return(invisible(list(ecdf = ecdf)))
    }

    p <- ecdf %>%
      mutate(
        median = .data$ecdf / .data$median,
        lcl = .data$ecdf / .data$lcl,
        ucl = .data$ecdf / .data$ucl
      ) %>%
      ggplot(aes_string(x = "x", y = "median", ymin = "lcl", ymax = "ucl")) +
      geom_hline(yintercept = 1, linetype = 2) +
      geom_ribbon(alpha = 0.1) +
      geom_line() +
      ylab("observed / expected")
    print(p)

    return(
      invisible(list(ecdf = ecdf, plot = p))
    )
  }
)
