#' Fast aggregation check
#' Compare the observed and modelled aggregated values.
#' @inheritParams get_observed
#' @inheritParams dispersion_check
#' @param grouping_vars character vector of variable names to group by.
#' @param fun function to apply to the aggregated values.
#' @name fast_aggregation_check
#' @rdname fast_aggregation_check
#' @exportMethod fast_aggregation_check
#' @docType methods
#' @importFrom methods setGeneric
#' @family checks
setGeneric(
  name = "fast_aggregation_check",
  def = function(object, grouping_vars, fun = sum, nsim = 1000) {
    standardGeneric("fast_aggregation_check") # nocov
  }
)

#' @rdname fast_distribution_check
#' @importFrom assertthat assert_that is.count
#' @importFrom methods new setMethod
setMethod(
  f = "fast_aggregation_check",
  signature = signature(object = "inla"),
  definition = function(object, grouping_vars, fun = sum, nsim = 1000) {
    stopifnot(
      "Only single responses are handled" = length(object$.args$family) == 1
    )
    assert_that(
      is.count(nsim), is.character(grouping_vars), noNA(grouping_vars),
      length(grouping_vars) > 0, is.function(fun),
      all(grouping_vars %in% names(object$.args$data))
    )

    observed <- get_observed(object)
    which_na <- is.na(observed)
    observed <- observed[!which_na]

    size <- switch(
      object$.args$family,
      gpoisson = "Overdispersion for gpoisson",
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
    if (is.na(zero_prob) && grepl("^zeroinflated.*0", object$.args$family)) {
      object$all.hyper$family[[1]]$hyper$theta$initial |>
        object$all.hyper$family[[1]]$hyper$theta$from.theta() -> zero_prob
    }

    if (is.null(object$model.spde2.blc)) {
      eta <- object$summary.linear.predictor[!which_na, "0.5quant"]
      covars <- object$.args$data[!which_na, grouping_vars, drop = FALSE]
    } else {
      object$summary.linear.predictor[
        grep("^APredictor", rownames(object$summary.linear.predictor)),
        "0.5quant"
      ] -> eta
      eta <- eta[!which_na]
      object$.args$data[grouping_vars] |>
        as.data.frame() -> covars
      spde <- object$.args$data[[grep("\\.group", names(object$.args$data))]]
      covars <- covars[is.na(spde), , drop = FALSE]
      covars <- covars[!which_na, , drop = FALSE]
    }
    to_rename <- grouping_vars %in% c("x", "type", "run")
    colnames(covars)[to_rename] <- paste0("covar_", colnames(covars)[to_rename])
    grouping_vars[to_rename] <- paste0("covar_", grouping_vars[to_rename])

    n_mu <- length(eta)
    x <- switch(
      object$.args$family,
      gpoisson = rgpoisson(n = n_mu * nsim, mu = exp(eta), phi = size) |>
        as.vector(),
      nbinomial = rnbinom(n = n_mu * nsim, mu = exp(eta), size = size),
      poisson = rpois(n_mu * nsim, lambda = exp(eta)),
      zeroinflatednbinomial0 = rzanbinom(
        n = n_mu * nsim, mu = exp(eta), size = size, prob = zero_prob
      ),
      zeroinflatednbinomial1 = rzinbinom(
        n = n_mu * nsim, mu = exp(eta), size = size, prob = zero_prob
      ),
      zeroinflatedpoisson0 = rzapois(
        n = n_mu * nsim, lambda = exp(eta), prob = zero_prob
      ),
      zeroinflatedpoisson1 = rzipois(
        n = n_mu * nsim, lambda = exp(eta), prob = zero_prob
      ),
      stop(object$.args$family, " is not yet handled")
    )

    data.frame(
      id = rep(seq_len(n_mu), nsim),
      run = rep(seq_len(nsim), each = n_mu),
      x = x
    ) |>
      inner_join(
        covars |>
          mutate(id = seq_len(n_mu)),
        by = "id"
      ) |>
      group_by(across(all_of(c(grouping_vars, "run")))) |>
      summarise(x = fun(.data$x), .groups = "drop") |>
      mutate(type = "model") |>
      bind_rows(
        covars |>
          mutate(observed = observed) |>
          group_by(across(all_of(grouping_vars))) |>
          summarise(x = fun(.data$observed), .groups = "drop") |>
          mutate(type = "observations")
      )
  }
)
