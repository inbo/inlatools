#' Use simulations to compare the observed distribution with the modelled
#' distribution
#'
#' This check uses the fitted values and thus ignores the uncertainty on the
#' predictions
#' @inheritParams get_observed
#' @inheritParams dispersion_check
#' @name fast_distribution_check
#' @rdname fast_distribution_check
#' @exportMethod fast_distribution_check
#' @docType methods
#' @importFrom methods setGeneric
#' @family checks
setGeneric(
  name = "fast_distribution_check",
  def = function(object, nsim = 1000) {
    standardGeneric("fast_distribution_check") # nocov
  }
)

#' @rdname fast_distribution_check
#' @importFrom methods new setMethod
#' @importFrom assertthat assert_that is.count
#' @importFrom purrr map_dfc
#' @importFrom dplyr arrange count group_by inner_join mutate mutate_all
#' summarise
#' @importFrom rlang .data
#' @importFrom tidyr complete
#' @importFrom stats quantile rpois rnbinom
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
#' fast_distribution_check(model)
setMethod(
  f = "fast_distribution_check",
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

    eta <- object$summary.linear.predictor[!which_na, "0.5quant"]
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
      run = rep(seq_len(nsim), each = n_mu),
      x = x
    ) |>
      count(.data$run, .data$x) -> n_sampled
    data.frame(x = observed) |>
      count(.data$x) -> n_observed
    n_count <- unique(c(n_observed$x, n_sampled$x))
    n_sampled |>
      complete(run = .data$run, x = n_count, fill = list(n = 0)) |>
      group_by(.data$run) |>
      arrange(.data$x) |>
      mutate(ecdf = cumsum(.data$n) / sum(.data$n)) |>
      group_by(.data$x) |>
      summarise(
        median = quantile(.data$ecdf, probs = 0.5),
        lcl = quantile(.data$ecdf, probs = 0.025),
        ucl = quantile(.data$ecdf, probs = 0.975)
      ) |>
      inner_join(
        n_observed |>
          complete(x = n_count, fill = list(n = 0)) |>
          arrange(.data$x) |>
          mutate(ecdf = cumsum(.data$n) / sum(.data$n)),
        by = "x"
      ) -> ecdf
    class(ecdf) <- c("distribution_check", class(ecdf))
    return(ecdf)
  }
)

#' @rdname fast_distribution_check
#' @importFrom methods setMethod new
#' @importFrom purrr map map2
setMethod(
  f = "fast_distribution_check",
  signature = signature(object = "list"),
  definition = function(object, nsim = 1000) {
    ecdf <- map(object, fast_distribution_check)
    if (is.null(names(object))) {
      ecdf <- map2(ecdf, seq_along(object), ~mutate(.x, model = .y))
    } else {
      ecdf <- map2(ecdf, names(object), ~mutate(.x, model = .y))
    }
    ecdf <- bind_rows(ecdf)
    class(ecdf) <- c("distribution_check", class(ecdf))
    return(ecdf)
  }
)

#' The generalised Poisson distribution
#' @param y a vector of positive integers for which to calculate the density
#' @param mu a vector of averages for which to calculate the density
#' @param phi a single overdispersion parameter
#' @return a matrix with the density for each combination of `y` (rows) and `mu`
#' (cols)
#' @export
#' @importFrom assertthat assert_that is.number
#' @rdname gpoisson
#' @family statistics
dgpoisson <- function(y, mu, phi) {
  assert_that(
    is.integer(y),
    all(y >= 0)
  )
  assert_that(
    is.numeric(mu),
    all(mu > 0)
  )
  assert_that(
    is.number(phi),
    phi > 0
  )

  a <- outer(phi * y, mu, "+")
  b <- 1 + phi
  exp(
    matrix(log(mu), nrow = length(y), ncol = length(mu), byrow = TRUE) +
    (y - 1) * log(a) - y * log(b) - lfactorial(y) - a / b
  )
}

#' @param n the number of simulated values
#' @importFrom assertthat assert_that is.number is.count
#' @rdname gpoisson
#' @export
#' @family statistics
rgpoisson <- function(n, mu, phi) {
  assert_that(is.count(n))
  assert_that(
    is.numeric(mu),
    all(mu > 0)
  )
  assert_that(
    is.number(phi),
    phi > 0
  )

  s <- sqrt(max(mu) * (1 + phi) ^ 2)
  low <- as.integer(max(0, min(mu) - 20 * s))
  high <- as.integer(max(mu) + 20 * s)
  prob <- dgpoisson(y = low:high, mu, phi)
  y <- apply(prob, 2, sample, x = low:high, replace = TRUE, size = n)
  return(y)
}

#' @importFrom assertthat assert_that is.count
rtpois <- function(n, lambda) {
  assert_that(is.count(n))
  assert_that(inherits(lambda, "numeric"))
  assert_that(all(lambda >= 0))
  if (length(lambda) < n) {
    lambda <- head(rep(lambda, ceiling(n / length(lambda))), n)
  }
  y <- rpois(n = n, lambda = lambda)
  while (any(y < 1)) {
    y[y < 1] <- rpois(sum(y < 1), lambda = lambda[y < 1])
  }
  return(y)
}
