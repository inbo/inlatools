#' Zero-altered Poisson
#'
#' Generate random numbers from a zero-altered Poisson distribution
#' @inheritParams stats::rpois
#' @param prob the point mass of zero
#' @param tol the tolerance for low numbers
#' @export
#' @importFrom assertthat assert_that is.count noNA
#' @importFrom stats dpois qpois rbinom rpois runif
rzapois <- function(n, lambda, prob, tol = 2e-10) {
  assert_that(
    is.count(n), is.numeric(lambda), is.number(prob), is.number(tol),
    noNA(lambda)
  )
  assert_that(
    0 <= prob, prob <= 1, 0 < tol, tol < 0.01, length(lambda) <= n,
    all(lambda > 0)
  )
  if (length(lambda) < n) {
    stopifnot(
      "n is not a multiple of length(lambda)" = n %% length(lambda) == 0
    )
    lambda <- rep(lambda, n %/% length(lambda))
  }
  count <- rbinom(n = n, size = 1, prob = 1 - prob)
  non_zero <- which(count == 1)
  n <- length(non_zero)
  if (n == 0) {
    return(count)
  }
  lambda <- lambda[non_zero]
  low <- which(lambda < tol)
  if (length(low) == 0) {
    dpois(x = 0, lambda = lambda) |>
      runif(n = n, max = 1) |>
      qpois(lambda = lambda) -> count[non_zero]
    return(count)
  }
  if (length(low) == n) {
    return(count)
  }
  dpois(0, lambda[-low]) |>
    runif(n = n - length(low), max = 1) |>
    qpois(lambda = lambda[-low]) -> count[non_zero][-low]
  return(count)
}

#' Zero-inflated Poisson
#'
#' Generate random numbers from a zero-inflated Poisson distribution
#' @inheritParams stats::rpois
#' @param prob the mass of extra zero's
#' @importFrom stats rbinom rpois
#' @export
rzipois <- function(n, lambda, prob) {
  rbinom(n = n, size = 1, prob = 1 - prob) * rpois(n, lambda = lambda)
}
