#' Zero altered negative binomial
#' @inheritParams rzapois
#' @inheritParams stats::rnbinom
#' @export
#' @importFrom assertthat assert_that is.count is.number noNA
#' @importFrom stats dnbinom qnbinom rbinom rnbinom runif
rzanbinom <- function(n, mu, size, prob, tol = 2e-10) {
  assert_that(
    is.count(n), noNA(n), is.numeric(mu), noNA(mu), is.numeric(size),
    noNA(size), is.numeric(prob), noNA(prob), is.number(tol)
  )
  assert_that(
    tol > 0, tol < 1e-5, length(mu) %in% c(1, n), length(size) %in% c(1, n),
    length(prob) %in% c(1, n), all(prob >= 0), all(prob <= 1), all(size > 0)
  )
  count <- rbinom(n = n, size = 1, prob = 1 - prob)
  non_zero <- which(count == 1)
  n <- length(non_zero)
  if (n == 0) {
    return(count)
  }
  if (length(mu) == 1) {
    mu <- rep(mu, n)
  } else {
    mu <- mu[non_zero]
  }
  if (length(size) == 1) {
    size <- rep(size, n)
  } else {
    size <- size[non_zero]
  }
  low <- which(mu < tol)
  if (length(low) == 0) {
    dnbinom(x = 0, mu = mu, size = size) |>
      runif(n = n, max = 1) |>
      qnbinom(mu = mu, size = size) -> count[non_zero]
    return(count)
  }
  if (length(low) == n) {
    return(count)
  }
  dnbinom(x = 0, mu = mu[-low], size = size) |>
    runif(n = n - length(low), max = 1) |>
    qnbinom(mu = mu[-low], size = size) -> count[non_zero][-low]
  return(count)
}

#' Zero inflated negative binomial
#' @inheritParams rzipois
#' @inheritParams stats::rnbinom
#' @export
#' @importFrom stats rbinom rnbinom
rzinbinom <- function(n, mu, size, prob) {
  rbinom(n = n, size = 1, prob = 1 - prob) * rnbinom(n, mu = mu, size = size)
}
