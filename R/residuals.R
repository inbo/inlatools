#' Calculate the Residuals From an INLA Model
#' @param object An `inla` object.
#' @param type Currently only Pearson residuals are available.
#' Other types are only listed for compatibility with the default `residuals`
#' function.
#' @param ... Currently ignored.
#' @rdname residuals
#' @importFrom assertthat assert_that
#' @importFrom methods setMethod
#' @importClassesFrom INLA inla
#' @export
#' @family statistics
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
#' residuals(model)
setMethod(
  f = "residuals",
  signature = signature(object = "inla"),
  definition = function(
    object,
    type = c("pearson", "deviance", "working", "response", "partial"),
    ...
  ) {
    type <- match.arg(type)
    assert_that(type == "pearson", msg = "only Pearson residuals are available")
    assert_that(
      length(object$.args$family) == 1,
      msg = "Only single responses are handled"
    )
    assert_that(
      object$.args$family %in% c(
        "nbinomial", "poisson", "zeroinflatednbinomial1", "zeroinflatedpoisson1"
      ),
      msg = sprintf(
        "`%s` distribution not (yet) handled by `residuals()`.
Please open an issue at https://github.com/inbo/inlatools.",
        object$.args$family
      )
    )
    observed <- get_observed(object)
    fitted <- fitted(object)
    variance <- switch(
      object$.args$family,
      nbinomial = var_nbinom(mu = fitted, size = get_mean_size(object)),
      poisson = fitted,
      zeroinflatednbinomial1 = var_zinbinom1(
        mu = fitted, size = get_mean_size(object),
        zero = get_mean_zero_prob(object)
      ),
      zeroinflatedpoisson1 = var_zipois1(
        mu = fitted, zero = get_mean_zero_prob(object)
      )
    )
    (observed - fitted) / sqrt(variance)
  }
)

#' @importFrom assertthat assert_that
get_mean_size <- function(object) {
  which_size <- grep(
    "size for .*nbinomial.*observations",
    rownames(object$summary.hyperpar)
  )
  assert_that(
    length(which_size) == 1, msg = "Can't find size for nbinomial observations."
  )
  object$summary.hyperpar$mean[which_size]
}

#' Variance of the negative binomial distribution
#' @param mu mean of the distribution.
#' Must be on the original scale.
#' @param size Size of the negative binomial distribution.
#' Must be strict positive.
#' @rdname variance
#' @family variance
#' @export
var_nbinom <- function(mu, size) {
  assert_that(all(mu > 0), all(size > 0))
  mu * (1 + mu / size)
}

#' Variance of the zero-inflated negative binomial distribution type 1
#'
#' The type 1 zero-inflated negative binomial distribution is a standard
#' negative binomial distribution with an additional point mass at zero.
#' @inheritParams var_nbinom
#' @inheritParams var_zipois1
#' @rdname variance
#' @family variance
#' @export
var_zinbinom1 <- function(mu, size, zero) {
  assert_that(all(mu > 0), all(size > 0), all(zero > 0), all(zero < 1))
  mu * (1 + mu * (zero + 1 / size) / (1 - zero))
}

#' Variance of the zero-inflated poisson distribution type 1
#'
#' The type 1 zero-inflated poisson is a standard Poisson distribution with an
#' additional point mass at zero.
#' @inheritParams var_nbinom
#' @param zero Probability of the point mass at zero.
#' @rdname variance
#' @family variance
#' @export
var_zipois1 <- function(mu, zero) {
  assert_that(all(mu > 0), all(zero > 0), all(zero < 1))
  mu * (1 + mu * zero / (1 - zero))
}

#' @importFrom assertthat assert_that
get_mean_zero_prob <- function(object) {
  which_prob <- grep(
    "zero-probability",
    rownames(object$summary.hyperpar)
  )
  assert_that(
    length(which_prob) == 1, msg = "Can't find zero-probability parameter."
  )
  object$summary.hyperpar$mean[which_prob]
}
