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
      object$.args$family %in% c("nbinomial", "poisson"),
      msg = paste(object$.args$family, "distribution not handled")
    )
    observed <- get_observed(object)
    fitted <- fitted(object)
    variance <- switch(
      object$.args$family,
      nbinomial = fitted + (1 + fitted / get_mean_size(object)),
      poisson = fitted
    )
    (observed - fitted) / sqrt(variance)
  }
)

#' @importFrom assertthat assert_that
get_mean_size <- function(object) {
  which_size <- grep(
    "size for the nbinomial observations",
    rownames(object$summary.hyperpar)
  )
  assert_that(
    length(which_size) == 1, msg = "Can't find size for nbinomial observations"
  )
  object$summary.hyperpar$mean[which_size]
}
