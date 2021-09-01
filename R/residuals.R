#' Calculate the Residuals From an INLA Model
#' @param object An `inla` object.
#' @param type Currently only Pearson residuals are available.
#' Other types are only listed for compatibility with the default `residuals`
#' function.
#' @param ... Currently ignored.
#' @rdname residuals
#' @importFrom assertthat assert_that
#' @importFrom methods setMethod
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
      object$.args$family %in% "poisson",
      msg = paste(object$.args$family, "distribution not handled")
    )
    observed <- get_observed(object)
    fitted <- fitted(object)
    (observed - fitted) / sqrt(fitted) # poisson
  }
)
