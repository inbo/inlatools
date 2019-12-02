#' Calculate the Residuals From an INLA Model
#' @param object An `inla` object.
#' @param type Currently only Pearson residuals are available.
#' Other types are only listed for compatibility with the default `residuals`
#' function.
#' @param ... Currently ignored.
#' @rdname residuals
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
    if (type != "pearson") {
      stop("only Pearson residuals are available")
    }
    if (length(object$.args$family) > 1) {
      stop("Only single responses are handled")
    }
    observed <- get_observed(object)
    fitted <- fitted(object)
    if (object$.args$family == "poisson") {
      (observed - fitted) / sqrt(fitted)
    } else {
      stop(object$.args$family, " distribution not handled")
    }
  }
)
