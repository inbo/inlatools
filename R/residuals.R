#' Calculate the residuals from an INLA model
#' @inheritParams stats::residuals.glm
#' @inheritParams fitted
#' @rdname residuals
#' @importFrom methods setMethod
#' @export
#' @include s3_classes.R
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
