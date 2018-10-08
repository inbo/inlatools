#' Calculate the residuals from an INLA model
#' @param object the INLA object
#' @inheritParams stats::residuals.glm
#' @rdname residuals
#' @importFrom methods setMethod
#' @export
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
    if (nrow(object$summary.fitted.values) == 0) {
      stop("no fitted values in object")
    }
    fitted <- object$summary.fitted.values[, "mean"]
    observed <- get_observed(object)
    if (object$.args$family == "poisson") {
      (observed - fitted) / sqrt(fitted)
    } else {
      stop(object$.args$family, " distribution not handled")
    }
  }
)
