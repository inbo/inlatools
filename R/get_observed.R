#' get the observed values from the model object
#' @param object the INLA model
#' @export
#' @importFrom assertthat assert_that is.string
#' @family statistics
get_observed <- function(object) {
  assert_that(inherits(object, "inla"))
  response <- as.character(object$.args$formula[2])
  assert_that(is.string(response))
  object$.args$data[, response, drop = TRUE]
}

#' Calculate the residuals from an INLA model
#' @inheritParams stats::fitted
#' @rdname fitted
#' @importFrom methods setMethod
#' @export
#' @include s3_classes.R
#' @family statistics
setMethod(
  f = "fitted",
  signature = signature(object = "inla"),
  definition = function(
    object,
    ...
  ) {
    if (nrow(object$summary.fitted.values) == 0) { #nolint
      stop("no fitted values in object.
Refit the object with 'control.compute = list(compute = TRUE)'")
    }
    object$summary.fitted.values[, "mean"] #nolint
  }
)
