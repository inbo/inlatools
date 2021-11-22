#' get the observed values from the model object
#' @param object the INLA model
#' @export
#' @importFrom assertthat assert_that is.string
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
#' get_observed(model)
get_observed <- function(object) {
  assert_that(inherits(object, "inla"))
  response <- as.character(object$.args$formula[2])
  assert_that(is.string(response))
  object$.args$data[, response, drop = TRUE]
}

#' Extract the fitted values from an INLA model
#' @inheritParams stats::fitted
#' @rdname fitted
#' @importFrom assertthat assert_that noNA
#' @importFrom methods setMethod
#' @export
#' @family statistics
#' @examples
#' library(INLA)
#' set.seed(20181202)
#' model <- inla(#'   poisson ~ 1,
#'   poisson ~ 1,
#'   family = "poisson",
#'   data = data.frame(
#'     poisson = rpois(20, lambda = 10),
#'     base = 1
#'   ),
#'   control.predictor = list(compute = TRUE)
#' )
#' fitted(model)
setMethod(
  f = "fitted",
  signature = signature(object = "inla"),
  definition = function(
    object,
    ...
  ) {
    assert_that(
      nrow(object$summary.fitted.values) > 0,
      msg = "no fitted values in object.
Refit the object with 'control.predictor = list(compute = TRUE)'"
    )
    assert_that(
      noNA(object$misc$family),
      msg = "Missing response variables found.
Refit the object with `control.predictor=list(link = 1)`"
    )
    object$summary.fitted.values[, "mean"] #nolint
  }
)
