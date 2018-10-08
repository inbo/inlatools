#' get the observed values from the model object
#' @inheritParams fitted
#' @export
#' @importFrom assertthat assert_that is.string
get_observed <- function(object) {
  assert_that(inherits(object, "inla"))
  response <- as.character(object$.args$formula[2])
  assert_that(is.string(response))
  object$.args$data[, response, drop = TRUE]
}

#' get the mean of the fitted values from the model object
#' @export
#' @param object the INLA model
#' @importFrom assertthat assert_that
fitted <- function(object) {
  assert_that(inherits(object, "inla"))
  if (nrow(object$summary.fitted.values) == 0) {
    stop("no fitted values in object")
  }
  object$summary.fitted.values[, "mean"]
}
