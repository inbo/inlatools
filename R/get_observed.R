#' get the observed values from the model object
#' @param model the INLA model
#' @export
#' @importFrom assertthat assert_that is.string
get_observed <- function(model) {
  assert_that(inherits(model, "inla"))
  response <- as.character(model$.args$formula[2])
  assert_that(is.string(response))
  model$.args$data[, response, drop = TRUE]
}
