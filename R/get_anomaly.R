#' Get a set of anomalies of the model
#'
#' Returns a named list with an element called `observations` and one element
#' for every random effect.
#' The random effect components use the name of the random effect.
#'
#' `observations` is a subset of the original data.frame.
#' It contains the rows with the `n` largest absolute value of the Pearson
#' residuals.
#' The random effect components contain a subset of the random effects.
#' Here we select the `n` rows with the largest absolute values of the mean.
#' @inheritParams get_observed
#' @param n the number of anomalies per criterion.
#' Defaults to `20`.
#' @name get_anomaly
#' @rdname get_anomaly
#' @exportMethod get_anomaly
#' @docType methods
#' @importFrom methods setGeneric
#' @family checks
setGeneric(
  name = "get_anomaly",
  def = function(object, n = 20) {
    standardGeneric("get_anomaly") # nocov
  }
)

#' @rdname get_anomaly
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.count
#' @importFrom purrr map
#' @importFrom utils tail
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
#' dc <- get_anomaly(model, n = 2)
#' str(dc)
setMethod(
  f = "get_anomaly",
  signature = signature(object = "inla"),
  definition = function(object, n = 20) {
    assert_that(is.count(n))
    resids <- residuals(object)
    extreme_obs <- tail(order(abs(resids)), n)
    output <- list(
      observations = object$.args$data[rev(extreme_obs), ]
    )
    output <- c(output,
      map(
        object$summary.random,
        function(x) {
          extreme_obs <- tail(order(abs(x$mean)), n)
          return(x[rev(extreme_obs), ])
        }
      )
    )
    return(output)
  }
)
