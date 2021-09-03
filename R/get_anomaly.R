#' Get a set of anomalies of the model
#'
#' Returns a named list with an element called `observations` and one element
#' for every random effect.
#' The random effect components use the name of the random effect.
#'
#' `observations` is a subset of the original data.frame.
#' It contains the rows with the `n` largest and `n` smallest values of the
#' Pearson residuals.
#' The random effect components contain a subset of the random effects.
#' Here we select the rows with the `n` largest and `n` lowest values of the
#' mean.
#' @inheritParams get_observed
#' @param n the number of anomalies per criterion.
#' Defaults to `10`.
#' @name get_anomaly
#' @rdname get_anomaly
#' @exportMethod get_anomaly
#' @docType methods
#' @importFrom methods setGeneric
#' @family checks
setGeneric(
  name = "get_anomaly",
  def = function(object, n = 10) {
    standardGeneric("get_anomaly") # nocov
  }
)

#' @rdname get_anomaly
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.count
#' @importFrom dplyr bind_cols
#' @importFrom purrr map
#' @importFrom utils tail
#' @importClassesFrom INLA inla
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
    extreme_low_obs <- head(order(resids), n)
    extreme_high_obs <- head(order(-resids), n)
    extreme_obs <- unique(c(extreme_low_obs, rev(extreme_high_obs)))
    output <- list(
      observations = bind_cols(
        object$.args$data[extreme_obs, ],
        object$summary.fitted.values[extreme_obs, ]
      )
    )

    output <- c(output,
      map(
        object$summary.random,
        function(x) {
          extreme_low_obs <- head(order(x$mean), n)
          extreme_high_obs <- head(order(-x$mean), n)
          extreme_obs <- unique(c(extreme_low_obs, rev(extreme_high_obs)))
          return(x[rev(extreme_obs), ])
        }
      )
    )
    return(output)
  }
)
