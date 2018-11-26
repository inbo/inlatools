context("dispersion test")
test_that("basic functionality works", {
  nsim <- 10L
  ds <- generate_data(n_random = 10, n_replicate = 3)

  object <- INLA::inla(
    poisson ~ f(id, model = "iid"), family = "poisson", data = ds,
    control.predictor = list(compute = TRUE),
    control.compute = list(config = TRUE)
  )
  expect_is(dc <- dispersion_check(object, nsim = nsim), "dispersion_check")
  expect_named(dc, c("data", "model"))
  expect_is(dc$data, "numeric")
  expect_is(dc$model, "numeric")
  expect_identical(length(dc$data), 1L)
  expect_identical(length(dc$model), nsim)

  object <- INLA::inla(
    poisson ~ f(id, model = "iid"), family = "nbinomial", data = ds,
    control.predictor = list(compute = TRUE),
    control.compute = list(config = TRUE)
  )
  expect_is(dc <- dispersion_check(object, nsim = nsim), "dispersion_check")
  expect_named(dc, c("data", "model"))
  expect_is(dc$data, "numeric")
  expect_is(dc$model, "numeric")
  expect_identical(length(dc$data), 1L)
  expect_identical(length(dc$model), nsim)

  object <- INLA::inla(
    poisson ~ f(id, model = "iid"), family = "zeroinflatednbinomial1", data = ds,
    control.predictor = list(compute = TRUE),
    control.compute = list(config = TRUE)
  )
  expect_is(dc <- dispersion_check(object, nsim = nsim), "dispersion_check")
  expect_named(dc, c("data", "model"))
  expect_is(dc$data, "numeric")
  expect_is(dc$model, "numeric")
  expect_identical(length(dc$data), 1L)
  expect_identical(length(dc$model), nsim)

  object <- INLA::inla(
    poisson ~ f(id, model = "iid"), family = "zeroinflatedpoisson1", data = ds,
    control.predictor = list(compute = TRUE),
    control.compute = list(config = TRUE)
  )
  expect_is(dc <- dispersion_check(object, nsim = nsim), "dispersion_check")
  expect_named(dc, c("data", "model"))
  expect_is(dc$data, "numeric")
  expect_is(dc$model, "numeric")
  expect_identical(length(dc$data), 1L)
  expect_identical(length(dc$model), nsim)
})

test_that("error handling", {
  ds <- generate_data(n_random = 10, n_replicate = 3)

  object <- INLA::inla(
    poisson ~ f(id, model = "iid"), data = ds,
    control.predictor = list(compute = TRUE),
    control.compute = list(config = TRUE)
  )
  expect_error(dispersion_check(object), "gaussian is not yet handled")
})
