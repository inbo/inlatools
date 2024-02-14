context("dispersion test")
set.seed(20181209)
test_that("basic functionality works", {
  nsim <- 10L
  ds <- generate_data(n_random = 10, n_replicate = 3)

  object <- INLA::inla(
    poisson ~ f(group_id, model = "iid"), family = "poisson", data = ds,
    control.predictor = list(compute = TRUE),
    control.compute = list(config = TRUE)
  )
  expect_is(dc <- dispersion_check(object, nsim = nsim), "dispersion_check")
  expect_named(dc, c("data", "model"))
  expect_is(dc$data, "numeric")
  expect_is(dc$model, "numeric")
  expect_identical(length(dc$data), 1L)
  expect_identical(length(dc$model), nsim)
  expect_false(is.na(dc$data))
  expect_identical(sum(is.na(dc$model)), 0L)
  expect_is(plot(dc), c("gg", "ggplot"))

  object <- INLA::inla(
    poisson ~ f(group_id, model = "iid"), family = "nbinomial", data = ds,
    control.predictor = list(compute = TRUE),
    control.compute = list(config = TRUE)
  )
  expect_is(dc <- dispersion_check(object, nsim = nsim), "dispersion_check")
  expect_named(dc, c("data", "model"))
  expect_is(dc$data, "numeric")
  expect_is(dc$model, "numeric")
  expect_identical(length(dc$data), 1L)
  expect_identical(length(dc$model), nsim)
  expect_false(is.na(dc$data))
  expect_identical(sum(is.na(dc$model)), 0L)

  object <- INLA::inla(
    poisson ~ f(group_id, model = "iid"), family = "zeroinflatednbinomial1",
    data = ds,
    control.predictor = list(compute = TRUE),
    control.compute = list(config = TRUE)
  )
  expect_is(dc <- dispersion_check(object, nsim = nsim), "dispersion_check")
  expect_named(dc, c("data", "model"))
  expect_is(dc$data, "numeric")
  expect_is(dc$model, "numeric")
  expect_identical(length(dc$data), 1L)
  expect_identical(length(dc$model), nsim)
  expect_false(is.na(dc$data))
  expect_identical(sum(is.na(dc$model)), 0L)

  object <- INLA::inla(
    poisson ~ f(group_id, model = "iid"), family = "zeroinflatedpoisson1",
    data = ds,
    control.predictor = list(compute = TRUE),
    control.compute = list(config = TRUE)
  )
  expect_is(dc <- dispersion_check(object, nsim = nsim), "dispersion_check")
  expect_named(dc, c("data", "model"))
  expect_is(dc$data, "numeric")
  expect_is(dc$model, "numeric")
  expect_identical(length(dc$data), 1L)
  expect_identical(length(dc$model), nsim)
  expect_false(is.na(dc$data))
  expect_identical(sum(is.na(dc$model)), 0L)

  object <- INLA::inla(
    poisson ~ f(group_id, model = "iid"), family = "zeroinflatedpoisson0",
    data = ds,
    control.predictor = list(compute = TRUE),
    control.compute = list(config = TRUE)
  )
  expect_is(dc <- dispersion_check(object, nsim = nsim), "dispersion_check")
  expect_named(dc, c("data", "model"))
  expect_is(dc$data, "numeric")
  expect_is(dc$model, "numeric")
  expect_identical(length(dc$data), 1L)
  expect_identical(length(dc$model), nsim)
  expect_false(is.na(dc$data))
  expect_identical(sum(is.na(dc$model)), 0L)

  object <- INLA::inla(
    poisson ~ f(group_id, model = "iid"), family = "zeroinflatednbinomial0",
    data = ds,
    control.predictor = list(compute = TRUE),
    control.compute = list(config = TRUE)
  )
  expect_is(dc <- dispersion_check(object, nsim = nsim), "dispersion_check")
  expect_named(dc, c("data", "model"))
  expect_is(dc$data, "numeric")
  expect_is(dc$model, "numeric")
  expect_identical(length(dc$data), 1L)
  expect_identical(length(dc$model), nsim)
  expect_false(is.na(dc$data))
  expect_identical(sum(is.na(dc$model)), 0L)
})

test_that("handles missing responses", {
  nsim <- 10L
  ds <- generate_data(n_random = 10, n_replicate = 3)
  ds$poisson[sample(nrow(ds), ceiling(nrow(ds) / 10))] <- NA

  object <- INLA::inla(
    poisson ~ f(group_id, model = "iid"), family = "poisson", data = ds,
    control.predictor = list(compute = TRUE, link = 1),
    control.compute = list(config = TRUE),
  )
  expect_is(dc <- dispersion_check(object, nsim = nsim), "dispersion_check")
  expect_named(dc, c("data", "model"))
  expect_is(dc$data, "numeric")
  expect_is(dc$model, "numeric")
  expect_identical(length(dc$data), 1L)
  expect_identical(length(dc$model), nsim)
  expect_false(is.na(dc$data))
  expect_identical(sum(is.na(dc$model)), 0L)

  object <- INLA::inla(
    poisson ~ f(group_id, model = "iid"), family = "zeroinflatedpoisson1",
    data = ds,
    control.predictor = list(compute = TRUE, link = 1),
    control.compute = list(config = TRUE),
  )
  expect_is(dc <- dispersion_check(object, nsim = nsim), "dispersion_check")
  expect_named(dc, c("data", "model"))
  expect_is(dc$data, "numeric")
  expect_is(dc$model, "numeric")
  expect_identical(length(dc$data), 1L)
  expect_identical(length(dc$model), nsim)
  expect_false(is.na(dc$data))
  expect_identical(sum(is.na(dc$model)), 0L)

  object <- INLA::inla(
    poisson ~ f(group_id, model = "iid"), family = "nbinomial", data = ds,
    control.predictor = list(compute = TRUE, link = 1),
    control.compute = list(config = TRUE),
  )
  expect_is(dc <- dispersion_check(object, nsim = nsim), "dispersion_check")
  expect_named(dc, c("data", "model"))
  expect_is(dc$data, "numeric")
  expect_is(dc$model, "numeric")
  expect_identical(length(dc$data), 1L)
  expect_identical(length(dc$model), nsim)
  expect_false(is.na(dc$data))
  expect_identical(sum(is.na(dc$model)), 0L)

  object <- INLA::inla(
    poisson ~ f(group_id, model = "iid"), family = "zeroinflatednbinomial1",
    data = ds,
    control.predictor = list(compute = TRUE, link = 1),
    control.compute = list(config = TRUE),
  )
  expect_is(dc <- dispersion_check(object, nsim = nsim), "dispersion_check")
  expect_named(dc, c("data", "model"))
  expect_is(dc$data, "numeric")
  expect_is(dc$model, "numeric")
  expect_identical(length(dc$data), 1L)
  expect_identical(length(dc$model), nsim)
  expect_false(is.na(dc$data))
  expect_identical(sum(is.na(dc$model)), 0L)
})

test_that("error handling", {
  ds <- generate_data(n_random = 10, n_replicate = 3)

  object <- INLA::inla(
    poisson ~ f(group_id, model = "iid"), data = ds,
    control.predictor = list(compute = TRUE),
    control.compute = list(config = TRUE)
  )
  expect_error(dispersion_check(object), "gaussian is not yet handled")

  selected <- sample(nrow(ds), ceiling(0.5 * nrow(ds)))
  ds$poisson[selected] <- NA
  ds$zipoisson[-selected] <- NA
  model <- INLA::inla(
    cbind(poisson, zipoisson) ~ f(group_id, model = "iid"),
    family = c("poisson", "poisson"),
    data = ds
  )
  expect_error(dispersion_check(model), "Only single responses are handled")
})
