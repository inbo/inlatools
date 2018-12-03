context("fast_distribution_check")
ds <- generate_data()
test_that("handles poisson", {
  model <- INLA::inla(
    poisson ~ f(id, model = "iid"),
    family = "poisson",
    data = ds,
    control.predictor = list(compute = TRUE)
  )
  expect_is(
    fdc <- fast_distribution_check(model, nsim = 10),
    c("distribution_check", "tbl_df", "tbl", "data.frame")
  )
  expect_named(fdc, c("x", "median", "lcl", "ucl", "n", "ecdf"))
  expect_true(all(ds$poisson %in% fdc$x))
  expect_is(plot(fdc), c("gg", "ggplot"))
})

test_that("handles nbinomial", {
  model <- INLA::inla(
    poisson ~ f(id, model = "iid"),
    family = "nbinomial",
    data = ds,
    control.predictor = list(compute = TRUE)
  )
  expect_is(
    fdc <- fast_distribution_check(model, nsim = 10),
    c("distribution_check", "tbl_df", "tbl", "data.frame")
  )
  expect_named(fdc, c("x", "median", "lcl", "ucl", "n", "ecdf"))
  expect_true(all(ds$poisson %in% fdc$x))
})

test_that("handles gpoisson", {
  model <- INLA::inla(
    poisson ~ f(id, model = "iid"),
    family = "gpoisson",
    data = ds,
    control.predictor = list(compute = TRUE)
  )
  expect_is(
    fdc <- fast_distribution_check(model, nsim = 10),
    c("distribution_check", "tbl_df", "tbl", "data.frame")
  )
  expect_named(fdc, c("x", "median", "lcl", "ucl", "n", "ecdf"))
  expect_true(all(ds$poisson %in% fdc$x))
  expect_is(plot(fdc), c("gg", "ggplot"))
})

test_that("handles zeroinflatedpoisson1", {
  model <- INLA::inla(
    poisson ~ f(id, model = "iid"),
    family = "zeroinflatedpoisson1",
    data = ds,
    control.predictor = list(compute = TRUE)
  )
  expect_is(
    fdc <- fast_distribution_check(model, nsim = 10),
    c("distribution_check", "tbl_df", "tbl", "data.frame")
  )
  expect_named(fdc, c("x", "median", "lcl", "ucl", "n", "ecdf"))
  expect_true(all(ds$poisson %in% fdc$x))
  expect_is(plot(fdc), c("gg", "ggplot"))
})

test_that("handles zeroinflatednbinomial1", {
  model <- INLA::inla(
    poisson ~ f(id, model = "iid"),
    family = "zeroinflatednbinomial1",
    data = ds,
    control.predictor = list(compute = TRUE)
  )
  expect_is(
    fdc <- fast_distribution_check(model, nsim = 10),
    c("distribution_check", "tbl_df", "tbl", "data.frame")
  )
  expect_named(fdc, c("x", "median", "lcl", "ucl", "n", "ecdf"))
  expect_true(all(ds$poisson %in% fdc$x))
})

test_that("checks the model properties", {
  model <- INLA::inla(
    poisson ~ f(id, model = "iid"),
    family = "poisson",
    data = ds
  )
  expect_error(
    fast_distribution_check(model),
    "Refit the object with 'control.predictor = list\\(compute = TRUE\\)'"
  )

  model <- INLA::inla(
    poisson ~ f(id, model = "iid"),
    family = "gaussian",
    data = ds,
    control.predictor = list(compute = TRUE)
  )
  expect_error(
    fast_distribution_check(model),
    "gaussian is not yet handled"
  )

  selected <- sample(nrow(ds), ceiling(0.5 * nrow(ds)))
  ds$poisson[selected] <- NA
  ds$zipoisson[-selected] <- NA
  model <- INLA::inla(
    cbind(poisson, zipoisson) ~ f(id, model = "iid"),
    family = c("poisson", "poisson"),
    data = ds
  )
  expect_error(
    fast_distribution_check(model),
    "Only single responses are handled"
  )
})
