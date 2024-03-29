context("fast_distribution_check")
set.seed(20181209)
ds <- generate_data(n_random = 10, n_replicate = 4)
test_that("handles poisson", {
  model <- INLA::inla(
    poisson ~ f(group_id, model = "iid"),
    family = "poisson",
    data = ds,
    control.predictor = list(compute = TRUE)
  )
  expect_is(
    fdc <- fast_distribution_check(model, nsim = 100),
    c("distribution_check", "tbl_df", "tbl", "data.frame")
  )
  expect_named(fdc, c("x", "median", "lcl", "ucl", "n", "ecdf"))
  expect_true(all(ds$poisson %in% fdc$x))
  expect_is(plot(fdc), c("gg", "ggplot"))
})

test_that("handles nbinomial", {
  model <- INLA::inla(
    poisson ~ f(group_id, model = "iid"),
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
  suppressWarnings({
    model <- INLA::inla(
      poisson ~ f(group_id, model = "iid"),
      family = "gpoisson",
      data = ds,
      control.predictor = list(compute = TRUE)
    )
  })
  expect_is(
    fdc <- fast_distribution_check(model, nsim = 10),
    c("distribution_check", "tbl_df", "tbl", "data.frame")
  )
  expect_named(fdc, c("x", "median", "lcl", "ucl", "n", "ecdf"))
  expect_true(all(ds$poisson %in% fdc$x))
  expect_is(plot(fdc), c("gg", "ggplot"))
})

test_that("handles zeroinflatedpoisson0", {
  model <- INLA::inla(
    zipoisson ~ f(group_id, model = "iid"),
    family = "zeroinflatedpoisson0",
    data = ds,
    control.predictor = list(compute = TRUE)
  )
  expect_is(
    fdc <- fast_distribution_check(model, nsim = 10),
    c("distribution_check", "tbl_df", "tbl", "data.frame")
  )
  expect_named(fdc, c("x", "median", "lcl", "ucl", "n", "ecdf"))
  expect_true(all(ds$zipoisson %in% fdc$x))
  expect_is(plot(fdc), c("gg", "ggplot"))

  model <- INLA::inla(
    zipoisson ~ f(group_id, model = "iid"),
    family = "zeroinflatedpoisson0",
    data = ds,
    control.predictor = list(compute = TRUE),
    control.family = list(
      hyper = list(theta = list(initial = -10, fixed = TRUE))
    )
  )
  expect_is(
    fdc <- fast_distribution_check(model, nsim = 10),
    c("distribution_check", "tbl_df", "tbl", "data.frame")
  )
  expect_named(fdc, c("x", "median", "lcl", "ucl", "n", "ecdf"))
  expect_true(all(ds$zipoisson %in% fdc$x))
  expect_is(plot(fdc), c("gg", "ggplot"))
})

test_that("handles zeroinflated", {
  model <- list(
    poisson = INLA::inla(
      poisson ~ f(group_id, model = "iid"),
      family = "zeroinflatedpoisson1",
      data = ds,
      control.predictor = list(compute = TRUE)
    ),
    nbinomial = INLA::inla(
      poisson ~ f(group_id, model = "iid"),
      family = "zeroinflatednbinomial1",
      data = ds,
      control.predictor = list(compute = TRUE)
    )
  )
  expect_is(
    fdc <- fast_distribution_check(model, nsim = 10),
    c("distribution_check", "tbl_df", "tbl", "data.frame")
  )
  expect_named(fdc, c("x", "median", "lcl", "ucl", "n", "ecdf", "model"))
  expect_true(all(ds$poisson %in% fdc$x))
  expect_identical(names(model), unique(fdc$model))
  expect_is(plot(fdc), c("gg", "ggplot"))
  expect_is(
    fdc <- fast_distribution_check(unname(model), nsim = 10),
    c("distribution_check", "tbl_df", "tbl", "data.frame")
  )
  expect_named(fdc, c("x", "median", "lcl", "ucl", "n", "ecdf", "model"))
  expect_identical(seq_along(model), unique(fdc$model))
})

test_that("checks the model properties", {
  model <- INLA::inla(
    poisson ~ f(group_id, model = "iid"),
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
    cbind(poisson, zipoisson) ~ f(group_id, model = "iid"),
    family = c("poisson", "poisson"),
    data = ds
  )
  expect_error(
    fast_distribution_check(model),
    "Only single responses are handled"
  )
})
