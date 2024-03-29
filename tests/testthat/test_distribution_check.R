context("distribution_check")
seed <- 20181209L
set.seed(seed)
ds <- generate_data(n_random = 10, n_replicate = 2)
test_that("handles poisson", {
  model <- INLA::inla(
    poisson ~ f(group_id, model = "iid"),
    family = "poisson",
    data = ds,
    control.compute = list(config = TRUE)
  )
  expect_is(
    suppressWarnings(dc <- distribution_check(model, nsim = 10, seed = seed)),
    c("distribution_check", "tbl_df", "tbl", "data.frame")
  )
  expect_named(dc, c("x", "median", "lcl", "ucl", "n", "ecdf"))
  expect_true(all(ds$poisson %in% dc$x))
  expect_is(plot(dc), c("gg", "ggplot"))
  expect_is(plot(dc, n = TRUE), c("gg", "ggplot"))
})

test_that("handles gpoisson", {
  suppressWarnings(
    model <- INLA::inla(
      poisson ~ f(group_id, model = "iid"),
      family = "gpoisson",
      data = ds,
      control.compute = list(config = TRUE)
    )
  )
  expect_is(
    suppressWarnings(dc <- distribution_check(model, nsim = 10, seed = seed)),
    c("distribution_check", "tbl_df", "tbl", "data.frame")
  )
  expect_named(dc, c("x", "median", "lcl", "ucl", "n", "ecdf"))
  expect_true(all(ds$poisson %in% dc$x))
  expect_is(plot(dc), c("gg", "ggplot"))
})

test_that("handles nbinomial", {
  model <- INLA::inla(
    poisson ~ f(group_id, model = "iid"),
    family = "nbinomial",
    data = ds,
    control.compute = list(config = TRUE)
  )
  expect_is(
    suppressWarnings(dc <- distribution_check(model, nsim = 10, seed = seed)),
    c("distribution_check", "tbl_df", "tbl", "data.frame")
  )
  expect_named(dc, c("x", "median", "lcl", "ucl", "n", "ecdf"))
  expect_true(all(ds$poisson %in% dc$x))
  expect_is(plot(dc), c("gg", "ggplot"))
})

test_that("handles zeroinflatedpoisson1", {
  model <- INLA::inla(
    poisson ~ f(group_id, model = "iid"),
    family = "zeroinflatedpoisson1",
    data = ds,
    control.compute = list(config = TRUE)
  )
  expect_is(
    suppressWarnings(dc <- distribution_check(model, nsim = 10, seed = seed)),
    c("distribution_check", "tbl_df", "tbl", "data.frame")
  )
  expect_named(dc, c("x", "median", "lcl", "ucl", "n", "ecdf"))
  expect_true(all(ds$poisson %in% dc$x))
  expect_is(plot(dc), c("gg", "ggplot"))
})

test_that("checks the model properties", {
  model <- INLA::inla(
    poisson ~ f(group_id, model = "iid"),
    family = "poisson",
    data = ds
  )
  expect_error(
    suppressWarnings(distribution_check(model, nsim = 1, seed = seed)),
    regexp = "'control.compute=list\\(config = TRUE\\)'"
  )

  model <- INLA::inla(
    poisson ~ f(group_id, model = "iid"),
    family = "gaussian",
    data = ds,
    control.compute = list(config = TRUE)
  )
  expect_error(
    suppressWarnings(distribution_check(model, nsim = 1, seed = seed)),
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
    suppressWarnings(distribution_check(model, nsim = 1, seed = seed)),
    "Only single responses are handled"
  )
})
