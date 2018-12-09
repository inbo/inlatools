context("simulate_rw")

test_that("simulate_rw() yields sensible output", {
  expect_error(
    simulate_rw(),
    "either 'sigma' or 'tau' must be specified"
  )
  expect_error(
    simulate_rw(sigma = 0.1, tau = 100),
    "either 'sigma' or 'tau' must be NULL"
  )
  this_length <- 20
  this_sim <- 30
  this_start <- -5
  expect_is(
    x <- simulate_rw(
      sigma = 0.1, length = this_length, start = this_start, n_sim = this_sim
    ),
    c("sim_rw", "data.frame")
  )
  expect_equal(nrow(x), this_length * this_sim)
  expect_named(x, c("x", "y", "replicate"))
  expect_identical(x$replicate, rep(seq_len(this_sim), each = this_length))
  expect_identical(
    x$x,
    rep(seq(this_start, length = this_length, by = 1), this_sim)
  )
  expect_identical(sum(is.na(x$y)), 0L)
  expect_is(x$y, "numeric")
  expect_identical(attr(x, "sigma"), 0.1)
  expect_is(
    x <- simulate_rw(
      tau = 100, length = this_length, start = this_start, n_sim = this_sim
    ),
    c("sim_rw", "data.frame")
  )
  expect_identical(attr(x, "sigma"), 0.1)
})

test_that("plots on simulated random walks work", {
  x <- simulate_rw(sigma = 0.001, length = 20, n_sim = 20)
  expect_is(plot(x), c("gg", "ggplot"))
  expect_is(plot(x, link = "log"), c("gg", "ggplot"))
  expect_is(plot(x, link = "logit"), c("gg", "ggplot"))
  expect_is(plot(x, baseline = 10, link = "log"), c("gg", "ggplot"))
  expect_is(plot(x, center = "mean"), c("gg", "ggplot"))
  expect_is(plot(x, center = "bottom"), c("gg", "ggplot"))
  expect_is(plot(x, center = "top"), c("gg", "ggplot"))
})

test_that("select_poly()", {
  x <- simulate_rw(sigma = 0.5, n_sim = 10)
  expect_is(select_poly(x, coefs = c(0, 0), n = 1), "sim_rw")
  expect_is(selection <- select_poly(x, n = 1), "sim_rw")
  expect_identical(attr(x, "sigma"), attr(selection, "sigma"))
})

test_that("select_quantile()", {
  x <- simulate_rw(sigma = 0.5, n_sim = 100)
  expect_is(
    selection <- select_quantile(x, quantiles = c(0.5, 0.75)),
    c("sim_rw_quant", "sim_rw")
  )
  expect_identical(attr(x, "sigma"), attr(selection, "sigma"))
  expect_is(plot(selection), c("gg", "ggplot"))
})

test_that("select_change()", {
  x <- simulate_rw(sigma = 0.5, n_sim = 10)
  expect_is(selection <- select_change(x, n = 1), "sim_rw")
  expect_identical(attr(x, "sigma"), attr(selection, "sigma"))
})

test_that("select_divergence()", {
  x <- simulate_rw(sigma = 0.5, n_sim = 10)
  expect_is(selection <- select_divergence(x, n = 1), "sim_rw")
  expect_identical(attr(x, "sigma"), attr(selection, "sigma"))
})
