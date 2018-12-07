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
  expect_is(plot(x, type = "divergence"), c("gg", "ggplot"))
  expect_is(plot(x, type = "stationary"), c("gg", "ggplot"))
  expect_is(plot(x, type = "quantile"), c("gg", "ggplot"))
  expect_is(plot(x, type = "change"), c("gg", "ggplot"))
  expect_is(plot(x, type = "poly"), c("gg", "ggplot"))
  expect_is(plot(x, link = "log"), c("gg", "ggplot"))
  expect_is(plot(x, type = "divergence", link = "log"), c("gg", "ggplot"))
  expect_is(plot(x, type = "stationary", link = "log"), c("gg", "ggplot"))
  expect_is(plot(x, type = "quantile", link = "log"), c("gg", "ggplot"))
  expect_is(plot(x, type = "change", link = "log"), c("gg", "ggplot"))
  expect_is(plot(x, type = "poly", link = "log"), c("gg", "ggplot"))
  expect_is(plot(x, link = "logit"), c("gg", "ggplot"))
  expect_is(plot(x, type = "divergence", link = "logit"), c("gg", "ggplot"))
  expect_is(plot(x, type = "stationary", link = "logit"), c("gg", "ggplot"))
  expect_is(plot(x, type = "quantile", link = "logit"), c("gg", "ggplot"))
  expect_is(plot(x, type = "change", link = "logit"), c("gg", "ggplot"))
  expect_is(plot(x, type = "poly", link = "logit"), c("gg", "ggplot"))
})

test_that("select_poly()", {
  x <- simulate_rw(sigma = 0.5)
  expect_is(inlatools:::select_poly(x, coefs = c(0, 0), n = 10), "sim_rw")
})
