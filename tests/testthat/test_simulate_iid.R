context("simulate_iid")

test_that("simulate_iid() yields sensible output", {
  expect_error(
    simulate_iid(),
    "either 'sigma' or 'tau' must be specified"
  )
  expect_error(
    simulate_iid(sigma = 0.1, tau = 100),
    "either 'sigma' or 'tau' must be NULL"
  )
  this_sim <- 30
  expect_is(
    x <- simulate_iid(sigma = 0.1, n_sim = this_sim),
    c("sim_rw", "numeric")
  )
  expect_equal(length(x), this_sim)
  expect_identical(sum(is.na(x)), 0L)
  expect_identical(attr(x, "sigma"), 0.1)
  expect_is(
    x <- simulate_iid(tau = 100, n_sim = this_sim),
    c("sim_rw", "numeric")
  )
  expect_identical(attr(x, "sigma"), 0.1)
})

test_that("plots on simulated random intercepts work", {
  x <- simulate_iid(sigma = 0.1, n_sim = 20)
  expect_is(plot(x), c("gg", "ggplot"))
  expect_is(plot(x, link = "log"), c("gg", "ggplot"))
  expect_is(plot(x, link = "logit"), c("gg", "ggplot"))
})
