test_that("zero altered poisson", {
  set.seed(20240215)
  n_sim <- 10
  expect_type(z <- rzapois(n_sim, lambda = 1000, prob = 0), "double")
  expect_length(z, n_sim)
  expect_true(noNA(z))
  expect_true(all(z > 0))

  expect_type(
    z <- rzapois(n_sim, lambda = 1e-6, prob = 0, tol = 1e-3), "integer"
  )
  expect_length(z, n_sim)
  expect_true(noNA(z))
  expect_true(all(z == 1))

  expect_type(
    z <- rzapois(n_sim, lambda = 1e-6, prob = 1), "integer"
  )
  expect_length(z, n_sim)
  expect_true(noNA(z))
  expect_true(all(z == 0))

  expect_type(
    z <- rzapois(n_sim, lambda = c(1e-6, 100), prob = 0, tol = 1e-3), "double"
  )
  expect_length(z, n_sim)
  expect_true(noNA(z))
  expect_true(all(z > 0))
})

test_that("zero altered nbinomial", {
  set.seed(20240215)
  n_sim <- 10
  expect_type(z <- rzanbinom(n_sim, mu = 1000, size = 0.2, prob = 0), "double")
  expect_length(z, n_sim)
  expect_true(noNA(z))
  expect_true(all(z > 0))

  expect_type(
    z <- rzanbinom(n_sim, mu = 1e-6, size = 0.2, prob = 0, tol = 2e-6),
    "integer"
  )
  expect_length(z, n_sim)
  expect_true(noNA(z))
  expect_true(all(z == 1))

  expect_type(z <- rzanbinom(n_sim, mu = 1e-6, size = 0.2, prob = 1), "integer")
  expect_length(z, n_sim)
  expect_true(noNA(z))
  expect_true(all(z == 0))

  expect_type(
    z <- rzanbinom(n_sim, mu = c(1e-6, 100), size = 0.2, prob = 0, tol = 2e-6),
    "double"
  )
  expect_length(z, n_sim)
  expect_true(noNA(z))
  expect_true(all(z > 0))
})
