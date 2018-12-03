context("distribution_check")
test_that("checks the model properties", {
  ds <- generate_data()
  model <- INLA::inla(
    poisson ~ f(id, model = "iid"),
    family = "poisson",
    data = ds,
    control.predictor = list(compute = TRUE)
  )
  expect_error(
    residuals(model, type = "deviance"),
    "only Pearson residuals are available"
  )
  expect_is(resid <- residuals(model), "numeric")
  expect_identical(length(resid), nrow(ds))

  model <- INLA::inla(
    poisson ~ f(id, model = "iid"),
    family = "gaussian",
    data = ds,
    control.predictor = list(compute = TRUE)
  )
  expect_error(residuals(model), "gaussian distribution not handled")

  selected <- sample(nrow(ds), ceiling(0.5 * nrow(ds)))
  ds$poisson[selected] <- NA
  ds$zipoisson[-selected] <- NA
  model <- INLA::inla(
    cbind(poisson, zipoisson) ~ f(id, model = "iid"),
    family = c("poisson", "poisson"),
    data = ds
  )
  expect_error(residuals(model), "Only single responses are handled")
})
