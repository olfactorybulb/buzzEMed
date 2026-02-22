test_that("buzzMcat returns an mcmc.list object", {
  # 1. Create minimal test data
  set.seed(2026)
  test_dataset <- data.frame(
    outcome = rnorm(20),
    predictor = rnorm(20),
    mediator1 = rbinom(20, 1, 0.5)
  )

  # 2. Run the function with minimal iterations
  res <- buzzMcat(
    dataset = test_dataset,
    X = "predictor",
    Y = "outcome",
    M = "mediator1",
    n_burnin = 5,
    n_iter = 10
  )

  # 3. Assertions
  expect_s3_class(res, "mcmc.list")
  expect_true(length(res) > 0)
})
