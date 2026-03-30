library(testthat)
library(buzzEMed)

test_that("buzzEBMedAuto correctly identifies and fits models", {
  # 1. Create a dummy dataset
  set.seed(123)
  n <- 100
  x <- rnorm(n)
  m1 <- 0.5 * x + rnorm(n)
  m2 <- ifelse(0.5 * x + rnorm(n) > 0, 1, 0) # Binary mediator
  y <- 0.5 * m1 + 0.5 * m2 + rnorm(n)

  dat <- data.frame(X = x, M1 = m1, M2 = m2, Y = y)

  # 2. Test the lavaan-style syntax parsing
  model_syntax <- "
    M1 + M2 ~ X
    Y ~ M1 + M2 + X
  "

  # 3. Run the model (short chains for fast testing)
  # We wrap this in expectation to ensure no errors
  expect_error(
    fit <- buzzEBMedAuto(
      model = model_syntax,
      dataset = dat,
      n_burnin = 10,
      n_iter = 50
    ),
    NA
  )

  # 4. Check the output structure
  expect_s3_class(fit, "buzz_result") # Assumes you've set this class in your functions
  expect_true(!is.null(fit$results))
})

test_that("Specialized functions accept correct data types", {
  # Testing the specific dispatch for continuous M and Y
  # (Replace these names with your actual data if needed)
  expect_error(
    buzzEBMcontYcont(dataset = dat, x = "X", m = "M1", y = "Y", n_iter = 10),
    NA
  )
})
