test_that("results are well-formed", {
  test.data <- as.matrix(iris)
  default <- split_train_and_test(test.data)

  # result is a list
  expect_equal(typeof(list()), typeof(default))

  # one sampled observation per original observation by default
  expect_equal(sum(default$counts), nrow(test.data))

  # test and train matrices are well-formed
  expect_equal(ncol(default$train), ncol(test.data))
  expect_equal(ncol(default$test), ncol(test.data))
  resulting.obs <- nrow(default$train) + nrow(default$test)
  expect_equal(nrow(test.data), resulting.obs)
})

test_that("split_train_and_test behaves as expected", {
  threshold <- 0.02
  test.data <- as.matrix(iris)
  default <- split_train_and_test(test.data)

  # proportions are near 50/50 by default
  default.diff <- abs(default$contingency.table[2] - default$contingency.table[1])
  expect_lt(default.diff, threshold)

  # Increasing the number of seeds improves split proportions
  many.seeds <- split_train_and_test(test.data, seeds=1:63)
  many.diff <- abs(many.seeds$contingency.table["test"] - many.seeds$contingency.table["train"])
  expect_lt(many.diff, default.diff)

  # Setting proportions approaches expected proportions
  expected.train <- 0.8
  expected.test <- 0.2
  eighty.twenty <- split_train_and_test(iris, train.prop=expected.train, test.prop=expected.test, seeds=1:63)
  train.diff <- abs(eighty.twenty$contingency.table["train"] - expected.train)
  test.diff <- abs(eighty.twenty$contingency.table["test"] - expected.test)
  expect_lt(train.diff, threshold)
  expect_lt(test.diff, threshold)

  # setting obs reduces the number of resulting observations
  expected.count <- 100
  subset <- split_train_and_test(iris, obs=expected.count)
  resulting.count <- sum(subset$counts)
  expect_equal(resulting.count, expected.count)
})
