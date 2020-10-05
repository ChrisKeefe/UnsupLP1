test_that("clustering is well-formed", {
  k <- 3
  result <- KMEANS(iris[1:4], k)

  # result is a list
  expect_equal(typeof(list()), typeof(result))

  # cluster is an int vector
  expect_equal(typeof(result$cluster), "integer")

  # one clustering per observation
  expect_equal(NROW(iris), length(result$cluster))

  # cluster labels are valid
  min_val <- min(result$cluster)
  max_val <- max(result$cluster)
  expect_equal(min_val, 1)
  expect_equal(max_val, k)

  # centers is well-formed
  expect_equal(k, NROW(result$centers))

  # one within-cluster sum of squares per cluster
  expect_equal(k, length(result$withinss))

  # tot.withinss calculated correctly
  expect_equal(sum(result$withinss), result$tot.withinss)
})
