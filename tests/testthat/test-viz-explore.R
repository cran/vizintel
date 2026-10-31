library(testthat)

test_that("viz_explore returns correlation matrix and plot", {
  result <- viz_explore(mtcars)
  expect_true(is.matrix(result$correlation))
  expect_true(inherits(result$plot, "ggplot"))
})

test_that("viz_explore detects numeric variables", {
  result <- viz_explore(mtcars)
  expect_true(ncol(result$correlation) >= 2)
  expect_true(nrow(result$correlation) >= 2)
})


test_that("viz_explore correlation matrix has values between -1 and 1", {
  result <- viz_explore(mtcars)
  expect_true(all(result$correlation >= -1))
  expect_true(all(result$correlation <= 1))
})


test_that("viz_explore rejects non-data-frame input", {
  expect_error(
    viz_explore(c(1, 2, 3)),
    "data.frame"
  )
})


test_that("viz_explore rejects data with fewer than two numeric variables", {
  data <- data.frame(
    name = c("A", "B", "C"),
    city = c("Chennai", "Delhi", "Mumbai")
  )
  expect_error(
    viz_explore(data),
    "At least two numeric variables"
  )
})


test_that("viz_explore handles numeric columns with NAs (complete.obs)", {
  df <- data.frame(
    x = c(1.0, NA, 3.0, 4.0, 5.0),
    y = c(NA, 2.0, 3.0, 4.0, 5.0)
  )
  # Should succeed because complete.obs is used; at least 3 complete rows
  expect_no_error(result <- viz_explore(df))
  expect_true(is.matrix(result$correlation))
  expect_equal(nrow(result$correlation), 2L)
})


test_that("viz_explore returns a ggplot that can be rendered off-screen", {
  result <- viz_explore(mtcars)
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = 600, height = 600)
  expect_no_error(print(result$plot))
  grDevices::dev.off()
  unlink(tmp)
})


test_that("viz_explore correlation matrix has correct row/col names", {
  df <- data.frame(a = 1:10, b = 11:20, c = 21:30)
  result <- viz_explore(df)
  expect_equal(sort(rownames(result$correlation)), c("a", "b", "c"))
  expect_equal(sort(colnames(result$correlation)), c("a", "b", "c"))
})