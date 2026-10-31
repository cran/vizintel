library(testthat)
library(ggplot2)

test_that("viz_accessibility works for basic plot", {
  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point()
  
  result <- viz_accessibility(p)
  
  expect_true(is.list(result))
  expect_true("summary" %in% names(result))
  expect_true("issues" %in% names(result))
  expect_equal(result$issues[[1]]$check, "overall")
})


test_that("viz_accessibility detects colour encoding", {
  p <- ggplot(
    mtcars,
    aes(wt, mpg, colour = factor(cyl))
  ) +
    geom_point()
  
  result <- viz_accessibility(p)
  
  checks <- vapply(
    result$issues,
    function(x) x$check,
    character(1L)
  )
  
  expect_true("colour_encoding" %in% checks)
})


test_that("viz_accessibility detects excessive colours", {
  df <- data.frame(
    category = paste0("Category_", 1:12),
    value = 1:12
  )
  
  p <- ggplot(
    df,
    aes(category, value, fill = category)
  ) +
    geom_col()
  
  result <- viz_accessibility(p)
  
  checks <- vapply(
    result$issues,
    function(x) x$check,
    character(1L)
  )
  
  expect_true("colour_count" %in% checks)
})


test_that("viz_accessibility detects excessive shapes", {
  df <- data.frame(
    category = paste0("Category_", 1:10),
    value = 1:10
  )
  
  p <- ggplot(
    df,
    aes(category, value, shape = category)
  ) +
    geom_point()
  
  result <- suppressWarnings(viz_accessibility(p))
  
  checks <- vapply(
    result$issues,
    function(x) x$check,
    character(1L)
  )
  
  expect_true("shape_count" %in% checks)
})


test_that("viz_accessibility detects small text", {
  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    geom_text(
      aes(label = rownames(mtcars)),
      size = 2
    )
  
  result <- viz_accessibility(p)
  
  checks <- vapply(
    result$issues,
    function(x) x$check,
    character(1L)
  )
  
  expect_true("text_size" %in% checks)
})


test_that("viz_accessibility detects excessive text labels", {
  df <- data.frame(
    x = 1:40,
    y = 1:40,
    label = paste0("Point_", 1:40)
  )
  
  p <- ggplot(df, aes(x, y)) +
    geom_text(aes(label = label))
  
  result <- viz_accessibility(p)
  
  checks <- vapply(
    result$issues,
    function(x) x$check,
    character(1L)
  )
  
  expect_true("label_count" %in% checks)
})


test_that("viz_accessibility detects overplotting", {
  set.seed(123)
  
  df <- data.frame(
    x = rnorm(1000),
    y = rnorm(1000)
  )
  
  p <- ggplot(df, aes(x, y)) +
    geom_point()
  
  result <- viz_accessibility(p)
  
  checks <- vapply(
    result$issues,
    function(x) x$check,
    character(1L)
  )
  
  expect_true("overplotting" %in% checks)
})


test_that("viz_accessibility recognises transparency for overplotting", {
  set.seed(123)
  
  df <- data.frame(
    x = rnorm(1000),
    y = rnorm(1000)
  )
  
  p <- ggplot(df, aes(x, y)) +
    geom_point(alpha = 0.25)
  
  result <- viz_accessibility(p)
  
  overplotting_issue <- result$issues[
    vapply(
      result$issues,
      function(x) x$check == "overplotting",
      logical(1L)
    )
  ]
  
  expect_true(length(overplotting_issue) > 0L)
  
  expect_equal(
    overplotting_issue[[1]]$level,
    "info"
  )
})


test_that("viz_accessibility rejects non-ggplot input", {
  expect_error(
    viz_accessibility(mtcars),
    "ggplot"
  )
})


test_that("viz_accessibility returns a summary", {
  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point()
  
  result <- viz_accessibility(p)
  
  expect_true(is.character(result$summary))
  expect_length(result$summary, 1)
})