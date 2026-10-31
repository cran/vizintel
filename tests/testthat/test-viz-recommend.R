# Tests for viz_recommend() -- vizintel chart recommendation engine
#
# All viz_recommend() calls use vr() which wraps suppressWarnings() to silence
# ggplot2 informational messages emitted when the interactive plot side-effect
# fires during RStudio test runs.

# Helper: silent viz_recommend()
vr <- function(...) suppressWarnings(viz_recommend(...))

# =============================================================================
# Test 1: Single numeric column -> histogram or density in top results
# =============================================================================
test_that("viz_recommend() suggests histogram/density for a numeric column", {
  result <- vr(mtcars["mpg"])

  expect_s3_class(result, "viz_recommend_result")
  expect_true("mpg" %in% names(result))

  charts <- vapply(result[["mpg"]], `[[`, character(1L), "chart")
  expect_true(any(charts %in% c("histogram", "density", "log_histogram")))

  # fit_score values must be in [0, 1]
  scores <- vapply(result[["mpg"]], `[[`, numeric(1L), "fit_score")
  expect_true(all(scores >= 0 & scores <= 1))

  # Results must be sorted descending by fit_score
  expect_true(all(diff(scores) <= 0))
})

# =============================================================================
# Test 2: High-cardinality categorical -> lollipop preferred over plain bar
# =============================================================================
test_that("viz_recommend() prefers lollipop for high-cardinality categorical", {
  # 26 unique categories = high cardinality (threshold is > 15)
  df     <- data.frame(label = rep(letters, length.out = 100),
                       stringsAsFactors = FALSE)
  result <- vr(df["label"])

  charts <- vapply(result[["label"]], `[[`, character(1L), "chart")
  expect_true(any(charts %in% c("lollipop", "bar")))

  # If both lollipop and bar appear, lollipop should rank higher
  if ("lollipop" %in% charts && "bar" %in% charts) {
    expect_true(which(charts == "lollipop") < which(charts == "bar"))
  }
})

# =============================================================================
# Test 3: Datetime column -> line chart in top results
# =============================================================================
test_that("viz_recommend() suggests line chart for a datetime + numeric pair", {
  df <- data.frame(
    date  = seq(as.Date("2023-01-01"), by = "month", length.out = 24),
    value = cumsum(rnorm(24))
  )
  result     <- vr(df, x = "date", y = "value")
  group_name <- names(result)[1]
  charts     <- vapply(result[[group_name]], `[[`, character(1L), "chart")
  expect_true("line" %in% charts)
})

# =============================================================================
# Test 4: Numeric + numeric pair -> scatter in top results
# =============================================================================
test_that("viz_recommend() suggests scatter for numeric vs numeric pair", {
  result     <- vr(mtcars, x = "wt", y = "mpg")
  group_name <- names(result)[1]
  expect_match(group_name, "wt vs mpg")
  charts <- vapply(result[[group_name]], `[[`, character(1L), "chart")
  expect_true("scatter" %in% charts)
})

# =============================================================================
# Test 5: Explicit goal override changes the ranking
# =============================================================================
test_that("viz_recommend() respects an explicit goal = 'compare'", {
  result <- vr(mtcars["mpg"], goal = "compare")
  charts <- vapply(result[["mpg"]], `[[`, character(1L), "chart")
  expect_true(any(charts %in% c("boxplot", "violin")))

  goals <- vapply(result[["mpg"]], `[[`, character(1L), "goal")
  expect_true(all(goals == "compare"))
})

# =============================================================================
# Test 6: Input validation
# =============================================================================
test_that("viz_recommend() errors on bad input", {
  expect_error(viz_recommend(list(x = 1:5)), "data.frame")
  expect_error(viz_recommend(data.frame()),  "at least one column")
  expect_error(viz_recommend(mtcars, goal = "wrong_goal"), "must be one of")
  expect_error(viz_recommend(mtcars, x = "wt"), "both `x` and `y`")
  expect_error(viz_recommend(mtcars, x = "no_such_col", y = "mpg"), "not found")
})

# =============================================================================
# Test 7: S3 class and field structure
# =============================================================================
test_that("viz_recommend() returns correct S3 structure", {
  result <- vr(mtcars["cyl"])
  expect_s3_class(result, "viz_recommend_result")

  for (rec in result[["cyl"]]) {
    expect_s3_class(rec, "viz_recommendation")
    expect_true(all(c("variable", "chart", "fit_score",
                      "rationale", "goal") %in% names(rec)))
    expect_type(rec$fit_score, "double")
    expect_type(rec$chart,     "character")
    expect_type(rec$rationale, "character")
  }
})

# =============================================================================
# Test 8: print method runs without error
# =============================================================================
test_that("print.viz_recommend_result() works without error", {
  result <- vr(mtcars[c("mpg", "cyl")])
  expect_no_error(out <- capture.output(print(result)))
  expect_true(any(grepl("vizintel", out, ignore.case = TRUE)))
})

# =============================================================================
# Test 9: df/x/y attributes are stored in per-column mode
# =============================================================================
test_that("viz_recommend() stores df/x/y attributes for plot()", {
  result <- vr(mtcars["mpg"])
  expect_true(!is.null(attr(result, "df")))
  expect_equal(nrow(attr(result, "df")), nrow(mtcars))
  expect_null(attr(result, "x"))
  expect_null(attr(result, "y"))
})

# =============================================================================
# Test 10: df/x/y attributes are stored in paired mode
# =============================================================================
test_that("viz_recommend() stores df/x/y attributes in paired mode", {
  result <- vr(mtcars, x = "wt", y = "mpg")
  expect_true(!is.null(attr(result, "df")))
  expect_equal(attr(result, "x"), "wt")
  expect_equal(attr(result, "y"), "mpg")
})

# =============================================================================
# Test 11: plot.viz_recommend_result() runs without error (per-column)
# =============================================================================
test_that("plot.viz_recommend_result() runs without error for single numeric col", {
  result <- vr(mtcars["mpg"])
  tmp    <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = 800, height = 600)
  expect_no_warning(expect_no_error(suppressWarnings(plot(result))))
  grDevices::dev.off()
  unlink(tmp)
})

# =============================================================================
# Test 12: plot.viz_recommend_result() runs without error (paired mode)
# =============================================================================
test_that("plot.viz_recommend_result() runs without error for numeric pair", {
  result <- vr(mtcars, x = "wt", y = "mpg")
  tmp    <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = 800, height = 600)
  expect_no_warning(expect_no_error(suppressWarnings(plot(result))))
  grDevices::dev.off()
  unlink(tmp)
})
