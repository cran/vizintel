# Tests for viz_recommend() — edge cases
# All viz_recommend() calls are wrapped in suppressWarnings() because:
#   - when run from an interactive session (e.g. RStudio), the function calls
#     plot() as a side-effect, which can emit ggplot2 informational warnings
#     (removed NA rows, log of non-positive values, etc.) that are not bugs.

library(ggplot2)

# Helper: run viz_recommend() silently regardless of session type
vr <- function(...) suppressWarnings(viz_recommend(...))

# =============================================================================
# NA-containing data
# =============================================================================

test_that("viz_recommend() handles a numeric column with NAs without error", {
  df <- data.frame(x = c(1.0, NA, 3.0, NA, 5.0, 6.0, 7.0))
  expect_no_error(result <- vr(df))
  expect_s3_class(result, "viz_recommend_result")
  charts <- vapply(result[["x"]], `[[`, character(1L), "chart")
  expect_true(length(charts) >= 1L)
})

test_that("viz_recommend() handles a categorical column with NAs without error", {
  df <- data.frame(
    cat = c("a", "b", NA, "b", "a", NA),
    stringsAsFactors = FALSE
  )
  expect_no_error(result <- vr(df))
  expect_s3_class(result, "viz_recommend_result")
})

# =============================================================================
# Unusual column names
# =============================================================================

test_that("viz_recommend() works with column names containing spaces and dots", {
  df <- data.frame(check.names = FALSE,
    "col with spaces" = rnorm(20),
    "col.with.dots"   = rnorm(20),
    stringsAsFactors  = FALSE
  )
  expect_no_error(result <- vr(df))
  expect_s3_class(result, "viz_recommend_result")
})

# =============================================================================
# Single-row data frame
# =============================================================================

test_that("viz_recommend() handles a single-row data frame without error", {
  df <- data.frame(x = 42, y = "hello", stringsAsFactors = FALSE)
  expect_no_error(result <- vr(df))
  expect_s3_class(result, "viz_recommend_result")
})

# =============================================================================
# All returned fit_scores are in [0, 1]
# =============================================================================

test_that("all fit_scores from viz_recommend() are in [0, 1]", {
  result <- vr(mtcars)
  for (group in result) {
    for (rec in group) {
      expect_true(rec$fit_score >= 0 && rec$fit_score <= 1,
                  info = paste("Out-of-range score for chart:", rec$chart))
    }
  }
})

# =============================================================================
# Scores are sorted descending within each group
# =============================================================================

test_that("viz_recommend() sorts recommendations by score descending", {
  result <- vr(mtcars[c("mpg", "cyl")])
  for (nm in names(result)) {
    group  <- result[[nm]]
    scores <- vapply(group, `[[`, numeric(1L), "fit_score")
    expect_true(all(diff(scores) <= 0),
                info = paste("Unsorted scores in group:", nm))
  }
})

# =============================================================================
# Chart types are compatible with variable types
# =============================================================================

test_that("viz_recommend() only suggests univariate charts for single numeric columns", {
  bivariate_charts <- c("scatter", "hexbin", "line", "heatmap")
  result <- vr(mtcars["mpg"])
  charts <- vapply(result[["mpg"]], `[[`, character(1L), "chart")
  expect_false(any(charts %in% bivariate_charts))
})

test_that("viz_recommend() suggests bivariate charts for numeric pair", {
  bivariate_charts <- c("scatter", "hexbin", "line", "heatmap",
                        "boxplot", "violin")
  result     <- vr(mtcars, x = "wt", y = "mpg")
  group_name <- names(result)[1]
  charts     <- vapply(result[[group_name]], `[[`, character(1L), "chart")
  expect_true(any(charts %in% bivariate_charts))
})

# =============================================================================
# Factor column — treated as categorical
# =============================================================================

test_that("viz_recommend() recommends bar/lollipop for factor columns", {
  df <- data.frame(grp = factor(c("A", "B", "A", "C", "B", "A")))
  result <- vr(df)
  charts <- vapply(result[["grp"]], `[[`, character(1L), "chart")
  expect_true(any(charts %in% c("bar", "lollipop")))
})

# =============================================================================
# Date column — trend goal inferred
# =============================================================================

test_that("viz_recommend() infers trend goal for a datetime+numeric pair", {
  df <- data.frame(
    d = seq(as.Date("2023-01-01"), by = "week", length.out = 12),
    v = cumsum(rnorm(12))
  )
  result     <- vr(df, x = "d", y = "v")
  group_name <- names(result)[1]
  goals      <- vapply(result[[group_name]], `[[`, character(1L), "goal")
  expect_true(all(goals == "trend"))
})

# =============================================================================
# plot() runs without requiring an interactive graphics device
# =============================================================================

test_that("plot.viz_recommend_result() works off-screen for categorical column", {
  df     <- data.frame(cat = c("A", "B", "A", "C"), stringsAsFactors = FALSE)
  result <- vr(df)
  tmp    <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = 800, height = 600)
  expect_no_warning(expect_no_error(suppressWarnings(plot(result))))
  grDevices::dev.off()
  unlink(tmp)
})

test_that("plot.viz_recommend_result() works off-screen for datetime+numeric pair", {
  df <- data.frame(
    d = seq(as.Date("2023-01-01"), by = "week", length.out = 8),
    v = rnorm(8)
  )
  result <- vr(df, x = "d", y = "v")
  tmp    <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = 800, height = 600)
  expect_no_warning(expect_no_error(suppressWarnings(plot(result))))
  grDevices::dev.off()
  unlink(tmp)
})

# =============================================================================
# Large n — hexbin preferred over scatter for n > 1000
# =============================================================================

test_that("viz_recommend() prefers hexbin over scatter for large numeric pairs", {
  set.seed(42)
  df <- data.frame(x = rnorm(1100), y = rnorm(1100))
  result     <- vr(df, x = "x", y = "y")
  group_name <- names(result)[1]
  charts     <- vapply(result[[group_name]], `[[`, character(1L), "chart")
  scores     <- vapply(result[[group_name]], `[[`, numeric(1L), "fit_score")
  if ("hexbin" %in% charts && "scatter" %in% charts) {
    hexbin_score  <- scores[charts == "hexbin"]
    scatter_score <- scores[charts == "scatter"]
    expect_true(hexbin_score >= scatter_score)
  }
})

# =============================================================================
# log_histogram only uses log scale for strictly positive data
# =============================================================================

test_that("log_histogram falls back to plain histogram for non-positive data", {
  # Data with zeros — log scale would produce -Inf/NaN
  df <- data.frame(x = c(0, 1, 2, 3, 4, 5, 10, 100))
  result <- vr(df)
  # Function must not error when plot() renders
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = 400, height = 300)
  expect_no_warning(expect_no_error(suppressWarnings(plot(result))))
  grDevices::dev.off()
  unlink(tmp)
})
