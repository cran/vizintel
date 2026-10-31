# Tests for viz_audit() — vizintel plot integrity auditor
# Fixtures: truncated bar chart, heavily overplotted scatter, clean scatter,
#           input validation, S3 class check, print method.

library(ggplot2)

# =============================================================================
# Fixture helpers
# =============================================================================

# Fixture 1: Bar chart with deliberately truncated y-axis
# Data has bars at y = 50 and y = 80; axis starts at 42 instead of 0.
make_truncated_bar <- function() {
  df <- data.frame(x = c("A", "B"), y = c(50, 80))
  ggplot(df, aes(x = x, y = y)) +
    geom_col() +
    coord_cartesian(ylim = c(42, 85))
}

# Fixture 2: Scatter plot with massive overplotting
# 1000 points drawn from only 5 distinct x values and 5 distinct y values
# -> 200 points per cell -> overlap fraction = 1.0 (all collide)
make_overplotted_scatter <- function() {
  set.seed(99)
  n  <- 1000L
  df <- data.frame(
    x = sample(1:5, n, replace = TRUE),
    y = sample(1:5, n, replace = TRUE)
  )
  ggplot(df, aes(x = x, y = y)) + geom_point()
}

# Fixture 3: Clean scatter — 50 well-spread points, natural axis limits
make_clean_scatter <- function() {
  set.seed(7)
  df <- data.frame(x = rnorm(50), y = rnorm(50))
  ggplot(df, aes(x = x, y = y)) + geom_point()
}

# =============================================================================
# Test 1: Truncated bar chart — critical issue flagged
# =============================================================================
test_that("viz_audit() flags a truncated bar chart y-axis as critical", {
  report <- viz_audit(make_truncated_bar())

  # At least one issue
  expect_true(report$n_issues >= 1L)

  # The truncated_axis check must be present
  check_ids <- vapply(report$issues, `[[`, character(1L), "check")
  expect_true("truncated_axis" %in% check_ids)

  # It must be critical
  trunc_issue <- report$issues[[which(check_ids == "truncated_axis")[1L]]]
  expect_equal(trunc_issue$severity, "critical")

  # Details should expose the numeric values
  expect_true(is.numeric(trunc_issue$details$y_min_displayed))
  expect_true(trunc_issue$details$y_min_displayed > 0)
})

# =============================================================================
# Test 2: Heavily overplotted scatter — warning issued
# =============================================================================
test_that("viz_audit() flags heavy overplotting with a warning", {
  report <- viz_audit(make_overplotted_scatter())

  check_ids <- vapply(report$issues, `[[`, character(1L), "check")
  expect_true("overplotting" %in% check_ids)

  op_issue <- report$issues[[which(check_ids == "overplotting")[1L]]]
  expect_equal(op_issue$severity, "warning")

  # Overlap fraction should be very high (>= 0.30 threshold)
  expect_true(op_issue$details$overlap_fraction >= 0.30)
})

# =============================================================================
# Test 3: Clean scatter — no critical issues
# =============================================================================
test_that("viz_audit() finds no critical issues in a clean scatter plot", {
  report <- viz_audit(make_clean_scatter())
  expect_equal(report$n_critical, 0L)
})

# =============================================================================
# Test 4: Input validation — informative error on non-ggplot
# =============================================================================
test_that("viz_audit() throws an informative error for non-ggplot input", {
  expect_error(viz_audit(list(x = 1:5)),  "ggplot object")
  expect_error(viz_audit("a string"),      "ggplot object")
  expect_error(viz_audit(NULL),            "ggplot object")
})

# =============================================================================
# Test 5: S3 class — result is a viz_audit_report
# =============================================================================
test_that("viz_audit() returns an object of class 'viz_audit_report'", {
  report <- viz_audit(make_clean_scatter())
  expect_s3_class(report, "viz_audit_report")

  # Required fields present
  expect_true(all(c("n_issues", "n_critical", "n_warning", "n_info",
                    "issues", "plot_geoms") %in% names(report)))

  # Count fields are non-negative integers
  expect_true(report$n_issues   >= 0L)
  expect_true(report$n_critical >= 0L)
  expect_true(report$n_warning  >= 0L)
  expect_true(report$n_info     >= 0L)

  # n_issues == sum of severities
  expect_equal(
    report$n_issues,
    report$n_critical + report$n_warning + report$n_info
  )
})

# =============================================================================
# Test 6: print method — prints without error and returns invisibly
# =============================================================================
test_that("print.viz_audit_report() works without error", {
  report <- viz_audit(make_truncated_bar())

  # print() should not throw
  expect_no_error(out <- capture.output(print(report)))

  # Output should mention "audit report"
  expect_true(any(grepl("audit report", out, ignore.case = TRUE)))

  # summary() should also work
  expect_no_error(capture.output(summary(report)))
})

# =============================================================================
# Test 7 (bonus): summary method returns correct status strings
# =============================================================================
test_that("summary.viz_audit_report() prints FAIL for critical issues", {
  report <- viz_audit(make_truncated_bar())
  if (report$n_critical > 0L) {
    out <- capture.output(summary(report))
    expect_true(any(grepl("FAIL", out)))
  }
})

# =============================================================================
# Test 8 (bonus): Clean bar chart (no truncation) — no truncated_axis issue
# =============================================================================
test_that("viz_audit() does NOT flag a properly-zeroed bar chart", {
  df     <- data.frame(x = c("A", "B"), y = c(50, 80))
  p      <- ggplot(df, aes(x = x, y = y)) + geom_col()   # default: y from 0
  report <- viz_audit(p)

  check_ids <- vapply(report$issues, `[[`, character(1L), "check")
  expect_false("truncated_axis" %in% check_ids)
})

# =============================================================================
# Test 9: explanation field is present on every issue
# =============================================================================
test_that("viz_audit() issues all carry a non-empty explanation field", {
  # truncated bar
  report <- viz_audit(make_truncated_bar())
  for (issue in report$issues) {
    expect_true(!is.null(issue$explanation),
                info = paste("No explanation on check:", issue$check))
    expect_true(nzchar(issue$explanation),
                info = paste("Empty explanation on check:", issue$check))
  }

  # overplotted scatter
  report2 <- viz_audit(make_overplotted_scatter())
  for (issue in report2$issues) {
    expect_true(!is.null(issue$explanation))
    expect_true(nzchar(issue$explanation))
  }
})

# =============================================================================
# Test 10: print output contains "In plain terms:" for issues with explanation
# =============================================================================
test_that("print.viz_audit_report() shows 'In plain terms:' for each issue", {
  report <- viz_audit(make_truncated_bar())
  if (report$n_issues > 0L) {
    out <- capture.output(print(report))
    expect_true(any(grepl("In plain terms", out, ignore.case = TRUE)))
  }
})

# =============================================================================
# Test 11: explanation for truncated_axis mentions zero / bars
# =============================================================================
test_that("truncated_axis explanation mentions bars or zero", {
  report    <- viz_audit(make_truncated_bar())
  check_ids <- vapply(report$issues, `[[`, character(1L), "check")
  if ("truncated_axis" %in% check_ids) {
    trunc_issue <- report$issues[[which(check_ids == "truncated_axis")[1L]]]
    # Should mention something about zero or bars or misleading
    has_key_word <- grepl("zero|bar|mislead", trunc_issue$explanation,
                          ignore.case = TRUE)
    expect_true(has_key_word)
  }
})

