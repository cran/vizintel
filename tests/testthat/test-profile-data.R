# Tests for profile_data() — vizintel internal engine
# Covers: numeric, high-cardinality categorical, datetime, and large-numeric (n>=5000)

# ── Helper: access the internal function ─────────────────────────────────────
# profile_data() is internal; use ::: in tests.
pd <- vizintel:::profile_data

# =============================================================================
# Test 1: Numeric column
# =============================================================================
test_that("profile_data() correctly profiles a numeric column", {
  set.seed(42)
  df  <- data.frame(x = rnorm(80))
  out <- pd(df)

  # One row returned
  expect_equal(nrow(out), 1L)

  # Column names present
  expect_named(
    out,
    c("variable", "type", "n_missing", "skewness", "kurtosis",
      "is_normal", "shapiro_p", "cardinality", "is_regular")
  )

  # Type classification
  expect_equal(out$variable, "x")
  expect_equal(out$type, "numeric")

  # No missings in clean data
  expect_equal(out$n_missing, 0L)

  # Distributional stats are finite numbers
  expect_true(is.finite(out$skewness))
  expect_true(is.finite(out$kurtosis))

  # Normality flag is a logical (TRUE or FALSE), not NA, for n < 5000
  expect_true(is.logical(out$is_normal))
  expect_false(is.na(out$is_normal))

  # Shapiro p-value is in [0, 1]
  expect_true(out$shapiro_p >= 0 && out$shapiro_p <= 1)

  # Non-applicable fields are NA
  expect_true(is.na(out$cardinality))
  expect_true(is.na(out$is_regular))
})

# =============================================================================
# Test 2: Categorical column with high cardinality
# =============================================================================
test_that("profile_data() correctly profiles a high-cardinality categorical column", {
  df  <- data.frame(id = as.character(1:200), stringsAsFactors = FALSE)
  out <- pd(df)

  # One row returned
  expect_equal(nrow(out), 1L)

  # Type classification
  expect_equal(out$variable, "id")
  expect_equal(out$type, "categorical")

  # Cardinality equals number of unique values
  expect_equal(out$cardinality, 200L)

  # Non-applicable fields are NA
  expect_true(is.na(out$skewness))
  expect_true(is.na(out$kurtosis))
  expect_true(is.na(out$is_normal))
  expect_true(is.na(out$shapiro_p))
  expect_true(is.na(out$is_regular))
})

# =============================================================================
# Test 3: Datetime column with regular intervals
# =============================================================================
test_that("profile_data() correctly profiles a regular datetime column", {
  df  <- data.frame(
    ts = seq(as.Date("2020-01-01"), by = "day", length.out = 30)
  )
  out <- pd(df)

  # One row returned
  expect_equal(nrow(out), 1L)

  # Type classification
  expect_equal(out$variable, "ts")
  expect_equal(out$type, "datetime")

  # Regular intervals detected
  expect_true(isTRUE(out$is_regular))

  # Non-applicable fields are NA
  expect_true(is.na(out$skewness))
  expect_true(is.na(out$kurtosis))
  expect_true(is.na(out$is_normal))
  expect_true(is.na(out$shapiro_p))
  expect_true(is.na(out$cardinality))
})

# =============================================================================
# Test 4 (bonus): Datetime column with IRREGULAR intervals
# =============================================================================
test_that("profile_data() detects irregular datetime intervals", {
  df  <- data.frame(
    ts = as.Date(c("2020-01-01", "2020-01-03", "2020-01-10", "2020-02-01"))
  )
  out <- pd(df)
  expect_equal(out$type, "datetime")
  expect_false(isTRUE(out$is_regular))
})

# =============================================================================
# Test 5 (bonus): Large numeric column — is_normal and shapiro_p must be NA
# =============================================================================
test_that("profile_data() skips Shapiro-Wilk for n >= 5000", {
  set.seed(1)
  df  <- data.frame(big = rnorm(5000))
  out <- pd(df)

  expect_equal(out$type, "numeric")
  expect_true(is.na(out$is_normal))
  expect_true(is.na(out$shapiro_p))

  # Skewness and kurtosis are still computed
  expect_true(is.finite(out$skewness))
  expect_true(is.finite(out$kurtosis))
})

# =============================================================================
# Test 6 (bonus): Mixed data frame returns correct number of rows and types
# =============================================================================
test_that("profile_data() handles a mixed data frame correctly", {
  df <- data.frame(
    score  = c(3.1, 2.7, 4.0, 3.5, 2.9),
    grade  = c("A", "B", "A", "C", "B"),
    date   = seq(as.Date("2024-01-01"), by = "week", length.out = 5),
    stringsAsFactors = FALSE
  )
  out <- pd(df)

  expect_equal(nrow(out), 3L)
  expect_equal(out$type, c("numeric", "categorical", "datetime"))
  expect_equal(out$variable, c("score", "grade", "date"))
})

# =============================================================================
# Test 7: Input validation
# =============================================================================
test_that("profile_data() errors on non-data-frame input", {
  expect_error(pd(list(x = 1:5)), "`df` must be a data.frame")
  expect_error(pd(data.frame()), "`df` must have at least one column")
})
