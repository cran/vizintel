# Tests for profile_data() — extended edge cases
# Covers: NA values, all-missing columns, constant columns,
#         integer/factor/logical/Date types, small samples, outliers.

pd <- vizintel:::profile_data

# =============================================================================
# NA handling
# =============================================================================

test_that("profile_data() counts NA values correctly", {
  # 5 values, 2 NA → 3 clean; skewness computable (n>=3) but kurtosis/Shapiro need n>=4
  df  <- data.frame(x = c(1.0, NA, 3.0, NA, 5.0))
  out <- pd(df)
  expect_equal(out$n_missing, 2L)
  # Skewness computable on 3 non-NA values (n>=3)
  expect_true(is.finite(out$skewness))
  # Kurtosis and Shapiro require n>=4; with only 3 clean values these stay NA
  expect_true(is.na(out$kurtosis))
  expect_true(is.na(out$shapiro_p))
})

test_that("profile_data() handles a column that is all NA", {
  df  <- data.frame(x = NA_real_)
  out <- pd(df)
  expect_equal(out$type, "numeric")
  expect_equal(out$n_missing, 1L)
  # With zero clean values, stats cannot be computed
  expect_true(is.na(out$skewness))
  expect_true(is.na(out$kurtosis))
  expect_true(is.na(out$is_normal))
  expect_true(is.na(out$shapiro_p))
})

test_that("profile_data() handles a character column that is all NA", {
  df  <- data.frame(x = NA_character_, stringsAsFactors = FALSE)
  out <- pd(df)
  expect_equal(out$type, "categorical")
  expect_equal(out$n_missing, 1L)
  expect_equal(out$cardinality, 0L)  # no non-NA unique values
})

# =============================================================================
# Variable type classification
# =============================================================================

test_that("profile_data() classifies integer columns as numeric", {
  df  <- data.frame(x = 1L:10L)
  out <- pd(df)
  expect_equal(out$type, "numeric")
})

test_that("profile_data() classifies factor columns as categorical", {
  df  <- data.frame(x = factor(c("a", "b", "a", "c")))
  out <- pd(df)
  expect_equal(out$type, "categorical")
  expect_equal(out$cardinality, 3L)
})

test_that("profile_data() classifies logical columns as categorical", {
  df  <- data.frame(x = c(TRUE, FALSE, TRUE, TRUE))
  out <- pd(df)
  expect_equal(out$type, "categorical")
  expect_equal(out$cardinality, 2L)
})

test_that("profile_data() classifies POSIXct columns as datetime", {
  df  <- data.frame(
    ts = as.POSIXct(
      c("2024-01-01 00:00", "2024-01-02 00:00", "2024-01-03 00:00"),
      tz = "UTC"
    )
  )
  out <- pd(df)
  expect_equal(out$type, "datetime")
})

test_that("profile_data() classifies Date columns as datetime", {
  df  <- data.frame(d = seq(as.Date("2024-01-01"), by = "day", length.out = 5))
  out <- pd(df)
  expect_equal(out$type, "datetime")
})

# =============================================================================
# Constant numeric columns
# =============================================================================

test_that("profile_data() handles a constant numeric column without error", {
  df  <- data.frame(x = rep(5.0, 10))
  # Must not error: skewness of constant = 0, kurtosis may be NaN,
  # shapiro.test on constant data throws an error caught by tryCatch
  expect_no_error(out <- pd(df))
  expect_equal(out$type, "numeric")
  expect_equal(out$n_missing, 0L)
  # Shapiro-Wilk errors on constant data → shapiro_p and is_normal stay NA
  expect_true(is.na(out$shapiro_p))
  expect_true(is.na(out$is_normal))
})

test_that("profile_data() handles a column with only one unique value (factor)", {
  df  <- data.frame(x = factor(rep("only", 5)))
  out <- pd(df)
  expect_equal(out$type, "categorical")
  expect_equal(out$cardinality, 1L)
})

# =============================================================================
# Small-sample Shapiro-Wilk
# =============================================================================

test_that("profile_data() computes Shapiro-Wilk for n = 4 without error", {
  df  <- data.frame(x = c(1.1, 2.2, 3.3, 4.4))
  expect_no_error(out <- pd(df))
  expect_equal(out$type, "numeric")
  expect_true(!is.na(out$shapiro_p))
  expect_true(out$shapiro_p >= 0 && out$shapiro_p <= 1)
})

test_that("profile_data() does NOT compute Shapiro-Wilk for n < 4", {
  # n=3: skewness computable but kurtosis and Shapiro need n>=4
  df  <- data.frame(x = c(1.0, 2.0, 3.0))
  expect_no_error(out <- pd(df))
  expect_true(is.finite(out$skewness))  # skewness needs n>=3
  expect_true(is.na(out$kurtosis))      # kurtosis needs n>=4
  expect_true(is.na(out$shapiro_p))     # Shapiro needs n>=4
  expect_true(is.na(out$is_normal))
})

# =============================================================================
# Skewed distributions
# =============================================================================

test_that("profile_data() detects a heavily right-skewed distribution", {
  set.seed(7)
  # log-normal is strongly right-skewed (skewness >> 1)
  df  <- data.frame(x = rlnorm(200, meanlog = 0, sdlog = 1.5))
  out <- pd(df)
  expect_equal(out$type, "numeric")
  expect_true(out$skewness > 1)
})

# =============================================================================
# Column with outliers — should not error
# =============================================================================

test_that("profile_data() handles a column with extreme outliers without error", {
  df  <- data.frame(x = c(rep(1, 50), 1e9))
  expect_no_error(out <- pd(df))
  expect_equal(out$type, "numeric")
  expect_true(is.finite(out$skewness))
})

# =============================================================================
# Multi-column data frame with NAs in mixed types
# =============================================================================

test_that("profile_data() processes a mixed frame with NAs across types correctly", {
  df <- data.frame(
    num  = c(1.0, NA, 3.0, 4.0, 5.0),
    cat  = c("a", "b", NA,  "b", "a"),
    date = as.Date(c("2024-01-01", "2024-01-02", NA, "2024-01-04", "2024-01-05")),
    stringsAsFactors = FALSE
  )
  out <- pd(df)
  expect_equal(nrow(out), 3L)
  expect_equal(out$n_missing, c(1L, 1L, 1L))
  expect_equal(out$type, c("numeric", "categorical", "datetime"))
})
