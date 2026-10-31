#' Profile a data frame's columns
#'
#' An internal engine that inspects every column of a data frame, classifies it
#' as `"numeric"`, `"categorical"`, or `"datetime"`, and computes type-specific
#' summary statistics. The result is a tidy data frame with one row per
#' variable -- intended to feed downstream visualization-recommendation logic.
#'
#' @param df A `data.frame` (or `tibble`). Must have at least one column.
#'
#' @return A `data.frame` with one row per column of `df` and the following
#'   nine columns:
#'
#'   | Column | Type | Description |
#'   |--------|------|-------------|
#'   | `variable` | character | Column name |
#'   | `type` | character | `"numeric"`, `"categorical"`, or `"datetime"` |
#'   | `n_missing` | integer | Number of `NA` values |
#'   | `skewness` | numeric | Skewness (numeric, n >= 3 only; `NA` otherwise) |
#'   | `kurtosis` | numeric | Excess kurtosis (numeric, n >= 4 only; `NA` otherwise) |
#'   | `is_normal` | logical | `TRUE` if Shapiro-Wilk p > 0.05 and 4 <= n < 5000; `NA` otherwise |
#'   | `shapiro_p` | numeric | Shapiro-Wilk p-value (numeric, 4 <= n < 5000 only; `NA` otherwise) |
#'   | `cardinality` | integer | Number of distinct non-`NA` values (categorical only; `NA` otherwise) |
#'   | `is_regular` | logical | `TRUE` if datetime intervals are all equal; `NA` for non-datetime columns |
#'
#' @importFrom e1071 skewness kurtosis
#' @keywords internal
profile_data <- function(df) {
  # -- Input validation -------------------------------------------------------
  if (!is.data.frame(df)) {
    stop("`df` must be a data.frame or tibble.", call. = FALSE)
  }
  if (ncol(df) == 0L) {
    stop("`df` must have at least one column.", call. = FALSE)
  }

  # -- Per-column profiling ---------------------------------------------------
  rows <- lapply(names(df), function(col_name) {
    x <- df[[col_name]]

    # Count missing values before dropping them
    n_missing <- sum(is.na(x))
    x_clean   <- x[!is.na(x)]

    # -- Classify column type --------------------------------------------------
    col_type <- classify_column(x)

    # -- Defaults (NA for fields not applicable to this type) ------------------
    skew_val    <- NA_real_
    kurt_val    <- NA_real_
    is_normal   <- NA
    shapiro_p   <- NA_real_
    cardinality <- NA_integer_
    is_regular  <- NA

    # -- Type-specific computations --------------------------------------------
    if (col_type == "numeric") {
      n_clean <- length(x_clean)

      if (n_clean >= 3L) {
        skew_val <- suppressWarnings(e1071::skewness(x_clean, type = 2))
      }
      if (n_clean >= 4L) {
        kurt_val <- suppressWarnings(e1071::kurtosis(x_clean, type = 2))
      }

      if (n_clean >= 4L && n_clean < 5000L) {
        sw_result <- tryCatch(
          suppressWarnings(stats::shapiro.test(x_clean)),
          error = function(e) NULL
        )
        if (!is.null(sw_result)) {
          shapiro_p <- sw_result$p.value
          is_normal <- isTRUE(shapiro_p > 0.05)
        }
      }

    } else if (col_type == "categorical") {
      cardinality <- length(unique(x_clean))

    } else if (col_type == "datetime") {
      if (length(x_clean) >= 2L) {
        diffs      <- diff(sort(as.numeric(x_clean)))
        is_regular <- (length(unique(diffs)) == 1L)
      }
    }

    # -- Assemble row ----------------------------------------------------------
    data.frame(
      variable    = col_name,
      type        = col_type,
      n_missing   = n_missing,
      skewness    = skew_val,
      kurtosis    = kurt_val,
      is_normal   = is_normal,
      shapiro_p   = shapiro_p,
      cardinality = cardinality,
      is_regular  = is_regular,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

# -- Helper: classify a single vector ----------------------------------------
#
# Returns one of "numeric", "categorical", or "datetime".
# @noRd
classify_column <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
    "datetime"
  } else if (is.numeric(x) || is.integer(x)) {
    "numeric"
  } else {
    # character, factor, logical, ordered, etc.
    "categorical"
  }
}
