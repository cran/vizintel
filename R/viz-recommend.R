# =============================================================================
# viz_recommend() -- Chart Recommendation Engine
# =============================================================================

#' Recommend chart types for a data frame's columns
#'
#' Calls `profile_data()` internally to understand each column, then scores
#' candidate chart types from an internal taxonomy against the column profile
#' and any goal or variable hint the user provides. Returns a ranked list of
#' recommendations, highest scoring first.
#'
#' @param df A `data.frame` or tibble with at least one column.
#' @param goal One of `"distribution"`, `"compare"`, `"trend"`,
#'   `"relationship"`, `"composition"`, or `NULL` (default). When `NULL`,
#'   the best-fitting goal is inferred from the column types.
#' @param x Name of the column to use as the x variable (character). When
#'   supplied together with `y`, a paired recommendation is returned for that
#'   specific combination instead of per-column recommendations.
#' @param y Name of the column to use as the y variable (character). See `x`.
#'
#' @details
#' ## How scoring works
#'
#' An internal taxonomy table lists chart types alongside the conditions under
#' which each chart is a good fit (variable types, goal, skewness, cardinality,
#' data size). Each applicable chart starts with a base score and is adjusted:
#'
#' - +0.15 if the goal matches exactly rather than being a generic match
#' - -0.20 if the data is skewed (|skewness| > 1) and the chart does not
#'   handle skew well
#' - -0.20 if the column has more than 15 unique categories and the chart
#'   becomes unreadable at that cardinality
#' - -0.15 if there are more than 1000 rows and the chart does not scale well
#'
#' Charts with a final score below 0.10 are dropped. The top 3 per
#' variable/pair are returned.
#'
#' ## Taxonomy overview
#'
#' | Chart | Best for |
#' |-------|---------|
#' | `histogram` | Single numeric, distribution |
#' | `log_histogram` | Skewed numeric (suggests log scale) |
#' | `density` | Single numeric, smooth distribution shape |
#' | `boxplot` | Numeric + categorical group comparison |
#' | `violin` | Same as boxplot, better for larger samples |
#' | `bar` | Categorical counts or low-cardinality cat vs numeric |
#' | `lollipop` | High-cardinality categorical (bar gets overcrowded) |
#' | `scatter` | Numeric vs numeric, relationship |
#' | `hexbin` | Numeric vs numeric with many points (overplotting risk) |
#' | `line` | Datetime vs numeric, trend over time |
#' | `heatmap` | Two categoricals or categorical vs datetime |
#'
#' @return A list of class `"viz_recommend_result"`. Each element is a list of
#'   class `"viz_recommendation"` with fields:
#'   \describe{
#'     \item{`variable`}{Column name, or `"x vs y"` string for pairs.}
#'     \item{`chart`}{Chart type string.}
#'     \item{`fit_score`}{Numeric in \[0, 1\]. Higher is better.}
#'     \item{`rationale`}{One sentence explaining why this chart fits.}
#'     \item{`goal`}{The goal used when scoring this recommendation.}
#'   }
#'
#'   The result also has attributes `df`, `x`, and `y` (the data and variable
#'   names stored so that `plot()` can be called on the result directly,
#'   without passing data again).
#'
#'   **Side effect:** when called in an interactive session, the recommended
#'   charts are automatically rendered in the Plots pane via
#'   `plot.viz_recommend_result()`. You can also call `plot()` manually on the
#'   returned object at any time.
#'
#' @examples
#' \donttest{
#' # Per-column recommendations for the whole data frame
#' viz_recommend(mtcars)
#' }
#'
#' # Two specific columns as a pair
#' viz_recommend(mtcars, x = "wt", y = "mpg")
#'
#' # Force a goal
#' viz_recommend(mtcars["mpg"], goal = "distribution")
#'
#' @importFrom dplyr filter arrange desc
#' @export
viz_recommend <- function(df, goal = NULL, x = NULL, y = NULL) {

  # -- Input validation ---------------------------------------------------------
  if (!is.data.frame(df)) {
    stop("`df` must be a data.frame or tibble.", call. = FALSE)
  }
  if (ncol(df) == 0L) {
    stop("`df` must have at least one column.", call. = FALSE)
  }

  valid_goals <- c("distribution", "compare", "trend",
                   "relationship", "composition")
  if (!is.null(goal) && !goal %in% valid_goals) {
    stop(
      "`goal` must be one of: ",
      paste(valid_goals, collapse = ", "),
      ". Got: ", goal,
      call. = FALSE
    )
  }

  if (xor(is.null(x), is.null(y))) {
    stop("Supply both `x` and `y`, or neither.", call. = FALSE)
  }

  if (!is.null(x) && !x %in% names(df)) {
    stop("`x` column '", x, "' not found in `df`.", call. = FALSE)
  }
  if (!is.null(y) && !y %in% names(df)) {
    stop("`y` column '", y, "' not found in `df`.", call. = FALSE)
  }

  # -- Profile the data once ----------------------------------------------------
  profile <- profile_data(df)
  tax     <- build_taxonomy()
  n_rows  <- nrow(df)

  # -- Paired mode: one recommendation for x vs y -------------------------------
  if (!is.null(x) && !is.null(y)) {
    px    <- profile[profile$variable == x, ]
    py    <- profile[profile$variable == y, ]
    label <- paste(x, "vs", y)
    recs  <- score_candidates(
      x_type    = px$type,
      y_type    = py$type,
      x_skewed  = is_skewed(px),
      y_skewed  = is_skewed(py),
      x_card    = px$cardinality,
      goal      = infer_goal(px$type, py$type, goal),
      n_rows    = n_rows,
      taxonomy  = tax,
      label     = label
    )
    res <- structure(list(recs), names = label,
                     class = "viz_recommend_result")
    attr(res, "df") <- df
    attr(res, "x")  <- x
    attr(res, "y")  <- y
    if (interactive()) plot(res)
    return(res)
  }

  # -- Per-column mode: one recommendation group per column ---------------------
  results <- lapply(seq_len(nrow(profile)), function(i) {
    row           <- profile[i, ]
    inferred_goal <- infer_goal(row$type, NA_character_, goal)
    score_candidates(
      x_type   = row$type,
      y_type   = NA_character_,
      x_skewed = is_skewed(row),
      y_skewed = FALSE,
      x_card   = row$cardinality,
      goal     = inferred_goal,
      n_rows   = n_rows,
      taxonomy = tax,
      label    = row$variable
    )
  })
  names(results) <- profile$variable

  # Store original data and variable names for plot()
  res <- structure(results, class = "viz_recommend_result")
  attr(res, "df") <- df
  attr(res, "x")  <- x
  attr(res, "y")  <- y
  if (interactive()) plot(res)
  return(res)
}


# =============================================================================
# S3 plot method for viz_recommend_result
# =============================================================================

#' Plot recommended charts from a viz_recommend_result
#'
#' Renders each recommended chart type as a real `ggplot` using the data
#' stored inside the result object, then arranges all panels into a single
#' view with [gridExtra::grid.arrange()]. Each panel is labelled with the
#' chart type and its fit score.
#'
#' Call `plot()` directly on the object returned by [viz_recommend()] -- the
#' data is stored automatically so you do not need to pass it again.
#'
#' @param x A `viz_recommend_result` object returned by [viz_recommend()].
#' @param ... Additional arguments passed to [gridExtra::grid.arrange()]
#'   (e.g. `ncol`, `nrow`).
#'
#' @return Invisibly returns `NULL`. Called for its side-effect of rendering
#'   the arranged plot grid.
#'
#' @examples
#' \donttest{
#' res <- viz_recommend(mtcars["mpg"])
#' plot(res)
#'
#' # Paired mode -- scatter / hexbin arranged together
#' res2 <- viz_recommend(mtcars, x = "wt", y = "mpg")
#' plot(res2)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_histogram geom_density geom_boxplot
#'   geom_violin geom_bar geom_point geom_hex geom_line geom_bin2d
#'   scale_x_log10 ggtitle coord_flip
#' @importFrom rlang .data
#' @importFrom gridExtra grid.arrange
#' @export
plot.viz_recommend_result <- function(x, ...) {
  df   <- attr(x, "df")
  xvar <- attr(x, "x")
  yvar <- attr(x, "y")

  if (is.null(df)) {
    warning("No data found in result object. Re-run viz_recommend() to enable plotting.")
    return(invisible(NULL))
  }

  # Are we in paired mode?  (xvar/yvar are real column names)
  is_pair <- !is.null(xvar) && !is.null(yvar)

  plots <- list()

  for (group_name in names(x)) {
    recs <- x[[group_name]]
    for (rec in recs) {
      # In per-column mode rec$variable is a real column name.
      # In paired mode it is the display label "X vs Y" -- not a column.
      # Use xvar/yvar as the actual columns when in paired mode.
      x_col <- if (is_pair) xvar else rec$variable
      y_col <- if (is_pair) yvar else NULL

      p <- switch(rec$chart,

        # ---- univariate / x-only charts ------------------------------------
        "histogram" = {
          ggplot(df, aes(x = .data[[x_col]])) +
            geom_histogram(bins = 30)
        },
        "log_histogram" = {
          x_vals <- df[[x_col]]
          if (all(is.finite(x_vals[!is.na(x_vals)]) & x_vals[!is.na(x_vals)] > 0)) {
            ggplot(df, aes(x = .data[[x_col]])) +
              geom_histogram(bins = 30) +
              scale_x_log10()
          } else {
            ggplot(df, aes(x = .data[[x_col]])) +
              geom_histogram(bins = 30) +
              ggtitle("log_histogram (log scale skipped: data contains non-positive values)")
          }
        },
        "density" = {
          ggplot(df, aes(x = .data[[x_col]])) +
            geom_density()
        },

        # ---- boxplot: univariate or grouped (cat x + numeric y) ------------
        "boxplot" = {
          if (is_pair) {
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]])) +
              geom_boxplot()
          } else {
            ggplot(df, aes(x = factor(""), y = .data[[x_col]])) +
              geom_boxplot() +
              ggplot2::labs(x = NULL)
          }
        },

        # ---- violin: univariate or grouped ---------------------------------
        "violin" = {
          if (is_pair) {
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]])) +
              geom_violin()
          } else {
            ggplot(df, aes(x = factor(""), y = .data[[x_col]])) +
              geom_violin() +
              ggplot2::labs(x = NULL)
          }
        },

        # ---- categorical count charts --------------------------------------
        "bar" = {
          ggplot(df, aes(x = .data[[x_col]])) +
            geom_bar()
        },
        "lollipop" = {
          ggplot(df, aes(x = .data[[x_col]])) +
            geom_bar() +
            coord_flip()
        },

        # ---- bivariate charts (always need x + y) -------------------------
        "scatter" = {
          if (is_pair) {
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]])) +
              geom_point()
          } else {
            ggplot() + ggtitle("scatter: supply x and y to viz_recommend()")
          }
        },
        "hexbin" = {
          if (is_pair) {
            if (requireNamespace("hexbin", quietly = TRUE)) {
              ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]])) +
                geom_hex()
            } else {
              ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]])) +
                geom_point() +
                ggtitle("hexbin (hexbin pkg not installed -- showing scatter)")
            }
          } else {
            ggplot() + ggtitle("hexbin: supply x and y to viz_recommend()")
          }
        },
        "line" = {
          if (is_pair) {
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]])) +
              geom_line()
          } else {
            ggplot() + ggtitle("line: supply x and y to viz_recommend()")
          }
        },
        "heatmap" = {
          if (is_pair) {
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]])) +
              geom_bin2d()
          } else {
            ggplot() + ggtitle("heatmap: supply x and y to viz_recommend()")
          }
        },

        # ---- fallback ------------------------------------------------------
        ggplot() + ggtitle(paste("Unsupported chart:", rec$chart))
      )

      p <- p + ggtitle(paste0(rec$chart, " (score: ", rec$fit_score, ")"))
      plots[[length(plots) + 1L]] <- p
    }
  }

  if (length(plots) == 0L) return(invisible(NULL))
  suppressWarnings(gridExtra::grid.arrange(grobs = plots, ...))
  invisible(NULL)
}


#' Print a viz_recommend_result
#'
#' @param x A `viz_recommend_result` object returned by `viz_recommend()`.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.viz_recommend_result <- function(x, ...) {
  cat("== vizintel chart recommendations ==\n\n")
  for (group_name in names(x)) {
    recs <- x[[group_name]]
    if (length(recs) == 0L) {
      cat(" ", group_name, "->  (no recommendations)\n\n")
      next
    }
    first <- TRUE
    for (rec in recs) {
      prefix <- if (first) sprintf("  %-20s ->  ", group_name) else
                            sprintf("  %-20s     ", "")
      cat(sprintf("%s%-14s (score: %.2f)\n", prefix, rec$chart, rec$fit_score))
      cat(sprintf("  %-20s     \"%s\"\n", "", rec$rationale))
      first <- FALSE
    }
    cat("\n")
  }
  invisible(x)
}


# =============================================================================
# Internal: taxonomy table
# =============================================================================

# build_taxonomy() returns a data.frame where every row is one rule.
# score_candidates() filters and scores rows from this table.
# New chart types can be added here without touching any other code.
# @noRd
build_taxonomy <- function() {
  data.frame(
    chart        = c(
      "histogram",   "log_histogram", "density",
      "boxplot",     "violin",
      "boxplot",     "violin",
      "bar",         "lollipop",
      "scatter",     "hexbin",
      "line",
      "heatmap"
    ),
    x_type       = c(
      "numeric",     "numeric",      "numeric",
      "categorical", "categorical",
      "numeric",     "numeric",
      "categorical", "categorical",
      "numeric",     "numeric",
      "datetime",
      "categorical"
    ),
    y_type       = c(
      NA,            NA,             NA,
      "numeric",     "numeric",
      NA,            NA,
      NA,            NA,
      "numeric",     "numeric",
      "numeric",
      "categorical"
    ),
    goal         = c(
      "distribution", "distribution", "distribution",
      "compare",       "compare",
      "compare",       "compare",
      "compare",       "compare",
      "relationship",  "relationship",
      "trend",
      "composition"
    ),
    skewed_ok    = c(
      FALSE,  TRUE,   TRUE,
      TRUE,   TRUE,
      TRUE,   TRUE,
      TRUE,   TRUE,
      TRUE,   TRUE,
      TRUE,
      TRUE
    ),
    high_card_ok = c(
      TRUE,   TRUE,   TRUE,
      FALSE,  FALSE,
      TRUE,   TRUE,
      FALSE,  TRUE,
      TRUE,   TRUE,
      TRUE,
      FALSE
    ),
    large_n_ok   = c(
      TRUE,   TRUE,   TRUE,
      TRUE,   TRUE,
      TRUE,   TRUE,
      FALSE,  FALSE,
      FALSE,  TRUE,
      TRUE,
      TRUE
    ),
    base_score   = c(
      0.85,   0.75,   0.80,
      0.85,   0.80,
      0.80,   0.78,
      0.80,   0.75,
      0.88,   0.80,
      0.90,
      0.78
    ),
    rationale    = c(
      "Shows the full distribution of a numeric variable.",
      "Better than a histogram when the data is heavily right- or left-skewed.",
      "Smoother alternative to a histogram for continuous data.",
      "Compares a numeric variable across groups; works well for small-to-medium n.",
      "Like a boxplot but shows distribution shape; useful when n is larger.",
      "Shows spread and outliers for a single numeric variable.",
      "Shows spread and distribution shape for a single numeric variable.",
      "Counts or aggregated values for each category; best with few categories.",
      "Cleaner than a bar chart when there are many categories.",
      "Shows the relationship between two numeric variables.",
      "Handles overplotting better than scatter when there are many points.",
      "Shows how a numeric variable changes over time.",
      "Shows the pattern between two categorical variables."
    ),
    stringsAsFactors = FALSE
  )
}


# =============================================================================
# Internal: scoring
# =============================================================================

# score_candidates() filters taxonomy rows that match the variable types and
# goal, applies score adjustments for skewness / cardinality / n, and returns
# the top 3 as a list of viz_recommendation objects.
# @noRd
score_candidates <- function(x_type, y_type, x_skewed, y_skewed,
                              x_card, goal, n_rows, taxonomy, label) {

  is_pair <- !is.na(y_type)

  # Filter: x_type must match
  rows <- taxonomy[taxonomy$x_type == x_type, ]

  # Filter: y_type must match (NA in taxonomy means univariate chart)
  if (is_pair) {
    rows <- rows[!is.na(rows$y_type) & rows$y_type == y_type, ]
  } else {
    rows <- rows[is.na(rows$y_type), ]
  }

  if (nrow(rows) == 0L) return(list())

  # Goal filter: keep rows where goal matches or is "any"
  if (!is.null(goal)) {
    rows <- rows[rows$goal == goal | rows$goal == "any", ]
  }
  if (nrow(rows) == 0L) return(list())

  # Score adjustments
  score <- rows$base_score

  if (!is.null(goal)) {
    score <- score + ifelse(rows$goal == goal, 0.15, 0)
  }
  if (isTRUE(x_skewed)) {
    score <- score - ifelse(!rows$skewed_ok, 0.20, 0)
  }
  if (!is.na(x_card) && x_card > 15L) {
    score <- score - ifelse(!rows$high_card_ok, 0.20, 0)
  }
  if (n_rows > 1000L) {
    score <- score - ifelse(!rows$large_n_ok, 0.15, 0)
  }

  # Clamp to [0, 1] and drop very low scores
  score <- pmin(pmax(score, 0), 1)
  keep  <- score >= 0.10
  rows  <- rows[keep, ]
  score <- score[keep]

  if (nrow(rows) == 0L) return(list())

  # Sort descending, take top 3
  ord   <- order(score, decreasing = TRUE)
  rows  <- rows[ord, ]
  score <- score[ord]
  top_n <- min(3L, nrow(rows))

  lapply(seq_len(top_n), function(i) {
    structure(
      list(
        variable  = label,
        chart     = rows$chart[i],
        fit_score = round(score[i], 3),
        rationale = rows$rationale[i],
        goal      = if (is.null(goal)) rows$goal[i] else goal
      ),
      class = "viz_recommendation"
    )
  })
}


# =============================================================================
# Internal helpers
# =============================================================================

# infer_goal() picks a sensible default goal from variable types when the
# user has not specified one.
# @noRd
infer_goal <- function(x_type, y_type, user_goal) {
  if (!is.null(user_goal)) return(user_goal)
  is_pair <- !is.na(y_type)
  if (!is_pair) {
    if (x_type == "numeric")     return("distribution")
    if (x_type == "categorical") return("compare")
    if (x_type == "datetime")    return("trend")
  } else {
    if (x_type == "datetime")                            return("trend")
    if (x_type == "numeric"   && y_type == "numeric")   return("relationship")
    if (x_type == "categorical" && y_type == "numeric") return("compare")
    if (x_type == "categorical" && y_type == "categorical") return("composition")
  }
  NULL
}

# is_skewed() returns TRUE when |skewness| > 1 for a profiled row.
# @noRd
is_skewed <- function(profile_row) {
  s <- profile_row$skewness
  !is.na(s) && abs(s) > 1
}
