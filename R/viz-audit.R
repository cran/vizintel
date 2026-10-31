# =============================================================================
# viz_audit() -- Plot Integrity Auditor
# =============================================================================

#' Audit a ggplot object for common visualization integrity problems
#'
#' Inspects a `ggplot` object by rendering it internally via
#' [ggplot2::ggplot_build()] and running four integrity checks drawn from
#' data-visualization best practices (Tufte, Cairo, Wilke). Returns a
#' structured S3 report with one entry per detected issue.
#'
#' @param plot A `ggplot` object to audit.
#'
#' @details
#' ## Checks performed
#'
#' | ID | Check | Triggers | Severity |
#' |----|-------|---------|----------|
#' | `truncated_axis` | Y-axis does not include 0 on a bar/column plot | Bar/col geom + displayed y-min > min(0, data min) | `"critical"` |
#' | `lie_factor` | Displayed axis range distorts data range | Range ratio outside \[0.95, 1.05\] | `"warning"` |
#' | `overplotting` | Many points overlap in a scatter plot | Point/jitter geom + collision fraction > 0.30 | `"warning"` |
#' | `dual_y_axis` | A secondary y-axis is defined | `sec.axis` present on y scale | `"info"` |
#'
#' ## Lie factor note
#' The lie factor is computed in **data space** (displayed axis range vs actual
#' data range), not pixel space. Pixel-based computation requires a live
#' graphics device and is not reproducible in a non-interactive context.
#'
#' ## Plain-English explanations
#' Every issue includes an `explanation` field with a plain-English description
#' of what the problem means for how misleading the chart looks. This is
#' printed after the technical message when you call `print()` on the report.
#'
#' @return An S3 object of class `"viz_audit_report"`, which is a list with:
#' \describe{
#'   \item{`n_issues`}{Total number of issues detected (integer).}
#'   \item{`n_critical`}{Number of critical issues (integer).}
#'   \item{`n_warning`}{Number of warning-level issues (integer).}
#'   \item{`n_info`}{Number of info-level issues (integer).}
#'   \item{`issues`}{A list of issue lists, each with fields `check`,
#'     `severity`, `message`, `details`, and `explanation`.}
#'   \item{`plot_geoms`}{Character vector of geom class names in the plot.}
#' }
#'
#' @examples
#' \donttest{
#' library(ggplot2)
#'
#' # A bar chart with a truncated y-axis -- should flag "truncated_axis"
#' bad <- ggplot(data.frame(x = c("A", "B"), y = c(50, 80)), aes(x, y)) +
#'   geom_col() +
#'   coord_cartesian(ylim = c(40, 85))
#' report <- viz_audit(bad)
#' print(report)
#'
#' # A clean scatter plot -- should return no critical issues
#' good <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' viz_audit(good)
#' }
#'
#' @importFrom ggplot2 ggplot_build
#' @export
viz_audit <- function(plot) {
  # -- Input validation --------------------------------------------------------
  if (!inherits(plot, "ggplot")) {
    stop(
      "`plot` must be a ggplot object. ",
      "Did you forget to build your plot with ggplot() + geom_*() ?",
      call. = FALSE
    )
  }

  # -- Build once via shared helper --------------------------------------------
  geom   <- extract_plot_geometry(plot)
  issues <- list()

  # -- Check a: Truncated axis -------------------------------------------------
  issues <- c(issues, check_truncated_axis(geom))

  # -- Check b: Lie factor -----------------------------------------------------
  issues <- c(issues, check_lie_factor(geom))

  # -- Check c: Overplotting ---------------------------------------------------
  issues <- c(issues, check_overplotting(geom))

  # -- Check d: Dual y-axis ----------------------------------------------------
  issues <- c(issues, check_dual_y_axis(geom))

  # -- Assemble report ---------------------------------------------------------
  severities <- vapply(issues, `[[`, character(1L), "severity")

  structure(
    list(
      n_issues   = length(issues),
      n_critical = sum(severities == "critical"),
      n_warning  = sum(severities == "warning"),
      n_info     = sum(severities == "info"),
      issues     = issues,
      plot_geoms = geom$geom_names
    ),
    class = "viz_audit_report"
  )
}


# =============================================================================
# S3 print method
# =============================================================================

#' Print a viz_audit_report
#'
#' Prints each detected issue with its severity prefix and technical message,
#' followed by a plain-English explanation (the `explanation` field) on the
#' next line prefixed with `"In plain terms: "`.
#'
#' @param x A `viz_audit_report` object returned by `viz_audit()`.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.viz_audit_report <- function(x, ...) {
  severity_prefix <- c(
    critical = "x [CRITICAL]",
    warning  = "! [WARNING] ",
    info     = "i [INFO]    "
  )

  cat("== vizintel audit report ==\n")
  cat("Geoms detected:", paste(x$plot_geoms, collapse = ", "), "\n")
  cat(sprintf(
    "Issues: %d critical, %d warning, %d info\n\n",
    x$n_critical, x$n_warning, x$n_info
  ))

  if (x$n_issues == 0L) {
    cat("  No issues detected. Plot passes all checks.\n")
  } else {
    for (issue in x$issues) {
      prefix <- severity_prefix[[issue$severity]]
      cat(prefix, issue$message, "\n")
      if (!is.null(issue$explanation) && nzchar(issue$explanation)) {
        cat("   In plain terms:", issue$explanation, "\n")
      }
    }
  }

  invisible(x)
}


# =============================================================================
# S3 summary method
# =============================================================================

#' Summarise a viz_audit_report
#'
#' @param object A `viz_audit_report` object returned by `viz_audit()`.
#' @param ... Ignored.
#' @return `object`, invisibly.
#' @export
summary.viz_audit_report <- function(object, ...) {
  status <- if (object$n_critical > 0L) "FAIL" else if (object$n_warning > 0L) "WARN" else "PASS"
  cat(sprintf(
    "[%s] %d issue(s) -- %d critical / %d warning / %d info\n",
    status, object$n_issues,
    object$n_critical, object$n_warning, object$n_info
  ))
  invisible(object)
}


# =============================================================================
# Internal check functions
# =============================================================================

# -- a. Truncated axis ---------------------------------------------------------
#
# Flags bar/column plots where the y-axis minimum does not include 0 (or the
# data minimum if negative), which exaggerates visual differences between bars.
# @noRd
check_truncated_axis <- function(geom) {
  bar_geoms <- c("GeomBar", "GeomCol")
  has_bar   <- any(geom$geom_names %in% bar_geoms)
  if (!has_bar) return(list())

  y_min_displayed <- geom$y_range[1L]
  if (is.na(y_min_displayed)) return(list())

  # Collect actual y data minimums across all bar layers
  bar_idx     <- which(geom$geom_names %in% bar_geoms)
  data_y_vals <- unlist(lapply(bar_idx, function(i) {
    d <- geom$data[[i]]
    if (is.null(d)) return(numeric(0))
    # geom_col stores ymin/ymax after stat; geom_bar uses y
    y_cols <- intersect(c("y", "ymin", "ymax"), names(d))
    unlist(d[y_cols], use.names = FALSE)
  }))
  data_y_vals <- data_y_vals[is.finite(data_y_vals)]
  if (length(data_y_vals) == 0L) return(list())

  data_y_min     <- min(data_y_vals)
  expected_floor <- min(0, data_y_min)   # should reach 0 at minimum

  if (y_min_displayed > expected_floor + 1e-9) {
    list(list(
      check    = "truncated_axis",
      severity = "critical",
      message  = sprintf(
        "Y-axis starts at %.4g, not %.4g \u2014 bar heights are visually exaggerated.",
        y_min_displayed, expected_floor
      ),
      details  = list(
        y_min_displayed = y_min_displayed,
        y_min_expected  = expected_floor,
        data_y_min      = data_y_min
      ),
      explanation = paste0(
        "Because the Y-axis does not start at zero, the bars appear much taller ",
        "relative to each other than the actual values justify. A small real ",
        "difference can look enormous, making the chart misleading at a glance."
      )
    ))
  } else {
    list()
  }
}


# -- b. Lie factor -------------------------------------------------------------
#
# Data-space lie factor: (displayed_range / data_range) for each axis.
# A ratio far from 1 means the axis is stretched or compressed relative to the
# actual spread of the data, distorting visual comparisons.
# Checked on both x and y axes; reports the worst offender.
# @noRd
check_lie_factor <- function(geom) {
  issues <- list()

  check_axis <- function(axis_name, displayed_range, data_vals) {
    if (any(is.na(displayed_range))) return(NULL)
    data_vals <- data_vals[is.finite(data_vals)]
    if (length(data_vals) < 2L) return(NULL)

    displayed_span <- diff(displayed_range)
    data_span      <- diff(range(data_vals))
    if (data_span < 1e-12 || displayed_span < 1e-12) return(NULL)

    lf <- displayed_span / data_span
    if (lf < 0.95 || lf > 1.05) {
      exaggerated <- lf > 1
      list(
        check    = "lie_factor",
        severity = "warning",
        message  = sprintf(
          "%s-axis lie factor = %.3f (displayed span %.4g vs data span %.4g). Visual effect is %s.",
          toupper(axis_name), lf, displayed_span, data_span,
          if (exaggerated) "exaggerated" else "understated"
        ),
        details  = list(
          axis           = axis_name,
          lie_factor     = lf,
          displayed_span = displayed_span,
          data_span      = data_span
        ),
        explanation = if (exaggerated) {
          paste0(
            "The axis spans more space than the data actually covers, ",
            "making differences between values look bigger than they really are. ",
            "A viewer glancing at the chart will overestimate how much the values vary."
          )
        } else {
          paste0(
            "The axis is compressed relative to the actual spread of the data, ",
            "making real differences look smaller than they are. ",
            "Important variation in the data may be hidden or downplayed."
          )
        }
      )
    } else {
      NULL
    }
  }

  # Collect all x and y values across layers
  all_x <- unlist(lapply(geom$data, function(d) d[["x"]]),  use.names = FALSE)
  all_y <- unlist(lapply(geom$data, function(d) d[["y"]]),  use.names = FALSE)

  # Only check continuous axes (numeric values)
  if (is.numeric(all_x)) {
    issue <- check_axis("x", geom$x_range, all_x)
    if (!is.null(issue)) issues <- c(issues, list(issue))
  }
  if (is.numeric(all_y)) {
    issue <- check_axis("y", geom$y_range, all_y)
    if (!is.null(issue)) issues <- c(issues, list(issue))
  }

  issues
}


# -- c. Overplotting -----------------------------------------------------------
#
# For scatter/point geoms: rounds every point's (x, y) to a grid of 1% of
# the axis range, then counts positions with more than one point. The
# collision fraction = (n_duplicate_points / n_total_points). Flags if > 0.30.
# @noRd
check_overplotting <- function(geom) {
  point_geoms <- c("GeomPoint", "GeomJitter")
  point_idx   <- which(geom$geom_names %in% point_geoms)
  if (length(point_idx) == 0L) return(list())

  pts <- do.call(rbind, lapply(point_idx, function(i) {
    d <- geom$data[[i]]
    if (is.null(d) || !all(c("x", "y") %in% names(d))) return(NULL)
    d[c("x", "y")]
  }))
  pts <- pts[!is.na(pts$x) & !is.na(pts$y), ]

  if (!is.numeric(pts$x) || !is.numeric(pts$y)) return(list())
  n_total <- nrow(pts)
  if (n_total < 2L) return(list())

  # Grid resolution: 1% of displayed axis range
  x_res <- diff(geom$x_range) * 0.01
  y_res <- diff(geom$y_range) * 0.01
  if (is.na(x_res) || x_res < 1e-12) x_res <- (max(pts$x) - min(pts$x)) * 0.01
  if (is.na(y_res) || y_res < 1e-12) y_res <- (max(pts$y) - min(pts$y)) * 0.01
  if (x_res < 1e-12 || y_res < 1e-12) return(list())

  gx <- round(pts$x / x_res)
  gy <- round(pts$y / y_res)
  cell_counts <- table(paste(gx, gy, sep = ","))

  n_colliding  <- sum(as.integer(cell_counts[cell_counts > 1L]))
  overlap_frac <- n_colliding / n_total

  if (overlap_frac > 0.30) {
    list(list(
      check    = "overplotting",
      severity = "warning",
      message  = sprintf(
        "%.0f%% of points overlap on a 1%%-grid (%d of %d points collide). Consider alpha, jitter, or geom_bin2d().",
        overlap_frac * 100, n_colliding, n_total
      ),
      details  = list(
        overlap_fraction = overlap_frac,
        n_colliding      = n_colliding,
        n_total          = n_total
      ),
      explanation = paste0(
        "Many data points are drawn on top of each other, so you are seeing only a ",
        "fraction of the actual data. Dense clusters look the same as sparse areas, ",
        "hiding the true distribution and making it impossible to judge how many ",
        "observations fall in any given region."
      )
    ))
  } else {
    list()
  }
}


# -- d. Dual y-axis ------------------------------------------------------------
#
# Checks whether any y scale in the plot defines a secondary axis.
# Secondary axes are often used to overlay two unrelated variables on one
# panel, which can be misleading (Wilke 2019, ch. 9). Flagged as "info"
# since a rescaled secondary axis (e.g. degC <-> degF) is not inherently wrong.
# @noRd
check_dual_y_axis <- function(geom) {
  scales_list <- geom$scales$scales
  has_sec     <- FALSE

  for (sc in scales_list) {
    if (!is.null(sc$secondary.axis) &&
        !inherits(sc$secondary.axis, "waiver")) {
      has_sec <- TRUE
      break
    }
  }

  if (has_sec) {
    list(list(
      check    = "dual_y_axis",
      severity = "info",
      message  = "Plot has a secondary y-axis. Dual axes can mislead if the two scales are unrelated.",
      details  = list(),
      explanation = paste0(
        "When two different variables share one chart using two Y-axes, viewers may ",
        "incorrectly assume the variables are directly comparable or causally linked. ",
        "The apparent relationship between the lines can change completely just by ",
        "rescaling either axis."
      )
    ))
  } else {
    list()
  }
}
