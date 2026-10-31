# -- extract_plot_geometry() --------------------------------------------------
#
# Internal helper that wraps ggplot2::ggplot_build() and returns a tidy list
# of the most useful structural elements. Called by viz_audit() and intended
# to be reused by the future viz_accessibility() function so the build step
# is never duplicated.
#
# @param plot A `ggplot` object.
# @return A named list with elements:
#   * `built`        - raw output of `ggplot2::ggplot_build(plot)`
#   * `data`         - list of per-layer data frames (after stat transforms)
#   * `geom_names`   - character vector of geom class names for each layer
#                      (e.g. `"GeomBar"`, `"GeomPoint"`)
#   * `panel_params` - panel layout parameters (scales, ranges, etc.)
#   * `x_range`      - numeric(2): displayed x-axis limits [min, max]
#   * `y_range`      - numeric(2): displayed y-axis limits [min, max]
#   * `scales`       - the plot's scale list (from `built$plot$scales`)
#
# @keywords internal
# @noRd
extract_plot_geometry <- function(plot) {
  built <- ggplot2::ggplot_build(plot)

  # -- Layer data (after stat transforms) --------------------------------------
  layer_data <- built$data  # list of data.frames, one per layer

  # -- Geom class names ---------------------------------------------------------
  geom_names <- vapply(
    plot$layers,
    function(lyr) class(lyr$geom)[1L],
    character(1L)
  )

  # -- Panel parameters ---------------------------------------------------------
  # ggplot2 >= 3.3 stores panel params in built$layout$panel_params
  panel_params <- built$layout$panel_params[[1L]]

  # -- Displayed axis ranges -----------------------------------------------------
  # Works for both continuous and discrete scales; falls back to NA gracefully.
  x_range <- tryCatch(
    panel_params$x.range %||% panel_params$x$limits,
    error = function(e) c(NA_real_, NA_real_)
  )
  y_range <- tryCatch(
    panel_params$y.range %||% panel_params$y$limits,
    error = function(e) c(NA_real_, NA_real_)
  )

  # Ensure length-2 numeric vectors
  if (is.null(x_range) || length(x_range) < 2L) x_range <- c(NA_real_, NA_real_)
  if (is.null(y_range) || length(y_range) < 2L) y_range <- c(NA_real_, NA_real_)

  list(
    built        = built,
    data         = layer_data,
    geom_names   = geom_names,
    panel_params = panel_params,
    x_range      = as.numeric(x_range[1:2]),
    y_range      = as.numeric(y_range[1:2]),
    scales       = built$plot$scales
  )
}

# -- Tiny null-coalescing operator (base-R safe) ------------------------------
# @noRd
`%||%` <- function(a, b) if (!is.null(a)) a else b
