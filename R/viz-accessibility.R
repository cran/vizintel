#' Check visualization accessibility
#'
#' Checks an existing ggplot for common visualization accessibility and
#' readability issues, including excessive colours, colour-only encoding,
#' excessive shapes, small text, excessive text labels, and overplotting.
#'
#' @param plot A ggplot object to check for common accessibility issues.
#'
#' @return A list containing a `summary` and an `issues` element.
#'   Each issue contains a severity level, check name, and descriptive message.
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(mtcars, aes(x = wt, y = mpg, colour = factor(cyl))) +
#'   geom_point()
#'
#' viz_accessibility(p)
#'
#' @export

viz_accessibility <- function(plot) {
  
  # Check that the input is a ggplot object
  if (!inherits(plot, "ggplot")) {
    stop("`plot` must be a ggplot object.", call. = FALSE)
  }
  
  # Extract information from the plot
  geometry <- extract_plot_geometry(plot)
  
  # Store accessibility issues
  issues <- list()
  
  
  # --------------------------------------------------
  # Check 1: Number of colours / fills
  # --------------------------------------------------
  
  colour_scale <- geometry$scales$get_scales("colour")
  fill_scale <- geometry$scales$get_scales("fill")
  
  colour_count <- 0L
  fill_count <- 0L
  
  if (!is.null(colour_scale) &&
      !is.null(colour_scale$range$range)) {
    colour_count <- length(colour_scale$range$range)
  }
  
  if (!is.null(fill_scale) &&
      !is.null(fill_scale$range$range)) {
    fill_count <- length(fill_scale$range$range)
  }
  
  if (colour_count > 8L || fill_count > 8L) {
    
    colour_total <- max(colour_count, fill_count)
    
    issues <- c(
      issues,
      list(
        list(
          level = "warning",
          check = "colour_count",
          message = paste0(
            colour_total,
            " colour/fill values are represented. More than 8 categories ",
            "may be difficult to distinguish."
          )
        )
      )
    )
  }
  
  
  # --------------------------------------------------
  # Check 2: Colour-only encoding
  # --------------------------------------------------
  
  has_colour <- !is.null(colour_scale)
  has_fill <- !is.null(fill_scale)
  shape_scale <- geometry$scales$get_scales("shape")
  has_shape <- !is.null(shape_scale)
  
  if ((has_colour || has_fill) && !has_shape) {
    
    issues <- c(
      issues,
      list(
        list(
          level = "info",
          check = "colour_encoding",
          message = paste0(
            "Categories are distinguished using colour/fill. ",
            "Consider adding another visual property such as shape ",
            "if colour is important for interpretation."
          )
        )
      )
    )
  }
  
  
  # --------------------------------------------------
  # Check 3: Number of shapes
  # --------------------------------------------------
  
  shape_count <- 0L
  
  if (!is.null(shape_scale) &&
      !is.null(shape_scale$range$range)) {
    shape_count <- length(shape_scale$range$range)
  }
  
  if (shape_count > 8L) {
    
    issues <- c(
      issues,
      list(
        list(
          level = "warning",
          check = "shape_count",
          message = paste0(
            shape_count,
            " shape values are represented. More than 8 shapes ",
            "may be difficult to distinguish."
          )
        )
      )
    )
  }
  
  
  # --------------------------------------------------
  # Check 4: Text size
  # --------------------------------------------------
  
  text_layers <- vapply(
    plot$layers,
    function(layer) {
      inherits(layer$geom, "GeomText") ||
        inherits(layer$geom, "GeomLabel")
    },
    logical(1L)
  )
  
  small_text_found <- FALSE
  
  if (any(text_layers)) {
    
    for (i in which(text_layers)) {
      
      text_size <- plot$layers[[i]]$aes_params$size
      
      if (!is.null(text_size) && text_size < 3) {
        small_text_found <- TRUE
      }
    }
  }
  
  if (small_text_found) {
    
    issues <- c(
      issues,
      list(
        list(
          level = "warning",
          check = "text_size",
          message = "Text size may be too small for comfortable reading."
        )
      )
    )
  }
  
  
  # --------------------------------------------------
  # Check 5: Number of text labels
  # --------------------------------------------------
  
  label_count <- 0L
  
  if (any(text_layers)) {
    
    for (i in which(text_layers)) {
      
      layer_data <- geometry$data[[i]]
      
      if (!is.null(layer_data)) {
        label_count <- label_count + nrow(layer_data)
      }
    }
  }
  
  if (label_count > 30L) {
    
    issues <- c(
      issues,
      list(
        list(
          level = "warning",
          check = "label_count",
          message = paste0(
            label_count,
            " text labels are present. A large number of labels ",
            "may make the visualization difficult to read."
          )
        )
      )
    )
  }
  
  
  # --------------------------------------------------
  # Check 6: Overplotting
  # --------------------------------------------------
  
  point_layers <- vapply(
    plot$layers,
    function(layer) {
      inherits(layer$geom, "GeomPoint")
    },
    logical(1L)
  )
  
  overplotting_found <- FALSE
  overplotting_mitigated <- FALSE
  
  if (any(point_layers)) {
    for (i in which(point_layers)) {
      
      layer_data <- geometry$data[[i]]
      point_layer <- plot$layers[[i]]
      
      if (is.null(layer_data) ||
          nrow(layer_data) < 50L ||
          !all(c("x", "y") %in% names(layer_data))) {
        next
      }
      
      x <- layer_data$x
      y <- layer_data$y
      
      valid <- is.finite(x) & is.finite(y)
      x <- x[valid]
      y <- y[valid]
      
      if (length(x) < 50L) {
        next
      }
      
      # Divide the plotting area into a grid and count
      # how many points fall into the same/similar regions.
      x_scaled <- (x - min(x)) / (max(x) - min(x))
      y_scaled <- (y - min(y)) / (max(y) - min(y))
      
      x_bin <- pmin(20L, floor(x_scaled * 20L) + 1L)
      y_bin <- pmin(20L, floor(y_scaled * 20L) + 1L)
      
      bins <- paste(x_bin, y_bin, sep = "_")
      bin_counts <- table(bins)
      
      overlapping_points <- sum(
        bin_counts[bin_counts > 1]
      )
      
      overlap_ratio <- overlapping_points / length(x)
      
      if (overlap_ratio > 0.25) {
        
        overplotting_found <- TRUE
        
        # Check whether fixed transparency is being used.
        alpha <- point_layer$aes_params$alpha
        
        if (!is.null(alpha) && alpha < 0.7) {
          overplotting_mitigated <- TRUE
        }
      }
    }
  }
  if (overplotting_found && overplotting_mitigated) {
    
    issues <- c(
      issues,
      list(
        list(
          level = "info",
          check = "overplotting",
          message = paste0(
            "Many points overlap, but transparency is being used ",
            "to make overlapping observations more visible."
          )
        )
      )
    )
    
  } else if (overplotting_found) {
    
    issues <- c(
      issues,
      list(
        list(
          level = "warning",
          check = "overplotting",
          message = paste0(
            "Many points overlap, which may hide the density of ",
            "observations. Consider using transparency, jittering, ",
            "or a density/hexbin plot."
          )
        )
      )
    )
  }
  
  
  # --------------------------------------------------
  # Check 7: Overall result
  # --------------------------------------------------
  
  if (length(issues) == 0L) {
    
    summary_message <- "No obvious accessibility issues detected."
    
    issues <- list(
      list(
        level = "info",
        check = "overall",
        message = summary_message
      )
    )
    
  } else {
    
    warning_count <- sum(
      vapply(
        issues,
        function(x) x$level == "warning",
        logical(1L)
      )
    )
    
    info_count <- sum(
      vapply(
        issues,
        function(x) x$level == "info",
        logical(1L)
      )
    )
    
    summary_parts <- character(0)
    
    if (warning_count > 0L) {
      summary_parts <- c(
        summary_parts,
        paste(
          warning_count,
          ifelse(warning_count == 1, "issue", "issues")
        )
      )
    }
    
    if (info_count > 0L) {
      summary_parts <- c(
        summary_parts,
        paste(
          info_count,
          ifelse(info_count == 1, "consideration", "considerations")
        )
      )
    }
    
    summary_message <- paste0(
      paste(summary_parts, collapse = " and "),
      " detected."
    )
  }
  
  
  # Return the accessibility report
  list(
    summary = summary_message,
    issues = issues
  )
}