#' Suggest alternative visualizations
#'
#' Suggests and generates alternative visualization types for an existing
#' ggplot object. The generated alternatives are displayed via the active
#' graphics device and are also returned for further use. Informational
#' messages about the alternatives can be suppressed with
#' `suppressMessages()`.
#'
#' @param plot A ggplot object.
#'
#' @return A list containing:
#'   \describe{
#'     \item{`recommendations`}{Character vector of suggested alternative
#'       visualization names.}
#'     \item{`plots`}{A named list of generated `ggplot` objects for the
#'       alternatives.}
#'   }
#'
#' @examples
#' \donttest{
#' library(ggplot2)
#'
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point()
#'
#' result <- suppressMessages(viz_alternatives(p))
#' result$recommendations
#' }
#'
#' @importFrom rlang .data
#' @export
viz_alternatives <- function(plot) {

  if (!inherits(plot, "ggplot")) {
    stop(
      "`plot` must be a ggplot object.",
      call. = FALSE
    )
  }


  geometry <- extract_plot_geometry(plot)

  layer <- plot$layers[[1L]]

  geom <- geometry$geom_names[1L]
  stat <- class(layer$stat)[1L]


  data <- plot$data

  if (is.null(data)) {
    stop(
      "The plot must contain data that can be used to generate alternatives.",
      call. = FALSE
    )
  }


  mapping <- plot$mapping

  x_var <- get_mapping_name(
    mapping$x,
    data
  )

  y_var <- get_mapping_name(
    mapping$y,
    data
  )


  data_alt <- data

  if (!is.null(x_var)) {
    data_alt$.viz_x <- x_var
  }

  if (!is.null(y_var)) {
    data_alt$.viz_y <- y_var
  }


  # --------------------------------------------------
  # Scatter plot alternatives
  # --------------------------------------------------

  if (geom == "GeomPoint" &&
      !is.null(x_var) &&
      !is.null(y_var)) {

    recommendations <- c(
      "2D Density Plot",
      "2D Histogram"
    )

    plots <- list(

      `2D Density Plot` =
        ggplot2::ggplot(
          data_alt,
          ggplot2::aes(
            x = .data$.viz_x,
            y = .data$.viz_y
          )
        ) +
        ggplot2::geom_density_2d() +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = "2D Density Plot",
          subtitle = "Shows areas of observation density"
        ),

      `2D Histogram` =
        ggplot2::ggplot(
          data_alt,
          ggplot2::aes(
            x = .data$.viz_x,
            y = .data$.viz_y
          )
        ) +
        ggplot2::geom_bin2d(bins = 15) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = "2D Histogram",
          subtitle = "Groups observations into rectangular bins"
        )
    )

    message("\nRecommended alternatives:")

    for (i in seq_along(recommendations)) {
      message(i, ". ", recommendations[i])
    }

    message("\nDisplaying alternatives...\n")

    show_alternatives(plots)

    return(
      list(
        recommendations = recommendations,
        plots = plots
      )
    )
  }


  # --------------------------------------------------
  # Histogram alternatives
  # --------------------------------------------------

  if (geom == "GeomBar" &&
      stat == "StatBin" &&
      !is.null(x_var)) {

    recommendations <- c(
      "Density Plot",
      "ECDF"
    )

    plots <- list(

      `Density Plot` =
        ggplot2::ggplot(
          data_alt,
          ggplot2::aes(
            x = .data$.viz_x
          )
        ) +
        ggplot2::geom_density() +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = "Density Plot",
          subtitle = "Shows the distribution as a smooth curve"
        ),

      `ECDF` =
        ggplot2::ggplot(
          data_alt,
          ggplot2::aes(
            x = .data$.viz_x
          )
        ) +
        ggplot2::stat_ecdf() +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = "ECDF",
          subtitle = "Shows the cumulative distribution"
        )
    )

    message("\nRecommended alternatives:")

    for (i in seq_along(recommendations)) {
      message(i, ". ", recommendations[i])
    }

    message("\nDisplaying alternatives...\n")

    show_alternatives(plots)

    return(
      list(
        recommendations = recommendations,
        plots = plots
      )
    )
  }


  # --------------------------------------------------
  # Bar / column alternatives
  # --------------------------------------------------

  if (geom %in% c("GeomBar", "GeomCol") &&
      !is.null(x_var)) {

    recommendations <- c(
      "Dot Plot",
      "Lollipop Plot"
    )


    # --------------------------------------------------
    # geom_col: explicit values
    # --------------------------------------------------

    if (!is.null(y_var)) {

      plots <- list(

        `Dot Plot` =
          ggplot2::ggplot(
            data_alt,
            ggplot2::aes(
              x = .data$.viz_x,
              y = .data$.viz_y
            )
          ) +
          ggplot2::geom_point(size = 3) +
          ggplot2::theme_minimal() +
          ggplot2::labs(
            title = "Dot Plot",
            subtitle = "Shows values using points"
          ),

        `Lollipop Plot` =
          ggplot2::ggplot(
            data_alt,
            ggplot2::aes(
              x = .data$.viz_x,
              y = .data$.viz_y
            )
          ) +
          ggplot2::geom_segment(
            ggplot2::aes(
              xend = .data$.viz_x,
              y = 0,
              yend = .data$.viz_y
            )
          ) +
          ggplot2::geom_point(size = 3) +
          ggplot2::theme_minimal() +
          ggplot2::labs(
            title = "Lollipop Plot",
            subtitle = "Shows values using points and connecting lines"
          )
      )


    } else {

      # --------------------------------------------------
      # geom_bar: calculate category frequencies
      # --------------------------------------------------

      counts <- as.data.frame(
        table(data_alt$.viz_x)
      )

      names(counts) <- c(
        ".viz_x",
        ".viz_y"
      )

      counts$.viz_x <- factor(
        counts$.viz_x,
        levels = levels(factor(data_alt$.viz_x))
      )


      plots <- list(

        `Dot Plot` =
          ggplot2::ggplot(
            counts,
            ggplot2::aes(
              x = .data$.viz_x,
              y = .data$.viz_y
            )
          ) +
          ggplot2::geom_point(size = 3) +
          ggplot2::theme_minimal() +
          ggplot2::labs(
            title = "Dot Plot",
            subtitle = "Shows category frequencies"
          ),

        `Lollipop Plot` =
          ggplot2::ggplot(
            counts,
            ggplot2::aes(
              x = .data$.viz_x,
              y = .data$.viz_y
            )
          ) +
          ggplot2::geom_segment(
            ggplot2::aes(
              xend = .data$.viz_x,
              y = 0,
              yend = .data$.viz_y
            )
          ) +
          ggplot2::geom_point(size = 3) +
          ggplot2::theme_minimal() +
          ggplot2::labs(
            title = "Lollipop Plot",
            subtitle = "Shows category frequencies"
          )
      )
    }


    message("\nRecommended alternatives:")

    for (i in seq_along(recommendations)) {
      message(i, ". ", recommendations[i])
    }

    message("\nDisplaying alternatives...\n")

    show_alternatives(plots)

    return(
      list(
        recommendations = recommendations,
        plots = plots
      )
    )
  }


  # --------------------------------------------------
  # Boxplot alternatives
  # --------------------------------------------------

  if (geom == "GeomBoxplot" &&
      !is.null(x_var) &&
      !is.null(y_var)) {

    recommendations <- c(
      "Violin Plot",
      "Strip Plot"
    )

    plots <- list(

      `Violin Plot` =
        ggplot2::ggplot(
          data_alt,
          ggplot2::aes(
            x = .data$.viz_x,
            y = .data$.viz_y
          )
        ) +
        ggplot2::geom_violin() +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = "Violin Plot",
          subtitle = "Shows the distribution of each group"
        ),

      `Strip Plot` =
        ggplot2::ggplot(
          data_alt,
          ggplot2::aes(
            x = .data$.viz_x,
            y = .data$.viz_y
          )
        ) +
        ggplot2::geom_jitter(
          width = 0.15
        ) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = "Strip Plot",
          subtitle = "Shows individual observations"
        )
    )

    message("\nRecommended alternatives:")

    for (i in seq_along(recommendations)) {
      message(i, ". ", recommendations[i])
    }

    message("\nDisplaying alternatives...\n")

    show_alternatives(plots)

    return(
      list(
        recommendations = recommendations,
        plots = plots
      )
    )
  }


  # --------------------------------------------------
  # Unsupported visualization
  # --------------------------------------------------

  message("\nNo alternatives available for this visualization.")

  list(
    recommendations = character(0),
    plots = list()
  )
}


# --------------------------------------------------
# Internal helper: display generated alternative plots
# --------------------------------------------------
# @noRd
show_alternatives <- function(plots) {

  if (length(plots) == 0L) {
    return(invisible(NULL))
  }

  n <- length(plots)

  ncol <- 2L
  nrow <- ceiling(n / ncol)

  grid::grid.newpage()

  layout <- grid::grid.layout(
    nrow = nrow,
    ncol = ncol
  )

  grid::pushViewport(
    grid::viewport(layout = layout)
  )

  for (i in seq_along(plots)) {

    row <- ceiling(i / ncol)
    col <- ((i - 1L) %% ncol) + 1L

    print(
      plots[[i]],
      vp = grid::viewport(
        layout.pos.row = row,
        layout.pos.col = col
      )
    )
  }

  grid::popViewport()

  invisible(NULL)
}


# --------------------------------------------------
# Internal helper: evaluate ggplot mapping
# --------------------------------------------------
# @noRd
get_mapping_name <- function(mapping, data) {

  if (is.null(mapping)) {
    return(NULL)
  }

  value <- rlang::eval_tidy(
    mapping,
    data = data
  )

  value
}