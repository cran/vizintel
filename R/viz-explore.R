#' Explore Relationships Between Numeric Variables
#'
#' Explores relationships between numeric variables using
#' correlation analysis and a heatmap.
#'
#' @param data A data frame or tibble containing the variables to explore.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{correlation}: A correlation matrix.
#'   \item \code{plot}: A ggplot2 correlation heatmap.
#' }
#'
#' @examples
#' result <- viz_explore(mtcars)
#' result$correlation
#' \donttest{
#' result$plot
#' }
#'
#' @importFrom rlang .data
#' @export

# viz_explore()
# Explores relationships between numeric variables
# using a correlation matrix and heatmap.

viz_explore <- function(data) {
  
  # Check that the input is a data frame
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame or tibble.", call. = FALSE)
  }
  
  # Get information about each column
  # using the package's existing profiling function
  profile <- profile_data(data)
  
  # Select only numeric variables
  numeric_vars <- profile$variable[
    profile$type == "numeric"
  ]
  
  # At least two numeric variables are needed
  # to calculate relationships between variables
  if (length(numeric_vars) < 2L) {
    stop(
      "At least two numeric variables are required for correlation analysis.",
      call. = FALSE
    )
  }
  
  # Extract the numeric columns from the data
  numeric_data <- data[numeric_vars]
  
  # Calculate the correlation matrix
  # complete observations are used for missing values
  cor_matrix <- stats::cor(
    numeric_data,
    use = "complete.obs"
  )
  
  # Convert the correlation matrix into a data frame
  # so that it can be used with ggplot2
  cor_data <- as.data.frame(as.table(cor_matrix))
  
  # Give meaningful names to the columns
  names(cor_data) <- c(
    "variable_x",
    "variable_y",
    "correlation"
  )
  
  # Create the correlation heatmap
  heatmap <- ggplot2::ggplot(
    cor_data,
    ggplot2::aes(
      x = .data[["variable_x"]],
      y = .data[["variable_y"]],
      fill = .data[["correlation"]]
    )
  ) +
    ggplot2::geom_tile() +

    # Display the correlation value inside each tile
    ggplot2::geom_text(
      ggplot2::aes(
        label = round(.data[["correlation"]], 2)
      )
    ) +
    
    ggplot2::labs(
      title = "Correlation Matrix",
      x = NULL,
      y = NULL,
      fill = "Correlation"
    ) +
    
    # Use a simple clean theme
    ggplot2::theme_minimal()
  
  # Return both the correlation matrix and the plot
  list(
    correlation = cor_matrix,
    plot = heatmap
  )
}