library(testthat)
library(ggplot2)

# Helper: run viz_alternatives() inside an off-screen PNG device so that
# show_alternatives() (which calls print(<ggplot>)) has a device to write
# to, preventing automatic Rplots.pdf creation during R CMD check.
run_alternatives <- function(plot) {
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = 800, height = 600)
  on.exit({
    grDevices::dev.off()
    unlink(tmp)
  })
  suppressMessages(viz_alternatives(plot))
}


test_that("viz_alternatives works for scatter plots", {

  p <- ggplot(
    mtcars,
    aes(wt, mpg)
  ) +
    geom_point()

  result <- run_alternatives(p)

  expect_type(result, "list")
  expect_equal(
    result$recommendations,
    c(
      "2D Density Plot",
      "2D Histogram"
    )
  )

  expect_length(result$plots, 2)
})


test_that("viz_alternatives works for histograms", {

  p <- ggplot(
    mtcars,
    aes(mpg)
  ) +
    geom_histogram()

  result <- run_alternatives(p)

  expect_equal(
    result$recommendations,
    c(
      "Density Plot",
      "ECDF"
    )
  )

  expect_length(result$plots, 2)
})


test_that("viz_alternatives works for bar plots", {

  p <- ggplot(
    mtcars,
    aes(factor(cyl))
  ) +
    geom_bar()

  result <- run_alternatives(p)

  expect_equal(
    result$recommendations,
    c(
      "Dot Plot",
      "Lollipop Plot"
    )
  )

  expect_length(result$plots, 2)
})


test_that("viz_alternatives works for column plots", {

  df <- data.frame(
    category = c("A", "B", "C"),
    value = c(10, 25, 15)
  )

  p <- ggplot(
    df,
    aes(category, value)
  ) +
    geom_col()

  result <- run_alternatives(p)

  expect_equal(
    result$recommendations,
    c(
      "Dot Plot",
      "Lollipop Plot"
    )
  )

  expect_length(result$plots, 2)
})


test_that("viz_alternatives works for boxplots", {

  p <- ggplot(
    mtcars,
    aes(factor(cyl), mpg)
  ) +
    geom_boxplot()

  result <- run_alternatives(p)

  expect_equal(
    result$recommendations,
    c(
      "Violin Plot",
      "Strip Plot"
    )
  )

  expect_length(result$plots, 2)
})


test_that("viz_alternatives handles transformed mappings", {

  p <- ggplot(
    mtcars,
    aes(
      factor(cyl),
      mpg
    )
  ) +
    geom_boxplot()

  result <- run_alternatives(p)

  expect_equal(
    result$recommendations,
    c(
      "Violin Plot",
      "Strip Plot"
    )
  )

  expect_length(result$plots, 2)
})


test_that("viz_alternatives rejects non-ggplot input", {

  expect_error(
    viz_alternatives(mtcars),
    "`plot` must be a ggplot object."
  )
})


test_that("viz_alternatives returns empty result for unsupported plots", {

  p <- ggplot(
    mtcars,
    aes(wt, mpg)
  ) +
    geom_line()

  result <- suppressMessages(viz_alternatives(p))

  expect_equal(
    result$recommendations,
    character(0)
  )

  expect_length(
    result$plots,
    0
  )
})


test_that("viz_alternatives messages can be suppressed", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = 400, height = 300)
  on.exit({ grDevices::dev.off(); unlink(tmp) })
  # No message should escape suppressMessages()
  expect_no_condition(
    suppressMessages(viz_alternatives(p)),
    class = "message"
  )
})