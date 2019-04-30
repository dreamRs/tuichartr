
#' Create a \code{tuichart} htmlwidget
#'
#' @param type Type of chart.
#' @param data A \code{list} of parameters for the data used in the chart.
#' @param options A \code{list} of options for the chart.
#' @param width A numeric input in pixels.
#' @param height A numeric input in pixels.
#' @param elementId Use an explicit element ID for the widget.
#'
#' @return A \code{tuichart} \code{htmlwidget} object.
#' @export
#'
#' @importFrom htmlwidgets createWidget shinyWidgetOutput shinyRenderWidget sizingPolicy
#'
# @examples
tuichart <- function(type = "bar", data = NULL, options = NULL, width = NULL, height = NULL, elementId = NULL) {

  type <- match.arg(
    arg = type,
    choices = c("bar", "column", "line", "area", "scatter",
                "bubble", "heatmap", "treemap", "boxplot",
                "radial", "pie")
  )

  theme <- getOption("tuichartr.theme")
  if (!is.null(theme)) {
    if (is.null(options))
      options <- list()
    options$theme <- list(theme$name)
  }
  x <- dropNulls(list(
    type = paste0(type, "Chart"),
    data = data,
    options = options,
    theme = theme
  ))

  htmlwidgets::createWidget(
    name = 'tuichart',
    x = x,
    width = width,
    height = height,
    package = 'tuichartr',
    elementId = elementId,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = "100%",
      defaultHeight = "100%",
      viewer.defaultHeight = "100%",
      viewer.defaultWidth = "100%",
      knitr.figure = FALSE,
      knitr.defaultWidth = "100%",
      knitr.defaultHeight = "350px",
      browser.fill = TRUE,
      viewer.suppress = FALSE,
      browser.external = TRUE,
      padding = 20
    )
  )
}

#' Shiny bindings for tuichart
#'
#' Output and render functions for using tuichart within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a tuichart
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name tuichart-shiny
#'
#' @export
tuichartOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'tuichart', width, height, package = 'tuichartr')
}

#' @rdname tuichart-shiny
#' @export
renderTuichart <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, tuichartOutput, env, quoted = TRUE)
}
