#' <Add Title>
#'
#' <Add Description>
#'
#' @importFrom htmlwidgets createWidget shinyWidgetOutput shinyRenderWidget
#'
#' @export
tuichart <- function(data = NULL, type = "bar", options = list(), width = NULL, height = NULL, elementId = NULL) {

  type <- match.arg(type, choices = c("bar", "column", "line", "area", "scatter", "heatmap"))

  x <- list(
    type = paste0(type, "Chart"),
    data = data,
    options = options
  )

  htmlwidgets::createWidget(
    name = 'tuichart',
    x = x,
    width = width,
    height = height,
    package = 'tuichartr',
    elementId = elementId
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
