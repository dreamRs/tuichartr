#' Create a \code{tuichart} htmlwidget
#'
#' @param shape An \code{sf} object.
#' @param code Variable to use as unique identifier for the polygons.
#' @param name Varaible to use as label for the polygons.
#' @param data A \code{list} of parameters for the data used in the chart.
#' @param options A \code{list} of options for the chart.
#' @param width A numeric input in pixels.
#' @param height A numeric input in pixels.
#' @param elementId Use an explicit element ID for the widget.
#'
#' @return A \code{tuichart} \code{htmlwidget} object.
#' @export
#'
#' @importFrom sf st_transform st_bbox
#' @importFrom geojsonio geojson_json
#' @importFrom htmlwidgets createWidget shinyWidgetOutput shinyRenderWidget sizingPolicy
#'
# @examples
tuimaps <- function(shape, code, name, data = NULL, options = NULL, width = NULL, height = NULL, elementId = NULL) {

  theme <- getOption("tuichartr.theme")
  if (!is.null(theme)) {
    if (is.null(options))
      options <- list()
    options$theme <- list(theme$name)
  }
  shape <- st_transform(shape, crs = 3857)
  bbox <- as.list(st_bbox(shape))
  names(bbox) <- c("left", "bottom", "right", "top")
  geojson <- geojsonio::geojson_json(input = shape)
  x <- dropNulls(list(
    geodata = list(
      code = as.character(shape[[code]]),
      name = as.character(shape[[name]])
    ),
    bbox = bbox,
    geojson = geojson,
    data = data,
    options = options,
    theme = theme
  ))

  htmlwidgets::createWidget(
    name = 'tuimaps',
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

#' Shiny bindings for tuimaps
#'
#' Output and render functions for using tuimaps within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a tuimaps
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name tuimaps-shiny
#'
#' @export
tuimapsOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'tuimaps', width, height, package = 'tuichartr')
}

#' @rdname tuimaps-shiny
#' @export
renderTuimaps <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, tuimapsOutput, env, quoted = TRUE)
}
