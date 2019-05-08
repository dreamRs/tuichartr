#' Create a \code{tuimap} htmlwidget
#'
#' @param shape An \code{sf} object.
#' @param code Variable to use as unique identifier for the polygons.
#' @param label Variable to use as label for the polygons.
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
#' @examples
#'
#' library(rnaturalearth)
#'
#' # Retrieve polygons
#' sa <- ne_countries(continent = "south america", returnclass = "sf")
#' # add a random numeric variable
#' sa$random <- sample(1:100, nrow(sa), TRUE)
#'
#' # draw map
#' tuimap() %>%
#'   add_map_data(sa, aes(code = adm0_a3, label = name, value = random)) %>%
#'   tui_chart(title = "A map")
#'
tuimap <- function(shape = NULL, code = NULL, label = NULL, data = NULL,
                   options = NULL, width = NULL, height = NULL, elementId = NULL) {

  theme <- getOption("tuichartr.theme")
  if (!is.null(theme)) {
    if (is.null(options))
      options <- list()
    options$theme <- list(theme$name)
  }
  if (!is.null(shape)) {
    shape <- st_transform(shape, crs = 3857) # 4326
    bbox <- st_bbox(shape)
    bbox <- as.list(bbox)
    names(bbox) <- c("left", "bottom", "right", "top")
    geojson <- geojsonio::geojson_json(input = shape)
    geodata <- list(
      code = as.character(shape[[code]]),
      name = as.character(shape[[label]])
    )
  } else {
    geojson <- NULL
    bbox <- NULL
    geodata <- list(
      code = NULL,
      name = NULL
    )
  }
  x <- dropNulls(list(
    geodata = geodata,
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

enlarge_bbox <- function(bbox, ratio = 0.1) {
  diff13 <- diff(bbox[c(1, 3)])
  diff24 <- diff(bbox[c(2, 4)])
  bbox + c(-1, -1, 1, 1) * c(diff13 * ratio, diff24 * ratio, diff13 * ratio, diff24 * ratio)
}

#' Shiny bindings for tuimap
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
#' @name tuimap-shiny
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'
#'   library(shiny)
#'   library(rnaturalearth)
#'   library(dplyr)
#'   library(tuichartr)
#'
#'   # Retrieve world map
#'   world <- ne_countries(returnclass = "sf") %>%
#'     filter(continent != "Antarctica")
#'
#'   ui <- fluidPage(
#'     tags$h2("Include tuimap in Shiny"),
#'     fluidRow(
#'       column(
#'         width = 3,
#'         actionButton(
#'           inputId = "refresh",
#'           label = "Refresh data"
#'         )
#'       ),
#'       column(
#'         width = 9,
#'         tuimapOutput(outputId = "my_map")
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'
#'     output$my_map <- renderTuimap({
#'
#'       input$refresh
#'
#'       # add a random numeric variable
#'       world$random <- sample(1:100, nrow(world), TRUE)
#'
#'       # draw map
#'       tuimap() %>%
#'         add_map_data(
#'           data = world,
#'           mapping = aes(code = adm0_a3, label = name, value = random)
#'         ) %>%
#'         tui_chart(title = "World map (minus Antarctica)")
#'
#'     })
#'
#'   }
#'
#'   shinyApp(ui, server)
#' }
tuimapOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'tuimaps', width, height, package = 'tuichartr')
}

#' @rdname tuimap-shiny
#' @export
renderTuimap <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, tuimapOutput, env, quoted = TRUE)
}
