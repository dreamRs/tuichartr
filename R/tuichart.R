
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
#' @examples
#'
#' library(gapminder)
#' library(dplyr)
#'
#' n_countries <- gapminder_unfiltered %>%
#'   filter(year == 2007) %>%
#'   count(continent)
#'
#' # a barchart
#' tuichart("column") %>%
#'   add_data(n_countries, aes(x = continent, y = n)) %>%
#'   tui_series(showLabel = TRUE)
#'
#' # a treemap
#' tuichart("treemap") %>%
#'   add_data(n_countries, aes(level1 = continent, value = n)) %>%
#'   tui_series(showLabel = TRUE)
#'
#' # a pie chart
#' tuichart("pie") %>%
#'   add_data(n_countries, aes(x = continent, y = n)) %>%
#'   tui_series(showLabel = TRUE)
#'
#'
#'
#' gdp_italy <- gapminder_unfiltered %>%
#'   filter(country == "Italy")
#'
#' # line chart
#' tuichart("line") %>%
#'   add_data(gdp_italy, aes(x = year, y = gdpPercap)) %>%
#'   tui_chart(
#'     title = "A chart",
#'     format = "0.00"
#'   ) %>%
#'   tui_series(
#'     zoomable = TRUE
#'   ) %>%
#'   tui_plot(
#'     hideLine = TRUE
#'   ) %>%
#'   tui_xAxis(
#'     tickInterval = "auto",
#'     title = "X axis title"
#'   )
#'
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
#'
#' @examples
#' if (interactive()) {
#'
#'   library(shiny)
#'   library(dplyr)
#'   library(ggplot2)
#'   library(tuichartr)
#'
#'   ui <- fluidPage(
#'     tags$h2("Include tuichart in Shiny"),
#'     fluidRow(
#'       column(
#'         width = 3,
#'         checkboxGroupInput(
#'           inputId = "year",
#'           label = "Year:",
#'           choices = c(1999, 2008),
#'           selected = c(1999, 2008)
#'         )
#'       ),
#'       column(
#'         width = 9,
#'         tuichartOutput(outputId = "my_chart")
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'
#'     output$my_chart <- renderTuichart({
#'
#'       data <- filter(mpg, year %in% input$year) %>%
#'         count(manufacturer)
#'
#'       tuichart("bar") %>%
#'         add_data(data, aes(x = manufacturer, y = n)) %>%
#'         tui_chart(title = "My cool chart") %>%
#'         tui_xAxis(title = "Count") %>%
#'         tui_legend(visible = FALSE) %>%
#'         tui_series(showLabel = TRUE)
#'     })
#'
#'
#'   }
#'
#'   shinyApp(ui, server)
#' }
tuichartOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'tuichart', width, height, package = 'tuichartr')
}

#' @rdname tuichart-shiny
#' @export
renderTuichart <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, tuichartOutput, env, quoted = TRUE)
}
