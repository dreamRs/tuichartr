
#' Set options for a chart
#'
#' @param tui A \code{tuichart} \code{htmlwidget} object.
#' @param ... Named parameters, see \url{http://nhn.github.io/tui.chart/latest/tui.chart}.
#'
#' @return A \code{tuichart} \code{htmlwidget} object.
#' @export
#'
#' @name tui-options
#'
#' @examples
#' # See the vignette for examples
#' vignette("options", package = "tuichartr")
#'
#'
#' library(tuichartr)
#'
#' # a classic scatter plot
#' my_scatter <- tuichart("scatter") %>%
#'   add_data(iris, aes(Sepal.Length, Sepal.Width, group = Species))
#'
#'
#' # add a title
#' my_scatter %>%
#'   tui_chart(title = "Classic Iris example")
#'
#' # format numeric values in all chart
#' # (x-axis, y-axis, tooltip)
#' my_scatter %>%
#'   tui_chart(format = "0.00")
#'
#'
#' # hide legend
#' my_scatter %>%
#'   tui_legend(visible = FALSE)
#'
#' # or change position and hide
#' # checkbox to show/hide group
#' my_scatter %>%
#'   tui_legend(
#'     align = "bottom", # or top, left, right
#'     showCheckbox = FALSE
#'   )
#'
#'
#' # hide the export menu
#' my_scatter %>%
#'   tui_chartExportMenu(visible = FALSE)
#'
#'
#' # some options for axis
#' my_scatter %>%
#'   tui_xAxis(
#'     tickInterval = "auto",
#'     title = "X axis title" # title
#'   ) %>%
#'   tui_yAxis(
#'     max = 10 # y max value
#'   )
tui_legend <- function(tui, ...) {
  .widget_opt(
    widget = tui,
    name = "legend",
    ...
  )
}



#' @export
#'
#' @rdname tui-options
#'
tui_yAxis <- function(tui, ...) {
  .widget_opt(
    widget = tui,
    name = "yAxis",
    ...
  )
}


#' @export
#'
#' @rdname tui-options
#'
tui_xAxis <- function(tui, ...) {
  .widget_opt(
    widget = tui,
    name = "xAxis",
    ...
  )
}



#' @export
#'
#' @rdname tui-options
#'
tui_chart <- function(tui, ...) {
  .widget_opt(
    widget = tui,
    name = "chart",
    ...
  )
}



#' @export
#'
#' @rdname tui-options
#'
tui_series <- function(tui, ...) {
  .widget_opt(
    widget = tui,
    name = "series",
    ...
  )
}



#' @export
#'
#' @rdname tui-options
#'
tui_tooltip <- function(tui, ...) {
  .widget_opt(
    widget = tui,
    name = "tooltip",
    ...
  )
}



#' @export
#'
#' @rdname tui-options
#'
tui_plot <- function(tui, ...) {
  .widget_opt(
    widget = tui,
    name = "plot",
    ...
  )
}


#' @export
#'
#' @rdname tui-options
#'
tui_chartExportMenu <- function(tui, ...) {
  .widget_opt(
    widget = tui,
    name = "chartExportMenu",
    ...
  )
}



