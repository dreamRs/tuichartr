
#' Set options for a chart
#'
#' @param tui A \code{tuichart} \code{htmlwidget} object.
#' @param ... Named parameters
#'
#' @return A \code{tuichart} \code{htmlwidget} object.
#' @export
#'
#' @name tui-options
#'
# @examples
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
# @examples
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
# @examples
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
# @examples
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
# @examples
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
# @examples
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
# @examples
tui_plot <- function(tui, ...) {
  .widget_opt(
    widget = tui,
    name = "plot",
    ...
  )
}

