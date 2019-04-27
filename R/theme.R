
#' Use a custom theme or set global theme
#'
#' @param tui A \code{tuichart} \code{htmlwidget} object.
#' @param name The name of the theme.
#' @param ... Parameters to define theme, see \url{http://nhn.github.io/tui.chart/latest/tui.chart#registerTheme}.
#'
#' @return A \code{tuichart} \code{htmlwidget} object.
#' @export
#'
#' @name theme
#'
tui_theme <- function(tui, ..., name = NULL) {
  if (is.null(name))
    name <- "tui-theme"
  tui <- .widget_opt(
    widget = tui,
    name = "theme",
    .list = list(name)
  )
  tui$x$theme <- list(
    name = name,
    values = list(...)
  )
  tui
}

#' @export
#'
#' @rdname theme
set_tui_theme <- function(name, ...) {
  options("tuichartr.theme" = list(
    name = name,
    values = list(...)
  ))
}

