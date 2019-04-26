
# dropNulls
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

#' Utility function to create tui-chart parameters JSON
#'
#' @param ax A \code{tuichart} \code{htmlwidget} object.
#' @param name Slot's name to edit.
#' @param ... Arguments for the slot.
#' @param .list Alternative to \code{...} to use a \code{list}.
#'
#' @return A \code{tuichart} \code{htmlwidget} object.
#'
#' @importFrom utils modifyList
#'
#' @noRd
.widget_opt <- function(widget, name, ..., .list = NULL) {

  if (is.null(.list)) {
    .list <- dropNulls(list(...))
  }

  if (is.null(widget$x$options[[name]])) {
    widget$x$options[[name]] <- .list
  } else {
    widget$x$options[[name]] <- utils::modifyList(
      x = widget$x$options[[name]],
      val = .list,
      keep.null = TRUE
    )
  }

  return(widget)
}
