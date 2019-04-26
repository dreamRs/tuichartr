
#' Add data to a \code{tuichart}
#'
#' @param tui A \code{tuichart} \code{htmlwidget} object.
#' @param data A \code{data.frame}, or an object coercible to \code{data.frame}.
#' @param mapping Default list of aesthetic mappings to use for chart.
#'
#' @return A \code{tuichart} \code{htmlwidget} object.
#' @export
#'
#' @importFrom rlang as_label eval_tidy
#'
# @examples
add_data <- function(tui, data, mapping) {
  data <- as.data.frame(data)
  mapdata <- lapply(mapping, rlang::eval_tidy, data = data)
  if (is.null(mapping$group)) {
    series <- list(list(
      name = rlang::as_label(mapping$y),
      data = mapdata$y
    ))
  } else {
    series <- split(x = mapdata$y, f = mapdata$group)
    series <- lapply(
      X = seq_along(series),
      FUN = function(i) {
        list(name = names(series)[i], data = series[[i]])
      }
    )
  }
  tui$x$data <- list(
    categories = unique(mapdata$x),
    series = series
  )
  tui
}
