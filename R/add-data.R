
#' Add data to a \code{tuichart}
#'
#' @param tui A \code{tuichart} \code{htmlwidget} object.
#' @param data A \code{data.frame}, or an object coercible to \code{data.frame}.
#' @param mapping Default list of aesthetic mappings to use for chart.
#'
#' @return A \code{tuichart} \code{htmlwidget} object.
#' @export
#'
#' @name add-data
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

#' @export
#'
#' @rdname add-data
add_scatter_data <- function(tui, data, mapping) {
  data <- as.data.frame(data)
  mapdata <- lapply(mapping, rlang::eval_tidy, data = data)
  if (is.null(mapping$group)) {
    series <- list(list(
      name = rlang::as_label(mapping$y),
      data = mapply(
        FUN = function(x, y, label, r) {
          if (r >= 0) {
            list(x = x, y = y, label = label, r = r)
          } else {
            list(x = x, y = y, label = label)
          }
        },
        x = mapdata$x,
        y = mapdata$y,
        label = if (is.null(mapdata$label)) character(1) else mapdata$label,
        r = if (is.null(mapdata$size)) -1 else mapdata$size,
        SIMPLIFY = FALSE
      )
    ))
  } else {
    mapdata$group <- as.character(mapdata$group)
    mapdata <- as.data.frame(mapdata, stringsAsFactors = FALSE)
    series <- split(x = mapdata, f = mapdata$group)
    series <- lapply(
      X = seq_along(series),
      FUN = function(i) {
        data <- series[[i]]
        list(
          name = names(series)[i],
          data = mapply(
            FUN = function(x, y, label, r) {
              if (r >= 0) {
                list(x = x, y = y, label = label, r = r)
              } else {
                list(x = x, y = y, label = label)
              }
            },
            x = data$x,
            y = data$y,
            label = if (is.null(data$label)) character(1) else data$label,
            r = if (is.null(data$size)) -1 else data$size,
            SIMPLIFY = FALSE
          )
        )
      }
    )
  }
  tui$x$data <- list(
    series = series
  )
  tui
}


#' @export
#'
#' @rdname add-data
add_heat_data <- function(tui, data, mapping) {
  data <- as.data.frame(data)
  mapdata <- lapply(mapping, rlang::eval_tidy, data = data)
  tui$x$data <- list(
    categories = list(
      x = as.character(unique(mapdata$x)),
      y = as.character(unique(mapdata$y))
    ),
    series = unname(split(x = mapdata$value, f = as.character(mapdata$y)))
  )
  tui
}



