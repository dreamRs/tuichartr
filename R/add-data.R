
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
  type <- tui$x$type
  if (type %in% c("scatterChart", "bubbleChart")) {
    tui <- add_scatter_data(tui, data, mapping)
  } else if (type %in% c("heatmapChart")) {
    tui <- add_heat_data(tui, data, mapping)
  } else if (type %in% c("boxplotChart")) {
    tui <- add_boxplot_data(tui, data, mapping)
  } else if (type %in% c("treemapChart")) {
    tui <- add_tree_data(tui, data, mapping)
  } else {
    tui <- add_default_data(tui, data, mapping)
  }
  tui
}

#' @export
#'
#' @rdname add-data
add_default_data <- function(tui, data, mapping) {
  data <- as.data.frame(data)
  mapdata <- lapply(mapping, rlang::eval_tidy, data = data)
  if (is.null(mapping$group)) {
    series <- list(list(
      name = rlang::as_label(mapping$y),
      data = mapdata$y
    ))
  } else {
    series <- split(x = mapdata$y, f = as.character(mapdata$group))
    series <- lapply(
      X = seq_along(series),
      FUN = function(i) {
        list(name = names(series)[i], data = series[[i]])
      }
    )
  }
  tui$x$data <- list(
    categories = as.character(unique(mapdata$x)),
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

#' @export
#'
#' @rdname add-data
add_tree_data <- function(tui, data, mapping) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  mapdata <- lapply(mapping, rlang::eval_tidy, data = data)
  mapdata <- lapply(
    X = mapdata,
    FUN = function(x) {
      if (inherits(x, "factor"))
        as.character(x)
      else
        x
    }
  )
  mapdata <- as.data.frame(mapdata, stringsAsFactors = FALSE)
  tui$x$data <- list(
    series = split_rec(mapdata)
  )
  tui
}

make_child <- function(labels, values) {
  mapply(
    FUN = function(label, value) {
      list(
        label = label,
        value = value
      )
    },
    label = labels,
    value = values,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )
}

split_rec <- function(df, level = 1) {
  if (is.null(df[[paste0("level", level)]])) {
    stop("[tuichart - treemap] Incorrect type of data, groups levels are probably not unique", call. = FALSE)
  }
  if (all(table(df[[paste0("level", level)]]) == 1)) {
    list(
      label = unique(df[[paste0("level", level - 1)]]),
      children = make_child(df[[paste0("level", level)]], values = df$value)
    )
  } else {
    dfsplit <- split(x = df, f = df[[paste0("level", level)]])
    lapply(
      X = unname(dfsplit),
      FUN = split_rec,
      level = level + 1
    )
  }
}


#' @export
#'
#' @rdname add-data
#' @importFrom stats median quantile
add_boxplot_data <- function(tui, data, mapping) {
  data <- as.data.frame(data)
  mapdata <- lapply(mapping, rlang::eval_tidy, data = data)
  if (is.null(mapping$group)) {
    splitdata <- split(x = mapdata$y, f = mapdata$x)
    series <- lapply(
      X = seq_along(splitdata),
      FUN = function(i) {
        x <- splitdata[[i]]
        # list(
        #   name = names(splitdata)[i],
        #   data = list(unname(c(
        #     min(x, na.rm = TRUE),
        #     quantile(x = x, na.rm = TRUE, probs = 1/4),
        #     median(x = x, na.rm = TRUE),
        #     quantile(x = x, na.rm = TRUE, probs = 3/4),
        #     max(x, na.rm = TRUE)
        #   )))
        # )
        unname(c(
          min(x, na.rm = TRUE),
          quantile(x = x, na.rm = TRUE, probs = 1/4),
          median(x = x, na.rm = TRUE),
          quantile(x = x, na.rm = TRUE, probs = 3/4),
          max(x, na.rm = TRUE)
        ))
      }
    )
    series <- list(list(name = rlang::as_label(mapping$y), data = series))
  } else {
    splitgroup <- split(x = as.data.frame(mapdata), f = mapdata$group)
    series <- lapply(
      X = seq_along(splitgroup),
      FUN = function(i) {
        splity <- split(x = splitgroup[[i]], f = splitgroup[[i]]$x)
        list(
          name = names(splitgroup)[i],
          data = lapply(
            X = seq_along(splity),
            FUN = function(j) {
              x <- splity[[j]]$y
              unname(c(
                min(x, na.rm = TRUE),
                quantile(x = x, na.rm = TRUE, probs = 1/4),
                median(x = x, na.rm = TRUE),
                quantile(x = x, na.rm = TRUE, probs = 3/4),
                max(x, na.rm = TRUE)
              ))
            }
          )
        )
      }
    )
  }
  categories <- as.character(unique(mapdata$x))
  if (length(categories) == 1)
    categories <- list(categories)
  tui$x$data <- list(
    categories = categories,
    series = series
  )
  tui
}




