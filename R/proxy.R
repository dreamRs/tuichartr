
#' Proxy for tuichart htmlwidget
#'
#' @param shinyId single-element character vector indicating the output ID of the
#'   chart to modify (if invoked from a Shiny module, the namespace will be added
#'   automatically).
#' @param session the Shiny session object to which the chart belongs; usually the
#'   default value will suffice.
#'
#' @noRd
#'
#' @importFrom shiny getDefaultReactiveDomain
#'
tuichartProxy <- function(shinyId, session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session)) {
    stop("tuichartProxy must be called from the server function of a Shiny app")
  }

  if (!is.null(session$ns) && nzchar(session$ns(NULL)) && substring(shinyId, 1, nchar(session$ns(""))) != session$ns("")) {
    shinyId <- session$ns(shinyId)
  }

  structure(
    list(
      session = session,
      id = shinyId,
      x = list()
    ),
    class = c("tuichartProxy", "htmlwidgetProxy")
  )
}

#' Call a proxy method
#'
#' @param proxy  A \code{proxy} \code{htmlwidget} object.
#' @param name Proxy method.
#' @param ... Arguments passed to method.
#'
#' @return A \code{htmlwidgetProxy} \code{htmlwidget} object.
#' @noRd
.call_proxy <- function(proxy, name, ...) {
  if (!"htmlwidgetProxy" %in% class(proxy))
    stop("This function must be used with a htmlwidgetProxy object", call. = FALSE)
  proxy$session$sendCustomMessage(
    type = sprintf("proxy-tui-chart-%s", name),
    message = list(id = proxy$id, data = dropNulls(list(...)))
  )
  proxy
}
.call_proxy2 <- function(proxy, name, l) {
  if (!"htmlwidgetProxy" %in% class(proxy))
    stop("This function must be used with a htmlwidgetProxy object", call. = FALSE)
  proxy$session$sendCustomMessage(
    type = sprintf("proxy-tui-chart-%s", name),
    message = list(id = proxy$id, data = l)
  )
  proxy
}

prox_add_data <- function(proxy, categories, values) {
  .call_proxy(
    proxy = proxy,
    name = "addData",
    categories = categories,
    values = list(values)
  )
}

prox_set_data <- function(proxy, categories, values) {
  .call_proxy(
    proxy = proxy,
    name = "setData",
    categories = categories,
    values = list(values)
  )
}

