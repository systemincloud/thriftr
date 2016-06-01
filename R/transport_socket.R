#' transport_socket.R

#' Socket implementation for client side.
#'
#' The implementation uses a (configurable) fixed-size read buffer
#' but buffers all writes until a flush is performed.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
TSocket <- R6Class("TSocket",
  public = list(
    host = NA,
    port = NA,
    sock = NA,
	  #' @param host the host to connect to
	  #' @param port the (TCP) port to connect to
    initialize = function(host='localhost', port=9090) {
      cat('TSocket\n')
      self$host <- host
      self$port <- port
    },
    is_open = function() isOpen(sock, rw = ""),
    open = function() {
      self$sock <- socketConnection(host=self$host,
                                    port=self$port,
                                    blocking=TRUE,
                                    server=FALSE,
                                    open="r+b")
    }

  )
)
