#' transport_buffered.R

#' Class that wraps another transport and buffers its I/O.
#'
#' The implementation uses a (configurable) fixed-size read buffer
#' but buffers all writes until a flush is performed.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
TBufferedTransport <- R6Class("TBufferedTransport",
  inherit=TTransportBase,
  public = list(
    DEFAULT_BUFFER = 4096,
    initialize = function(trans, buf_size=TBufferedTransport$DEFAULT_BUFFER) {
      private$trans <- trans
#      private$wbuf <- raw(buf_size)
#      private$rbuf <- raw(buf_size)
#      private$buf_size <- buf_size
    },
    is_open = function() private$trans$is_open(),
    open = function() private$trans$open(),
    close = function() private$trans$close()
  ),
  private = list(
    trans = NA,
    wbuf = NA,
    rbuf = NA,
    buf_size = NA
  )
)

#' @docType class
#' @importFrom R6 R6Class
#' @export TBufferedTransportFactory
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
TBufferedTransportFactory <- R6Class("TBufferedTransportFactory",
  public = list(
    get_transport = function(trans) {
      return(TBufferedTransport$new(trans))
    }
  )
)