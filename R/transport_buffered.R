
DEFAULT_BUFFER <- 4096

#' TBufferedTransport
#' 
#' Class that wraps another transport and buffers its I/O.
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#' 
#' @export
TBufferedTransport <- R6Class("TBufferedTransport",
  inherit = TTransportBase,
  public = list(
    initialize = function(trans, buf_size = DEFAULT_BUFFER) {
      private$trans <- trans
      private$wbuf <- TMemoryBuffer$new()
      private$rbuf <- TMemoryBuffer$new()
      private$buf_size <- buf_size
    },
    is_open = function() {
      return(private$trans$is_open())
    },
    open = function() {
      return(private$trans$open())
    },
    close = function() {
      return(private$trans$close())
    },
    read = function(sz) {
      private$rbuf <- TMemoryBuffer$new()
      private$rbuf$setvalue(private$trans$read(sz))
      private$rbuf$read(sz)
    },
    write = function(buf) {
      private$wbuf$write(buf)
    },
    flush = function() {
      out <- private$wbuf$getvalue()
      # reset wbuf before write/flush to preserve state on underlying failure
      private$wbuf <- TMemoryBuffer$new()
      private$trans$write(out)
      private$trans$flush()
    },
    getvalue = function() {
      return(self$trans$getvalue())
    }
  ),
  private = list(
    trans = NA,
    wbuf = NA,
    rbuf = NA,
    buf_size = NA,
    read_ = function(sz) {
      res <- head(self$buffer, sz)
      self$buffer <- tail(self$buffer, -sz)
      self$pos <- self$pos + length(res)
      return(res)
    }
  )
)

#' TBufferedTransportFactory
#'
#' TBufferedTransportFactory generates TBufferedTransport.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#'
#' @export
TBufferedTransportFactory <- R6Class("TBufferedTransportFactory",
  public = list(
    get_transport = function(trans) {
      return(TBufferedTransport$new(trans))
    }
  )
)
