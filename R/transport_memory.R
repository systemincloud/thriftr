
#' TMemoryBuffer
#' 
#' Wraps a raw array as a TTransport.
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#' 
#' @export
TMemoryBuffer <- R6Class("TMemoryBuffer",
  inherit = TTransportBase,
  public = list(
    buffer = NA,
    pos = NA,
    # value -- a value as the initial value in the buffer object.
    # If value is set, the transport can be read first.
    initialize = function(value = NULL) {
      if (!is.null(value)) {
        self$buffer <- value
      } else {
        self$buffer <- raw()
      }
      self$pos <- 0
    },
    is_open = function() {
        return(!self$buffer.closed)
    },
    open = function() {
    },
    close = function() {
        buffer.close()
    },
    read = function(sz) {
        return(private$read_(sz))
    },
    write = function(buf) {
      self$buffer <- c(self$buffer, buf)
    },
    flush = function() {
    },
    getvalue = function() {
      return(self$buffer)
    },
    setvalue = function(value) {
        self$buffer <- value
        self$pos <- 0
    }
  ),
  private = list(
    read_ = function(sz) {
      res <- head(self$buffer, sz)
      self$buffer <- tail(self$buffer, -sz)
      self$pos <- self$pos + length(res)
      return(res)
    }
  )
)
