#' protocol_binary.R
#'
#' @export TBinaryProtocolFactory

TBinaryProtocolFactory <- R6Class("TBinaryProtocolFactory",
  public = list(
    strict_read = TRUE,
    strict_write = TRUE,
    initialize = function(strict_read, strict_write) {
      cat('TBinaryProtocolFactory\n')
      if (!missing(strict_read)) self$strict_read <- strict_read
      if (!missing(strict_write)) self$strict_write <- strict_write
    }
  )
)