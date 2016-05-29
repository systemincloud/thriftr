#' binary.R

library(R6)

TBinaryProtocolFactory <- R6Class("TBinaryProtocolFactory",
  public = list(
    strict_read = TRUE,
    strict_write = TRUE,
    initialize = function(strict_read, strict_write) {
      if (!missing(strict_read)) self$strict_read <- strict_read
      if (!missing(strict_write)) self$strict_write <- strict_write
    }
  )
)