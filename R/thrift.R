#' thrift.R

TType <- R6Class("TType",
  public = list(
    STOP = 0,
    VOID = 1,
    BOOL = 2,
    BYTE = 3,
    I08 = 3,
    DOUBLE = 4,
    I16 = 6,
    I32 = 8,
    I64 = 10,
    STRING = 11,
    UTF7 = 11,
    BINARY = 11,  # This here just for parsing. For all purposes, it's a string
    STRUCT = 12,
    MAP = 13,
    SET = 14,
    LIST = 15,
    UTF8 = 16,
    UTF16 = 17
  )
)


TClient <- R6Class("TClient",
  public = list(
    initialize = function(service, iprot, oprot) {
      private$service <- service
      private$iprot <- iprot
      private$oprot <- if (missing(oprot)) iprot else oprot
      private$seqid <- 0
    },
    close = function() {
      self$iprot$trans$close()
      if (self$iprot != self$oprot)
            self$oprot$trans$close()
    }
  ),
  private = list(
    service = NA,
    iprot = NA,
    oprot = NA,
    seqid = NA
  )
)
