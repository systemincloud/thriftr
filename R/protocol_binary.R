#' protocol_binary.R
#'


#' Binary implementation of the Thrift protocol driver.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
TBinaryProtocol <- R6Class("TBinaryProtocol",
  public = list(
    trans = NA,
    strict_read = NA,
    strict_write = NA,
    decode_response = NA,
    initialize = function(trans, strict_read,
                                 strict_write,
                                 decode_response) {
      self$trans <- trans
      self$strict_read <- strict_read
      self$strict_write <- strict_write
      self$decode_response <- decode_response
    },
    skip = function(ttype) {
      skip(self$trans, ttype)
    },
    read_message_begin = function(ttype) {
      return(read_message_begin(self$trans, strict=self.strict_read))
    },
    read_message_end = function() { },
    write_message_begin = function(name, ttype, seqid) {
      write_message_begin(self$trans, name, ttype, seqid, strict=self$strict_write)
    },
    write_message_end = function() { },
    read_struct = function(obj) {
      return(read_struct(self$trans, obj, self$decode_response))
    },
    write_struct = function(obj) {
      write_val(self$trans, TType.STRUCT, obj)
    }
  )
)


#' @docType class
#' @importFrom R6 R6Class
#' @export TBinaryProtocolFactory
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
TBinaryProtocolFactory <- R6Class("TBinaryProtocolFactory",
  public = list(
    strict_read = NA,
    strict_write = NA,
    decode_response = NA,
    initialize = function(strict_read=TRUE,
                          strict_write=TRUE,
                          decode_response=TRUE) {
      self$strict_read <- strict_read
      self$strict_write <- strict_write
      self$decode_response <- decode_response
    },
    get_protocol = function(trans) {
      return(TBinaryProtocol$new(trans, self$strict_read,
                                        self$strict_write,
                                        self$decode_response))
    }
  )
)