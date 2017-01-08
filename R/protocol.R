# MIT License
#
# Copyright (c) 2016 System in Cloud - Marek Jagielski
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#    
#    The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# Binary implementation of the Thrift protocol driver
TBinaryProtocol <- R6Class("TBinaryProtocol",
  public = list(
    trans           = NA,
    strict_read     = NA,
    strict_write    = NA,
    decode_response = NA,
    initialize = function(trans, strict_read=TRUE, 
                                 strict_write=TRUE, 
                                 decode_response=TRUE) {
      self$trans           <- trans
      self$strict_read     <- strict_read
      self$strict_write    <- strict_write
      self$decode_response <- decode_response
    },
    skip = function(ttype) {
    },
    read_message_begin = function() {
    },
    read_message_end = function() {
    },
    write_message_begin = function(name, ttype, seqid) {
    },
    write_message_end = function() {
    },
    read_struct = function(obj) {
    },
    write_struct = function(obj) {
    }
  )
)

#' TBinaryProtocolFactory ...
#' 
#' This class is ...
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#' 
#' @export
TBinaryProtocolFactory <- R6Class("TBinaryProtocolFactory",
  public = list(
    strict_read     = NA,
    strict_write    = NA,
    decode_response = NA,
    initialize = function(strict_read=TRUE, strict_write=TRUE, decode_response=TRUE) {
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