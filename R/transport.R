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

# Class that wraps another transport and buffers its I/O.
#
# The implementation uses a (configurable) fixed-size read buffer
# but buffers all writes until a flush is performed.
TBufferedTransport <- R6Class("TBufferedTransport",
  public = list(
    DEFAULT_BUFFER = 4096,
    trans    = NA,
    wbuf     = NA,
    rbuf     = NA,
    buf_size = NA,
    initialize = function(trans, buf_size=self$DEFAULT_BUFFER) {
      self$trans    <- trans
      self$wbuf     <- NULL
      self$rbuf     <- NULL
      self$buf_size <- buf_size
    },
    is_open = function() {
    },
    open = function() {
    },
    close = function() {
    },
    read = function(sz) {
    },
    write = function(buf) {
    },
    flush = function() {
    },
    getvalue = function() {
    }
  )
)

#' TBufferedTransportFactory ...
#' 
#' This class is ...
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