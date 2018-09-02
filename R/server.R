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

TServer <- R6Class("TServer",
  public = list(
    processor = NA,
    trans = NA,
    itrans_factory = NA,
    iprot_factory = NA,
    otrans_factory = NA,
    oprot_factory = NA,
    initialize = function(
        processor,
        trans,
        itrans_factory = NULL,
        iprot_factory = NULL,
        otrans_factory = NULL,
        oprot_factory = NULL) {
      self$processor <- processor
      self$trans <- trans

      self$itrans_factory <- itrans_factory
      if (is.null(self$itrans_factory)) {
        self$itrans_factory <- TBufferedTransportFactory$new()
      }
      self$iprot_factory  <- iprot_factory
      if (is.null(self$iprot_factory)) {
        self$iprot_factory <- TBinaryProtocolFactory$new()
      }
      self$otrans_factory <- otrans_factory
      if (is.null(self$otrans_factory)) {
        self$otrans_factory <- itrans_factory
      }
      self$oprot_factory  <- oprot_factory
      if (is.null(self$oprot_factory)) {
        self$oprot_factory <- iprot_factory
      }
    },
    serve = function() {
    },
    close = function() {
    }
  )
)

TSimpleServer <- R6Class("TSimpleServer",
  inherit = TServer,
  public = list(
    closed = NA,
    initialize = function(
        processor = NULL,
        trans = NULL,
        itrans_factory = NULL,
        iprot_factory = NULL,
        otrans_factory = NULL,
        oprot_factory = NULL) {
      super$initialize(
        processor,
        trans,
        itrans_factory,
        iprot_factory,
        otrans_factory,
        oprot_factory
      )
      self$closed <- FALSE
    },
    serve = function() {
      self$trans$listen()
      while (TRUE) {
        self$closed <- FALSE
        client <- self$trans$accept()
        itrans <- self$itrans_factory$get_transport(client)
        otrans <- self$otrans_factory$get_transport(client)
        iprot <- self$iprot_factory$get_protocol(itrans)
        oprot <- self$oprot_factory$get_protocol(otrans)

        while (!self$closed) {
          tryCatch({
            self$processor$process(iprot, oprot)
          }, error = function(e) {
            self$close()
          })
        }

        itrans$close()
        otrans$close()
      }
    },
    close = function() {
      self$closed <- TRUE
    }
  )
)
