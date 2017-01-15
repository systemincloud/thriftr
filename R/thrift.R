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

#' TType
#'
#' @export
TType <- new.env()
TType$STOP   <- as.integer(0)
TType$VOID   <- as.integer(1)
TType$BOOL   <- as.integer(2)
TType$BYTE   <- as.integer(3)
TType$I08    <- as.integer(3)
TType$DOUBLE <- as.integer(4)
TType$I16    <- as.integer(6)
TType$I32    <- as.integer(8)
TType$I64    <- as.integer(10)
TType$STRING <- as.integer(11)
TType$UTF7   <- as.integer(11)
TType$BINARY <- as.integer(11)  # This here just for parsing. For all purposes, it's a string
TType$STRUCT <- as.integer(12)
TType$MAP    <- as.integer(13)
TType$SET    <- as.integer(14)
TType$LIST   <- as.integer(15)
TType$UTF8   <- as.integer(16)
TType$UTF16  <- as.integer(17)


gen_init = function(cls, thrift_spec=NA, default_spec=NA) {
  if(!is.na(thrift_spec))  cls$thrift_spec <- thrift_spec
#  if(!is.na(default_spec)) cls$add_public('intialize', init_func_generator(cls, default_spec)
  return(cls)
}


TPayload <- R6Class("TPayload",
  public = list(
    add_public = function(name, obj) {
      self[[name]] <- obj
      environment(self[[name]]) <- environment(self$add_public)
    }
  )
)

#' TClient ...
#' 
#' This class is ...
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#' 
#' @export
TClient <- R6Class("TClient",
  public = list(
    service = NA,
    iprot   = NA,
    oprot   = NA,
    initialize = function(service, iprot, oprot=TRUE) {
    }
  )
)