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

#' TSocket
#'
#' Socket implementation for client side.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#'
#' @export
TSocket <- R6Class("TSocket",
  public = list(
    host = NA,
    port = NA,
    server = NA,
    sock = NA,
    initialize = function(
        host,
        port,
        server = FALSE) {
        self$host <- host
        self$port <- port
        self$server <- server
    },
    is_open = function() {
      !is.na(self$sock) && isOpen(self$sock)
    },
    open = function() {
      private$init_sock()
    },
    read = function(sz) {
      ret <- readBin(self$sock, raw(), sz)
      if (length(ret) == 0) {
        stop()
      }
      ret
    },
    write = function(buff) {
      writeBin(buff, self$sock)
    },
    flush = function() {
    },
    close = function() {
      if (is.na(self$sock)) {
        return()
      }
      close(self$sock)
      self$sock <- NA
    }
  ),
  private = list(
    init_sock = function() {
      self$sock <- socketConnection(
          host = self$host,
          port = self$port,
          blocking = TRUE,
          server = self$server,
          open = "r+b")
    }
  )
)


#' TServerSocket
#'
#' Socket implementation for server side.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#'
#' @export
TServerSocket <- R6Class("TServerSocket",
  public = list(
    host = NA,
    port = NA,
    sock = NA,
    initialize = function(
        host = NA,
        port = NA) {
      self$host <- host
      self$port <- port
    },
    listen = function() {
      self$sock <- TSocket$new(self$host, self$port, TRUE)
    },
    accept = function() {
      self$sock$open()
      return(self$sock)
    }
  )
)
