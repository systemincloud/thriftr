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

#' Create client side thrift API
#'
#' @param service parsed service
#' @param host server host
#' @param port server tcp port
#' @param proto_factory factory that generates protocol implementation
#' @param trans_factory factory that generates transport implementation
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # File calc.thrift content:
#' # service Calculator {
#' #   i32 add(1:i32 a, 2:i32 b);
#' #   i32 sub(1:i32 a, 2:i32 b);
#' #   i32 mult(1:i32 a, 2:i32 b);
#' #   i32 div(1:i32 a, 2:i32 b);
#' # }
#' #
#'
#' calc_thrift <- thriftr::t_load("calc.thrift", module_name="calc_thrift")
#'
#' cal <- thriftr::make_client(
#'     calc_thrift$Calculator,
#'     "127.0.0.1",
#'     6000)
#'
#' a <- cal$mult(5, 2)
#' b <- cal$sub(7, 3)
#' c <- cal$sub(6, 4)
#' d <- cal$mult(b, 10)
#' e <- cal$add(a, d)
#' f <- cal$div(e, c)
#' print(f)
#' }
make_client = function(service,
    host="localhost",
    port=9090,
    proto_factory=TBinaryProtocolFactory$new(),
    trans_factory=TBufferedTransportFactory$new()) {

  socket <- NA
  if(!is.na(host) || !is.na(port)) {
    socket <- TSocket$new(host, port)
  } else stop("Host/port must be provided.")

  transport <- trans_factory$get_transport(socket)
  protocol <- proto_factory$get_protocol(transport)
  transport$open()

  return(TClient$new(service, protocol))
}

#' Create server side thrift API
#'
#' @param service parsed service
#' @param handler R6 class implementing service
#' @param host server host
#' @param port port server tcp port
#' @param proto_factory factory that generates protocol implementation
#' @param trans_factory factory that generates transport implementation
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # File calc.thrift content:
#' # service Calculator {
#' #   i32 add(1:i32 a, 2:i32 b);
#' #   i32 sub(1:i32 a, 2:i32 b);
#' #   i32 mult(1:i32 a, 2:i32 b);
#' #   i32 div(1:i32 a, 2:i32 b);
#' # }
#' #
#'
#' calc_thrift <- thriftr::t_load("calc.thrift", module_name="calc_thrift")
#'
#' Dispatcher <- R6::R6Class("Dispatcher",
#'   public = list(
#'     add = function(a, b) {
#'       print(sprintf("add -> %s + %s", a, b))
#'       return(a + b)
#'     },
#'     sub = function(a, b) {
#'       print(sprintf("sub -> %s - %s", a, b))
#'       return(a - b)
#'     },
#'     mult = function(a, b) {
#'       print(sprintf("mult -> %s * %s", a, b))
#'       return(a * b)
#'     },
#'     div = function(a, b) {
#'       print(sprintf("div -> %s / %s", a, b))
#'       return(a / b)
#'     }
#'   )
#' )
#'
#' server <- thriftr::make_server(
#'     calc_thrift$Calculator,
#'     Dispatcher$new(),
#'     "127.0.0.1",
#'     6000)
#'
#' print("serving...")
#'
#' server$serve()
#' }
make_server = function(service,
    handler,
    host="localhost",
    port=9090,
    proto_factory=TBinaryProtocolFactory$new(),
    trans_factory=TBufferedTransportFactory$new()) {
  processor <- TProcessor$new(service, handler)

  server_socket <- TServerSocket$new(
     host=host, port=port)

  server <- TSimpleServer$new(processor, server_socket,
      iprot_factory=proto_factory,
      itrans_factory=trans_factory)

  return(server)
}