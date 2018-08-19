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
#' @param service a
#' @param host a
#' @param port a
#' @param proto_factory a
#' @param trans_factory a
#' 
#' @export
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