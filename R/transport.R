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


# Socket implementation for client side
TSocket <- R6Class("TSocket",
  public = list(
    # Initialize a TSocket
    #
    # TSocket can be initialized in 2 ways:
    #  * host + port. can configure to use AF_INET/AF_INET6
    #  * socket. should pass already opened socket here.
    #
    # @param host(str)    The host to connect to.
    # @param port(int)    The (TCP) port to connect to.
    # @param sock(socket)     Initialize with opened socket directly.
    # 
    # If this param used, the host, port and unix_socket params will
    # be ignored.
    #  @param socket_family(str) socket.AF_INET or socket.AF_INET6. only
    #  take effect when using host/port
    #  @param socket_timeout   socket timeout in ms
    #  @param connect_timeout  connect timeout in ms, only used in
    #  connection, will be set to socket_timeout if not set.
    initialize = function(host=NA,
                          port=NA,
                          sock=NA,
                          socket_timeout=3000,
                          connect_timeout=NA) {
                        
    },
    set_handle = function(sock) {
    },
    set_timeout = function(ms) {
    },
    is_open = function() {
    },
    open = function() {
    },
    read = function(sz) {
    },
    write = function(buff) {
    },
    flush = function() {
    },
    close = function() {
    }
  )
)


# SSL socket implementation for client side
TSSLSocket <- R6Class("TSSLSocket",
  inherit = TSocket,
  public = list(
    # Initialize a TSSLSocket
    #
    # @param validate(bool)       Set to False to disable SSL certificate
    # validation and hostname validation. Default enabled.
    # @param cafile(str)          Path to a file of concatenated CA
    # certificates in PEM format.
    # @param capath(str)           path to a directory containing several CA
    # certificates in PEM format, following an OpenSSL specific layout.
    # @param certfile(str)        The certfile string must be the path to a
    # single file in PEM format containing the certificate as well as
    # any number of CA certificates needed to establish the
    # certificate’s authenticity.
    # @param keyfile(str)         The keyfile string, if not present,
    # the private key will be taken from certfile as well.
    # @param ciphers(list<str>)   The cipher suites to allow
    # @param ssl_context(SSLContext)  Customize the SSLContext, can be used
    # to persist SSLContext object. Caution it's easy to get wrong, only
    # use if you know what you're doing.
    #
    # The `host` must be the same with server if validate enabled.
    initialize = function(host=NA,
                          port=NA,
                          socket_timeout=3000,
                          connect_timeout=NA,
                          ssl_context=NA,
                          validate=TRUE,
                          cafile=NA,
                          capath=NA,
                          certfile=NA,
                          keyfile=NA,
                          ciphers=NULL) {
    }
  )
)
        

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