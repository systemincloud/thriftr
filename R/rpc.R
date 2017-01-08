#' rpc.R

#' Create client side thrift API
#'
#' @param service a
#' @param host a
#' @param port a
#' @param proto_factory a
#' @param trans_factory a
#' @param timeout a
#' @param cafile a
#' @param ssl_context a
#' @param certfile a
#' @param keyfile a
#' 
#' @export
make_client = function(service,
                       host="localhost",
                       port=9090,
                       proto_factory=TBinaryProtocolFactory$new(),
                       trans_factory=TBufferedTransportFactory$new(),
                       timeout=NA,
                       cafile=NA, 
                       ssl_context=NA, 
                       certfile=NA, 
                       keyfile=NA) {
  socket <- NA
  
  if(!is.na(host) && !is.na(port)) {
    if(!is.na(cafile) || !is.na(ssl_context))
      socket <- TSSLSocket$new(host, 
                               port, 
                               socket_timeout=timeout,
                               cafile=cafile,
                               certfile=certfile, 
                               keyfile=keyfile,
                               ssl_context=ssl_context)
    else socket <- TSocket$new(host, port, socket_timeout=timeout)
  } else stop("Host/port must be provided.")
  
  transport <- trans_factory$get_transport(socket)
  protocol  <- proto_factory$get_protocol(transport)
  transport$open()
  return(TClient$new(service, protocol))
}