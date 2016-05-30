#' rpc.R
#'
#' @include protocol_binary.R
#' @include transport_buffered.R
#' @include transport_socket.R


#' @export
make_client <- function(service, host="localhost",
                                 port=9090,
                                 proto_factory,
                                 trans_factory,
                                 timeout) {
  cat('make_client\n')
  if (missing(proto_factory)) proto_factory <- TBinaryProtocolFactory$new()
  if (missing(trans_factory)) trans_factory <- TBufferedTransportFactory$new()

  socket <- TSocket$new(host, port)

  transport <- trans_factory$get_transport(socket)
  protocol <- proto_factory$get_protocol(transport)
  transport$open()

  return(TClient$new(service, protocol))
}