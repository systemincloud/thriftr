#' rpc.R
#'
#' @include protocol_binary.R
#' @include transport_buffered.R
#'
#' @export


make_client <- function(service, host="localhost",
                                 port=9090,
                                 proto_factory,
                                 trans_factory,
                                 timeout) {
  cat('make_client\n')
  if (missing(proto_factory)) proto_factory <- TBinaryProtocolFactory$new()
  if (missing(trans_factory)) proto_factory <- TBufferedTransportFactory$new()
}