#' rpc.R
#'
#' @include protocol/binary.R
#' @include transport/buffered/binary.R

make_client <- function(service, host="localhost",
                                 port=9090,
                                 unix_socket=NA,
                                 proto_factory=TBinaryProtocolFactory$new(),
                                 trans_factory=TBufferedTransportFactory$new(),
                                 timeout=NA) {

}