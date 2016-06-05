#' transport.R

readall = function(read_fn, sz) {
  buff = raw(0)
  have = 0
  while (have < sz) {
    chunk = read_fn(sz - have)
	  have = have + len(chunk)
	  buff = buff + chunk
	  if (len(chunk) == 0) {
		  stop("End of file reading from transport")
	  }
  }
  return(buff)
}


#' Base class for Thrift transport layer.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
TTransportBase <- R6Class("TTransportBase",
  public = list(
	  read = function(sz) {
	    return(readall(private$read_, sz))
    }
  ),
  private  = list(
	  read_ = function(sz) {
	    stop("Not Implemented.")
    }
  )
)
