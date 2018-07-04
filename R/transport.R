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




readall = function(read_fn, sz) {
  buff <- raw()
  have <- 0
  while (have < sz) {
    chunk <- read_fn(sz - have)
    have <- have + length(chunk)
    buff <- c(buff, chunk)

    if (length(chunk) == 0) {
      stop(paste("[TTransportException]", TTransportException$END_OF_FILE,
        "End of file reading from transport"))

    }
  }

  return(buff)
}


# Base class for Thrift transport layer.
TTransportBase <- R6Class("TTransportBase",
  public = list(
    read = function(sz) {
      return(readall(private$read_, sz))
    }
  ),
  private = list(
    read_ = function(sz) {
      stop("NotImplementedError")
    }
  )
)

TTransportException <- new.env(hash=TRUE)
TTransportException$UNKNOWN <- 0
TTransportException$NOT_OPEN <- 1
TTransportException$ALREADY_OPEN <- 2
TTransportException$TIMED_OUT <- 3
TTransportException$END_OF_FILE <- 4