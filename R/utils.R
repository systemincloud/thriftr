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

#' hexlify
#'
#' String representation of raw array
#'
#' @param byte_array raw array
#' @param delimeter separation character
#'
#' @return string
#'
#' @export
hexlify = function(byte_array, delimeter = ' ') {
  as.character(paste(byte_array, collapse = delimeter))
}

#' to_proper_struct
#'
#' Help method for tests. It changes predefined structure to parsed thrift
#' instead of parsing file.
#'
#' @param thrift_spec_list raw array
#' @param default_spec separation character
#'
#' @return R6 class
#'
#' @export
to_proper_struct <- function(thrift_spec_list, default_spec) {
  thrift_spec <- new.env(hash = TRUE)
  for (input in thrift_spec_list) {
    thrift_spec[[input[[1]]]] <- input[[2]]
  }

  TItem <- R6::R6Class(
    "TItem",
    inherit = thriftr::TPayload,
    lock_objects = FALSE,
    public = list(
    )
  )

  TItem$set("public", 'thrift_spec', thrift_spec)
  TItem$set("public", 'default_spec', default_spec)

  gen_init(
    TItem,
    thrift_spec,
    default_spec
  )
}