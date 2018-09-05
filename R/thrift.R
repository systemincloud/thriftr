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

#' parse_spec
#'
#' String representation of specification
#'
#' @param ttype type
#' @param spec specification
#'
#' @return string representation
#'
#' @export
parse_spec = function(ttype, spec = NA) {
  name_map <- as.list(TType)
  name_map_tmp <- name_map 
  name_map <- names(name_map)
  names(name_map) <- unlist(name_map_tmp, use.names=FALSE)

  type_ = function(s) {
    if (is.list(s)) {
      return(parse_spec(s[[1]], s[[2]]))
    } else {
      return(name_map[[as.character(s)]])
    }
  }

  if (length(spec) == 1 && is.na(spec)) {
    return(name_map[[as.character(ttype)]])
  }
        
  if (ttype == TType$STRUCT) {
    return(spec$classname)
  }

  if (ttype %in% c(TType$LIST, TType$SET)) {
    return(sprintf("%s<%s>", name_map[[as.character(ttype)]], type_(spec)))
  }

  if (ttype == TType$MAP) {
    return(sprintf("MAP<%s, %s>", type_(spec[[1]]), type_(spec[[2]])))
  }
}

# init_func_generator
#
# Generate `initialize` function based on TPayload$default_spec
#
# @param cls R6 class
# @param spec specification
#
# @return constructor function
init_func_generator = function(cls, spec) {
  if(length(spec) == 0) return(function() { })

  args <- alist()
  for(s in spec) {
    args[[s[[1]]]] <- s[[2]]
  }

  func <- function() {
    argg <- as.list(environment())
    for(arg_name in names(argg)) {
      self[[arg_name]] <- argg[[arg_name]]
    }
  }
  formals(func) <- args

  return(func)
}

#' TType
#'
#' Identificator of value type.
#'
#' @export
TType <- new.env(hash=TRUE)
TType$STOP <- as.integer(0)
TType$VOID <- as.integer(1)
TType$BOOL <- as.integer(2)
TType$BYTE <- as.integer(3)
TType$I08 <- as.integer(3)
TType$DOUBLE <- as.integer(4)
TType$I16 <- as.integer(6)
TType$I32 <- as.integer(8)
TType$I64 <- as.integer(10)
TType$STRING <- as.integer(11)
TType$UTF7 <- as.integer(11)
TType$BINARY <- as.integer(11)  # This here just for parsing. For all purposes, it's a string
TType$STRUCT <- as.integer(12)
TType$MAP <- as.integer(13)
TType$SET <- as.integer(14)
TType$LIST <- as.integer(15)
TType$UTF8 <- as.integer(16)
TType$UTF16 <- as.integer(17)

# TMessageType
TMessageType <- new.env(hash=TRUE)
TMessageType$CALL <- as.integer(1)
TMessageType$REPLY <- as.integer(2)
TMessageType$EXCEPTION <- as.integer(3)
TMessageType$ONEWAY <- as.integer(4)


gen_init = function(cls, thrift_spec=NULL, default_spec=NULL) {
  if(!is.null(thrift_spec)) cls$thrift_spec <- thrift_spec
  if(!is.null(default_spec))
    cls$set('public', 'initialize', init_func_generator(cls, default_spec))
  return(cls)
}

#' TPayload
#'
#' Base class for all complex types of api.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#'
#' @export
TPayload <- R6Class("TPayload",
  public = list(
    read = function(iprot) {
      iprot$read_struct(self)
    },
    write = function(oprot) {
      oprot$write_struct(self)
    }
  )
)

#' TClient
#'
#' TClient implements client api of thrift service.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#'
#' @export
TClient <- R6Class("TClient",
  lock_objects = FALSE,
  public = list(
    initialize = function(service, iprot, oprot=NULL) {
      private$service <- service
      private$iprot <- iprot
      private$oprot <- iprot
      if (!is.null(oprot)) {
        private$oprot <- oprot
      }
      private$seqid <- 0

      for (api in private$service$thrift_services) {
        func <- private$getFunc(api)

        args <- alist()
        args_thrift <- private$service[[paste(api, "args", sep = "_")]]
        args_spec <- args_thrift$thrift_spec
        args_default <- args_thrift$default_spec
        for(s in names(args_spec)) {
          arg_name <- args_spec[[s]][[2]]
          default <- NA
          for (d in args_default) {
            if (d[[1]] == arg_name) default <- d[[2]]
          }
          args[[arg_name]] <- default
        }
        formals(func) <- args

        self$add_function(api, func)
      }
    },
    add_function = function(name, meth) {
      self[[name]] <- meth
    }
  ),
  private = list(
    service = NA,
    iprot = NA,
    oprot = NA,
    seqid = NA,
    getFunc = function(v_api) {
      api <- v_api
      result_cls <- private$service[[paste(api, "result", sep = "_")]]$new()
      function() {
        kwargs <- as.list(environment())
        private$send(api, kwargs)
        # wait result only if non-oneway
        if (!result_cls$oneway) {
          return(private$recv(api))
        }
      }
    },
    send = function(api, kwargs) {
      private$oprot$write_message_begin(api, TMessageType$CALL, private$seqid)
      args <- private$service[[paste(api, "args", sep = "_")]]$new()
      for(arg_name in names(kwargs)) {
        args[[arg_name]] <- kwargs[[arg_name]]
      }
      args$write(private$oprot)
      private$oprot$write_message_end()
      private$oprot$trans$flush()
    },
    recv = function(api) {
      fname_mtype_rseqid <- private$iprot$read_message_begin()
      fname <- fname_mtype_rseqid[[1]]
      mtype <- fname_mtype_rseqid[[2]]
      rseqid <- fname_mtype_rseqid[[3]]
      if (mtype == TMessageType$EXCEPTION) {
        x <- TApplicationException()
        x$read(private$iprot)
        private$iprot$read_message_end()
        stop(x)
      }
      result <- private$service[[paste(api, "result", sep = "_")]]$new()
      result$read(private$iprot)
      private$iprot$read_message_end()

      if (!is.null(result$success) && !is.na(result$success)) {
        return(result$success)
      }

      # void api without throws
      if (length(names(result$thrift_spec)) == 0) {
        return
      }

      # check throws
      # for k, v in result.__dict__.items():
      # if k != "success" and v:
      # raise v

      # no throws & not void api
      if (!is.null(result$success)) {
        stop(paste("[TApplicationException]", TApplicationException$MISSING_RESULT))
      }
    }
  )
)

# TProcessor
#
# Base class for procsessor, which works on two streams.
TProcessor <- R6Class("TProcessor",
  public = list(
    initialize = function(service, handler) {
      private$service <- service
      private$handler <- handler
    },
    process_in = function(iprot) {
      api_type_seqid <- iprot$read_message_begin()
      api <- api_type_seqid[[1]]
      type <- api_type_seqid[[2]]
      seqid <- api_type_seqid[[3]]
      if (!(api %in% private$service$thrift_services)) {
        iprot$skip(TType$STRUCT)
        iprot$read_message_end()
        return(list(api, seqid, TApplicationException$new(TApplicationException$UNKNOWN_METHOD), None))  # noqa
      }

      args <- private$service[[paste(api, "args", sep = "_")]]$new()
      args$read(iprot)
      iprot$read_message_end()
      result <- private$service[[paste(api, "result", sep = "_")]]$new()

      # convert kwargs to args
      api_args <- list()
      for (k in names(args$thrift_spec)) {
        api_args[[length(api_args) + 1]] <- args$thrift_spec[[k]][[2]]
      }

      call <- function() {
        f <- private$handler[[api]]
        as <- list()
        for (k in api_args) {
          as[[length(as) + 1]] <- args[[k]]
        }
        names(as) <- api_args
        return(do.call(f, as))
      }

      return(list(api, seqid, result, call))
    },
    send_exception = function(oprot, api, exc, seqid) {
      oprot$write_message_begin(api, TMessageType$EXCEPTION, seqid)
      exc$write(oprot)
      oprot$write_message_end()
      oprot$trans.flush()
    },
    send_result = function(oprot, api, result, seqid) {
      oprot$write_message_begin(api, TMessageType$REPLY, seqid)
      result$write(oprot)
      oprot$write_message_end()
      oprot$trans$flush()
    },
    handle_exception = function(e, result) {
      should_raise <- FALSE
      for (k in result$thrift_spec) {
        should_raise <- TRUE
        if (result$thrift_spec[[k]][[2]] == "success") next

        na_exc_name_exc_cls_na = result$thrift_spec[[k]]
        # if isinstance(e, exc_cls):
        #   setattr(result, exc_name, e)
        #   break
      }
      if (should_raise) stop()
    },
    process = function(iprot, oprot) {
      api_seqid_result_call <- self$process_in(iprot)
      api <- api_seqid_result_call[[1]]
      seqid <- api_seqid_result_call[[2]]
      result <- api_seqid_result_call[[3]]
      call <- api_seqid_result_call[[4]]

      if (class(result)[[1]] == "TApplicationException") {
        return(self$send_exception(oprot, api, result, seqid))
      }

      tryCatch({
        result$success <- call()
      }, error = function(e) {
        # raise if api don't have throws
        self$handle_exception(e, result)
      })

      if (!result$oneway) {
        self$send_result(oprot, api, result, seqid)
      }
    }
  ),
  private = list(
    service = NA,
    handler = NA
  )
)

TException <- R6Class("TException",
  inherit = TPayload,
  public = list(
  )
)

TApplicationException <- R6Class("TApplicationException",
  inherit = TException,
  public = list(
  )
)

TApplicationException$UNKNOWN <- 0
TApplicationException$UNKNOWN_METHOD <- 1
TApplicationException$INVALID_MESSAGE_TYPE <- 2
TApplicationException$WRONG_METHOD_NAME <- 3
TApplicationException$BAD_SEQUENCE_ID <- 4
TApplicationException$MISSING_RESULT <- 5
TApplicationException$INTERNAL_ERROR <- 6
TApplicationException$PROTOCOL_ERROR <- 7
