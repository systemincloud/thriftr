#' thrift.R

TType <- R6Class("TType",
  public = list(
    STOP = 0,
    VOID = 1,
    BOOL = 2,
    BYTE = 3,
    I08 = 3,
    DOUBLE = 4,
    I16 = 6,
    I32 = 8,
    I64 = 10,
    STRING = 11,
    UTF7 = 11,
    BINARY = 11,  # This here just for parsing. For all purposes, it's a string
    STRUCT = 12,
    MAP = 13,
    SET = 14,
    LIST = 15,
    UTF8 = 16,
    UTF16 = 17
  )
)


TClient <- R6Class("TClient",
  public = list(
    initialize = function(service, iprot, oprot) {
      private$service <- service
      private$iprot <- iprot
      private$oprot <- if (missing(oprot)) iprot else oprot
      private$seqid <- 0
    },
    close = function() {
      self$iprot$trans$close()
      if (self$iprot != self$oprot)
            self$oprot$trans$close()
    }
  ),
  private = list(
    service = NA,
    iprot = NA,
    oprot = NA,
    seqid = NA
  )
)



def __getattr__(self, _api):
    if _api in self._service.thrift_services:
    return functools.partial(self._req, _api)

raise AttributeError("{} instance has no attribute '{}'".format(
        self.__class__.__name__, _api))

def __dir__(self):
    return self._service.thrift_services

def _req(self, _api, *args, **kwargs):
    _kw = args2kwargs(getattr(self._service, _api + "_args").thrift_spec,
        *args)
kwargs.update(_kw)
result_cls = getattr(self._service, _api + "_result")

self._send(_api, **kwargs)
# wait result only if non-oneway
if not getattr(result_cls, "oneway"):
      return self._recv(_api)

def _send(self, _api, **kwargs):
    self._oprot.write_message_begin(_api, TMessageType.CALL, self._seqid)
args = getattr(self._service, _api + "_args")()
for k, v in kwargs.items():
    setattr(args, k, v)
args.write(self._oprot)
self._oprot.write_message_end()
self._oprot.trans.flush()

def _recv(self, _api):
    fname, mtype, rseqid = self._iprot.read_message_begin()
if mtype == TMessageType.EXCEPTION:
      x = TApplicationException()
x.read(self._iprot)
self._iprot.read_message_end()
raise x
result = getattr(self._service, _api + "_result")()
result.read(self._iprot)
self._iprot.read_message_end()

if hasattr(result, "success") and result.success is not None:
      return result.success

# void api without throws
if len(result.thrift_spec) == 0:
      return

# check throws
for k, v in result.__dict__.items():
    if k != "success" and v:
          raise v

# no throws & not void api
if hasattr(result, "success"):
      raise TApplicationException(TApplicationException.MISSING_RESULT)

