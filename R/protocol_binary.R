
VERSION_MASK = -65536
VERSION_1 <- -2147418112
TYPE_MASK = 0x000000ff

pack_i8 <- function(byte) {
  as.raw(strtoi(strsplit(gsub("(.{2})", "\\1 ", as.character(format(as.hexmode(byte), width = 2))), " ")[[1]], 16L))
}

pack_i16 <- function(i16) {
  as.raw(strtoi(strsplit(gsub("(.{2})", "\\1 ", as.character(format(as.hexmode(i16), width = 4))), " ")[[1]], 16L))
}

pack_i32 <- function(i32) {
  as.raw(strtoi(strsplit(gsub("(.{2})", "\\1 ", as.character(format(as.hexmode(i32), width = 8))), " ")[[1]], 16L))
}

pack_i64 <- function(i64) {
  as.raw(
    strtoi(
      c(
        strsplit(gsub("(.{2})", "\\1 ", as.character(format(as.hexmode(i64[[1]]), width = 4))), " ")[[1]],
        strsplit(gsub("(.{2})", "\\1 ", as.character(format(as.hexmode(i64[[2]]), width = 4))), " ")[[1]]
      ),
      16L)
  )
}

pack_double <- function(dub) {
  writeBin(dub, raw(), size = 8, endian = "big")
}

pack_string <- function(string) {
  b <- charToRaw(string)
  c(pack_i32(length(b)), b)
}

unpack_i8 <- function(buf) {
  readBin(buf, integer(), 1, size = 1, signed = TRUE)
}

unpack_i16 <- function(buf) {
  readBin(buf, integer(), 1, size = 2, signed = TRUE, endian="big")
}

unpack_i32 <- function(buf) {
  readBin(buf, integer(), 1, size = 4, signed = TRUE, endian="big")
}

unpack_i64 <- function(buf) {
  readBin(buf, integer(), 2, size = 4, signed = TRUE, endian="big")
}

unpack_double <- function(buf) {
  readBin(buf, numeric(), 1, size = 8, signed = TRUE, endian="big")
}

write_message_begin = function(outbuf, name, ttype, seqid, strict = TRUE) {
  if (strict) {
    outbuf <- c(outbuf, pack_i32(bitwOr(VERSION_1, ttype)))
    outbuf <- c(outbuf, pack_string(name))
  } else {
    outbuf <- c(outbuf, pack_string(name))
    outbuf <- c(outbuf, pack_i8(ttype))
  }
  outbuf <- c(outbuf, pack_i32(seqid))
  return(outbuf)
}

write_field_begin = function(outbuf, ttype, fid) {
  c(outbuf, pack_i8(ttype), pack_i16(fid))
}


write_field_stop = function(outbuf) {
  c(outbuf, pack_i8(TType$STOP))
}

write_list_begin = function(outbuf, etype, size) {
  c(outbuf, pack_i8(etype), pack_i32(size))
}

write_map_begin = function(outbuf, ktype, vtype, size) {
  c(outbuf, pack_i8(ktype), pack_i8(vtype), pack_i32(size))
}


#' @export
binary_write_val <- function(outbuf, ttype, val, spec = NA) {
  if (ttype == TType$BOOL) {
    if (val) {
      return(c(outbuf, pack_i8(1)))
    } else {
      return(c(outbuf, pack_i8(0)))
    }
  } else if (ttype == TType$BYTE) {
    return(c(outbuf, pack_i8(val)))
  } else if (ttype == TType$I16) {
    return(c(outbuf, pack_i16(val)))
  } else if (ttype == TType$I32) {
    return(c(outbuf, pack_i32(val)))
  } else if (ttype == TType$I64) {
    return(c(outbuf, pack_i64(val)))
  } else if (ttype == TType$DOUBLE) {
    return(c(outbuf, pack_double(val)))
  } else if (ttype == TType$STRING) {
    return(c(outbuf, pack_string(val)))
  } else if (ttype == TType$SET || ttype == TType$LIST) {
    if (length(spec) == 2) {
      e_type <- spec[[1]]
      t_spec <- spec[[2]]
    } else {
      e_type <- spec[[1]]
      t_spec <- NA
    }

    val_len <- length(val)
    outbuf <- write_list_begin(outbuf, e_type, val_len)
    for (e_val in val) {
      outbuf <- binary_write_val(outbuf, e_type, e_val, t_spec)
    }
    return(outbuf)
  } else if (ttype == TType$MAP) {
    if (typeof(spec[[1]]) == "integer") {
      k_type <- spec[[1]]
      k_spec <- NA
    } else {
      k_type <- spec[[1]][[1]]
      k_spec <- spec[[1]][[2]]
    }

    if (typeof(spec[[2]]) == "integer") {
      k_type <- spec[[2]]
      k_spec <- NA
    } else {
      k_type <- spec[[2]][[1]]
      k_spec = spec[[2]][[2]]
    }

    outbuf <- write_map_begin(outbuf, k_type, v_type, length(val))
    for (k in names(val)) {
      outbuf <- binary_write_val(outbuf, k_type, k, k_spec)
      outbuf <- binary_write_val(outbuf, v_type, val[[k]], v_spec)
    }
    return(outbuf)
  } else if (ttype == TType$STRUCT) {
    for (fid in names(val$thrift_spec)) {
      f_spec <- val$thrift_spec[[fid]]
      if (length(f_spec) == 3) {
        f_type <- f_spec[[1]]
        f_name <- f_spec[[2]]
        f_req <- f_spec[[3]]
        f_container_spec <- NA
      } else {
        f_type <- f_spec[[1]]
        f_name <- f_spec[[2]]
        f_container_spec <- f_spec[[3]]
        f_req <- f_spec[[4]]
      }

      v <- val[[f_name]]
      if (is.na(v)) next

      outbuf <- write_field_begin(outbuf, f_type, fid)
      outbuf <- binary_write_val(outbuf, f_type, v, f_container_spec)
    }
  
    outbuf <- write_field_stop(outbuf)
    return(outbuf)
  }
}

read_message_begin <- function(inbuf, strict = TRUE) {
  sz <- unpack_i32(head(inbuf, 4))
  inbuf <- tail(inbuf, -4)
  if (sz < 0) {
    version <- bitwAnd(sz, VERSION_MASK)
    if (version != VERSION_1) {
      stop(sprintf("[TProtocolException][BAD_VERSION] Bad version in read_message_begin: %d'", sz))
    }
            
    name_sz <- unpack_i32(head(inbuf, 4))
    inbuf <- tail(inbuf, -4)
    name <- rawToChar(head(inbuf, name_sz))
    inbuf <- tail(inbuf, name_sz)

    type_ <- bitwAnd(sz, TYPE_MASK)
  } else {
    if (strict) {
      stop("[TProtocolException][BAD_VERSION] No protocol version header")
    }
    name <- rawToChar(head(inbuf, sz))
    inbuf <- tail(inbuf, -sz)
    type_ <- unpack_i8(head(inbuf, 1))
    inbuf <- tail(inbuf, -1)
  }

  seqid <- unpack_i32(head(inbuf, 4))
  inbuf <- tail(inbuf, -4)

  return(list(name, type_, seqid, inbuf))
}

read_field_begin <- function(inbuf) {
  f_type <- unpack_i8(head(inbuf, 1))
  inbuf <- tail(inbuf, -1)
  if (f_type == TType$STOP) return(list(f_type, 0, inbuf))
  return(list(f_type, unpack_i16(head(inbuf, 2)), tail(inbuf, -2)))
}

read_list_begin <- function(inbuf) {
  e_type <- unpack_i8(head(inbuf, 1))
  inbuf <- tail(inbuf, -1)
  sz <- unpack_i32(head(inbuf, 4))
  inbuf <- tail(inbuf, -4)
  return(list(e_type, sz, inbuf))
}

read_map_begin = function(inbuf) {
  k_type <- unpack_i8(head(inbuf, 1))
  inbuf <- tail(inbuf, -1)
  v_type <- unpack_i8(head(inbuf, 1))
  inbuf <- tail(inbuf, -1)
  sz <- unpack_i32(head(inbuf, 4))
  inbuf <- tail(inbuf, -4)
  return(list(k_type, v_type, sz, inbuf))
}

#' @export
binary_read_val <- function(inbuf, ttype, spec = NA, decode_response = TRUE) {
  if (ttype == TType$BOOL) {
    return(list(as.logical(unpack_i8(inbuf), tail(inbuf, -1))))
  } else if (ttype == TType$BYTE) {
    return(list(unpack_i8(inbuf), tail(inbuf, -1)))
  } else if (ttype == TType$I16) {
    return(list(unpack_i16(inbuf), tail(inbuf, -2)))
  } else if (ttype == TType$I32) {
    return(list(unpack_i32(inbuf), tail(inbuf, -4)))
  } else if (ttype == TType$I64) {
    return(list(unpack_i64(inbuf), tail(inbuf, -8)))
  } else if (ttype == TType$DOUBLE) {
    return(list(unpack_double(inbuf), tail(inbuf, -8)))
  } else if (ttype == TType$STRING) {
    sz = unpack_i32(inbuf)
    inbuf <- tail(inbuf, -4)
    byte_payload = head(inbuf, sz)
    inbuf <- tail(inbuf, -sz)

    # Since we cannot tell if we're getting STRING or BINARY
    # if not asked not to decode, try both
    if (decode_response) {
      return(list(rawToChar(byte_payload), inbuf))
      # TODO
    } else return(list(byte_payload, inbuf))
  } else if (ttype == TType$SET || ttype == TType$LIST) {
    if (length(spec) == 2) {
      v_type <- spec[[1]]
      v_spec <- spec[[2]]
    } else {
      v_type <- spec[[1]]
      v_spec <- NA
    }

    result <- list()
    r_type_sz_inbuf <- read_list_begin(inbuf)
    r_type <- r_type_sz_inbuf[[1]]
    sz <- r_type_sz_inbuf[[2]]
    inbuf <- r_type_sz_inbuf[[3]]
    # the v_type is useless here since we already get it from spec
    if (r_type != v_type) {
      for (i in 1:sz) {
        inbuf <- skip(inbuf, r_type)
      }
      return(list())
    }

    for(i in 1:sz) {
      obj_inbuf <- binary_read_val(inbuf, v_type, v_spec, decode_response)
      inbuf <- obj_inbuf[[2]]
      result[[length(result) + 1]] <- obj_inbuf[[1]]
    }
    return(list(result, inbuf))
  } else if (ttype == TType$MAP) {
    if (typeof(spec[[1]]) == "integer") {
      k_type <- spec[[1]]
      k_spec <- NA
    } else {
      k_type <- spec[[1]][[1]]
      k_spec <- spec[[1]][[2]]
    }

    if (typeof(spec[[2]]) == "integer") {
      k_type <- spec[[2]]
      k_spec <- NA
    } else {
      k_type <- spec[[2]][[1]]
      k_spec <- spec[[2]][[2]]
    }

    result <- new.env(hash=TRUE)
    sk_type_sv_type_sz_inbuf <- read_map_begin(inbuf)
    sk_type <- sk_type_sv_type_sz_inbuf[[1]]
    sv_type <- sk_type_sv_type_sz_inbuf[[2]]
    sz <- sk_type_sv_type_sz_inbuf[[3]]
    inbuf <- sk_type_sv_type_sz_inbuf[[4]]
    if (sk_type != k_type || sv_type != v_type) {
      for (i in 1:sz) {
        inbuf <- skip(inbuf, sk_type)
        inbuf <- skip(inbuf, sv_type)
        return(new.env(hash=TRUE))
      }  
    }

    for (i in 1:sz) {
      k_val <- read_val(inbuf, k_type, k_spec, decode_response)
      v_val <- read_val(inbuf, v_type, v_spec, decode_response)
      result[[k_val]] <- v_val
    }

    return(result)
  } else if (ttype == TType$STRUCT) {
    obj <- spec()
    inbuf <- read_struct(inbuf, obj, decode_response)
    return(list(obj, inbuf))
  }
}

read_struct <- function(inbuf, obj, decode_response=TRUE) {
  while (TRUE) {
    f_type_fid_inbuf <- read_field_begin(inbuf)
    f_type <- f_type_fid_inbuf[[1]]
    fid <- f_type_fid_inbuf[[2]]
    inbuf <- f_type_fid_inbuf[[3]]

    if (f_type == TType$STOP) break

    if (!(fid %in% names(obj$thrift_spec))) {
      inbuf <- skip(inbuf, f_type)
      next
    }

    if (length(obj$thrift_spec[[as.character(fid)]]) == 3) {
      sf_type_f_name_f_req <- obj$thrift_spec[[as.character(fid)]]
      sf_type <- sf_type_f_name_f_req[[1]]
      f_name <- sf_type_f_name_f_req[[2]]
      f_container_spec <- NA
    } else {
      sf_type_f_name_f_container_spec_f_req <- obj$thrift_spec[[as.character(fid)]]
      sf_type <- sf_type_f_name_f_container_spec_f_req[[1]]
      f_name <- sf_type_f_name_f_container_spec_f_req[[2]]
      f_container_spec <- sf_type_f_name_f_container_spec_f_req[[3]]
      f_req <- sf_type_f_name_f_container_spec_f_req[[4]]
    }

    # it really should equal here. but since we already wasted
    # space storing the duplicate info, let's check it.
    if (f_type != sf_type) {
      inbuf <- skip(inbuf, f_type)
      next
    }

    val_inbuf <- binary_read_val(inbuf, f_type, f_container_spec, decode_response)
    val <- val_inbuf[[1]]
    inbuf <- val_inbuf[[2]]
    obj[[f_name]] <- val
  }
  return(inbuf)
}

skip <- function(inbuf, ftype) {
  if (ftype == TType$BOOL || ftype == TType$BYTE)
    return(tail(inbuf, -1))
  else if (ftype == TType$I16)
    return(tail(inbuf, -2))
  else if (ftype == TType$I32)
    return(tail(inbuf, -4))
  else if (ftype == TType$I64)
    return(tail(inbuf, -8))
  else if (ftype == TType$DOUBLE)
    return(tail(inbuf, -8))
  else if (ftype == TType$STRING) {
    sz <- unpack_i32(head(inbuf, 4))
    inbuf <- tail(inbuf, -4)
    return(tail(inbuf, -sz))
  }
  else if (ftype == TType$SET || ftype == TType$LIST) {
    v_type_sz_inbuf <- read_list_begin(inbuf)
    inbuf <- v_type_sz_inbuf[[3]]
    for (i in 1:v_type_sz[[2]]) {
      inbuf <- skip(inbuf, v_type_sz[[1]])
    }
    return(inbuf)
  }
  else if (ftype == TType$MAP) {
    k_type_v_type_sz <- read_map_begin(inbuf)
    for (i in 1:k_type_v_type_sz[[3]]) {
      inbuf <- skip(inbuf, k_type_v_type_sz[[1]])
      inbuf <- skip(inbuf, k_type_v_type_sz[[2]])
    }
    return(inbuf)
  }
  else if (ftype == TType$STRUCT) {
    while (TRUE) {
      f_type_fid_inbuf <- read_field_begin(inbuf)
      if (f_type_fid_inbuf[[1]] == TType.STOP) break
      inbuf <- skip(f_type_fid_inbuf[[3]], f_type_fid_inbuf[[1]])
    }
    return(inbuf)
  }
}


#' TBinaryProtocol
#'
#' Binary implementation of the Thrift protocol driver.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#'
#' @export
TBinaryProtocol <- R6Class("TBinaryProtocol",
  public = list(
    trans = NA,
    strict_read = NA,
    strict_write = NA,
    decode_response = NA,
    initialize = function(trans, strict_read = TRUE, strict_write = TRUE, decode_response = TRUE) {
      self$trans <- trans
      self$strict_read <- strict_read
      self$strict_write <- strict_write
      self$decode_response <- decode_response
    },
    skip = function(ttype) {
      skip(self$trans, ttype)
    },
    read_message_begin = function() {
      api_ttype_seqid <- read_message_begin(self$trans, strict = self$strict_read)
      return(api_ttype_seqid)
    },
    read_message_end = function() {
    },
    write_message_begin = function(name, ttype, seqid) {
      write_message_begin(self$trans, name, ttype, seqid, strict = self$strict_write)
    },
    write_message_end = function() {
    },
    read_struct = function(obj) {
      return(read_struct(self$trans, obj, self$decode_response))
    },
    write_struct = function(obj) {
      return(binary_write_val(self$trans, TType$STRUCT, obj))
    }
  )
)

# class TBinaryProtocolFactory(object):
#     def __init__(self, strict_read=True, strict_write=True,
#                  decode_response=True):
#         self.strict_read = strict_read
#         self.strict_write = strict_write
#         self.decode_response = decode_response

#     def get_protocol(self, trans):
#         return TBinaryProtocol(trans,
#                                self.strict_read, self.strict_write,
#                                self.decode_response)