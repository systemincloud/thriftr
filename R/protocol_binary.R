
pack_i8 <- function(byte) {
  as.raw(strtoi(strsplit(gsub("(.{2})", "\\1 ", as.character(format(as.hexmode(byte), width = 1))), " ")[[1]], 16L))
}

pack_i16 <- function(i16) {
  as.raw(strtoi(strsplit(gsub("(.{2})", "\\1 ", as.character(format(as.hexmode(i16), width = 2))), " ")[[1]], 16L))
}

pack_i32 <- function(i32) {
  as.raw(strtoi(strsplit(gsub("(.{2})", "\\1 ", as.character(format(as.hexmode(i32), width = 4))), " ")[[1]], 16L))
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
  c(as.raw(strtoi(c("00", "00"))), pack_i32(length(b)), b)
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

#' @export
binary_write_val <- function(ttype, val, spec = NA) {
  if (ttype == TType$BOOL) {
    if (val) {
      return(pack_i8(1))
    } else {
      return(pack_i8(0))
    }
  } else if (ttype == TType$BYTE) {
    return(pack_i8(val))
  } else if (ttype == TType$I16) {
    return(pack_i16(val))
  } else if (ttype == TType$I32) {
    return(pack_i32(val))
  } else if (ttype == TType$I64) {
    return(pack_i64(val))
  } else if (ttype == TType$DOUBLE) {
    return(pack_double(val))
  } else if (ttype == TType$STRING) {
    return(pack_string(val))
  } else if (ttype == TType$SET || ttype == TType$LIST) {
   #     if isinstance(spec, tuple):
    #         e_type, t_spec = spec[0], spec[1]
    #     else:
    #         e_type, t_spec = spec, None

    #     val_len = len(val)
    #     write_list_begin(outbuf, e_type, val_len)
    #     for e_val in val:
    #         write_val(outbuf, e_type, e_val, t_spec)
  } else if (ttype == TType$MAP) {
    #     if isinstance(spec[0], int):
    #         k_type = spec[0]
    #         k_spec = None
    #     else:
    #         k_type, k_spec = spec[0]

    #     if isinstance(spec[1], int):
    #         v_type = spec[1]
    #         v_spec = None
    #     else:
    #         v_type, v_spec = spec[1]

    #     write_map_begin(outbuf, k_type, v_type, len(val))
    #     for k in iter(val):
    #         write_val(outbuf, k_type, k, k_spec)
    #         write_val(outbuf, v_type, val[k], v_spec)
  } else if (ttype == TType$STRUCT) {
    #     for fid in iter(val.thrift_spec):
    #         f_spec = val.thrift_spec[fid]
    #         if len(f_spec) == 3:
    #             f_type, f_name, f_req = f_spec
    #             f_container_spec = None
    #         else:
    #             f_type, f_name, f_container_spec, f_req = f_spec

    #         v = getattr(val, f_name)
    #         if v is None:
    #             continue

    #         write_field_begin(outbuf, f_type, fid)
    #         write_val(outbuf, f_type, v, f_container_spec)
    #     write_field_stop(outbuf)
  }
}

#' @export 
binary_read_val <- function(inbuf, ttype, spec = NA, decode_response = TRUE) {
  if (ttype == TType$BOOL) {
    return(as.logical(unpack_i8(inbuf)))
  } else if (ttype == TType$BYTE) {
    return(unpack_i8(inbuf))
  } else if (ttype == TType$I16) {
    return(unpack_i16(inbuf))
  } else if (ttype == TType$I32) {
    return(unpack_i32(inbuf))
  } else if (ttype == TType$I64) {
    return(unpack_i64(inbuf))
  } else if (ttype == TType$DOUBLE) {
    return(unpack_double(inbuf))
  } else if (ttype == TType$STRING) {
    sz = unpack_i32(inbuf)
    byte_payload = head(tail(inbuf, -4), sz)

    # Since we cannot tell if we're getting STRING or BINARY
    # if not asked not to decode, try both
    if (decode_response) {
      return(rawToChar(byte_payload))
      # TODO
    } else return(byte_payload)
  } else if (ttype == TType$SET || ttype == TType$LIST) {
    # TODO
  } else if (ttype == TType$MAP) {
    # TODO
  } else if (ttype == TType$STRUCT) {
    # TODO
  }
}