#! /usr/bin/env Rscript

library(testthat)

context("type_protocol_binary")

TItem <- thriftr::to_proper_struct(
  list(
    list("1", list(thriftr::TType$I32, 'id', FALSE)),
    list("2", list(thriftr::TType$LIST, 'phones', list(thriftr::TType$STRING), FALSE))
  ),
  list(
    list('id', NA),
    list('phones', NA)
  )
)

test_that("test_pack_i8", {
  b <- thriftr::TMemoryBuffer$new()
  thriftr::binary_write_val(b, thriftr::TType$I08, 123)
  expect_equal(thriftr::hexlify(b$getvalue()), "7b")
})

test_that("test_unpack_i8", {
  b <- thriftr::TMemoryBuffer$new(charToRaw("{"))
  expect_equal(thriftr::binary_read_val(b, thriftr::TType$I08), 123)
})

test_that("test_pack_i16", {
  b <- thriftr::TMemoryBuffer$new()
  thriftr::binary_write_val(b, thriftr::TType$I16, 12345)
  expect_equal(thriftr::hexlify(b$getvalue()), "30 39")
})

test_that("test_unpack_i16", {
  b <- thriftr::TMemoryBuffer$new(charToRaw("09"))
  expect_equal(thriftr::binary_read_val(b, thriftr::TType$I16), 12345)
})

test_that("test_pack_i32", {
  b <- thriftr::TMemoryBuffer$new()
  thriftr::binary_write_val(b, thriftr::TType$I32, 1234567890)
  expect_equal(thriftr::hexlify(b$getvalue()), "49 96 02 d2")
})

test_that("test_unpack_i32", {
  b <- thriftr::TMemoryBuffer$new(as.raw(strtoi(c("49", "96", "02", "d2"), 16L)))
  expect_equal(thriftr::binary_read_val(b, thriftr::TType$I32)[[1]], 1234567890)
})

test_that("test_pack_i64", {
  b <- thriftr::TMemoryBuffer$new()
  thriftr::binary_write_val(b, thriftr::TType$I64, c(287445236, 2112454933))
  expect_equal(thriftr::hexlify(b$getvalue()), "11 22 10 f4 7d e9 81 15")
})

test_that("test_unpack_i64", {
  b <- thriftr::TMemoryBuffer$new(as.raw(strtoi(c("11", "22", "10", "f4", "7d", "e9", "81", "15"), 16L)))
  l <- thriftr::binary_read_val(b, thriftr::TType$I64)
  expect_equal(l[1], 287445236)
  expect_equal(l[2], 2112454933)
})

test_that("test_pack_double", {
  b <- thriftr::TMemoryBuffer$new()
  thriftr::binary_write_val(b, thriftr::TType$DOUBLE, 1234567890.1234567)
  expect_equal(thriftr::hexlify(b$getvalue()), "41 d2 65 80 b4 87 e6 b7")
})

test_that("test_unpack_double", {
  b <- thriftr::TMemoryBuffer$new(as.raw(strtoi(c("41", "d2", "65", "80", "b4", "87", "e6", "b7"), 16L)))
  expect_equal(thriftr::binary_read_val(b, thriftr::TType$DOUBLE)[[1]], 1234567890.1234567, tolerance = 1e-17)
})

test_that("test_pack_string", {
  b <- thriftr::TMemoryBuffer$new()
  thriftr::binary_write_val(b, thriftr::TType$STRING, "hello world!")
  expect_equal(thriftr::hexlify(b$getvalue()), "00 00 00 0c 68 65 6c 6c 6f 20 77 6f 72 6c 64 21")

  b <- thriftr::TMemoryBuffer$new()
  thriftr::binary_write_val(b, thriftr::TType$STRING, "你好世界")
  expect_equal(thriftr::hexlify(b$getvalue()), "00 00 00 0c e4 bd a0 e5 a5 bd e4 b8 96 e7 95 8c")
})

test_that("test_unpack_string", {
  b <- thriftr::TMemoryBuffer$new(as.raw(strtoi(c("00", "00", "00", "0c", "e4", "bd", "a0",
    "e5", "a5", "bd", "e4", "b8", "96", "e7", "95", "8c"), 16L)))
  expect_equal(thriftr::binary_read_val(b, thriftr::TType$STRING), "你好世界")
})

test_that("test_unpack_binary", {
  b <- thriftr::TMemoryBuffer$new(as.raw(strtoi(c("00", "00", "00", "0c", "e4", "bd", "a0",
    "e5", "a5", "bd", "e4", "b8", "96", "e7", "95", "8c"), 16L)))
  expect_equal(thriftr::binary_read_val(b, thriftr::TType$STRING, decode_response = FALSE), charToRaw("你好世界"))
})

test_that("test_write_message_begin", {
  b <- thriftr::TMemoryBuffer$new()
  thriftr::TBinaryProtocol$new(b)$write_message_begin("test", thriftr::TType$STRING, 1)
  expect_equal(thriftr::hexlify(b$getvalue()), "80 01 00 0b 00 00 00 04 74 65 73 74 00 00 00 01")
})

test_that("test_write_message_begin_not_strict", {
  b <- thriftr::TMemoryBuffer$new()
  thriftr::TBinaryProtocol$new(b, strict_write = FALSE)$write_message_begin("test", thriftr::TType$STRING, 1)
  expect_equal(thriftr::hexlify(b$getvalue()), "00 00 00 04 74 65 73 74 0b 00 00 00 01")
})

test_that("test_read_message_begin", {
  b <- thriftr::TMemoryBuffer$new(as.raw(strtoi(c("80", "01", "00", "0b", "00", "00", "00", "04", "74", "65", "73", "74", "00", "00", "00", "01"), 16L)))
  res <- thriftr::TBinaryProtocol$new(b)$read_message_begin()
  expect_equal(res[[1]], "test")
  expect_equal(res[[2]], thriftr::TType$STRING)
  expect_equal(res[[3]], 1)
})

test_that("test_read_message_begin_not_strict", {
  b <- thriftr::TMemoryBuffer$new(as.raw(strtoi(c("00", "00", "00", "04", "74", "65", "73", "74", "0b", "00", "00", "00", "01"), 16L)))
  res <- thriftr::TBinaryProtocol$new(b, strict_read = FALSE)$read_message_begin()
  expect_equal(res[[1]], "test")
  expect_equal(res[[2]], thriftr::TType$STRING)
  expect_equal(res[[3]], 1)
})

test_that("test_write_struct", {
  item <- TItem$new(id=123, phones=list("123456", "abcdef"))
  b <- thriftr::TMemoryBuffer$new()
  thriftr::TBinaryProtocol$new(b)$write_struct(item)
  expect_equal(thriftr::hexlify(b$getvalue()), "08 00 01 00 00 00 7b 0f 00 02 0b 00 00 00 02 00 00 00 06 31 32 33 34 35 36 00 00 00 06 61 62 63 64 65 66 00")
})

test_that("test_read_struct", {
  b <- thriftr::TMemoryBuffer$new(as.raw(strtoi(c("08", "00", "01", "00", "00", "00", "7b", "0f", "00","02", "0b", "00", "00", "00",
    "02", "00", "00", "00", "06", "31", "32", "33", "34", "35", "36", "00", "00", "00", "06", "61", "62", "63", "64", "65", "66", "00"), 16L)))
  item <- TItem$new(id=123, phones=list("123456", "abcdef"))
  item2 <- TItem$new()
  thriftr::TBinaryProtocol$new(b)$read_struct(item2)
  expect_equal(item$id, item2$id)
  expect_equal(item$phones, item2$phones)
})

test_that("test_write_empty_struct", {
  item <- TItem$new()
  b <- thriftr::TMemoryBuffer$new()
  thriftr::TBinaryProtocol$new(b)$write_struct(item)
  expect_equal(thriftr::hexlify(b$getvalue()), "00")
})

test_that("test_read_empty_struct", {
  b <- thriftr::TMemoryBuffer$new(as.raw(strtoi(c("00"), 16L)))
  item <- TItem$new()
  item2 <- TItem$new()
  thriftr::TBinaryProtocol$new(b)$read_struct(item2)
  expect_equal(item$id, item2$id)
  expect_equal(item$phones, item2$phones)
})

test_that("test_write_huge_struct", {
  item <- TItem$new(id=12345, phones=rep("1234567890", 10000))
  b <- thriftr::TMemoryBuffer$new()
  thriftr::TBinaryProtocol$new(b)$write_struct(item)
})
