#! /usr/bin/env Rscript

library(testthat)

context("type_mismatch")

Struct <- thriftr::to_proper_struct(
  list(
    list("1", list(thriftr::TType$I32, 'a', FALSE)),
    list("2", list(thriftr::TType$STRING, 'b', FALSE)),
    list("3", list(thriftr::TType$DOUBLE, 'c', FALSE))
  ),
  list(
    list('a', NA),
    list('b', NA),
    list('c', NA)
  )
)

TItem <- thriftr::to_proper_struct(
  list(
    list("1", list(thriftr::TType$I32, 'id', FALSE)),
    list("2", list(thriftr::TType$LIST, 'phones', thriftr::TType$STRING, FALSE)),
    list("3", list(thriftr::TType$MAP, 'addr', list(thriftr::TType$I32, thriftr::TType$STRING), FALSE)),
    list("4", list(thriftr::TType$LIST, 'data', list(thriftr::TType$STRUCT, Struct), FALSE))
  ),
  list(
    list('id', NA),
    list('phones', NA),
    list('addr', NA),
    list('data', NA)
  )
)

test_that("test_list_type_mismatch", {
  TMismatchItem <- thriftr::to_proper_struct(
    list(
      list("1", list(thriftr::TType$I32, 'id', FALSE)),
      list("2", list(thriftr::TType$LIST, 'phones', list(thriftr::TType$I32, FALSE), FALSE))
    ),
    list(
      list('id', NA),
      list('phones', NA)
    )
  )

  t <- thriftr::TMemoryBuffer$new()
  p <- thriftr::TBinaryProtocol$new(t)

  item <- TItem$new(id=37, phones=list("23424", "235125"))
  p$write_struct(item)
  p$write_message_end()

  item2 <- TMismatchItem$new()
  p$read_struct(item2)

  expect_equal(length(item2$phones), 0)
})

test_that("test_map_type_mismatch", {
  TMismatchItem <- thriftr::to_proper_struct(
    list(
      list("1", list(thriftr::TType$I32, 'id', FALSE)),
      list("3", list(thriftr::TType$MAP, 'addr', list(thriftr::TType$STRING, thriftr::TType$STRING), FALSE))
    ),
    list(
      list('id', NA),
      list('addr', NA)
    )
  )

  t <- thriftr::TMemoryBuffer$new()
  p <- thriftr::TBinaryProtocol$new(t)

  addr <- new.env()
  addr[["1"]] <- "hello"
  addr[["2"]] <- "world"
  item <- TItem$new(id=37, addr=addr)

  p$write_struct(item)
  p$write_message_end()

  item2 <- TMismatchItem$new()
  p$read_struct(item2)

  expect_equal(length(names(item2$addr)), 0)
})

test_that("test_struct_mismatch", {
  MismatchStruct <- thriftr::to_proper_struct(
    list(
      list("1", list(thriftr::TType$STRING, 'a', FALSE)),
      list("2", list(thriftr::TType$STRING, 'b', FALSE))
    ),
    list(
      list('a', NA),
      list('b', NA)
    )
  )

  TMismatchItem <- thriftr::to_proper_struct(
    list(
      list("1", list(thriftr::TType$I32, 'id', FALSE)),
      list("2", list(thriftr::TType$LIST, 'phones', FALSE)),
      list("3", list(thriftr::TType$MAP, list(thriftr::TType$I32, thriftr::TType$STRING), 'addr', FALSE)),
      list("4", list(thriftr::TType$LIST, 'data', list(thriftr::TType$STRUCT, MismatchStruct), FALSE))
    ),
    list(
      list('id', NA),
      list('phones', NA),
      list('addr', NA)
    )
  )

  t <- thriftr::TMemoryBuffer$new()
  p <- thriftr::TBinaryProtocol$new(t)

  item <- TItem$new(id=37, data=list(
    Struct$new(a=1, b="hello", c=0.123),
    Struct$new(a=2, b="world", c=34.342346),
    Struct$new(a=3, b="when", c=25235.14)
  ))

  p$write_struct(item)
  p$write_message_end()

  item2 <- TMismatchItem$new()
  p$read_struct(item2)

  expect_equal(length(item2$data), 3)
  for (i in item2$data) {
    expect_false(is.na(i$b))
  }
})
