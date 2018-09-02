#! /usr/bin/env Rscript

library(testthat)

context("base")

ab  <- thriftr::t_load("addressbook.thrift")
ab2 <- thriftr::t_load("addressbook.thrift")

test_that("test_obj_equalcheck", {
  expect_equal(ab$Person$new(name="hello"),
      ab2$Person$new(name="hello"))
})

test_that("test_exc_equalcheck", {
  expect_equal(ab$PersonNotExistsError$new("exc"),
      ab$PersonNotExistsError$new("exc"))
})

test_that("test_cls_equalcheck", {
  expect_equal(ab$Person,
      ab2$Person)
})

test_that("test_isinstancecheck", {
  expect_equal(class(ab$Person$new())[[1]],
      "Person")
  expect_equal(class(ab$Person$new(name = "hello"))[[1]],
      "Person")
  expect_equal(class(ab$PersonNotExistsError$new())[[1]],
      "PersonNotExistsError")
})

test_that("test_default_value", {
  expect_equal(ab$PhoneNumber$new()$type,
      ab$PhoneType$MOBILE)
})

test_that("test_parse_spec", {
  cases <- list(
    list(list(thriftr::TType$I32, NA), "I32"),
    list(list(thriftr::TType$STRUCT, ab$PhoneNumber), "PhoneNumber"),
    list(list(thriftr::TType$LIST, thriftr::TType$I32), "LIST<I32>"),
    list(list(thriftr::TType$LIST, list(thriftr::TType$STRUCT, ab$PhoneNumber)), "LIST<PhoneNumber>"),
    list(list(thriftr::TType$MAP, list(thriftr::TType$STRING, list(
            thriftr::TType$LIST, list(thriftr::TType$MAP, list(thriftr::TType$STRING, thriftr::TType$STRING))))),
         "MAP<BINARY, LIST<MAP<BINARY, BINARY>>>")
  )

  for (spec_res in cases) {
    expect_equal(thriftr::parse_spec(spec_res[[1]][[1]], spec_res[[1]][[2]]),
        spec_res[[2]])
  }
})
