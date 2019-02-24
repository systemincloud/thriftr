#! /usr/bin/env Rscript

library(testthat)

context("recursive_definition")

test_that("test_recursive_definition", {
  thrift <- thriftr::t_load("recursive_definition.thrift")
  expect_equal(thrift$Bar$thrift_spec[["1"]][[1]], 12)
  expect_equal(thrift$Bar$thrift_spec[["1"]][[2]], "test")
  expect_equal(thrift$Bar$thrift_spec[["1"]][[3]], thrift$Foo)
  expect_equal(thrift$Bar$thrift_spec[["1"]][[4]], FALSE)
  expect_equal(thrift$Foo$thrift_spec[["1"]][[1]], 12)
  expect_equal(thrift$Foo$thrift_spec[["1"]][[2]], 'test')
  expect_equal(thrift$Foo$thrift_spec[["1"]][[3]], thrift$Bar)
  expect_equal(thrift$Foo$thrift_spec[["1"]][[4]], FALSE)
  expect_equal(thrift$Foo$thrift_spec[["2"]][[1]], 15)
  expect_equal(thrift$Foo$thrift_spec[["2"]][[2]], 'some_int')
  expect_equal(thrift$Foo$thrift_spec[["2"]][[3]], 8)
  expect_equal(thrift$Foo$thrift_spec[["2"]][[4]], FALSE)
})

test_that("test_const", {
  thrift <- thriftr::t_load("recursive_definition.thrift")
  expect_equal(thrift$SOME_INT[[1]], 1)
  expect_equal(thrift$SOME_INT[[2]], 2)
  expect_equal(thrift$SOME_INT[[3]], 3)
})
