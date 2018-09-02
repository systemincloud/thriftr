#! /usr/bin/env Rscript

library(testthat)

context("type")

test_that("test_set", {
  s <- thriftr::t_load("type.thrift")
  expect_that(s$thrift_file, equals('type.thrift'))
  expect_that(names(s$thrift_meta), equals('structs'))
  expect_that(s$Set$thrift_spec[["1"]][[1]], equals(thriftr::TType$SET))
  expect_that(s$Set$thrift_spec[["1"]][[2]], equals("a_set"))
  expect_that(s$Set$thrift_spec[["1"]][[3]], equals(thriftr::TType$STRING))
  expect_that(s$Set$thrift_spec[["1"]][[4]], equals(TRUE))
  expect_that(s$Set$new()$a_set, equals(NA))
  expect_that(s$Set$new(list('abc'))$a_set, equals(list('abc')))
})
