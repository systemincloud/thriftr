#! /usr/bin/env Rscript

library(testthat)

context("base")

ab  <- thriftr::load("addressbook.thrift")
ab2 <- thriftr::load("addressbook.thrift")

test_that("test_obj_equalcheck", {
  # TODO
})

test_that("test_exc_equalcheck", {
  # TODO
})

test_that("test_cls_equalcheck", {
  # TODO
})

test_that("test_isinstancecheck", {
  # TODO
})

test_that("test_hashable", {
  # TODO
})

test_that("test_default_value", {
  # TODO
})

test_that("test_parse_spec", {
  # TODO
})

test_that("test_init_func", {
  # TODO
})