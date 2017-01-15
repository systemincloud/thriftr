#! /usr/bin/env Rscript

library(testthat)

context("const")

thrift <- thriftr::load("const.thrift")

test_that("test_num_const", {
  expect_that(thrift$NEGATIVE_I16,    equals(-10))
  expect_that(thrift$NEGATIVE_DOUBLE, equals(-123.456))
  
  expect_that(thrift$I16_CONST,    equals(10))
  expect_that(thrift$I32_CONST,    equals(100000))
  expect_that(thrift$DOUBLE_CONST, equals(123.456))
})

test_that("test_string_const", {
  # TODO
})

test_that("test_const_with_sep", {
  # TODO
})

test_that("test_list_const", {
  # TODO
})