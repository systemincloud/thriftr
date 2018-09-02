#! /usr/bin/env Rscript

library(testthat)

context("const")

thrift <- thriftr::t_load("const.thrift")

test_that("test_num_const", {
  expect_equal(thrift$NEGATIVE_I16,    -10)
  expect_equal(thrift$NEGATIVE_DOUBLE, -123.456)

  expect_equal(thrift$I16_CONST,    10)
  expect_equal(thrift$I32_CONST,    100000)
  expect_equal(thrift$DOUBLE_CONST, 123.456)
})

test_that("test_string_const", {
  expect_equal(thrift$DOUBLE_QUOTED_CONST, "hello")
  expect_equal(thrift$SINGLE_QUOTED_CONST, "hello")
})

test_that("test_const_with_sep", {
  expect_equal(thrift$CONST_WITH_SEP1, "hello")
  expect_equal(thrift$CONST_WITH_SEP2, "hello")
})

test_that("test_list_const", {
  expect_output(str(thrift$I32_LIST_CONST),
  'List of 3
 $ : int 1
 $ : int 2
 $ : int 3', fixed=TRUE)
  expect_output(str(thrift$DOUBLE_LIST_CONST),
  'List of 3
 $ : num 1.1
 $ : num 2.2
 $ : num 3.3', fixed=TRUE)
  expect_output(str(thrift$STRING_LIST_CONST),
  'List of 2
 $ : chr "hello"
 $ : chr "world"', fixed=TRUE)

  expect_output(str(thrift$I32_LIST_LIST_CONST),
  'List of 2
 $ :List of 3
  ..$ : int 1
  ..$ : int 2
  ..$ : int 3
 $ :List of 3
  ..$ : int 4
  ..$ : int 5
  ..$ : int 6', fixed=TRUE)
  expect_output(str(thrift$DOUBLE_LIST_LIST_CONST),
  'List of 2
 $ :List of 3
  ..$ : num 1.1
  ..$ : num 2.2
  ..$ : num 3.3
 $ :List of 3
  ..$ : num 4.4
  ..$ : num 5.5
  ..$ : num 6.6', fixed=TRUE)
  expect_output(str(thrift$STRING_LIST_LIST_CONST),
  'List of 2
 $ :List of 2
  ..$ : chr "hello"
  ..$ : chr "world"
 $ :List of 2
  ..$ : chr "foo"
  ..$ : chr "bar"', fixed=TRUE)
})
