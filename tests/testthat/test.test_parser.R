#! /usr/bin/env Rscript

library(testthat)
library(thriftr)

context("parser")

test_that("test_comments", {
  thriftr_load("parser-cases/comments.thrift")
})

test_that("test_constants", {
  thrift <- thriftr_load("parser-cases/constants.thrift")
  expect_that(thrift$tbool,            equals(TRUE))
  expect_that(thrift$tboolint,         equals(TRUE))
  expect_that(thrift$int16,            equals(3))
  expect_that(thrift$int32,            equals(800))
  expect_that(thrift$int64,            equals(123456789))
  expect_that(thrift$tstr,             equals('hello world'))
  expect_that(thrift$integer32,        equals(900))
  expect_that(thrift$tdouble,          equals(1.3))
#  expect_that(thrift$tlist,            equals(list(1, 2, 3)))
#  expect_that(thrift$tset,             equals(list(1, 2, 3)))
#  expect_that(thrift$tmap1['key'],     equals('val'))
#  expect_that(thrift$tmap2['key'],     equals(32))
#  expect_that(thrift$my_country,       equals(4))
#  expect_that(thrift$tom,              equals(thrift$Person$new(name='tom')))
#  expect_that(thrift$country_map['1'], equals('US'))
})

