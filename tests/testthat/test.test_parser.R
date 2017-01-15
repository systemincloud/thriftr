#! /usr/bin/env Rscript

library(testthat)

context("parser")

test_that("test_comments", {
  thriftr_load("parser-cases/comments.thrift")
})

test_that("test_constants", {
  thrift <- thriftr::load("parser-cases/constants.thrift")
  expect_that(thrift$tbool,            equals(TRUE))
  expect_that(thrift$tboolint,         equals(TRUE))
  expect_that(thrift$int16,            equals(3))
  expect_that(thrift$int32,            equals(800))
  expect_that(thrift$int64,            equals(123456789))
  expect_that(thrift$tstr,             equals('hello world'))
  expect_that(thrift$integer32,        equals(900))
  expect_that(thrift$tdouble,          equals(1.3))
  expect_that(length(setdiff(thrift$tlist, list(1, 2, 3))), equals(0))
  expect_that(length(setdiff(thrift$tset, list(1, 2, 3))), equals(0))
  expect_that(thrift$tmap1[['key']],   equals('val'))
  expect_that(thrift$tmap2[['key']],   equals(32))
  expect_that(thrift$my_country,       equals(4))
  # TODO
#  expect_that(thrift$tom,              equals(thrift$Person$new(name='tom')))
#  expect_that(thrift$country_map['1'], equals('US'))
})

test_that("test_include", {
  # TODO
  #thrift <- thriftr_load("parser-cases/include.thrift")
  #expect_that(thrift$datetime, equals(1422009523))
})

test_that("test_tutorial", {
  # TODO
})

test_that("test_e_type_error", {
  # TODO
})

test_that("test_value_ref", {
  # TODO
})

test_that("test_type_ref", {
  # TODO
})

test_that("test_e_value_ref", {
  # TODO
})

test_that("test_enums", {
  # TODO
})

test_that("test_structs", {
  # TODO
})

test_that("test_e_structs", {
  # TODO
})

test_that("test_service", {
  # TODO
})

test_that("test_service_extends", {
  # TODO
})

test_that("test_e_service_extends", {
  # TODO
})

test_that("test_e_dead_include", {
  # TODO
})

test_that("test_e_grammer_error_at_eof", {
  # TODO
})

test_that("test_e_use_thrift_reserved_keywords", {
  expect_error(thriftr::load('parser-cases/e_use_thrift_reserved_keywords.thrift'),
               'Cannot use reserved language keyword: next at line 1')
})

test_that("test_e_duplicate_field_id_or_name", {
  # TODO
})

test_that("test_thrift_meta", {
  # TODO
})

test_that("test_load_fp", {
  # TODO
})

test_that("test_e_load_fp", {
  # TODO
})

test_that("test_recursive_union", {
  # TODO
})

test_that("test_issue_215", {
  # TODO
})