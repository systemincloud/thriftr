#! /usr/bin/env Rscript

library(testthat)
library(thriftr)

context("type")

test_that("test_set", {
  s <- thriftr_load("type.thrift")
  expect_that(s$Set$thrift_spec[["1"]][[1]], equals(TType$SET))
  expect_that(s$Set$thrift_spec[["1"]][[2]], equals("a_set"))
  expect_that(s$Set$thrift_spec[["1"]][[3]], equals(TType$STRING))
  expect_that(s$Set$thrift_spec[["1"]][[4]], equals(TRUE))
})
