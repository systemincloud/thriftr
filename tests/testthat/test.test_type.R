#! /usr/bin/env Rscript

library(testthat)
library(thriftr)

context("type")

test_that("test_set", {
  s <- thriftr_load("type.thrift")
  print(s$Set)
#  assert s.Set.thrift_spec == {1: (TType.SET, "a_set", TType.STRING, True)}
})
