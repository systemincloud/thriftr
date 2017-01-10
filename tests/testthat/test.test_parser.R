#! /usr/bin/env Rscript

library(testthat)
library(thriftr)

context("parser")

test_that("test_comments", {
  s <- thriftr_load("parser-cases/comments.thrift")
})
