#! /usr/bin/env Rscript

library(testthat)
library(parallel)

context("socket")

test_socket <- function(server_socket, client_socket) {

  server <- function() {
    server_socket$listen()
    conn <- server_socket$accept()
    buff3 <- conn$read(12)
    conn$write(buff3)
    conn$close()
  }

  client <- function () {
    Sys.sleep(2)
    client_socket$open()
    buff <- charToRaw("Hello World!")
    client_socket$write(buff)
    buff2 <- client_socket$read(12)
    client_socket$close()
    all(buff == buff2)
  }
  expect_true(mclapply(list(server, client), function(x) {
    x()
  }, mc.cores=2)[[2]])
}

test_that("test_inet_socket", {
  if(.Platform$OS.type == "unix") {
    server_socket <- thriftr::TServerSocket$new(host="localhost", port=6011)
    client_socket <- thriftr::TSocket$new(host="localhost", port=6011)

    test_socket(server_socket, client_socket)
  }
})
