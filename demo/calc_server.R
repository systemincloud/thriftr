#! /usr/bin/env Rscript

calc_thrift <- thriftr::t_load("calc/calc.thrift", module_name="calc_thrift")

Dispatcher <- R6::R6Class("Dispatcher",
  public = list(
    add = function(a, b) {
      print(sprintf("add -> %s + %s", a, b))
      return(a + b)
    },
    sub = function(a, b) {
      print(sprintf("sub -> %s - %s", a, b))
      return(a - b)
    },
    mult = function(a, b) {
      print(sprintf("mult -> %s * %s", a, b))
      return(a * b)
    },
    div = function(a, b) {
      print(sprintf("div -> %s / %s", a, b))
      return(a / b)
    }
  )
)

server <- thriftr::make_server(
    calc_thrift$Calculator,
    Dispatcher$new(),
    "127.0.0.1",
    6000)

print("serving...")

server$serve()
