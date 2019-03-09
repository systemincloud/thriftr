#! /usr/bin/env Rscript

sleep_thrift <- thriftr::t_load("oneway/sleep.thrift", module_name="sleep_thrift")

Dispatcher <- R6::R6Class("Dispatcher",
  public = list(
  sleep = function(seconds) {
      print(sprintf("I'm going to sleep %d seconds", seconds))
      Sys.sleep(seconds)
      print("Sleep over!")
    }
  )
)

server <- thriftr::make_server(
    sleep_thrift$Sleep,
    Dispatcher$new(),
    "127.0.0.1",
    6000)

print("serving...")

server$serve()
