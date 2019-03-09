#! /usr/bin/env Rscript

pp_thrift <- thriftr::t_load("ping/pingpong.thrift", module_name="pp_thrift")

Dispatcher <- R6::R6Class("Dispatcher",
  public = list(
    ping = function() {
      print("ping pong!")
      return("pong")
    }
  )
)

server <- thriftr::make_server(
    pp_thrift$PingService,
    Dispatcher$new(),
    "127.0.0.1",
    6000)

print("serving...")

server$serve()
