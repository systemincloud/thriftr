#! /usr/bin/env Rscript

pp_thrift <- thriftr::t_load("ping/pingpong.thrift", module_name="pp_thrift")

c <- thriftr::make_client(
    pp_thrift$PingService,
    "127.0.0.1",
    6000)

pong <- c$ping()
print(pong)
