#! /usr/bin/env Rscript

sleep_thrift <- thriftr::t_load("oneway/sleep.thrift", module_name="sleep_thrift")

client <- thriftr::make_client(
    sleep_thrift$Sleep,
    "127.0.0.1",
    6000)
# sleep multiple times, but the client won't wait any more
client$sleep(1)
client$sleep(2)
client$sleep(3)
client$sleep(4)
