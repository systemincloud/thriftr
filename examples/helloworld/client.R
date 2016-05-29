#! /usr/bin/env Rscript

cat('R:START\n')
port <- commandArgs(TRUE)[1]

thriftc <- thriftr::load("service.thrift", module_name="service_thrift")
client <- thriftr::make_client(thriftc.Service, '127.0.0.1', port)

cat('R:END\n')