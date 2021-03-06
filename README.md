[![Build Status](https://travis-ci.org/systemincloud/thriftr.svg?branch=master)](https://travis-ci.org/systemincloud/thriftr)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/thriftr)](https://cran.r-project.org/package=thriftr)
[![](https://cranlogs.r-pkg.org/badges/thriftr)](https://cran.r-project.org/package=thriftr)
[![codecov](https://codecov.io/gh/systemincloud/thriftr/branch/master/graph/badge.svg)](https://codecov.io/gh/systemincloud/thriftr)
[![PayPal donation](https://img.shields.io/badge/donation-PayPal-green.svg)](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=UR288FRQUSYQE&item_name=Thriftr+-+R+Thrift&currency_code=USD&source=url)

Introduction
============

ThriftR is a pure R implementation of [Apache  Thrift](http://thrift.apache.org) in R way. 
This project is a R clone of [ThriftPy](https://github.com/eleme/thriftpy).

How to Use
==========

     library(thriftr)

The examples directory contains several different examples.

A simple example is found at the end of this document

Resources
=========
The GitHub page for ThriftR can be found at:

     https://github.com/systemincloud/thriftr


Example
=======

ThriftR make it super easy to write server/client code with thrift. Let's checkout this simple pingpong service demo.

We need a 'pingpong.thrift' file:

```
    service PingPong {
        string ping(),
    }
```

Then we can make a server:


```R
library(thriftr)

pingpong_thrift = thriftr::t_load("pingpong.thrift", module_name="pingpong_thrift")

Dispatcher <- R6::R6Class("Dispatcher",
  public = list(
    ping = function() {
      return('pong')
    }
  )
)

server = thriftr::make_server(pingpong_thrift$PingPong, Dispatcher$new(), '127.0.0.1', 6000)
server$serve()
```

And a client:

```R
library(thriftr)

pingpong_thrift = thriftr::t_load("pingpong.thrift", module_name="pingpong_thrift")

client = thriftr::make_client(pingpong_thrift$PingPong, "127.0.0.1", 6000)
cut(client$ping())
```
