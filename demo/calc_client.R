#! /usr/bin/env Rscript

calc_thrift <- thriftr::t_load("calc/calc.thrift", module_name="calc_thrift")

cal <- thriftr::make_client(
    calc_thrift$Calculator,
    "127.0.0.1",
    6000)

a <- cal$mult(5, 2)
b <- cal$sub(7, 3)
c <- cal$sub(6, 4)
d <- cal$mult(b, 10)
e <- cal$add(a, d)
f <- cal$div(e, c)
print(f)
