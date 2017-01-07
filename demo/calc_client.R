#! /usr/bin/env Rscript

library(thriftr)

calc_thrift <- thriftr::thriftr_load("calc/calc.thrift", module_name="calc_thrift")

cal <- make_client()

#a <- cal$mult(5, 2)
#b <- cal$sub(7, 3)
#c <- cal$sub(6, 4)
#d <- cal$mult(b, 10)
#e <- cal$add(a, d)
#f <- cal$div(e, c)
#print(f)