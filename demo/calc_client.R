#! /usr/bin/env Rscript

library(thriftr)

calc_thrift <- thriftr::thriftr_load("calc/calc.thrift", module_name="calc_thrift")