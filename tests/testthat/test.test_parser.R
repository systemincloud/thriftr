#! /usr/bin/env Rscript

library(testthat)

context("parser")

test_that("test_comments", {
  thriftr::t_load("parser-cases/comments.thrift")
})

test_that("test_constants", {
  thrift <- thriftr::t_load("parser-cases/constants.thrift")
  expect_that(thrift$tbool, equals(TRUE))
  expect_that(thrift$tboolint, equals(TRUE))
  expect_that(thrift$int16, equals(3))
  expect_that(thrift$int32, equals(800))
  expect_that(thrift$int64, equals(123456789))
  expect_that(thrift$tstr, equals("hello world"))
  expect_that(thrift$integer32, equals(900))
  expect_that(thrift$tdouble, equals(1.3))
  expect_that(length(setdiff(thrift$tlist, list(1, 2, 3))), equals(0))
  expect_that(length(setdiff(thrift$tset, list(1, 2, 3))), equals(0))
  expect_that(thrift$tmap1[["key"]], equals("val"))
  expect_that(thrift$tmap2[["key"]], equals(32))
  expect_that(thrift$my_country, equals(4))
  expect_that(thrift$Person$new(name = "tom")$country, equals(1))
  expect_that(thrift$Person$new(name = "tom")$name, equals("tom"))
  expect_that(thrift$country_map[[toString(thrift$Country$US)]], equals("US"))
  expect_that(thrift$country_map[[toString(thrift$Country$UK)]], equals("UK"))
  expect_that(thrift$country_map[[toString(thrift$Country$CA)]], equals("CA"))
  expect_that(thrift$country_map[[toString(thrift$Country$CN)]], equals("CN"))
})

test_that("test_include", {
  thrift <- thriftr::t_load("parser-cases/include.thrift")
  expect_that(thrift$datetime, equals(1422009523))
})

test_that("test_tutorial", {
  thrift <- thriftr::t_load("parser-cases/tutorial.thrift",
      include_dirs = list("./parser-cases"))
  expect_equal(thrift$INT32CONSTANT, 9853)
  expect_equal(thrift$MAPCONSTANT[["hello"]], "world")
  expect_equal(thrift$MAPCONSTANT[["goodnight"]], "moon")
  expect_equal(thrift$Operation$ADD, 1)
  expect_equal(thrift$Operation$SUBTRACT, 2)
  expect_equal(thrift$Operation$MULTIPLY, 3)
  expect_equal(thrift$Operation$DIVIDE, 4)
  work <- thrift$Work$new()
  expect_equal(work$num1, 0)
  expect_equal(work$num2, NA)
  expect_equal(work$op, NA)
  expect_equal(work$comment, NA)
  expect_equal(thrift$Calculator$thrift_services[[1]], "ping")
  expect_equal(thrift$Calculator$thrift_services[[2]], "add")
  expect_equal(thrift$Calculator$thrift_services[[3]], "calculate")
  expect_equal(thrift$Calculator$thrift_services[[4]], "zip")
  expect_equal(thrift$Calculator$thrift_services[[5]], "getStruct")
})

test_that("test_e_type_error", {
  expect_error(thriftr::t_load("parser-cases/e_type_error_0.thrift"),
      "\\[ThriftParserError\\]Type error for constant int32 at line 1")
  expect_error(thriftr::t_load("parser-cases/e_type_error_1.thrift"),
      "\\[ThriftParserError\\]Type error for constant dct at line 1")
  expect_error(thriftr::t_load("parser-cases/e_type_error_2.thrift"),
      "\\[ThriftParserError\\]Type error for constant jack at line 6")
})

test_that("test_value_ref", {
  thrift <- thriftr::t_load("parser-cases/value_ref.thrift")
  expect_equal(thrift$container$key[[1]], 1)
  expect_equal(thrift$container$key[[2]], 2)
  expect_equal(thrift$container$key[[3]], 3)
  expect_equal(thrift$lst[[1]], 39)
  expect_equal(thrift$lst[[2]], 899)
  expect_equal(thrift$lst[[3]], 123)
})

test_that("test_type_ref", {
  thrift <- thriftr::t_load("parser-cases/type_ref.thrift")
  expect_equal(thrift$jerry$name, "jerry")
  expect_equal(thrift$jerry$age, 26)
  expect_equal(thrift$jerry$country, thrift$type_ref_shared$Country$US)
  expect_equal(thrift$book$name, "Hello World")
  expect_equal(thrift$book$writer$name, "jerry")
  expect_equal(thrift$book$writer$age, 26)
  expect_equal(thrift$book$writer$country, thrift$type_ref_shared$Country$US)
})

test_that("test_e_value_ref", {
  expect_error(thriftr::t_load("parser-cases/e_value_ref_0.thrift"),
      "\\[ThriftParserError\\]Can't find name ref at line 1")
  expect_error(thriftr::t_load("parser-cases/e_value_ref_1.thrift"),
      "\\[ThriftParserError\\]Couldn't find a named value in enum Lang for value 3")
  expect_error(thriftr::t_load("parser-cases/e_value_ref_2.thrift"),
      "\\[ThriftParserError\\]No enum value or constant found named Cookbook")
})

test_that("test_enums", {
  thrift <- thriftr::t_load("parser-cases/enums.thrift")
  expect_equal(thrift$Lang$C, 0)
  expect_equal(thrift$Lang$Go, 1)
  expect_equal(thrift$Lang$Java, 2)
  expect_equal(thrift$Lang$Javascript, 3)
  expect_equal(thrift$Lang$PHP, 4)
  expect_equal(thrift$Lang$Python, 5)
  expect_equal(thrift$Lang$Ruby, 6)
  expect_equal(thrift$Country$US, 1)
  expect_equal(thrift$Country$UK, 2)
  expect_equal(thrift$Country$CN, 3)
  expect_equal(thrift$OS$OSX, 0)
  expect_equal(thrift$OS$Win, 3)
  expect_equal(thrift$OS$Linux, 4)
})

test_that("test_structs", {
  thrift <- thriftr::t_load("parser-cases/structs.thrift")
  expect_equal(thrift$Person$thrift_spec[["1"]][[1]], thriftr::TType$STRING)
  expect_equal(thrift$Person$thrift_spec[["1"]][[2]], "name")
  expect_equal(thrift$Person$thrift_spec[["1"]][[3]], FALSE)
  expect_equal(thrift$Person$thrift_spec[["2"]][[1]], thriftr::TType$STRING)
  expect_equal(thrift$Person$thrift_spec[["2"]][[2]], "address")
  expect_equal(thrift$Person$thrift_spec[["2"]][[3]], FALSE)
  expect_equal(thrift$Person$default_spec[[1]][[1]], "name")
  expect_equal(thrift$Person$default_spec[[1]][[2]], NA)
  expect_equal(thrift$Person$default_spec[[2]][[1]], "address")
  expect_equal(thrift$Person$default_spec[[2]][[2]], NA)
  expect_equal(thrift$Email$thrift_spec[["1"]][[1]], thriftr::TType$STRING)
  expect_equal(thrift$Email$thrift_spec[["1"]][[2]], "subject")
  expect_equal(thrift$Email$thrift_spec[["1"]][[3]], FALSE)
  expect_equal(thrift$Email$thrift_spec[["2"]][[1]], thriftr::TType$STRING)
  expect_equal(thrift$Email$thrift_spec[["2"]][[2]], "content")
  expect_equal(thrift$Email$thrift_spec[["2"]][[3]], FALSE)
  expect_equal(thrift$Email$thrift_spec[["3"]][[1]], thriftr::TType$STRUCT)
  expect_equal(thrift$Email$thrift_spec[["3"]][[2]], "sender")
  expect_equal(thrift$Email$thrift_spec[["3"]][[3]], thrift$Person)
  expect_equal(thrift$Email$thrift_spec[["3"]][[4]], FALSE)
  expect_equal(thrift$Email$thrift_spec[["4"]][[1]], thriftr::TType$STRUCT)
  expect_equal(thrift$Email$thrift_spec[["4"]][[2]], "receiver")
  expect_equal(thrift$Email$thrift_spec[["4"]][[3]], thrift$Person)
  expect_equal(thrift$Email$thrift_spec[["4"]][[4]], TRUE)
  expect_equal(thrift$Email$default_spec[[1]][[1]], "subject")
  expect_equal(thrift$Email$default_spec[[1]][[2]], "Subject")
  expect_equal(thrift$Email$default_spec[[2]][[1]], "content")
  expect_equal(thrift$Email$default_spec[[2]][[2]], NA)
  expect_equal(thrift$Email$default_spec[[3]][[1]], "sender")
  expect_equal(thrift$Email$default_spec[[3]][[2]], NA)
  expect_equal(thrift$Email$default_spec[[4]][[1]], "receiver")
  expect_equal(thrift$Email$default_spec[[4]][[2]], NA)
  expect_equal(thrift$email$subject, "Hello")
  expect_equal(thrift$email$content, "Long time no see")
  expect_equal(thrift$email$sender$name, "jack")
  expect_equal(thrift$email$sender$address, "jack@gmail.com")
  expect_equal(thrift$email$receiver$name, "chao")
  expect_equal(thrift$email$receiver$address, "chao@gmail.com")
})

test_that("test_e_structs", {
  expect_error(thriftr::t_load("parser-cases/e_structs_0.thrift"),
      "\\[ThriftParserError\\]Field name was required to create constant for type User")
  expect_error(thriftr::t_load("parser-cases/e_structs_1.thrift"),
      "\\[ThriftParserError\\]No field named avatar was found in struct of type User")
})

test_that("test_service", {
  thrift <- thriftr::t_load("parser-cases/service.thrift")
  expect_equal(thrift$EmailService$thrift_services[[1]], "ping")
  expect_equal(thrift$EmailService$thrift_services[[2]], "send")
  expect_equal(length(names(thrift$EmailService$ping_args$thrift_spec)), 0)
  expect_equal(length(thrift$EmailService$ping_args$default_spec), 0)
  expect_equal(thrift$EmailService$ping_result$thrift_spec[["1"]][[1]], thriftr::TType$STRUCT)
  expect_equal(thrift$EmailService$ping_result$thrift_spec[["1"]][[2]], "network_error")
  expect_equal(thrift$EmailService$ping_result$thrift_spec[["1"]][[3]], thrift$NetworkError)
  expect_equal(thrift$EmailService$ping_result$thrift_spec[["1"]][[4]], FALSE)
  expect_equal(thrift$EmailService$ping_result$default_spec[[1]][[1]], "network_error")
  expect_equal(thrift$EmailService$ping_result$default_spec[[1]][[2]], NA)
  expect_equal(thrift$EmailService$send_args$thrift_spec[["1"]][[1]], thriftr::TType$STRUCT)
  expect_equal(thrift$EmailService$send_args$thrift_spec[["1"]][[2]], "recver")
  expect_equal(thrift$EmailService$send_args$thrift_spec[["1"]][[3]], thrift$User)
  expect_equal(thrift$EmailService$send_args$thrift_spec[["1"]][[4]], FALSE)
  expect_equal(thrift$EmailService$send_args$thrift_spec[["2"]][[1]], thriftr::TType$STRUCT)
  expect_equal(thrift$EmailService$send_args$thrift_spec[["2"]][[2]], "sender")
  expect_equal(thrift$EmailService$send_args$thrift_spec[["2"]][[3]], thrift$User)
  expect_equal(thrift$EmailService$send_args$thrift_spec[["2"]][[4]], FALSE)
  expect_equal(thrift$EmailService$send_args$thrift_spec[["3"]][[1]], thriftr::TType$STRUCT)
  expect_equal(thrift$EmailService$send_args$thrift_spec[["3"]][[2]], "email")
  expect_equal(thrift$EmailService$send_args$thrift_spec[["3"]][[3]], thrift$Email)
  expect_equal(thrift$EmailService$send_args$thrift_spec[["3"]][[4]], FALSE)
  expect_equal(thrift$EmailService$send_args$default_spec[[1]][[1]], "recver")
  expect_equal(thrift$EmailService$send_args$default_spec[[1]][[2]], NA)
  expect_equal(thrift$EmailService$send_args$default_spec[[2]][[1]], "sender")
  expect_equal(thrift$EmailService$send_args$default_spec[[2]][[2]], NA)
  expect_equal(thrift$EmailService$send_args$default_spec[[3]][[1]], "email")
  expect_equal(thrift$EmailService$send_args$default_spec[[3]][[2]], NA)
  expect_equal(thrift$EmailService$send_result$thrift_spec[["0"]][[1]], thriftr::TType$BOOL)
  expect_equal(thrift$EmailService$send_result$thrift_spec[["0"]][[2]], "success")
  expect_equal(thrift$EmailService$send_result$thrift_spec[["0"]][[3]], FALSE)
  expect_equal(thrift$EmailService$send_result$thrift_spec[["1"]][[1]], thriftr::TType$STRUCT)
  expect_equal(thrift$EmailService$send_result$thrift_spec[["1"]][[2]], "network_error")
  expect_equal(thrift$EmailService$send_result$thrift_spec[["1"]][[3]], thrift$NetworkError)
  expect_equal(thrift$EmailService$send_result$thrift_spec[["1"]][[4]], FALSE)
  expect_equal(thrift$EmailService$send_result$default_spec[[1]][[1]], "success")
  expect_equal(thrift$EmailService$send_result$default_spec[[1]][[2]], NA)
  expect_equal(thrift$EmailService$send_result$default_spec[[2]][[1]], "network_error")
  expect_equal(thrift$EmailService$send_result$default_spec[[2]][[2]], NA)
})

test_that("test_service_extends", {
  thrift <- thriftr::t_load("parser-cases/service_extends.thrift")
  expect_equal(thrift$PingService$thrift_services[[1]], "ping")
  expect_equal(thrift$PingService$thrift_services[[2]], "getStruct")
})

test_that("test_e_service_extends", {
  expect_error(thrift <- thriftr::t_load("parser-cases/e_service_extends_0.thrift"),
      "\\[ThriftParserError\\]Can't find service shared.NotExistService for service PingService to extend")
})

test_that("test_e_dead_include", {
  expect_error(thriftr::t_load("parser-cases/e_dead_include_0.thrift"),
      "\\[ThriftParserError\\]Dead including")
})

test_that("test_e_grammer_error_at_eof", {
  expect_error(thriftr::t_load("parser-cases/e_grammer_error_at_eof.thrift"),
      "\\[ThriftGrammerError\\]Grammar error at EOF")
})

test_that("test_e_use_thrift_reserved_keywords", {
  expect_error(thriftr::t_load("parser-cases/e_use_thrift_reserved_keywords.thrift"),
      "\\[ThriftLexerError\\]Cannot use reserved language keyword: next at line 1")
})

test_that("test_e_duplicate_field_id_or_name", {
  expect_error(thriftr::t_load("parser-cases/e_duplicate_field_id.thrift"),
      "\\[ThriftGrammerError\\]'1:efg' field identifier/name has already been used")
  expect_error(thriftr::t_load("parser-cases/e_duplicate_field_name.thrift"),
      "\\[ThriftGrammerError\\]\\'2:abc\\' field identifier/name has already been used")
})

test_that("test_thrift_meta", {
  thrift <- thriftr::t_load("parser-cases/tutorial.thrift")
  meta <- thrift$thrift_meta
  expect_equal(meta$consts[[1]], thrift$INT32CONSTANT)
  expect_equal(meta$consts[[2]], thrift$MAPCONSTANT)
  expect_equal(meta$enums[[1]], thrift$Operation)
  expect_equal(meta$structs[[1]], thrift$Work)
  expect_equal(meta$exceptions[[1]], thrift$InvalidOperation)
  expect_equal(meta$services[[1]], thrift$Calculator)
  expect_equal(meta$includes[[1]], thrift$shared)
})

test_that("test_recursive_union", {
  thrift <- thriftr::t_load("parser-cases/recursive_union.thrift")
  expect_equal(thrift$Dynamic$thrift_spec[["1"]][[1]], thriftr::TType$BOOL)
  expect_equal(thrift$Dynamic$thrift_spec[["1"]][[2]], "boolean")
  expect_equal(thrift$Dynamic$thrift_spec[["1"]][[3]], FALSE)
  expect_equal(thrift$Dynamic$thrift_spec[["2"]][[1]], thriftr::TType$I64)
  expect_equal(thrift$Dynamic$thrift_spec[["2"]][[2]], "integer")
  expect_equal(thrift$Dynamic$thrift_spec[["2"]][[3]], FALSE)
  expect_equal(thrift$Dynamic$thrift_spec[["3"]][[1]], thriftr::TType$DOUBLE)
  expect_equal(thrift$Dynamic$thrift_spec[["3"]][[2]], "doubl")
  expect_equal(thrift$Dynamic$thrift_spec[["3"]][[3]], FALSE)
  expect_equal(thrift$Dynamic$thrift_spec[["4"]][[1]], thriftr::TType$STRING)
  expect_equal(thrift$Dynamic$thrift_spec[["4"]][[2]], "str")
  expect_equal(thrift$Dynamic$thrift_spec[["4"]][[3]], FALSE)
  expect_equal(thrift$Dynamic$thrift_spec[["5"]][[1]], thriftr::TType$LIST)
  expect_equal(thrift$Dynamic$thrift_spec[["5"]][[2]], "arr")
  expect_equal(thrift$Dynamic$thrift_spec[["5"]][[3]][[1]], thriftr::TType$STRUCT)
  expect_equal(thrift$Dynamic$thrift_spec[["5"]][[3]][[2]], thrift$Dynamic)
  expect_equal(thrift$Dynamic$thrift_spec[["5"]][[4]], FALSE)
  expect_equal(thrift$Dynamic$thrift_spec[["6"]][[1]], thriftr::TType$MAP)
  expect_equal(thrift$Dynamic$thrift_spec[["6"]][[2]], "object")
  expect_equal(thrift$Dynamic$thrift_spec[["6"]][[3]][[1]], thriftr::TType$STRING)
  expect_equal(thrift$Dynamic$thrift_spec[["6"]][[3]][[2]][[1]], thriftr::TType$STRUCT)
  expect_equal(thrift$Dynamic$thrift_spec[["6"]][[3]][[2]][[2]], thrift$Dynamic)
  expect_equal(thrift$Dynamic$thrift_spec[["6"]][[4]], FALSE)
})

test_that("test_issue_215", {
  thrift <- thriftr::t_load("parser-cases/issue_215.thrift")
  expect_equal(thrift$abool, TRUE)
  expect_equal(thrift$falsevalue, 123)
})
