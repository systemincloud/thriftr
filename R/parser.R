# MIT License
#
# Copyright (c) 2018 System in Cloud - Marek Jagielski
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
#    The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

#' Load thrift file as a R6 instance.
#'
#' The module loaded and objects inside may only be pickled if module_name
#' was provided.
#'
#' @param path file path to parse, should be a string ending with '.thrift'
#' @param module_name the name for parsed module, the default is the basename
#'                    without extension of `path`
#' @param include_dirs directories to find thrift files while processing
#'                     the `include` directive, by default: ['.']
#'
#' @return Thrift R6 class instance
#'
#' @export
t_load <- function(path, module_name=NA, include_dirs=NA) {
  thrift <- parse(path, module_name, include_dirs = include_dirs)
  if (length(names(incomplete_type$dict)) > 0) {
    fill_incomplete_ttype(thrift, thrift)
  }
    
  return(thrift)
}

fill_incomplete_ttype <- function(tmodule, definition) {
  type <- typeof(definition)
  clazz <- class(definition)

  # fill incomplete ttype
  if (type == "list") {
    # fill const value
    if (typeof(definition[[1]]) == "character" && definition[[1]] == 'UNKNOWN_CONST') {
      ttype <- get_definition(
        tmodule, incomplete_type$dict[[as.character(definition[[2]])]][[1]], definition[[4]])
      return(Parser$new()$cast(ttype)(definition[[3]]))
    }
    # fill an incomplete alias ttype
    if (typeof(definition[[2]]) == "character" && definition[[2]] %in% names(incomplete_type$dict)) {
      it <- incomplete_type$dict[[definition[[2]]]]
      return(list(definition[[1]], get_definition(tmodule, it[[1]], it[[2]])))
    }
    # fill service method which has incomplete arg, return ttype
    else if (typeof(definition[[1]]) == "character" && definition[[1]] %in% names(incomplete_type$dict)) {
      it <- incomplete_type$dict[[as.character(definition[[1]])]]
      real_type <- get_definition(tmodule, it[[1]], it[[2]])
      return(list(real_type[[1]], definition[[2]], real_type[[2]], definition[[3]]))
    }
    # fill incomplete compound ttype
    else if (typeof(definition[[2]]) == "list") {
      return(list(definition[[1]], fill_incomplete_ttype(tmodule, definition[[2]])))
    }
  }
  # handle thrift module
  else if (type == "environment" && 
           length(clazz) > 1 &&
           clazz[[2]] == "thrift") {
    for (name in names(definition)) {
      # skip inner attribute
      if (name %in% c(
        "thrift_meta", 
        "thrift_file", 
        ".__enclos_env__",
        "clone",
        "add_public")) {
        next
      }
      attr <- definition[[name]]
      definition[[name]] <- fill_incomplete_ttype(definition, attr)
    }
  }
  # handle struct ttype
  else if (type == "environment" && 
           length(clazz) > 0 && 
           clazz[[1]] == "R6ClassGenerator" &&
           !is.null(definition$inherit) &&
           definition$inherit == "TPayload") {
    for (index in names(definition$thrift_spec)) {
      value <- definition$thrift_spec[[index]]
      # if the ttype of field is incomplete
      if (as.character(value[[1]]) %in% names(incomplete_type$dict)) {
        it <- incomplete_type$dict[[as.character(value[[1]])]]
        real_type <- fill_incomplete_ttype(tmodule, get_definition(tmodule, it[[1]], it[[2]]))
        # if deletion ttype is a compound type
        if (typeof(real_type) == "list") {
          definition$thrift_spec[[index]] <- list(real_type[[1]], value[[2]],real_type[[2]], value[[3]])
        }
        # if deletion ttype is a built-in ttype
        else {
          definition$thrift_spec[[index]] <- append(
            list(fill_incomplete_ttype(tmodule, get_definition(tmodule, it[[1]], it[[2]]))),
            head(value, 1))     
        }
      }
      # if the ttype which field's ttype contains is incomplete
      else if (typeof(value[[3]]) == "integer" && as.character(value[[3]]) %in% names(incomplete_type$dict)) {
        it <- incomplete_type$dict[[as.character(value[[3]])]]
        definition$thrift_spec[[index]] <- list(
          value[[1]],
          value[[2]],
          fill_incomplete_ttype(
            tmodule,
            get_definition(
              tmodule,
              it[[1]],
              it[[2]])
          ),
          value[[4]])
      }
    }
  }
  # handle service method
  else if (type == "environment" && 
           length(clazz) > 0 && 
           clazz[[1]] == "R6ClassGenerator" &&
           !(is.null(definition$thrift_services))) {
    for (name in names(definition)) {
      attr <- definition[[name]]
      if (typeof(attr) != "environment" || (typeof(attr) == "environment" && is.null(attr$thrift_spec))) {
        next
      }
      for (index in names(attr$thrift_spec)) {
        value <- attr$thrift_spec[[index]]
        attr$thrift_spec[[index]] <- fill_incomplete_ttype(tmodule, value)
      }
    }
  }
  return(definition)
}

get_definition <- function(thrift, name, lineno) {
  ref_type <- thrift
  for (n in strsplit(name, "\\.")[[1]]) {
    ref_type = thrift[[n]]
    if (is.null(ref_type)) {
      stop(sprintf('No type found: %s, at line %d', name, lineno))
    }
  
    if (typeof(ref_type) == "integer" && ref_type < 0) {
      stop(sprintf('No type found: %s, at line %d', incomplete_type$dict$get(ref_type), lineno))
    }

    if (!is.null(ref_type$ttype)) {
      return(list(ref_type$ttype, ref_type))
    } else {
      return(ref_type)
    }
  }
}


# lexer

LITERALS <- ":;,=*{}()<>[]"

THRIFT_RESERVED_KEYWORDS <- c(
  "BEGIN",
  "END",
  "__CLASS__",
  "__DIR__",
  "__FILE__",
  "__FUNCTION__",
  "__LINE__",
  "__METHOD__",
  "__NAMESPACE__",
  "abstract",
  "alias",
  "and",
  "args",
  "as",
  "assert",
  "begin",
  "break",
  "case",
  "catch",
  "class",
  "clone",
  "continue",
  "declare",
  "def",
  "default",
  "del",
  "delete",
  "do",
  "dynamic",
  "elif",
  "else",
  "elseif",
  "elsif",
  "end",
  "enddeclare",
  "endfor",
  "endforeach",
  "endif",
  "endswitch",
  "endwhile",
  "ensure",
  "except",
  "exec",
  "finally",
  "float",
  "for",
  "foreach",
  "function",
  "global",
  "goto",
  "if",
  "implements",
  "import",
  "in",
  "inline",
  "instanceof",
  "interface",
  "is",
  "lambda",
  "module",
  "native",
  "new",
  "next",
  "nil",
  "not",
  "or",
  "pass",
  "public",
  "print",
  "private",
  "protected",
  "public",
  "raise",
  "redo",
  "rescue",
  "retry",
  "register",
  "return",
  "self",
  "sizeof",
  "static",
  "super",
  "switch",
  "synchronized",
  "then",
  "this",
  "throw",
  "transient",
  "try",
  "undef",
  "union",
  "unless",
  "unsigned",
  "until",
  "use",
  "var",
  "virtual",
  "volatile",
  "when",
  "while",
  "with",
  "xor",
  "yield"
)

KEYWORDS <- c(
  "namespace",
  "include",
  "void",
  "bool",
  "byte",
  "i16",
  "i32",
  "i64",
  "double",
  "string",
  "binary",
  "map",
  "list",
  "set",
  "oneway",
  "typedef",
  "struct",
  "union",
  "exception",
  "extends",
  "throws",
  "service",
  "enum",
  "const",
  "required",
  "optional"
)

TOKENS <- c(c(
  "BOOLCONSTANT",
  "INTCONSTANT",
  "DUBCONSTANT",
  "LITERAL",
  "IDENTIFIER"),
  toupper(KEYWORDS)
)

Lexer <- R6::R6Class("Lexer",
  public = list(
    tokens = TOKENS,
    literals = LITERALS,
    t_ignore = " \t\r",
    t_error = function(t) {
      stop(sprintf("Illegal character %s at line %d", t$value[1], t$lineno))
    },
    t_newline = function(re="\\n+", t) {
      t$lexer$lineno <- t$lexer$lineno + nchar(t$value)
      return(NULL)
    },
    t_ignore_SILLYCOMM = function(re="\\/\\*\\**\\*\\/", t) {
      t$lexer$lineno <- t$lexer$lineno +
          lengths(regmatches(t$value, gregexpr("\n", t$value)))
      return(NULL)
    },
    t_ignore_MULTICOMM = function(
        re="\\/\\*[^*]\\/*([^*/]|[^*]\\/|\\*[^/])*\\**\\*\\/", t) {
      t$lexer$lineno <- t$lexer$lineno +
          lengths(regmatches(t$value, gregexpr("\n", t$value)))
      return(NULL)
    },
    t_ignore_DOCTEXT = function(
        re="\\/\\*\\*([^*/]|[^*]\\/|\\*[^/])*\\**\\*\\/", t) {
      t$lexer$lineno <- t$lexer$lineno +
          lengths(regmatches(t$value, gregexpr("\n", t$value)))
      return(NULL)
    },
    t_ignore_UNIXCOMMENT = function(re="\\#[^\\n]*", t) {
    },
    t_ignore_COMMENT = function(re="\\/\\/[^\\n]*", t) {
    },
    t_BOOLCONSTANT = function(re="\\btrue\\b|\\bfalse\\b", t) {
      t$value <- t$value == "true"
      return(t)
    },
    t_DUBCONSTANT = function(re="-?\\d+\\.\\d*(e-?\\d+)?", t) {
      t$value <- as.numeric(t$value)
      return(t)
    },
    t_HEXCONSTANT = function(re="0x[0-9A-Fa-f]+", t) {
      t$value <- strtoi(t$value)
      t$type <- "INTCONSTANT"
      return(t)
    },
    t_INTCONSTANT = function(re="[+-]?[0-9]+", t) {
      t$value <- strtoi(t$value)
      return(t)
    },
    t_LITERAL = function(
        re='(\\"([^\\\n]|(\\.))*?\")|\'([^\\\n]|(\\.))*?\'', t) {
      s <- substr(t$value, 2, nchar(t$value) - 1)
      maps <- new.env(hash = TRUE)
      maps[["t"]]  <- "\t"
      maps[["r"]]  <- "\r"
      maps[["n"]]  <- "\n"
      maps[["\\"]] <- "\\"
      maps[["\'"]] <- "\'"
      maps[['"']]  <- '\"'

      i <- 1
      length <- nchar(s)
      val <- ""
      while (i <= length) {
        if (substr(s, i, i) == "\\") {
          i <- i + 1
          if (substr(s, i, i) %in% maps) val <- val + maps[[substr(s, i, i)]]
          else {
            msg <- sprintf("Unexcepted escaping character: %s", substr(s, i, i))
            stop("[ThriftLexerError]", msg)
          }
        } else val <- paste(val, substr(s, i, i), sep = "")
        i <- i + 1
     }

      t$value <- val
      return(t)
    },
    t_IDENTIFIER = function(re="[a-zA-Z_](\\.[a-zA-Z_0-9]|[a-zA-Z_0-9])*", t) {
      if (t$value %in% KEYWORDS) {
        t$type <- toupper(t$value)
        return(t)
      }
      if (t$value %in% THRIFT_RESERVED_KEYWORDS)
        stop("[ThriftLexerError]", sprintf("Cannot use reserved language keyword: %s at line %d",
            t$value, t$lineno))
      return(t)
    }
  )
)


CurrentIncompleteType <- R6::R6Class("CurrentIncompleteType",
  public = list(
    dict = new.env(hash = TRUE),
    index = as.integer(-1),
    set_info = function(info) {
      self$dict[[as.character(self$index)]] <- info
      self$index <- self$index - 1
      return(as.integer(self$index + 1))
    }
  )
)


incomplete_type <- CurrentIncompleteType$new()


# parser

#' @importFrom utils tail
Parser <- R6::R6Class("Parser",
  public = list(
    thrift_stack = list(),
    include_dirs_ = list("."),
    thrift_cache = new.env(hash = TRUE),

    tokens = TOKENS,
    p_error = function(p) {
      if (is.null(p)) stop("[ThriftGrammerError]", "Grammar error at EOF")
      else stop("[ThriftGrammerError]", sprintf("Grammar error %s at '%s'", p$value, p$lineno))
    },
    p_start = function(doc="start : header definition", p) {
    },
    p_header = function(doc='header : header_unit_ header
                                    |', p) {
    },
    p_header_unit_ = function(doc="header_unit_ : header_unit ';'
                                                | header_unit", p) {
    },
    p_header_unit = function(doc='header_unit : include
                                              | namespace', p) {
    },
    p_include = function(doc="include : INCLUDE LITERAL", p) {
      thrift <- tail(Parser$thrift_stack, 1)[[1]]
      if (is.null(thrift$thrift_file))
        stop("[ThriftParserError]", "Unexcepted include statement while loading from file like object.")

      replace_include_dirs <- if (dirname(thrift$thrift_file) != ".")
                                append(Parser$include_dirs_, dirname(thrift$thrift_file), 0)
                              else Parser$include_dirs_
      for (include_dir in replace_include_dirs) {
        path <- file.path(include_dir, p$get(3))
        if (file.exists(path)) {
          child <- parse(path)
          thrift[[class(child)[[1]]]] <- child
          private$add_thrift_meta("includes", child)
          return()
        }
      }
      stop("[ThriftParserError]", sprintf("Couldn't include thrift %s in any directories provided", p$get(3)))
    },
    p_namespace = function(doc='namespace : NAMESPACE namespace_scope IDENTIFIER', p) {
     # namespace is useless in thriftpy
     # if p[2] == 'py' or p[2] == '*':
     #     setattr(thrift_stack[-1], '__name__', p[3])
    },
    p_namespace_scope = function(doc='namespace_scope : "*"
                                                      | IDENTIFIER', p) {
      p$set(1, p$get(2))
    },
    p_sep = function(doc='sep : ","
                              | ";"', p) {
    },
    p_definition = function(doc='definition : definition definition_unit_
                                            |', p) {
    },
    p_definition_unit_ = function(doc='definition_unit_ : definition_unit ";"
                                                        | definition_unit', p) {
    },
    p_definition_unit = function(doc='definition_unit : const
                                                      | ttype', p) {
    },
    p_const = function(
        doc='const : CONST field_type IDENTIFIER "=" const_value
                   | CONST field_type IDENTIFIER "=" const_value sep', p) {
      val <- tryCatch({
        self$cast(p$get(3), p$lineno(4))(p$get(6))
      }, error = function(e) {
        if (!grepl("\\[AssertionError\\]", e[[1]])) stop(e[[1]])
        stop("[ThriftParserError]", sprintf("Type error for constant %s at line %d", p$get(4), p$lineno(4)))
      })

      tail(Parser$thrift_stack, 1)[[1]]$add_public(p$get(4), val)
      private$add_thrift_meta('consts', val)
    },
    p_const_value = function(doc='const_value : INTCONSTANT
                                              | DUBCONSTANT
                                              | LITERAL
                                              | BOOLCONSTANT
                                              | const_list
                                              | const_map
                                              | const_ref', p) {
      p$set(1, p$get(2))
    },
    p_const_list = function(doc='const_list : "[" const_list_seq "]" ', p) {
      p$set(1, p$get(3))
    },
    p_const_list_seq = function(
        doc = "const_list_seq : const_value sep const_list_seq
                              | const_value const_list_seq
                              |", p) {
      private$parse_seq(p)
    },
    p_const_map = function(doc='const_map : "{" const_map_seq "}" ', p) {
      dict <- new.env(hash = TRUE)
      for (it in p$get(3)) {
        dict[[toString(it[[1]])]] <- it[[2]]
      }
      p$set(1, dict)
    },
    p_const_map_seq = function(
        doc = "const_map_seq : const_map_item sep const_map_seq
                             | const_map_item const_map_seq
                             |", p) {
      private$parse_seq(p)
    },
    p_const_map_item = function(
        doc = 'const_map_item : const_value ":" const_value ', p) {
      p$set(1, list(p$get(2), p$get(4)))
    },
    p_const_ref = function(doc = "const_ref : IDENTIFIER", p) {
      child <- tail(Parser$thrift_stack, 1)[[1]]
      for (name in strsplit(p$get(2), "\\.")[[1]]) {
        father <- child
        child <- child[[name]]
        if (is.null(child))
          stop("[ThriftParserError]",
              sprintf("Can\'t find name %s at line %d", p$get(2), p$lineno(2)))
      }

      father_type <- private$get_ttype(father)
      if (is.null(private$get_ttype(child)) ||
          (!is.null(father_type) && father_type == TType$I32)) {
        # child is a constant or enum value
        p$set(1, child)
      } else stop("[ThriftParserError]",
          sprintf("No enum value or constant found named %s", p$get(2)))
    },
    p_ttype = function(doc = "ttype : typedef
                                    | enum
                                    | struct
                                    | union
                                    | exception
                                    | service", p) {
    },
    p_typedef = function(doc = "typedef : TYPEDEF field_type IDENTIFIER", p) {
      tail(Parser$thrift_stack, 1)[[1]]$add_public(p$get(4), p$get(3))
    },
    p_enum = function(doc='enum : ENUM IDENTIFIER "{" enum_seq "}" ', p) {
      val <- private$make_enum(p$get(3), p$get(5))
      tail(Parser$thrift_stack, 1)[[1]]$add_public(p$get(3), val)
      private$add_thrift_meta('enums', val)
    },
    p_enum_seq = function(doc="enum_seq : enum_item sep enum_seq
                                        | enum_item enum_seq
                                        |", p) {
      private$parse_seq(p)
    },
    p_enum_item = function(doc='enum_item : IDENTIFIER "=" INTCONSTANT
                                          | IDENTIFIER
                                          |', p) {
           if (p$length() == 4) p$set(1, list(p$get(2), p$get(4)))
      else if (p$length() == 2) p$set(1, list(p$get(2), NULL))
    },
    p_struct = function(doc='struct : seen_struct "{" field_seq "}" ', p) {
      val <- private$fill_in_struct(p$get(2), p$get(4))
      private$add_thrift_meta("structs", val)
    },
    p_seen_struct = function(doc="seen_struct : STRUCT IDENTIFIER ", p) {
      val <- private$make_empty_struct(p$get(3))
      tail(Parser$thrift_stack, 1)[[1]]$add_public(p$get(3), val)
      p$set(1, val)
    },
    p_union = function(doc='union : seen_union "{" field_seq "}" ', p) {
      val <- private$fill_in_struct(p$get(2), p$get(4))
      private$add_thrift_meta("unions", val)
    },
    p_seen_union = function(doc="seen_union : UNION IDENTIFIER ", p) {
      val <- private$make_empty_struct(p$get(3))
      tail(Parser$thrift_stack, 1)[[1]]$add_public(p$get(3), val)
      p$set(1, val)
    },
    p_exception = function(doc='exception : EXCEPTION IDENTIFIER "{" field_seq "}" ', p) {
      val <- private$make_struct(p$get(3), p$get(5))
      tail(Parser$thrift_stack, 1)[[1]]$add_public(p$get(3), val)
      private$add_thrift_meta("exceptions", val)
    },
    p_service = function(doc='service : SERVICE IDENTIFIER "{" function_seq "}"
                                      | SERVICE IDENTIFIER EXTENDS IDENTIFIER "{" function_seq "}"', p) {
      thrift <- tail(Parser$thrift_stack, 1)[[1]]

      if (p$length() == 8) {
        extends <- thrift
        for (name in strsplit(p$get(5), "\\.")[[1]]) {
          extends <- extends[[name]]
          if (is.null(extends))
            stop("[ThriftParserError]", sprintf("Can't find service %s for service %s to extend",
                          p$get(5), p$get(3)))
        }

        if (is.null(extends[['thrift_services']]))
          stop(sprintf('Can\'t extends %s, not a service', p$get(5)))

      } else extends <- NULL

      val <- private$make_service(p$get(3), p$get(p$length() - 1), extends)
      thrift[[p$get(3)]] <- val
      private$add_thrift_meta('services', val)
    },
    p_function = function(
        doc='function : ONEWAY function_type IDENTIFIER "(" field_seq ")" throws
                      | ONEWAY function_type IDENTIFIER "(" field_seq ")"
                      | function_type IDENTIFIER "(" field_seq ")" throws
                      | function_type IDENTIFIER "(" field_seq ")" ', p) {
      oneway <- NA
      base   <- NA
      throws <- NA
      if (p$get(2)[[1]] == 'oneway') {
        oneway <- TRUE
        base   <- 2
      } else {
        oneway <- FALSE
        base   <- 1
      }

      if (p$get(p$length()) == ')') throws <- list()
      else                         throws <- p$get(p$length())

      p$set(1, list(oneway, p$get(base + 1), p$get(base + 2), p$get(base + 4), throws))
    },
    p_function_seq = function(doc='function_seq : function sep function_seq
                                                | function function_seq
                                                |', p) {
      private$parse_seq(p)
    },
    p_throws = function(doc='throws : THROWS "(" field_seq ")" ', p) {
      p$set(1, p$get(4))
    },
    p_function_type = function(doc='function_type : field_type
                                                  | VOID', p) {
      if(p$get(2)[[1]] == 'void') p$set(1, TType$VOID)
      else                        p$set(1, p$get(2))
    },
    p_field_seq = function(doc='field_seq : field sep field_seq
                                          | field field_seq
                                          |', p) {
      private$parse_seq(p)
    },
    p_field = function(doc='field : field_id field_req field_type IDENTIFIER
                                  | field_id field_req field_type IDENTIFIER "=" const_value', p) {
      val <- NA
      if (p$length() == 7) {
        val <- tryCatch({
          self$cast(p$get(4))(p$get(7))
        }, error = function(e) {
        })

        if (is.null(val))
          stop(sprintf('Type error for field %s at line %d', p$get(5), p$lineno(5)))
      }

      p$set(1, list(p$get(2), p$get(3), p$get(4), p$get(5), val))
    },
    p_field_id = function(doc='field_id : INTCONSTANT ":" ', p) {
      p$set(1, p$get(2))
    },
    p_field_req = function(doc='field_req : REQUIRED
                                          | OPTIONAL
                                          |', p) {
           if (p$length() == 2) p$set(1, p$get(2) == 'required')
      else if (p$length() == 1) p$set(1, FALSE)  # default: required=False
    },
    p_field_type = function(doc='field_type : ref_type
                                            | definition_type', p) {
      p$set(1, p$get(2))
    },
    p_ref_type = function(doc='ref_type : IDENTIFIER', p) {
      ref_type <- tail(Parser$thrift_stack, 1)[[1]]

      index <- 0
      for (name in strsplit(p$get(2), "\\.")[[1]]) {
        index <- index + 1
        ref_type <- ref_type[[name]]
        if (is.null(ref_type)) {
          if (index != (length(strsplit(p$get(2), "\\.")[[1]]))) {
            stop(sprintf('No type found: %s, at line %d', p$get(2), p$lineno(2)))
          }
          p$set(1, incomplete_type$set_info(list(p$get(2), p$lineno(2))))
          return()
        }
      }

      if (typeof(ref_type) == 'environment' &&
         !is.null(ref_type$ttype)) p$set(1, list(ref_type$ttype, ref_type))
      else                              p$set(1, ref_type)
    },
    p_base_type = function(doc='base_type : BOOL
                                          | BYTE
                                          | I16
                                          | I32
                                          | I64
                                          | DOUBLE
                                          | STRING
                                          | BINARY', p) {
      if (p$get(2) == 'bool')   p$set(1, TType$BOOL)
      if (p$get(2) == 'byte')   p$set(1, TType$BYTE)
      if (p$get(2) == 'i16')    p$set(1, TType$I16)
      if (p$get(2) == 'i32')    p$set(1, TType$I32)
      if (p$get(2) == 'i64')    p$set(1, TType$I64)
      if (p$get(2) == 'double') p$set(1, TType$DOUBLE)
      if (p$get(2) == 'string') p$set(1, TType$STRING)
      if (p$get(2) == 'binary') p$set(1, TType$BINARY)
    },
    p_container_type = function(doc='container_type : map_type
                                                    | list_type
                                                    | set_type', p) {
      p$set(1, p$get(2))
    },
    p_map_type = function(doc='map_type : MAP "<" field_type "," field_type ">" ', p) {
      p$set(1, list(TType$MAP, list(p$get(4), p$get(6))))
    },
    p_list_type = function(doc='list_type : LIST "<" field_type ">" ', p) {
      p$set(1, list(TType$LIST, p$get(4)))
    },
    p_set_type = function(doc='set_type : SET "<" field_type ">" ', p) {
      p$set(1, list(TType$SET, p$get(4)))
    },
    p_definition_type = function(doc='definition_type : base_type
                                                      | container_type', p) {
      p$set(1, p$get(2))
    },
    cast = function(t, linno=0) {
      if (is.integer(t) && t < 0) {
        return(private$lazy_cast_const(t, linno))
      } else if (typeof(t) != "list") {
        if (t == TType$BOOL)        return(private$cast_bool)
        if (t == TType$BYTE)        return(private$cast_byte)
        if (t == TType$I16)         return(private$cast_i16)
        if (t == TType$I32)         return(private$cast_i32)
        if (t == TType$I64)         return(private$cast_i64)
        if (t == TType$DOUBLE)      return(private$cast_double)
        if (t == TType$STRING)      return(private$cast_string)
        if (t == TType$BINARY)      return(private$cast_binary)
      } else {
        if (t[[1]] == TType$LIST)   return(private$cast_list(t))
        if (t[[1]] == TType$SET)    return(private$cast_set(t))
        if (t[[1]] == TType$MAP)    return(private$cast_map(t))
        if (t[[1]] == TType$I32)    return(private$cast_enum(t))
        if (t[[1]] == TType$STRUCT) return(private$cast_struct(t))
      }
    }
  ),
  private = list(
    add_thrift_meta = function(key, val) {
      thrift <- tail(Parser$thrift_stack, 1)[[1]]

      if (is.null(thrift$thrift_meta)) {
        thrift$add_public('thrift_meta', new.env(hash = TRUE))
      }
      if (is.null(thrift$thrift_meta[[key]])) {
        thrift$thrift_meta[[key]] <- list()
      }
      thrift$thrift_meta[[key]] <- append(thrift$thrift_meta[[key]], val)
    },
    parse_seq = function(p) {
           if (p$length() == 4) p$set(1, append(list(p$get(2)), p$get(4)))
      else if (p$length() == 3) p$set(1, append(list(p$get(2)), p$get(3)))
      else if (p$length() == 1) p$set(1, list())
    },
    lazy_cast_const = function(t, linno) {
      inner_cast <- function(v) {
        return(list('UNKNOWN_CONST', t, v, linno))
      }
      return(inner_cast)
    },
    cast_bool = function(v) {
      if (typeof(v) != "logical" && typeof(v) != "integer") stop("[AssertionError]")
      return(as.logical(v))
    },
    cast_byte = function(v) {
      # TODO
    },
    cast_i16 = function(v) {
      if (typeof(v) != "integer") stop("[AssertionError]")
      return(v)
    },
    cast_i32 = function(v) {
      if (typeof(v) != "integer") stop("[AssertionError]")
      return(v)
    },
    cast_i64 = function(v) {
      if (typeof(v) != "integer") stop("[AssertionError]")
      return(v)
    },
    cast_double = function(v) {
      if (typeof(v) != "double") stop("[AssertionError]")
      return(v)
    },
    cast_string = function(v) {
      if (typeof(v) != "character") stop("[AssertionError]")
      return(v)
    },
    cast_binary = function(v) {
      # TODO
    },
    cast_list = function(t) {
      if (t[[1]] != TType$LIST) stop("[AssertionError]")

      cast_list_ <- function(v) {
        if (typeof(v) != "list") stop("[AssertionError]")
        v <- lapply(v, self$cast(t[[2]]))
        return(v)
      }
      return(cast_list_)
    },
    cast_set = function(t) {
      if (t[[1]] != TType$SET) stop("[AssertionError]")

      cast_set_ <- function(v) {
        if (typeof(v) != "list") stop("[AssertionError]")
        v <- lapply(v, self$cast(t[[2]]))
        return(v)
      }
      return(cast_set_)
    },
    cast_map = function(t) {
      if (t[[1]] != TType$MAP) stop("[AssertionError]")

      cast_map_ <- function(v) {
        if (typeof(v) != "environment") stop("[AssertionError]")
        for (key in names(v)) {
          v[[toString(self$cast(t[[2]][[1]])(key))]] <-
              self$cast(t[[2]][[2]])(v[[key]])
        }
        return(v)
      }
      return(cast_map_)
    },
    cast_enum = function(t) {
      if (t[[1]] != TType$I32) stop("[AssertionError]")

      cast_enum_ <- function(v) {
        if (typeof(v) == "character") v <- strtoi(v)
        if (typeof(v) != "integer") stop("[AssertionError]")
        if (v %in% lapply(ls(t[[2]]), function(x) t[[2]][[x]]))
          return(v)
        stop("[ThriftParserError]",
            sprintf("Couldn't find a named value in enum %s for value %d",
                class(t[[2]])[[1]], v))
      }
      return(cast_enum_)
    },
    cast_struct = function(t) {   # struct/exception/union
      if (t[[1]] != TType$STRUCT) stop("[AssertionError]")

      cast_struct_ <- function(v) {
        if (class(v)[[1]] == t[[2]]$classname)
          return(v)  # already cast

        if (typeof(v) != "environment") stop("[AssertionError]")
        tspec <- t[[2]]$tspec

        for (key in names(tspec)) { # requirement check
          if (tspec[[key]][[1]] && !(key %in% names(v))) {
            stop("[ThriftParserError]",
                sprintf("Field %s was required to create constant for type %s",
                    key, t[[2]]$classname))
          }
        }

        # cast values
        for (key in names(v)) {
          if (!(key %in% names(tspec))) {
            stop("[ThriftParserError]",
                sprintf("No field named %s was found in struct of type %s",
                    key, t[[2]]$classname))
          }
          v[[key]] <- self$cast(tspec[[key]][[2]])(v[[key]])
        }
        args <- list()
        for (name in names(v)) {
          args[[name]] <- v[[name]]
        }
        return(do.call(t[[2]]$new, args))
      }
      return(cast_struct_)
    },
    make_enum = function(name, kvs) {
      cls <- R6::R6Class(
        name,
        inherit = TPayload,
        lock_objects = FALSE,
        public = list(
          module = tail(Parser$thrift_stack, 1)[[1]]$name,
          ttype = TType$I32
        )
      )
      cls$ttype <- TType$I32

      values_to_names <- new.env(hash = TRUE)
      names_to_values <- new.env(hash = TRUE)

      if (!is.null(kvs) && length(kvs) > 0) {
        val <- kvs[[1]][[2]]
        if (is.null(val)) val <- -1
        i <- 1
        for (item in kvs) {
          if (is.null(item[[2]])) {
            kvs[[i]][[2]] <- as.integer(val + 1)
          }
          val <- kvs[[i]][[2]]
          i <- i + 1
        }
        for (key_val in kvs) {
          key <- key_val[[1]]
          val <- key_val[[2]]
          cls$set("public", key, val)
          values_to_names[[as.character(val)]] <- key
          names_to_values[[key]] <- val
        }
      }
      cls$set("public", "VALUES_TO_NAMES", values_to_names)
      cls$set("public", "NAMES_TO_VALUES", names_to_values)
      return(cls$new())
    },
    make_empty_struct = function(name, ttype=TType$STRUCT) {
      cls <- R6::R6Class(
        name,
        inherit = TPayload,
        lock_objects = FALSE,
        public = list(
          module = class(tail(Parser$thrift_stack, 1)[[1]])[[1]]
        )
      )
      cls$ttype <- ttype
      return(cls)
    },
    fill_in_struct = function(cls, fields, gen_init=TRUE) {
      thrift_spec  <- new.env(hash = TRUE)
      default_spec <- list()
      tspec        <- new.env(hash = TRUE)

      for (field in fields) {
        if (as.character(field[[1]]) %in% names(thrift_spec) || field[[4]] %in% names(tspec))
          stop("[ThriftGrammerError]", sprintf("'%d:%s\' field identifier/name has already been used",
                       field[[1]], field[[4]]))
        ttype <- field[[3]]
        thrift_spec[[as.character(field[[1]])]] <- private$ttype_spec(ttype, field[[4]], field[[2]])
        default_spec[[length(default_spec)+1]] <- list(field[[4]], field[[5]])
        tspec[[field[[4]]]] <- list(field[[2]], ttype)
      }
      cls$set("public", 'thrift_spec', thrift_spec)
      cls$thrift_spec <- thrift_spec
      cls$set("public", 'default_spec', default_spec)
      cls$default_spec <- default_spec
      cls$set("public", 'tspec', tspec)
      cls$tspec <- tspec
      if (gen_init) gen_init(cls, thrift_spec, default_spec)
      return(cls)
    },
    make_struct = function(name, fields, ttype=TType$STRUCT, gen_init=TRUE) {
      cls <- private$make_empty_struct(name, ttype = ttype)
      return(private$fill_in_struct(cls, fields, gen_init = gen_init))
    },
    make_service = function(name, funcs, extends) {
      if (is.null(extends)) {
        extends <- R6::R6Class()
      }

      # print(class(tail(Parser$thrift_stack, 1)[[1]])[[1]])
      cls <- R6::R6Class(
        name,
      #   # inherit=extends,
        lock_objects = FALSE,
        public = list(
      #     module=class(tail(Parser$thrift_stack, 1)[[1]])[[1]],
        )
      )
      thrift_services <- list()

      for (func in funcs) {
        func_name <- func[[3]]
        # args payload cls
        args_name <- sprintf('%s_args', func_name)
        args_fields <- func[[4]]
        args_cls <- private$make_struct(args_name, args_fields)
        cls$set('public', args_name, args_cls)
        cls[[args_name]] <- args_cls
        # result payload cls
        result_name <- sprintf('%s_result', func_name)
        result_type <- func[[2]]
        result_throws <- func[[5]]
        result_oneway <- func[[1]]
        result_cls <- private$make_struct(result_name, result_throws, gen_init=FALSE)
        result_cls$set('public', 'oneway', result_oneway)
        if (typeof(result_type) == 'list' || result_type != TType$VOID) {
          result_cls$thrift_spec[['0']] <- private$ttype_spec(result_type, 'success')
          result_cls$default_spec <- append(list(list('success', NA)), result_cls$default_spec)
        }
        gen_init(result_cls, result_cls$thrift_spec, result_cls$default_spec)
        cls$set('public', result_name, result_cls)
        cls[[result_name]] <- result_cls
        thrift_services <- append(thrift_services, func_name)
      }
      if (!is.null(extends) && !is.null(extends$thrift_services)) {
         thrift_services <- append(thrift_services, extends$thrift_services)
      }
      cls$set('public', 'thrift_services', thrift_services)
      cls$thrift_services <- thrift_services
      return(cls)
    },
    ttype_spec = function(ttype, name, required=FALSE) {
      if (is.integer(ttype)) return(list(ttype, name, required))
      else                   return(list(ttype[[1]], name, ttype[[2]], required))
    },
    get_ttype = function(inst, default_ttype=NULL) {
      if (typeof(inst) == "environment" &&
          !is.null(inst$ttype)) return(inst$ttype)
      else                      return(default_ttype)
    }
  )
)
Parser$thrift_stack = list()
Parser$include_dirs_ = list(".")
Parser$thrift_cache = new.env(hash = TRUE)


#' Parse a single thrift file to R6 class instance
#'
#' @importFrom R6 R6Class
#' @importFrom rly lex
#' @importFrom rly yacc
#' @importFrom utils head
#'
#' @param path file path to parse, should be a string ending with '.thrift'
#' @param module_name the name for parsed module, the default is the basename
#'                    without extension of `path`
#' @param include_dirs directories to find thrift files while processing
#'                     the `include` directive, by default: ['.']
#' @param lexer rly lexer to use, if not provided, `parse` will use a new one
#' @param parser rly parser to use, if not provided, `parse` will use a new one
#' @param enable_cache if this is set to be `TRUE`, parsed module will be
#'                     cached, this is enabled by default. If `module_name`
#'                     is provided, use it as cache key, else use the `path`
#'
#' @return Thrift module
parse <- function(
    path,
    module_name=NA,
    include_dirs=NA,
    lexer=NA,
    parser=NA,
    enable_cache=TRUE) {

  # dead include checking on current stack
  for (thrift in Parser$thrift_stack) {
     if (!is.null(thrift$thrift_file) && path == thrift$thrift_file)
       stop("[ThriftParserError]", sprintf("Dead including on %s", path))
  }

  cache_key <- if (is.na(module_name)) path else module_name

  if (enable_cache && cache_key %in% names(Parser$thrift_cache))
    return(Parser$thrift_cache[[cache_key]])

  if (is.na(lexer)) lexer  <- rly::lex(Lexer)
  if (is.na(parser)) parser <- rly::yacc(Parser)

  if (!is.na(include_dirs)) Parser$include_dirs_ <- include_dirs

  if (!endsWith(path, ".thrift"))
    stop("[ThriftParserError]", "Path should end with .thrift")

  if (startsWith(path, "http://") || startsWith(path, "https://"))
    data <- url(path, "r+")
  else
    data <- readChar(path, file.info(path)$size)

  if (!is.na(module_name) && !endsWith(module_name, "_thrift"))
    stop("[ThriftParserError]", "ThriftPy can only generate module with '_thrift' suffix")

  if (is.na(module_name)) {
    module_name <- strsplit(basename(path), "\\.")[[1]]
  }

  thrift <- R6::R6Class(
    module_name,
    lock_objects = FALSE,
    public = list(thrift_file = path,
      add_public = function(name, obj) {
        self[[name]] <- obj
      }
    )
  )$new()
  Parser$thrift_stack <- append(Parser$thrift_stack, thrift)
  lexer$lineno <- 1
  parser$parse(data, lexer)
  Parser$thrift_stack <- head(Parser$thrift_stack, -1)

  if (enable_cache) Parser$thrift_cache[[cache_key]] <- thrift

  return(thrift)
}
