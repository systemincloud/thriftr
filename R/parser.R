# MIT License
#
# Copyright (c) 2016 System in Cloud - Marek Jagielski
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
thriftr_load <- function(path, module_name=NA, include_dirs=NA) {
  thrift <- thriftr_parse(path, module_name, include_dirs=include_dirs)
  return(thrift)
}

LITERALS = ':;,=*{}()<>[]'

THRIFT_RESERVED_KEYWORDS = c(
  'BEGIN',
  'END',
  '__CLASS__',
  '__DIR__',
  '__FILE__',
  '__FUNCTION__',
  '__LINE__',
  '__METHOD__',
  '__NAMESPACE__',
  'abstract',
  'alias',
  'and',
  'args',
  'as',
  'assert',
  'begin',
  'break',
  'case',
  'catch',
  'class',
  'clone',
  'continue',
  'declare',
  'def',
  'default',
  'del',
  'delete',
  'do',
  'dynamic',
  'elif',
  'else',
  'elseif',
  'elsif',
  'end',
  'enddeclare',
  'endfor',
  'endforeach',
  'endif',
  'endswitch',
  'endwhile',
  'ensure',
  'except',
  'exec',
  'finally',
  'float',
  'for',
  'foreach',
  'function',
  'global',
  'goto',
  'if',
  'implements',
  'import',
  'in',
  'inline',
  'instanceof',
  'interface',
  'is',
  'lambda',
  'module',
  'native',
  'new',
  'next',
  'nil',
  'not',
  'or',
  'pass',
  'public',
  'print',
  'private',
  'protected',
  'public',
  'raise',
  'redo',
  'rescue',
  'retry',
  'register',
  'return',
  'self',
  'sizeof',
  'static',
  'super',
  'switch',
  'synchronized',
  'then',
  'this',
  'throw',
  'transient',
  'try',
  'undef',
  'union',
  'unless',
  'unsigned',
  'until',
  'use',
  'var',
  'virtual',
  'volatile',
  'when',
  'while',
  'with',
  'xor',
  'yield'
)

KEYWORDS = c(
  'namespace',
  'include',
  'void',
  'bool',
  'byte',
  'i16',
  'i32',
  'i64',
  'double',
  'string',
  'binary',
  'map',
  'list',
  'set',
  'oneway',
  'typedef',
  'struct',
  'union',
  'exception',
  'extends',
  'throws',
  'service',
  'enum',
  'const',
  'required',
  'optional'
)

TOKENS = c(c(
  'BOOLCONSTANT',
  'INTCONSTANT',
  'DUBCONSTANT',
  'LITERAL',
  'IDENTIFIER'),
  toupper(KEYWORDS)
)

Lexer <- R6Class("Lexer",
  public = list(
    tokens = TOKENS,
    literals = LITERALS,
    t_ignore = " \t\r",
    t_error = function(t) {
      stop(sprintf('Illegal characher %s at line %d', t$value[1], t$lineno))
    },
    t_newline = function(re='\\n+', t) {
      t$lexer$lineno <- t$lexer$lineno + nchar(t$value)
      return(NULL)
    },
    t_ignore_SILLYCOMM = function(re='\\/\\*\\**\\*\\/', t) {
      t$lexer$lineno <- t$lexer$lineno + lengths(regmatches(t$value, gregexpr("\n", t$value)))
      return(NULL)
    },
    t_ignore_MULTICOMM = function(re='\\/\\*[^*]\\/*([^*/]|[^*]\\/|\\*[^/])*\\**\\*\\/', t) {
      t$lexer$lineno <- t$lexer$lineno + lengths(regmatches(t$value, gregexpr("\n", t$value)))
      return(NULL)
    },
    t_ignore_DOCTEXT = function(re='\\/\\*\\*([^*/]|[^*]\\/|\\*[^/])*\\**\\*\\/', t) {
      t$lexer$lineno <- t$lexer$lineno + lengths(regmatches(t$value, gregexpr("\n", t$value)))
      return(NULL)
    },
    t_ignore_UNIXCOMMENT = function(re='\\#[^\\n]*', t) {
    },
    t_ignore_COMMENT = function(re='\\/\\/[^\\n]*', t) {
    },
    t_BOOLCONSTANT = function(re='true|false', t) {
      t$value <- t$value == 'true'
      return(t)
    },
    t_DUBCONSTANT = function(re='-?\\d+\\.\\d*(e-?\\d+)?', t) {
      t$value <- as.numeric(t$value)
      return(t)
    },
    t_HEXCONSTANT = function(re='0x[0-9A-Fa-f]+', t) {
      t$value <- strtoi(t$value)
      t$type <- 'INTCONSTANT'
      return(t)
    },
    t_INTCONSTANT = function(re='[+-]?[0-9]+', t) {
      t$value <- strtoi(t$value)
      return(t)
    },
    t_LITERAL = function(re='(\\"([^\\\n]|(\\.))*?\")|\'([^\\\n]|(\\.))*?\'', t) {
      s <- tail(head(t, -1), -1)
      maps <- new.env(hash=TRUE)
      maps['t']  <- '\t'
      maps['r']  <- '\r'
      maps['n']  <- '\n'
      maps['\\'] <- '\\'
      maps['\''] <- '\''
      maps['"']  <- '\"'
      
      i <- 1
      length <- length(s)
      val <- ''
      while(i < length) {
        if(s[[i]] == '\\') {
          i <- i + 1
          if(s[[i]] %in% maps) val <- val + maps[[s[[i]]]]
          else {
            msg <- sprintf('Unexcepted escaping characher: %s', s[[i]])
            stop(msg)
          }
        } else val <- val + s[[i]]
        i <- i + 1
     }
    
      t$value <- val
      return(t)
    },
    t_IDENTIFIER = function(re='[a-zA-Z_](\\.[a-zA-Z_0-9]|[a-zA-Z_0-9])*', t) {
      if(t$value %in% KEYWORDS) {
        t$type <- toupper(t$value)
        return(t)
      }
      if(t$value %in% THRIFT_RESERVED_KEYWORDS)
        stop(sprintf('Cannot use reserved language keyword: %s at line %d', t$value, t$lineno))
      return(t)
    }
  )
)

#' @importFrom utils tail
Parser <- R6Class("Parser",
  public = list(
    thrift_stack = list(),
    
    tokens = TOKENS,   
    p_error = function(p) {
      if(is.null(p)) stop("Grammar error at EOF")
      else           stop(sprintf("Grammar error %s at '%s'", p$value, p$lineno))
    },
    p_start = function(doc='start : header definition', p) {
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
    p_include = function(doc='include : INCLUDE LITERAL', p) {
      # TODO
      print("p_include")
      thrift <- tail(Parser$thrift_stack, 1)[[1]]
      if(is.null(thrift$thrift_file__))
        stop('Unexcepted include statement while loading from file like object.')
      replace_include_dirs <- append(include_dirs_, thrift$thrift_file__)
      for(include_dir in replace_include_dirs) {
        if(dir.exists(file.path(include_dir, p[[3]]))) {
#          child <- parse(path)
#          setattr(thrift, child.__name__, child)
#          add_thrift_meta('includes', child)
          return()
        }
      }
      stop(sprintf('Couldn\'t include thrift %s in any directories provided', p[[3]]))
    },
    p_namespace = function(doc='namespace : NAMESPACE namespace_scope IDENTIFIER', p) {
      # TODO
      print("p_namespace")
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
    p_const = function(doc='const : CONST field_type IDENTIFIER "=" const_value
                                  | CONST field_type IDENTIFIER "=" const_value sep', p) {
      # TODO
      print("p_const")
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
    p_const_list_seq = function(doc='const_list_seq : const_value sep const_list_seq
                                                    | const_value const_list_seq
                                                    |', p) {
      private$parse_seq(p)
    },
    p_const_map = function(doc='const_map : "{" const_map_seq "}" ', p) {
      # TODO
      print("p_const_map")
    },
    p_const_map_seq = function(doc='const_map_seq : const_map_item sep const_map_seq
                                                  | const_map_item const_map_seq
                                                  |', p) {
      private$parse_seq(p)
    },
    p_const_map_item = function(doc='const_map_item : const_value ":" const_value ', p) {
      p$set(1, list(p$get(2), p$get(4)))
    },
    p_const_ref = function(doc='const_ref : IDENTIFIER', p) {
      # TODO
      print("p_const_ref")
    },
    p_ttype = function(doc='ttype : typedef
                                  | enum
                                  | struct
                                  | union
                                  | exception
                                  | service', p) {
    },
    p_typedef = function(doc='typedef : TYPEDEF field_type IDENTIFIER', p) {
      # TODO
      print("p_typedef")
    },
    p_enum = function(doc='enum : ENUM IDENTIFIER "{" enum_seq "}" ', p) {
      # TODO
      print("p_enum")
    },
    p_enum_seq = function(doc='enum_seq : enum_item sep enum_seq
                                        | enum_item enum_seq
                                        |', p) {
      private$parse_seq(p)
    },
    p_enum_item = function(doc='enum_item : IDENTIFIER "=" INTCONSTANT
                                          | IDENTIFIER
                                          |', p) {
           if(p$length() == 4) p$set(1, list(p$get(2), p$get(4)))
      else if(p$length() == 2) pset(1, list(p$get(2), NULL))
    },
    p_struct = function(doc='struct : seen_struct "{" field_seq "}" ', p) {
      val <- private$fill_in_struct(p$get(2), p$get(4))
      private$add_thrift_meta('structs', val)
    },
    p_seen_struct = function(doc='seen_struct : STRUCT IDENTIFIER ', p) {
      val <- private$make_empty_struct(p$get(3))
#      tail(Parser$thrift_stack, 1)[[1]]$set("public", p$get(3), val)
      tail(Parser$thrift_stack, 1)[[1]]$add_public(p$get(3), val)
      p$set(1, val)
    },
    p_union = function(doc='union : seen_union "{" field_seq "}" ', p) {
      # TODO
      print("p_union")
    },
    p_seen_union = function(doc='seen_union : UNION IDENTIFIER ', p) {
      # TODO
      print("p_seen_union")
    },
    p_exception = function(doc='exception : EXCEPTION IDENTIFIER "{" field_seq "}" ', p) {
      # TODO
      print("p_exception")
    },
    p_service = function(doc='service : SERVICE IDENTIFIER "{" function_seq "}"
                                      | SERVICE IDENTIFIER EXTENDS IDENTIFIER "{" function_seq "}"', p) {
      # TODO
      print("p_service")
    },
    p_function = function(doc='function : ONEWAY function_type IDENTIFIER "(" field_seq ")" throws
                                        | ONEWAY function_type IDENTIFIER "(" field_seq ")"
                                        | function_type IDENTIFIER "(" field_seq ")" throws
                                        | function_type IDENTIFIER "(" field_seq ")" ', p) {
      # TODO
      print("p_function")
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
      # TODO
      print("p_function_type")
    },
    p_field_seq = function(doc='field_seq : field sep field_seq
                                          | field field_seq
                                          |', p) {
      private$parse_seq(p)
    },
    p_field = function(doc='field : field_id field_req field_type IDENTIFIER
                                  | field_id field_req field_type IDENTIFIER "=" const_value', p) {
      val <- NA
      if(p$length() == 7) {
        val <- tryCatch({
          private$cast(p$get(4))(p$get(7))
        }, error = function(e) {
          stop(sprintf('Type error for field %s at line %d', p$get(5), p$lineno))
        })
      }
      
      p$set(1, list(p$get(2), p$get(3), p$get(4), p$get(5), val))
    },
    p_field_id = function(doc='field_id : INTCONSTANT ":" ', p) {
      p$set(1, p$get(2))
    },
    p_field_req = function(doc='field_req : REQUIRED
                                          | OPTIONAL
                                          |', p) {
           if(p$length() == 2) p$set(1, p$get(2) == 'required')
      else if(p$length() == 1) p$set(1, FALSE)  # default: required=False
    },
    p_field_type = function(doc='field_type : ref_type
                                            | definition_type', p) {
      p$set(1, p$get(2))
    },
    p_ref_type = function(doc='ref_type : IDENTIFIER', p) {
      # TODO
      print("p_ref_type")
    },
    p_base_type = function(doc='base_type : BOOL
                                          | BYTE
                                          | I16
                                          | I32
                                          | I64
                                          | DOUBLE
                                          | STRING
                                          | BINARY', p) {
      if(p$get(2) == 'bool')   p$set(1, TType$BOOL)
      if(p$get(2) == 'byte')   p$set(1, TType$BYTE)
      if(p$get(2) == 'i16')    p$set(1, TType$I16)
      if(p$get(2) == 'i32')    p$set(1, TType$I32)
      if(p$get(2) == 'i64')    p$set(1, TType$I64)
      if(p$get(2) == 'double') p$set(1, TType$DOUBLE)
      if(p$get(2) == 'string') p$set(1, TType$STRING)
      if(p$get(2) == 'binary') p$set(1, TType$BINARY)
    },
    p_container_type = function(doc='container_type : map_type
                                                    | list_type
                                                    | set_type', p) {
      p$set(1, p$get(2))
    },
    p_map_type = function(doc='map_type : MAP "<" field_type "," field_type ">" ', p) {
      # TODO
      print("p_map_type")
    },
    p_list_type = function(doc='list_type : LIST "<" field_type ">" ', p) {
      # TODO
      print("p_list_type")
    },
    p_set_type = function(doc='set_type : SET "<" field_type ">" ', p) {
      p$set(1, list(TType$SET, p$get(4)))
    },
    p_definition_type = function(doc='definition_type : base_type
                                                      | container_type', p) {
      p$set(1, p$get(2))
    }
  ),
  private = list(
    add_thrift_meta = function(key, value) {
      thrift <- tail(Parser$thrift_stack, 1)[[1]]
      # TODO
    },
    parse_seq = function(p) {
           if(p$length() == 4) p$set(1, append(list(p$get(2)), p$get(4)))
      else if(p$length() == 3) p$set(1, append(list(p$get(2)), p$get(3)))
      else if(p$length() == 1) p$set(1, list())
    },
    cast = function(t) {
      if(t == TType$BOOL)        return(private$cast_bool)
      if(t == TType$BYTE)        return(private$cast_byte)
      if(t == TType$I16)         return(private$cast_i16)
      if(t == TType$I32)         return(private$cast_i32)
      if(t == TType$I64)         return(private$cast_i64)
      if(t == TType$DOUBLE)      return(private$cast_double)
      if(t == TType$STRING)      return(private$cast_string)
      if(t == TType$BINARY)      return(private$cast_binary)
      if(t[[1]] == TType$LIST)   return(private$cast_list(t))
      if(t[[1]] == TType$SET)    return(private$cast_set(t))
      if(t[[1]] == TType$MAP)    return(private$cast_map(t))
      if(t[[1]] == TType$I32)    return(private$cast_enum(t))
      if(t[[1]] == TType$STRUCT) return(private$cast_struct(t))
    },
    cast_bool = function(v) {
      # TODO
    },
    cast_byte = function(v) {
      # TODO
    },
    cast_i16 = function(v) {
      # TODO
    },
    cast_i32 = function(v) {
      # TODO
    },
    cast_i64 = function(v) {
      # TODO
    },
    cast_double = function(v) {
      # TODO
    },
    cast_string = function(v) {
      if(typeof(v) != "character") stop()
      return(v)
    },
    cast_binary = function(v) {
      # TODO
    },
    cast_list = function(v) {
      # TODO
    },
    cast_set = function(v) {
      # TODO
    },
    cast_map = function(v) {
      # TODO
    },
    cast_enum = function(v) {
      # TODO
    },
    cast_struct = function(v) {
      # TODO
    },
    make_empty_struct = function(name, ttype=TType$STRUCT) {
      cls <- R6Class(name,
                     inherit=TPayload,
                     lock=FALSE, 
                     public=list(
                       module=tail(Parser$thrift_stack, 1)[[1]]$name,
                       ttype=ttype
                     ))$new()
      return(cls)
    },
    fill_in_struct = function(cls, fields, gen_init=TRUE) {
      thrift_spec  <- new.env()
      default_spec <- list()
      tspec        <- new.env()
      # TODO
      for(field in fields) {
        if(field[[1]] %in% names(thrift_spec) || field[[4]] %in% names(tspec))
          stop(sprintf('\'%d:%s\' field identifier/name has already been used',
                       field[[1]], field[[4]]))
        ttype <- field[[3]]
        thrift_spec[[as.character(field[[1]])]] <- private$ttype_spec(ttype, field[[4]], field[[2]])
        default_spec <- append(default_spec, list(field[[4]], field[[5]]))
        tspec[[field[[4]]]][[1]] <- field[[2]]
#        tspec[[field[[4]]]][[2]] <- ttype
      }
      cls$add_public('thrift_spec', thrift_spec)
      cls$add_public('default_spec', default_spec)
      cls$add_public('tspec', tspec)
#      if(gen_init) gen_init(cls, thrift_spec, default_spec)
      return(cls)
    },
    ttype_spec = function(ttype, name, required=FALSE) {
      if(is.integer(ttype)) return(list(ttype, name, required))
      else                  return(list(ttype[[1]], name, ttype[[2]], required))
    }
  )
)

include_dirs_ <- list('.')
thrift_cache  <- new.env(hash=TRUE)

#' Parse a single thrift file to R6 class instance
#' 
#' @importFrom R6 R6Class
#' @importFrom rly lex
#' @importFrom rly yacc
#' 
#' @param path file path to parse, should be a string ending with '.thrift'
#' @param module_name the name for parsed module, the default is the basename
#'                    without extension of `path`
#' @param include_dirs directories to find thrift files while processing
#'                     the `include` directive, by default: ['.']
#' @param lexer rly lexer to use, if not provided, `parse` will new one
#' @param parser rly parser to use, if not provided, `parse` will new one
#' @param enable_cache if this is set to be `TRUE`, parsed module will be
#'                     cached, this is enabled by default. If `module_name`
#'                     is provided, use it as cache key, else use the `path`
#' 
#' @return Thrift module
thriftr_parse = function(path, 
                         module_name=NA, 
                         include_dirs=NA, 
                         lexer=NA, 
                         parser=NA, 
                         enable_cache=TRUE) {
                       
  # dead include checking on current stack
  for(thrift in Parser$thrift_stack) {
    if(!is.null(thrift$thrift_file) && path == thrift$thrift_file)
      stop(sprintf('Dead including on %s', path))
  }
  
  cache_key <- if(is.na(module_name)) path else module_name
  
  if(enable_cache && cache_key %in% names(thrift_cache))
    return(thrift_cache[[cache_key]])
  
  if(is.na(lexer))  lexer  <- lex(Lexer)
  if(is.na(parser)) parser <- yacc(Parser)
  
  if(!is.na(include_dirs)) include_dirs_ <- include_dirs
  
  if(!endsWith(path, '.thrift'))
    stop('Path should end with .thrift')
  
  data <- readChar(path, file.info(path)$size)
  
  if(!is.na(module_name) && !endsWith(module_name, '_thrift'))
    stop('ThriftPy can only generate module with \'_thrift\' suffix')
  
  if(is.na(module_name)) {
    module_name <- strsplit(basename(path), "\\.")[[1]]
  }
  
  thrift <- R6Class(module_name, 
                    lock=FALSE, 
                    public=list(thrift_file=path,
                                add_public = function(name, obj) {
                                  self[[name]] <- obj
                                  environment(self[[name]]) <- environment(self$add_public)
                                }))$new()
  Parser$thrift_stack <- append(Parser$thrift_stack, thrift)
  parser$parse(data, lexer)
  Parser$thrift_stack <- tail(Parser$thrift_stack, 1)
  
  if(enable_cache) thrift_cache[[cache_key]] <- thrift
  
  return(thrift)
}