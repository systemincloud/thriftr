#' parser.R

#' Load thrift file as a module.
#'
#' @param path R6 class containing lex rules
#' @param module_name list of arguments that should be passed to constructor
#' @param include_dirs on and off debug mode
#' 
#' @return Thrift module
#' 
#' @importFrom R6 R6Class
#' 
#' @export
load <- function(path, module_name=NA, include_dirs=NA) {
  thrift = parse(path, module_name, include_dirs=include_dirs)
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
      t$lexer$lineno <- t$lexer$lineno + lengths(regmatches(t$value, gregexpr("a", t$value)))
      return(NULL)
    },
    t_ignore_MULTICOMM = function(re='\\/\\*[^*]\\/*([^*/]|[^*]\\/|\\*[^/])*\\**\\*\\/', t) {
      t$lexer$lineno <- t$lexer$lineno + lengths(regmatches(t$value, gregexpr("a", t$value)))
      return(NULL)
    },
    t_ignore_DOCTEXT = function(re='\\/\\*\\*([^*/]|[^*]\\/|\\*[^/])*\\**\\*\\/', t) {
      t$lexer$lineno <- t$lexer$lineno + lengths(regmatches(t$value, gregexpr("a", t$value)))
      return(NULL)
    },
    t_ignore_UNIXCOMMENT = '\\#[^\\n]*',
    t_ignore_COMMENT = '\\/\\/[^\\n]*',
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
      if(t$value %in% keywords) {
        t$type <- toupper(t$value)
        return(t)
      }
      if(t$value %in% thrift_reserved_keywords)
        stop(sprintf('Cannot use reserved language keyword: %s at line %d', t$value, t$lineno))
      return(t)
    }
  )
)

Parser <- R6Class("Parser",
  public = list(
    thrift_stack  = list(),
    include_dirs_ = list('.'),
    thrift_cache  = new.env(hash=TRUE),

    tokens   = TOKENS,
    literals = LITERALS,
    
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
      thrift <- tail(thrift_stack, 1)[[1]]
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
    }
  )
)


parse = function() {
  
}