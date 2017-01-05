#' parser.R

#' Load thrift file as a module.
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