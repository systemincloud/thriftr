#' lexer.R

ThriftLexer <- R6Class("ThriftLexer",
  public = list(
    literals = ':;,=*{}()<>[]',
    thrift_reserved_keywords = c(
    ),
    keywords = c('namespace',
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
    ),
    tokens = c(c('BOOLCONSTANT',
                 'INTCONSTANT',
                 'DUBCONSTANT',
                 'LITERAL',
                 'IDENTIFIER'), sapply(ThriftLexer$keywords, toupper)),
    t_ignore = ' \t\r',
    t_error = function(t) {
      stop(sprintf('Illegal characher %r at line %d', t.value[0], t.lineno))
    },
    t_newline = function(re = '\n+', t) {
      t$lexer$lineno = t$lexer$lineo + lenght(t.value)
    },
    t_ignore_SILLYCOMM = function(re = '\\/\\*\\**\\*\\/', t) {
      t.lexer.lineno = t.lexer.lineno + t.value.count('\n')
    },
    t_ignore_MULTICOMM = function(re = '\\/\\*[^*]\\/*([^*/]|[^*]\\/|\\*[^/])*\\**\\*\\/', t) {
      t.lexer.lineno = t.lexer.lineno + t.value.count('\n')
    },
    t_ignore_DOCTEXT = function(re = '\\/\\*\\*([^*/]|[^*]\\/|\\*[^/])*\\**\\*\\/', t) {
      t.lexer.lineno = t.lexer.lineno + t.value.count('\n')
    },
    t_ignore_UNIXCOMMENT = function(re = '\\#[^\n]*', t) { },
    t_ignore_COMMENT = function(re = '\\/\\/[^\n]*', t) { },
    t_BOOLCONSTANT = function(re = 'true|false', t) {
      t.value = t.value == 'true'
      return(t)
    },
    t_DUBCONSTANT = function(re = '-?\\d+\\.\\d*(e-?\\d+)?', t) {
      t.value = as.numeric(t.value)
      return(t)
    },
    t_HEXCONSTANT = function(re = '0x[0-9A-Fa-f]+', t) {
      t.value = strtoi(t.value, 16)
      t.type = 'INTCONSTANT'
      return(t)
    },
    t_INTCONSTANT = function(re = '[+-]?[0-9]+', t) {
      t.value = strtoi(t.value)
      return(t)
    },
    t_LITERAL = function(re = '(\"([^\\\n]|(\\.))*?\")|\'([^\\\n]|(\\.))*?\'', t) {
      s = t.value[1:-1]
      maps <- new.env(hash=TRUE,size=6)
      maps[['t']] <- '\t'
      maps[['r']] <- '\r'
      maps[['n']] <- '\n'
      maps[['\\']] <- '\\'
      maps[['\'']] <- '\''
      maps[['"']] <- '\"'
#      assign(x='r', value='\r', envir=maps)
#      assign(x='n', value='\n', envir=maps)
#      assign(x='\\', value='\\', envir=maps)
#      assign(x='\'', value='\'', envir=maps)
#      assign(x='"', value='\"', envir=maps)

      i = 0
      l = length(s)
      val = ''
      while(i < l) {
        if(s[i] == '\\') {
          i = i + 1
          if(s[i] %in% names(foo)) val = val + maps[s[i]]
          else stop(sprintf('Unexcepted escaping characher: %s', s[i]))
        } else {
          val = val + s[i]
        }
        i = i + 1
      }
      t.value = val
      return(t)
    },
    t_IDENTIFIER = function(re = '[a-zA-Z_](\\.[a-zA-Z_0-9]|[a-zA-Z_0-9])*', t) {
      if(t.value %in% keywords) {
        t.type = toupper(t.value)
        return(t)
      }
      if(t.value %in% thrift_reserved_keywords) {
        stop(sprintf('Cannot use reserved language keyword: %r at line %d', t.value, t.lineno))
      }
    }
  )
)
