#' parser_parser.R

#' Parse a single thrift file to module object.
#'
#' @param path file path to parse, should be a string ending with '.thrift'.
#' @param module_name the name for parsed module, the default is the basename without extension of `path`.
#' @param include_dirs directories to find thrift files while processing the `include` directive, by default: ['.'].
#' @param enable_cache if this is set to be `True`, parsed module will be cached,
#'        this is enabled by default. If `module_name` is provided, use it as cache key, else use the `path`.
parse = function(path, module_name=NA, include_dirs=NA, enable_cache=TRUE) {
  lexer = rly::lex(ThriftLexer$new())
  parser = rly::yacc(debug=FALSE, write_tables=0)

  if(!substring(path, nchar(path)) == ".thrift")
    stop('Path should end with .thrift')

  if(module_name != NA && !substring(module_name, nchar(module_name)) == '_thrift')
    stop('ThriftPy can only generate module with \'_thrift\' suffix')

  data = readChar(fileName, file.info(fileName)$size)

  if(module_name == NA) {
#    basename = os.path.basename(path)
#    module_name = os.path.splitext(basename)[0]
  }

  parser$parse(data)

  return(thrift)
}