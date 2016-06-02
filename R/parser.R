#' parser.R

#' Load thrift file as a module.
#'
#' @export
load <- function(path, module_name=NA, include_dirs=NA) {
  thrift = parse(path, module_name, include_dirs=include_dirs)
  return(thrift)
}
