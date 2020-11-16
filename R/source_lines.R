#' Source specific lines in an R file
#'
#' @param file_lines character vector containing lines read from a source file
#' @param lines numeric vector of lines to source in \code{file}.

source_lines <- function(file_lines, lines){
  source(textConnection(file_lines[lines]))
}
