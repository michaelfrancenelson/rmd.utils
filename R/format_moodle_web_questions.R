#' Format moodle question source file
#'
#' Format moodle question source file for insertion in a html doc such as
#' an assignment file, answer key, etc.
#'
#' @param question_source_files a character vector of filenames of source files to be parsed and formatted
#' @param include_solution boolean: whether nor not to include the 'Solution' section of the source files
#' @param include_metadata boolean: whether or not to include the 'Meta-information' section of the source files.
#'
#' @return source code lines
#'


format_moodle_web_questions = function(
  question_source_files,
  include_solution = FALSE,
  include_metadata = FALSE)
{
  # Read the content of all of the question files
  # options(warn = -1)
  source_lines = lapply(question_source_files, readLines)

  source_lines_formatted =
    lapply(source_lines, function(x)
      format_moodle_question_source(
        x,
        include_solution = include_solution,
        include_metadata = include_metadata)
    )

    return(source_lines_formatted)
}
