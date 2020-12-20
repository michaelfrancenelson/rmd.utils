
#' Retrieve the contents of a source .Rmd file
#'
#' Checks to see if the file has already been read.
#' If not, returns a character vector of the source file lines.
#' If so, simply returns the already read source lines.
#'
#' @param f_name A character representing the path to a .Rmd source file.
#' @param f_lines A character vector with the lines of the .Rmd source file.
get_f_lines = function(f_name = NULL, f_lines = NULL)
{
  if (is.null(f_lines)) return(readLines(f_name))
  return(f_lines)
}

#' naaaaa na na na na na na "utility function!"
#'
#' @inheritParams get_f_lines
#'
get_next_index = function(start_indices, delim_indices, f_lines)
{
  stopifnot(length(start_indices) > 0)
  stopifnot(length(delim_indices) > 0)

  start_index = start_indices[1]

  end_indices = subset(
    delim_indices,
    (delim_indices - 1) > start_index) - 2

  if(length(end_indices) == 0)
    return(c(start_index, length(f_lines)))
  return(c(start_index, end_indices[1]))
}




#' Get the line indices for a section
#'
#' Finds the line indices of a section in an .Rmd file formatted
#' for use with the 'exams' package.
#' @inheritParams get_f_lines
#' @param header Name of section, e.g. "Question", "Solution", "Meta-information"
#' @param delim The section break delimiter to search for.  By default
#' searches for three or more consecutive equals symbols.
#' @param end_header Name of section, e.g. "Question", "Solution", "Meta-information".  If Null, the default, the function looks for the ending delimiter for the section specified by 'header'.
#'
#' @export
#'
# @param invert_section
# @param ignore_error

get_question_section_indices = function(
  f_name = NULL, f_lines = NULL,
  header = "Question\\s{0,}$",
  delim = "^={3,}\\s{0,}$",
  end_header = NULL)
{

  lines = get_f_lines(f_name, f_lines)
  # rm(f_name, f_lines)

  start_indices = which(grepl(header, lines))
  delim_indices = which(grepl(delim, lines))

  index_range = get_next_index(start_indices, delim_indices, lines)

  if (!is.null(end_header))
  {
    index_range[2] = get_next_index(
      which(grepl(end_header, lines)),
      delim_indices, lines)[2] - 1
  }
  return(index_range)
}







#' Retrieve lines from a source .Rmd file, formatted as a Moodle quiz question from package 'exams'
#'
#' @inheritParams get_question_section_indices
#' @export

get_question_section = function(
  f_name = NULL, f_lines = NULL,
  header = "Question\\s{0,}$",
  delim = "^={3,}\\s{0,}$",
  cloze_regex = "\\\\#\\\\#ANSWER[0-9]*\\\\#\\\\#",
  cloze_replacement = "________",
  rm_css_chunk_name = TRUE,
  end_header = NULL)
{
  if (FALSE)
  {
    get_question_section(f_lines = f_lines, end_header = end_header)
  }

  lines = get_f_lines(f_name, f_lines)

  sec_indices = get_question_section_indices(
    f_lines = lines,
    header = header,
    end_header = end_header,
    delim = delim)
  sec_body = f_lines[sec_indices[1]:sec_indices[2]]

  if (!is.null(cloze_regex))
  {
    sec_body = gsub(
      pattern = cloze_regex,
      replacement = cloze_replacement,
      x = sec_body)
  }
  return(sec_body)
}


get_post_header = function(
  f_name = NULL, f_lines = NULL,
  header = "Question\\s{0,}$",
  yaml_header_delim = "---"
)
{
  f_lines = get_f_lines(f_name, f_lines)
  yaml_lines = grep(pattern = yaml_header_delim, x = f_lines)
  h_line = grep(pattern = header, x = f_lines)[1] - 1

  return(f_lines[tail(yaml_lines, 1):h_line])
}


# ---- test_data ----
if (FALSE)
{
  require(rmd.utils)
  # rm(list = ls())

  header = "Question\\s{0,}$"
  # header = "Solution\\s{0,}$"
  # header = "Meta-information\\s{0,}$"

  # end_header = "Meta-information\\s{0,}$"
  # end_header = "Solution\\s{0,}$"
  end_header = NULL

  delim = "^={3,}\\s{0,}$"

  cloze_regex = "\\\\#\\\\#ANSWER[0-9]*\\\\#\\\\#"
  cloze_replacement = "________"

  rm_css_chunk_name = TRUE

  f_name = NULL

  f_name = find_file("Q12", extension = ".Rmd")
  f_lines = get_f_lines(f_name, NULL)

  get_question_section_indices(
    f_lines = f_lines, delim = delim,
    header = header, end_header = end_header)

  q_dir = dirname(f_name)
  f_names = list.files(q_dir, full.names = TRUE)

  format_moodle_web_questions(f_names = f_names)

}

