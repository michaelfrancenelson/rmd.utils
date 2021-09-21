#' Get markup for a question?
#'
#' @param q_files blah
#' @param q_fmt blah
#' @param q_header blah
#' @param delim blah
#' @param a_list_fmt blah
#'
#' @export

get_question_markup = function(
  q_files,
  q_fmt = "## Question %s",
  q_header = "Question\\s{0,}$",
  delim = "^={3,}\\s{0,}$",
  a_list_fmt = "answerlist")
{
  q_text = unlist(
    lapply(
      q_files,
      function(x) get_question_section(
        f_lines = readLines(x))))

  grep(pattern = delim, x = q_text)

  q_text = gsub(
    pattern = delim, replacement = "", x = q_text)

  q_lines = grep(q_header, x = q_text)

  q_text[q_lines] = sprintf(q_fmt, 1:length(q_files))

  a_list_lines = grep(
    pattern = a_list_fmt, x = q_text)

  if (length(a_list_lines) > 0)
    q_text = q_text[-a_list_lines]

  return(trimws(q_text))
}


