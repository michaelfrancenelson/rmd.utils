
#' Format moodle question source file
#'
#' Format moodle question source file for insertion in a html doc such as
#' an assignment file, answer key, etc.
#'
#' @param include_solution boolean: whether nor not to include the 'Solution' section of the source files
#' @param include_metadata boolean: whether or not to include the 'Meta-information' section of the source files.
#' @param f_names a
#' @param q_header a
#' @param delim a
#' @param cloze_regex blah
#' @param cloze_replacement = "________",
#' @param rm_css_chunk_name = TRUE,
#' @param include_question  = TRUE,
#' @param include_solution  = FALSE,
#' @param include_metadata  = FALSE,
#' @param soln_header blah
#' @param meta_header blah
#' @param q_ext blah
#'
#' @return source code lines
#' @export
#'

format_moodle_web_questions = function(
  f_names,
  q_header          = "Question\\s{0,}$",
  delim             = "^={3,}\\s{0,}$",
  cloze_regex       = "\\\\#\\\\#ANSWER[0-9]*\\\\#\\\\#",
  cloze_replacement = "________",
  rm_css_chunk_name = TRUE,
  include_question  = TRUE,
  include_solution  = FALSE,
  include_metadata  = FALSE,
  soln_header       = "Solution\\s{0,}$",
  meta_header       = "Meta-information\\s{0,}$",
  q_ext = ".Rmd")
{


  f_names = f_names[grep(q_ext, x = f_names)]

  # Read the content of all of the question files
  # options(warn = -1)
  source_lines = lapply(f_names, readLines)

  prep_sections = q_sections = soln_sections = mdat_sections = rep(list(""), length(f_names))

  f_names
  get_post_header(f_names[[2]])
  prep_sections = lapply(f_names, function(x) get_post_header(x))

  if (include_question)
  {
    q_sections = lapply(source_lines, function(x)
      get_sec(headr = q_header, x))
  }
  if (include_solution)
  {
    soln_sections = lapply(source_lines, function(x)
      get_sec(headr = soln_header, x))
  }
  if (include_metadata)
  {
    mdat_sections = lapply(source_lines, function(x)
      get_sec(headr = meta_header, x))
  }

  text_out = unlist(sapply(
    1:length(f_names),
    function(i) c(
      prep_sections[[i]], q_sections[[i]],
      soln_sections[[i]], mdat_sections[[i]])))

  return(text_out)
}



#' get a section from a rmd file
#'
#' @param headr name of the header
#' @param lines how many lines to get


get_sec = function(headr, lines)
{
  return(
    get_question_section(
      f_name = NULL,
      f_lines = lines,
      header = headr,
      delim = delim,
      cloze_regex = cloze_regex,
      cloze_replacement = cloze_replacement,
      rm_css_chunk_name = rm_css_chunk_name,
      end_header = NULL)
  )
}


