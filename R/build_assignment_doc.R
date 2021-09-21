#' Create an answer key for a moodle quiz-question based assignment.
#'
#' @export
#'
#' @import here
#' @import rmarkdown
#'
#' @param doc_source_file a
#' @param question_source_files aaa
#' @param doc_target_file blah
#' @param key_target_file blah
#' @param include_solution blah
#' @param include_metadata blah
#' @param include_metadata_key  blah
#' @param question_number_fmt blah
#' @param solution_section_fmt blah


build_assignment_doc = function(
  doc_source_file,
  question_source_files,
  doc_target_file = NULL,
  key_target_file = NULL,
  include_solution = FALSE,
  include_metadata = FALSE,
  include_metadata_key = FALSE,
  question_number_fmt = "## Question %1$0.2d: %2$s",
  solution_section_fmt = "### Solution")
{

  if (FALSE)
  {

    a_f = gather_assignment_paths(7, "lab", here("docs", "assignments", "eco_634"))

    doc_source_file = a_f$doc_source_file
    question_source_files = a_f$question_source_files
    doc_target_file = a_f$doc_target_file
    key_target_file = a_f$key_target_file

    include_solution = FALSE
    include_metadata = FALSE
    include_metadata_key = FALSE

    question_number_fmt = "## Question %1$0.2d: %2$s"
    solution_section_fmt = "### Solution"

    build_assignment_doc(
      doc_source_file = a_f$doc_source_file,
      question_source_files = a_f$question_source_files,
      doc_target_file = a_f$doc_target_file,
      key_target_file = a_f$key_target_file
    )
  }


  if (!is.null(doc_target_file))
  {
    assignment_body = readLines(doc_source_file)
    questions_body = combine_moodle_quiz_question_source(
      question_source_files,
      include_solution = include_solution,
      include_metadata = include_metadata,
      question_number_fmt = question_number_fmt,
      solution_section_fmt = solution_section_fmt)

    assign_source = c(
      assignment_body,
      questions_body)

    temp = tempfile(fileext = ".Rmd")
    writeLines(assign_source, temp)
    render(temp, output_file = doc_target_file, envir = new.env())
  }

  if (!is.null(key_target_file))
    build_answer_key(
      doc_source_file = doc_source_file,
      question_source_files = question_source_files,
      key_target_file = key_target_file,
      include_metadata_key = include_metadata_key)
}


#' Create an answer key for a moodle quiz-question based assignment.
#'
#'
#' @inheritParams build_assignment_doc
#'
#' @param key_target_file blah
#'
#' @export


build_answer_key = function(
  doc_source_file,
  question_source_files,
  key_target_file = NULL,
  include_solution = FALSE,
  # include_metadata = FALSE,
  include_metadata_key = FALSE,
  question_number_fmt = "## Question %1$0.2d: %2$s",
  solution_section_fmt = "### Solution")
{
  key_body =
    combine_moodle_quiz_question_source(
      question_source_files,
      include_solution = TRUE,
      include_metadata = include_metadata_key,
      question_number_fmt = question_number_fmt,
      solution_section_fmt = solution_section_fmt)

  assignment_header = get_rmd_header(doc_source_file)
  assignment_title = get_rmd_header_attr(
    NULL,
    assignment_header,
    header_prefix = "subtitle:")

  key_title = paste0('"', "Answer key - ", assignment_title, '"')

  key_source = c(
    substitute_rmd_header_attr(
      assignment_header,
      "subtitle:",
      key_title),
    key_body)

  temp = tempfile(fileext = ".Rmd")
  writeLines(key_source, temp)
  render(temp, output_file = key_target_file)
}
