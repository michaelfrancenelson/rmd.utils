#' Wrapper for exams2moodle
#'
#'
#'
#' @export
#'
#' @import here
#' @import exams
#'
#' @param question_source_files blah
#' @param assignment_name blah
#' @param xml_output_path blah
#' @param exam_mchoice list(shuffle = TRUE)
#' @param exam_schoice list(shuffle = TRUE)
#' @param xml_output_filename = NULL


build_moodle_xml_questions = function(
  question_source_files,
  assignment_name,
  xml_output_path,
  exam_mchoice = list(shuffle = TRUE),
  exam_schoice = list(shuffle = TRUE),
  xml_output_filename = NULL
)
{

  output_filename = ifelse(
    is.null(xml_output_filename),
    assignment_name,
    xml_output_filename
  )

 exams2moodle(
    file = question_source_files,
    name = output_filename,
    dir = xml_output_path,
    mchoice = exam_mchoice,
    schoice = exam_schoice,
    verbose = TRUE
  )

}
