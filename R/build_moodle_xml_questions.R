#' Wrapper for exams2moodle
#'
#'
#'
#' @export


build_moodle_xml_questions = function(
  question_source_files,
  assignment_name,
  xml_output_path,
  exam_mchoice = list(shuffle = TRUE),
  exam_schoice = list(shuffle = TRUE),
  xml_output_filename = NULL
)
{

  if (FALSE)
  {
    load_all()
    require(mfn.teaching.utils)

    assignment_name = "test_assignment"
    xml_output_path = here::here("data")

    question_source_files =
      find_file(
        "Q",
        search_path = here::here("data"),
        extension = ".Rmd", return_all = TRUE)

    question_number = NA
    separate_output_files = FALSE
    exam_mchoice = list(shuffle = TRUE)
    exam_schoice = list(shuffle = TRUE)
    exam_verbose = TRUE
    xml_output_filename = NULL
  }

  output_filename = ifelse(
    is.null(xml_output_filename),
    assignment_name,
    xml_output_filename
  )

 exams::exams2moodle(
    file = question_source_files,
    name = output_filename,
    dir = xml_output_path,
    mchoice = exam_mchoice,
    schoice = exam_schoice,
    verbose = TRUE
  )

}
