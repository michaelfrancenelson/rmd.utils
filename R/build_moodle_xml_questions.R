#' Wrapper for exams2moodle
#'
#'
#'
#' @export


build_moodle_xml_questions = function(
  question_source_files,
  question_number = NA,
  separate_output_files = FALSE,
  exam_mchoice = list(shuffle = TRUE),
  exam_schoice = list(shuffle = TRUE),
  xml_output_path = NULL,
  xml_output_filename = NULL
)
{

  if (FALSE)
  {
    load_all()
    require(mfn.teaching.utils)

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
    xml_output_path = NULL
    xml_output_filename = NULL

  }


  question_source_files

  question_basenames = tools::file_path_sans_ext(basename(question_source_files))
  question_basenames

 exams::exams2moodle(
    file = question_source_files,



  )

}
