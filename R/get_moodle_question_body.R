#'
#'
#' @export



get_moodle_question_body = function(
  filename,
  file_lines = NULL,
  start_header = "Question",
  delimiter = "========",
  rm_css_chunk_name = TRUE)
{
  if (FALSE)
  {
    start_header = "Question"
    delimiter = "========"
    end_header = "Solution"

    start_header = "Solution"
    end_header = "Meta-Information"

    file_lines = NULL

    question_source_files
    filename = question_source_files$question_source_files[1]
  }

  if (is.null(file_lines)) file_lines = readLines(filename)

  # Find adjacent lines matching the `exams` package question and solution section delimiters
  delimiter_lines = which(grepl(delimiter, file_lines))
  question_lines = which(grepl(start_header, file_lines))

  q_line = question_lines[question_lines %in% (delimiter_lines - 1)]

  q_body = file_lines[-c(1:(q_line + 2))]

  if(rm_css_chunk_name) q_body = gsub("r CSS", "r", q_body)

  if (length(q_line) != 1)
    cat(sprintf(
      "Could not locate the Moodle Question delimiter in file: %s",
      filename))
  return(q_body)
  # return(file_lines[(q_line + 2) : (s_line - 1)])
}
