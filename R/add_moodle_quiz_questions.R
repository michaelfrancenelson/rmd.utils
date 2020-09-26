#'
#'
#' @export

add_moodle_quiz_questions = function(
  question_source_files,
  tmp_dir = here::here(),
  tmp_prefix = NULL,
  include_solution = FALSE,
  include_metadata = FALSE,
  question_number_fmt = "## Question %1$0.2d: %2$s",
  solution_section_fmt = "### Solution")
{

  if (FALSE)
  {
    tmp_dir = here::here()
    include_solution = FALSE
    include_metadata = FALSE
    include_solution = TRUE
    include_metadata = TRUE
    solution_section_fmt = "### Solution"
    tmp_prefix = NULL
    question_number_fmt = "## Question %1$0.2d: %2$s"
    add_moodle_quiz_questions(find_file("Q", search_path = find_file("lab_05", directory = TRUE), return_all = TRUE, extension = ".Rmd"))
  }

  if (is.null(tmp_prefix))
  {
    tmp_fmt = file.path(tmp_dir, "moodle_quiz_q_%0.2d.Rmd")
  } else
  {
    tmp_fmt = file.path(tmp_dir, paste0(tmp_prefix, "_q_%0.2d.Rmd"))
  }

  # Weed out any non .Rmd files
  question_source_files =
    question_source_files[grepl(".Rmd", question_source_files)]

  # Strip out the solution and/or metadata sections as needed
  question_file_lines = format_moodle_web_questions(
    question_source_files, include_solution = include_solution,
    include_metadata = include_metadata)

  n_q = length(question_file_lines)

  out_body = c()

  for (i in 1:n_q)
  {
    file_lines = question_file_lines[[i]]

    # Harvest the title and make a title line to display in the assignment doc.
    q_title = get_rmd_header_attr(file_lines = file_lines, header_prefix = "title:")
    title_line = sprintf(question_number_fmt, i, q_title)

    # Replace quesiton with the nicer question title
    q_title_lines = get_moodle_question_section_line_indices(file_lines, "Question")[1]
    file_lines[q_title_lines] = title_line
    file_lines[q_title_lines + 1] = ""

    header_line_indices = get_rmd_header_indices(file_lines = file_lines)

    if(include_solution)
    {
      solution_indices = get_moodle_question_section_line_indices(file_lines, "Solution")
      file_lines[solution_indices[1]] = solution_section_fmt
      file_lines[solution_indices[1] + 1] = ""
      file_lines[solution_indices]
    }

    if (include_metadata)
    {
      metadata_indices = get_moodle_question_section_line_indices(file_lines, "Meta-information")
      metadata = file_lines[metadata_indices]
      metadata[1] = "### Meta-information"
      metadata[2] = ""

      #Insert line breaks into metadata section
      metadata = paste0(metadata, "\n")
      file_lines[metadata_indices] = metadata
    }


    q_body = file_lines[-c(1:header_line_indices[2])]

    # Remove duplicated CSS chunk names
    q_body = gsub("r CSS", "r", q_body)

    out_body = c(
      out_body,
      "\n",
      q_body)

    # file_lines)

    # get_moodle_question_body(file_lines = question_file_lines[[i]]))
  }


  writeLines(out_body, sprintf(tmp_fmt, 1))

  # cat(out_body, sep = "\n")


  return(sprintf(tmp_fmt, 1))
  # cat(out_body[1:10], sep = "\n")
  #
  #
  #   tmp_files = sprintf(tmp_fmt, 1:n_q)
  #
  # child_chunk_fmt = "```{r child='%s', echo=FALSE, include=FALSE}\n```\n\n"
  # tmp_rm_chunk_fmt = "```{r echo=FALSE, results = 'hide'}\nfile.remove(\"%s\")\n```\n\n"
  #
  #
  #
  # for (i in 1:n_q)
  # {
  #   writeLines(question_file_lines[[i]], tmp_files[i])
  #   cat(sprintf(child_chunk_fmt, tmp_files[i]))
  #   cat(sprintf(tmp_rm_chunk_fmt, tmp_files[i]))
  # }

}


