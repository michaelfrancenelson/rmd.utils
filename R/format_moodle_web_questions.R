
#' Format moodle question source file
#'
#' Format moodle question source file for insertion in a html doc such as
#' an assignment file, answer key, etc.
#'
#' @param q_files a character vector of filenames of source files to be parsed and formatted
#' @param include_solution boolean: whether nor not to include the 'Solution' section of the source files
#' @param include_metadata boolean: whether or not to include the 'Meta-information' section of the source files.
#'
#' @inheritParams get_question_section
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

  # Test Case
  if (FALSE)
  {
    require(rmd.utils)
    # rm(list = ls())

    delim = "^={3,}\\s{0,}$"
    cloze_regex = "\\\\#\\\\#ANSWER[0-9]*\\\\#\\\\#"
    cloze_replacement = "________"
    rm_css_chunk_name = TRUE

    q_header = "Question\\s{0,}$"
    soln_header = "Solution\\s{0,}$"
    meta_header = "Meta-information\\s{0,}$"

    q_ext = ".Rmd"

    include_question = TRUE
    include_solution = TRUE
    include_solution = FALSE
    include_metadata = FALSE

    f_name = find_file("Q12", extension = ".Rmd")
    f_names = list.files(dirname(f_name), full.names = TRUE)
    # rm(f_name)
    q_path = file.path(
      dirname(find_file("week_03_reading_2021.Rmd", exact_match = TRUE, search_path = "/git")),
      "moodle_questions")


    f_names = find_file(
      "",
      search_path = q_path,
      extension = q_ext,
      return_all = TRUE)

    f_names = list.files(q_path, full.names = TRUE)

  }

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



#' #'
#' #'
#' #'
#' #'
#' #'
#'
#' add_moodle_quiz_questions = function(
#'   question_source_files,
#'   include_solution = FALSE,
#'   include_metadata = FALSE,
#'   question_number_fmt = "## Question %1$0.2d: %2$s",
#'   solution_section_fmt = "### Solution")
#' {
#'
#'   if (FALSE)
#'   {
#'     rm(list = ls())
#'     ass_path = find_file(
#'       "week_02_reading_2021.Rmd",
#'       exact_match = TRUE,
#'       search_path = "/git")
#'
#'     q_path = file.path(dirname(ass_path), "moodle_questions")
#'     question_source_files = list.files(q_path, full.names = TRUE)
#'
#'
#'
#'     tmp_dir = here::here()
#'     include_solution = FALSE
#'     include_metadata = FALSE
#'     include_solution = TRUE
#'     include_metadata = TRUE
#'     solution_section_fmt = "### Solution"
#'     tmp_prefix = NULL
#'     question_number_fmt = "## Question %1$0.2d: %2$s"
#'     add_moodle_quiz_questions(find_file("Q", search_path = find_file("lab_05", directory = TRUE), return_all = TRUE, extension = ".Rmd"))
#'   }
#'
#'
#'   out_body = combine_moodle_quiz_question_source(
#'     question_source_files,
#'     include_solution = include_solution,
#'     include_metadata = include_metadata,
#'     question_number_fmt = question_number_fmt,
#'     solution_section_fmt = solution_section_fmt)
#'
#'   temp = tempfile()
#'   writeLines(out_body, temp)
#'
#'   return(temp)
#' }
#'
#'
#'
#' format_moodle_question_source_ = function(
#'   lines_i,
#'   include_question = TRUE,
#'   include_solution = FALSE,
#'   # insert_line_breaks_in_metadata = TRUE,
#'   include_metadata = FALSE
#' )
#' {
#'
#'   if(FALSE)
#'   {
#'     filename = find_file("Q01", extension = ".Rmd")
#'     lines_i = readLines(filename)
#'     include_solution = FALSE
#'     include_metadata = TRUE
#'   }
#'
#'   if (include_question)
#'
#'     question_line_indices =
#'       get_question_section_line_indices(
#'         lines_i, section_name = "Question")
#'   metadata_line_indices =
#'     get_question_section_line_indices(
#'       lines_i, section_name = "Meta-information")
#'   solution_line_indices =
#'     get_question_section_line_indices(
#'       lines_i, section_name = "Solution")
#'
#'   question = ""
#'   solution = ""
#'   metadata = ""
#'
#'   if (include_metadata)
#'   {
#'     # Remove the ====== delimiter
#'     # metadata = lines_i[metadata_line_indices][-2]
#'     metadata = lines_i[metadata_line_indices]
#'     # metadata[1] = "### Meta-information"
#'
#'     #Insert line breaks into metadata section
#'     # metadata = insert_line_breaks(metadata)
#'   }
#'
#'   if (include_solution)
#'   {
#'     solution = lines_i[solution_line_indices]
#'   }
#'
#'   if (include_question)
#'   {
#'     question = lines_i[question_line_indices]
#'   }
#'
#'   lines_out = c(
#'     lines_i[-c(
#'       metadata_line_indices, solution_line_indices)],
#'     solution, metadata)
#'   return(lines_out)
#' }
#'
#'
#'
#' get_question_section_line_indices_ = function(
#'   rmd_source_lines,
#'   section_name = "Meta-information",
#'   section_delimiter = "=====")
#'   # ignore_error = FALSE,
#'   # invert_section = FALSE)
#' {
#'
#'   if (FALSE)
#'   {
#'     q_files = find_file("Q", search_path = find_file("lab_05", directory = TRUE), return_all = TRUE, extension = ".Rmd")
#'     rmd_source_lines = readLines(find_file("Q01", extension = ".Rmd"))
#'
#'     # rmd_source_lines = readLines(find_file("Q12", extension = ".Rmd"))
#'     # rmd_source_lines = file_lines
#'     section_name = "Meta-information"
#'     section_name = "Question"
#'     section_delimiter = "====="
#'   }
#'
#'   name_match = grep(section_name, rmd_source_lines)
#'   delim_matches = grep(section_delimiter, rmd_source_lines)
#'
#'   stopifnot(length(name_match) > 0)
#'   stopifnot(length(delim_matches) > 0)
#'
#'   name_match = name_match[name_match %in% (delim_matches - 1)]
#'   delim_index_start = which(delim_matches == name_match + 1)
#'
#'   # If it is the final section, cut everything after the section name and return lines
#'   if ((length(delim_matches) == delim_index_start))
#'   {
#'     line_indices = name_match:length(rmd_source_lines)
#'   } else
#'   {
#'     line_indices = name_match:(delim_matches[delim_index_start + 1] - 2)
#'   }
#'
#'   return(line_indices)
#' }
#'
#' if (FALSE)
#' {
#'   rmd_source_lines = readLines(find_file("bolker_questions_alternative_hypotheses.Rmd"), warn=FALSE)
#'   section_name = "Meta-information"
#'   section_name = "Solution"
#'   section_delimiter = "====="
#'   rmd_source_lines[get_question_section_line_indices(rmd_source_lines)]
#' }
#'
#'
#'
#' #'
#' #'
#' #' @export
#'
#' get_moodle_question_body_ = function(
#'   filename,
#'   file_lines = NULL,
#'   start_header = "Question",
#'   delimiter = "========",
#'   rm_css_chunk_name = TRUE,
#'   cloze_regex = "\\\\#\\\\#ANSWER[0-9]*\\\\#\\\\#",
#'   cloze_replacement = "________")
#' {
#'   if (FALSE)
#'   {
#'     # end_header = "Solution"
#'     # end_header = "Meta-Information"
#'
#'     rm(list = ls())
#'     start_header = "Question"
#'
#'     delimiter = "========"
#'     rm_css_chunk_name = TRUE
#'     cloze_regex = "\\\\#\\\\#ANSWER[0-9]*\\\\#\\\\#"
#'     cloze_replacement = "________"
#'
#'     file_lines = NULL
#'
#'     file_lines = readLines(find_file(
#'       "Q12", extension = ".Rmd"))
#'
#'     # filename = question_source_files$question_source_files[1]
#'   }
#'
#'   if (is.null(file_lines)) file_lines = readLines(filename)
#'
#'   # Find adjacent lines matching the `exams` package question and solution section delimiters
#'   delimiter_lines = which(grepl(delimiter, file_lines))
#'   question_lines = which(grepl(start_header, file_lines))
#'
#'   q_line = question_lines[question_lines %in% (delimiter_lines - 1)]
#'
#'   q_body = file_lines[-c(1:(q_line + 2))]
#'
#'   if(rm_css_chunk_name) q_body = gsub("r CSS", "r", q_body)
#'
#'   grep(pattern = cloze_regex, q_body)
#'
#'   q_body = gsub(pattern = cloze_regex, replacement = cloze_replacement, x = q_body)
#'
#'   if (length(q_line) != 1)
#'     cat(sprintf(
#'       "Could not locate the Moodle Question delimiter in file: %s",
#'       filename))
#'   return(q_body)
#'   # return(file_lines[(q_line + 2) : (s_line - 1)])
#' }
#'
#'
#' get_question_section_ = function(
#'   filename,
#'   file_lines = NULL,
#'   header = "Question",
#'   delim = "^={3,}\\s{0,}$",
#'   rm_css_chunk_name = TRUE)
#' {
#'   if (FALSE)
#'   {
#'     rm(list = ls())
#'     require(rmd.utils)
#'
#'     header = "Question"
#'     delim = "^={3,}\\s{0,}$"
#'     rm_css_chunk_name = TRUE
#'
#'     filename = find_file("Q01", extension = ".Rmd")
#'     file_lines = readLines(filename)
#'   }
#'
#'   if (is.null(file_lines)) file_lines = readLines(filename)
#'
#'   # Find adjacent lines matching the `exams` package question and solution section delimiters
#'   delim_lines = which(grepl(delimiter, file_lines))
#'   sec_lines = which(grepl(sec_header, file_lines))[1]
#'
#'   sec_lines %in% (delim_lines - 1)
#'
#'   sec_start = sec_lines[sec_lines %in% (delimiter_lines - 1)]
#'
#'   delimiter_lines==(sec_start_line + 1)
#'
#'   delimiter_end_index = which(delimiter_lines==(sec_start_line + 1)) + 1
#'
#'   if(length(delimiter_lines) < delimiter_end_index){
#'     sec_end_line = length(file_lines)
#'   } else sec_end_line = delimiter_lines[delimiter_end_index] - 2
#'
#'   sec_body = file_lines[sec_start_line:sec_end_line]
#'
#'   return(sec_body)
#'   # return(file_lines[(q_line + 2) : (s_line - 1)])
#' }
#'
#'
#'
#'
#' #' Format a moodle question source file
#' #'
#' #' Prepare the text of a moodle question source file by including or excluding
#' #' the "Solution" and "Meta-information" sections.
#' #'
#' #' @param lines_i a character vector containing the lines of a Rmd source file.
#' #' @param include_question boolean: whether nor not to include the 'Solution' section of the source files
#' #' @param include_solution boolean: whether nor not to include the 'Solution' section of the source files
#' #' @param include_metadata boolean: whether or not to include the 'Meta-information' section of the source files.
#' #' @param insert_line_breaks_in_metadata
#' #'
#' #' @export
#'
#'
#'
#'
#'
#'
#'
#' #' experimental
#' #'
#' #' @export
#'
#'
#'
#'
#' #' Combine questions from mutliple moodle quiz question source files
#' #'
#' #' @export
#'
#'
#'
#' #' Create an answer key for a moodle quiz-question based assignment.
#' #'
#' #' @export
#' #'
#' deprecated_build_assignment_doc = function(
#'   assignment_source_file,
#'   question_source_files,
#'   assignment_render_file = NULL,
#'   key_render_file = NULL,
#'   include_solution = FALSE,
#'   include_metadata = FALSE,
#'   include_metadata_key = FALSE,
#'   question_number_fmt = "## Question %1$0.2d: %2$s",
#'   solution_section_fmt = "### Solution")
#' {
#'
#'   if (FALSE)
#'   {
#'
#'     assignment_source_file = find_file("lab_05.Rmd", extension = ".Rmd")
#'     question_source_files =
#'       find_file("Q", search_path = find_file("lab_05", directory = TRUE), return_all = TRUE, extension = ".Rmd")
#'
#'     assignment_file = here::here("data", "lab_05")
#'     key_file = here::here("data", "lab_05_answer_key")
#'
#'     build_assignment_doc(
#'       assignment_source_file,
#'       question_source_files = question_source_files,
#'       assignment_render_file = NULL,
#'       # assignment_render_file = assignment_file,
#'       key_render_file = key_file)
#'   }
#'
#'
#'   if (!is.null(assignment_render_file))
#'   {
#'     assignment_body = readLines(assignment_source_file)
#'     questions_body = combine_moodle_quiz_question_source(
#'       question_source_files,
#'       include_solution = include_solution,
#'       include_metadata = include_metadata,
#'       question_number_fmt = question_number_fmt,
#'       solution_section_fmt = solution_section_fmt)
#'
#'     assign_source = c(
#'       assignment_body,
#'       questions_body)
#'
#'     temp = tempfile(fileext = ".Rmd")
#'     writeLines(assign_source, temp)
#'     rmarkdown::render(temp, output_file = assignment_render_file)
#'   }
#'
#'
#'   if (!is.null(key_render_file))
#'   {
#'     key_body = combine_moodle_quiz_question_source(
#'       question_source_files,
#'       include_solution = TRUE,
#'       include_metadata = include_metadata_key,
#'       question_number_fmt = question_number_fmt,
#'       solution_section_fmt = solution_section_fmt)
#'
#'     assignment_header = get_rmd_header(assignment_source_file)
#'     assignment_title = get_rmd_header_attr(NULL, assignment_header, header_prefix = "subtitle:")
#'     key_title = paste0('"', "Answer key - ", assignment_title, '"')
#'
#'     key_source = c(
#'       substitute_rmd_header_attr(assignment_header, "subtitle:", key_title),
#'       key_body)
#'
#'     temp = tempfile(fileext = ".Rmd")
#'     writeLines(key_source, temp)
#'     rmarkdown::render(temp, output_file = key_render_file)
#'   }
#'
#' }
#'
#'
#'
#'
#'
#'
#' #'
#' #'
#' #' @export
#'
#' deprecated_add_moodle_quiz_questions_ = function(
#'   question_source_files,
#'   tmp_dir = here::here(),
#'   tmp_prefix = NULL,
#'   include_solution = FALSE,
#'   include_metadata = FALSE,
#'   question_number_fmt = "## Question %1$0.2d: %2$s",
#'   solution_section_fmt = "### Solution")
#' {
#'
#'   if (FALSE)
#'   {
#'     tmp_dir = here::here()
#'     include_solution = FALSE
#'     include_metadata = FALSE
#'     include_solution = TRUE
#'     include_metadata = TRUE
#'     solution_section_fmt = "### Solution"
#'     tmp_prefix = NULL
#'     question_number_fmt = "## Question %1$0.2d: %2$s"
#'     add_moodle_quiz_questions(find_file("Q", search_path = find_file("lab_05", directory = TRUE), return_all = TRUE, extension = ".Rmd"))
#'   }
#'
#'   if (is.null(tmp_prefix))
#'   {
#'     tmp_fmt = file.path(tmp_dir, "moodle_quiz_q_%0.2d.Rmd")
#'   } else
#'   {
#'     tmp_fmt = file.path(tmp_dir, paste0(tmp_prefix, "_q_%0.2d.Rmd"))
#'   }
#'
#'   # Weed out any non .Rmd files
#'   question_source_files =
#'     question_source_files[grepl(".Rmd", question_source_files)]
#'
#'   # Strip out the solution and/or metadata sections as needed
#'   question_file_lines = format_moodle_web_questions(
#'     question_source_files, include_solution = include_solution,
#'     include_metadata = include_metadata)
#'
#'   n_q = length(question_file_lines)
#'
#'   out_body = c()
#'
#'   for (i in 1:n_q)
#'   {
#'     file_lines = question_file_lines[[i]]
#'
#'     # Harvest the title and make a title line to display in the assignment doc.
#'     q_title = get_rmd_header_attr(file_lines = file_lines, header_prefix = "title:")
#'     title_line = sprintf(question_number_fmt, i, q_title)
#'
#'     # Replace quesiton with the nicer question title
#'     q_title_lines = get_question_section_line_indices(file_lines, "Question")[1]
#'     file_lines[q_title_lines] = title_line
#'     file_lines[q_title_lines + 1] = ""
#'
#'     header_line_indices = get_rmd_header_indices(file_lines = file_lines)
#'
#'     if(include_solution)
#'     {
#'       solution_indices = get_question_section_line_indices(file_lines, "Solution")
#'       file_lines[solution_indices[1]] = solution_section_fmt
#'       file_lines[solution_indices[1] + 1] = ""
#'       file_lines[solution_indices]
#'     }
#'
#'     if (include_metadata)
#'     {
#'       metadata_indices = get_question_section_line_indices(file_lines, "Meta-information")
#'       metadata = file_lines[metadata_indices]
#'       metadata[1] = "### Meta-information"
#'       metadata[2] = ""
#'
#'       #Insert line breaks into metadata section
#'       metadata = paste0(metadata, "\n")
#'       file_lines[metadata_indices] = metadata
#'     }
#'
#'
#'     q_body = file_lines[-c(1:header_line_indices[2])]
#'
#'     # Remove duplicated CSS chunk names
#'     q_body = gsub("r CSS", "r", q_body)
#'
#'     out_body = c(
#'       out_body,
#'       "\n",
#'       q_body)
#'
#'   }
#'
#'
#'   writeLines(out_body, sprintf(tmp_fmt, 1))
#'
#'   return(sprintf(tmp_fmt, 1))
#'
#' }
#'
