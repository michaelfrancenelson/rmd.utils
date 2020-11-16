get_rmd_chunks = function(filename, file_lines = NULL)
{
  if (is.null(file_lines)) file_lines = readLines(filename)
  chunk_indices = chunk_indices = get_rmd_chunk_indices(file_lines = file_lines)

  chunk_indices
  chunk_list = apply(chunk_indices, 1, function(x) file_lines[x[1]:x[2]])


  names(chunk_list) = chunk_indices[, 3]

  return(chunk_list)
}

#' Get the yaml header from an Rmd file
#'
#' @param filename the name of the Rmd file to read
#' @param file_lines a character vector containing the lines of a source Rmd file.
#'
#' @export

get_rmd_chunk_indices = function(filename, file_lines = NULL)
{
  if (is.null(file_lines)) file_lines = readLines(filename)

  start_indices = which(grepl("```\\{r", file_lines))
  out = data.frame()

  for (i in 1:length(start_indices))
  {
    found = FALSE
    j = start_indices[i] + 1
    while (!found)
    {
      file_lines[j]

      if (grepl("```", file_lines[j]))
      {
        found = TRUE
        end_index = j - 1
      }
      j = j + 1
    }

    start_line = file_lines[start_indices[i]]
    reg_match = regmatches(start_line, regexpr("r\\s.+?[,}=<>]", file_lines[start_indices[i]]))

    # reg_match = regmatches(start_line, regexpr("r .+,", file_lines[start_indices[i]]))
    # reg_match = regmatches(start_line, regexpr("r .+?[\\s,}]", file_lines[start_indices[i]]))
    # reg_match = regmatches(start_line, regexpr("r .+[,}\\s]", file_lines[start_indices[i]]))
    # regmatches(start_line, regexpr("r .+[,,}\\s]", file_lines[start_indices[i]]))
    # regmatches(start_line, regexpr("r .+,", file_lines[start_indices[i]]))
    #
    # regmatches(start_line, regexpr("r .+", file_lines[start_indices[i]]))
    # regmatches(start_line, regexpr("r .+", file_lines[start_indices[i]]))
    # regmatches(start_line, regexpr("r .+,", file_lines[start_indices[i]]))


    # chunk_name = trimws(gsub(",", "", gsub("r ", "", reg_match)))
    chunk_name =
      # trimws(gsub("r ", "", gsub("[\\s,}]", "", reg_match)))
      trimws(gsub("r ", "", gsub("[,}=<>]", "", reg_match)))


    if(length(chunk_name) == 0) chunk_name = "" else


      chunk_name

    out_row = data.frame(
      start_index = start_indices[i] + 1,
      end_index = end_index,
      chunk_name = chunk_name)

    out = rbind(
      out,
      data.frame(
        start_index = start_indices[i] + 1,
        end_index = end_index,
        chunk_name = chunk_name))

  }
  return(out)
}



if (FALSE)
{
  rm(list = ls())
  require(rmd.utils)
  fn = find_file("lab_05.Rmd", exact_match = TRUE)
  file_lines = readLines(fn)
  chunk_indices = get_rmd_chunk_indices(file_lines = file_lines)

  chunk_indices

  i = 1


  chunk_text_1 = file_lines[chunk_indices[i, 1]-1]

  chunk_text_1

  regmatches(chunk_text_1, regexpr("r .+?[\\s,}]", chunk_text_1))


  match_1 = regmatches(chunk_text_1, regexpr("r .+,", chunk_text_1))
  match_1 = regmatches(chunk_text_1, regexpr("r .+\\s", chunk_text_1))
  regmatches(match_1, regexpr("r .+[,]", match_1))

  regmatches(chunk_text_1, regexpr("r .+/[,\\s]", chunk_text_1))
  regmatches(chunk_text_1, regexpr("r .+[\\s,}]", chunk_text_1))
  regmatches(chunk_text_1, regexpr("r .+ ", chunk_text_1))

  chunk_text_i = file_lines[chunk_indices[i, 1]:chunk_indices[i, 2]]
  evaluate::evaluate(textConnection(chunk_text_i), stop_on_error = 0)
}

#'
#'
#' @export

get_rmd_header_attr = function(
  filename,
  file_lines = NULL,
  header_prefix = "title:",
  yaml_header_delimiter = "----")
{

  if (FALSE)
  {
    file_lines = NULL
    filename = "C:/Users/michaelnelso/git/eco_602_634_2020/assignments/individual_assignments/week_03_data_exploration_deterministic_functions/moodle/Q1_histograms_elevation.Rmd"
    file_lines = readLines(filename, warn = FALSE)
    title_prefix = "title:"
  }

  # get_rmd_header = function(filename, file_lines = NULL)
  # {
  #   if (is.null(file_lines)) file_lines = readLines(filename)
  #   header_symbols = which(grepl("---", file_lines))
  #   return(file_lines[header_symbols[1]:header_symbols[2]])
  # }
  if (is.null(file_lines)) file_lines = readLines(filename, warn = FALSE)

  header_lines = get_rmd_header(NULL, file_lines = file_lines)
  header_line = header_lines[grepl(header_prefix, header_lines)]
  header_attr = gsub("\"", "", trimws(gsub(header_prefix, "", header_line)))
  return(header_attr)
}



#' Get the line indices of the header in an Rmd file
#'
#' @param filename the name of the Rmd file to read
#' @param file_lines a character vector containing the lines of a source Rmd file.
#' @param header_delimiter the delimiter for the beginning and ending of the Rmd header section.
#'
#' @export

get_rmd_header_indices = function(filename, file_lines = NULL, header_delimiter = "---")
{
  if (is.null(file_lines)) file_lines = readLines(filename)
  return(which(grepl(header_delimiter, file_lines))[1:2])
}




#' Swap the value of an Rmd header attribute
#'
#' @param header_lines A character vector with the lines of the header
#' @param attr_prefix The name of the attribute (with ending colon)
#' @param new_attr_val The value to substitute
#'
#'
#'
#' @export

substitute_rmd_header_attr = function(
  header_lines,
  attr_prefix = "title:",
  new_attr_val)
{
  attr_line = which(grepl(attr_prefix, header_lines))
  header_lines[attr_line] = sprintf("%s %s", attr_prefix, new_attr_val)
  return(header_lines)
}
