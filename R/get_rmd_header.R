#' Get the yaml header from an Rmd file
#'
#' @param filename the name of the Rmd file to read
#' @param file_lines a character vector containing the lines of a source Rmd file.
#'
#' @export

get_rmd_header = function(filename, file_lines = NULL)
{
  if (is.null(file_lines)) file_lines = readLines(filename)
  header_symbols = which(grepl("---", file_lines))
  return(file_lines[header_symbols[1]:header_symbols[2]])
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
    filename = "C:/Users/michaelnelso/git/eco_602_634_2020/assignments/eco_602/week_03_data_exploration_deterministic_functions/moodle/Q1_histograms_elevation.Rmd"
    file_lines = readLines(filename, warn = FALSE)
    title_prefix = "title:"
  }

  get_rmd_header = function(filename, file_lines = NULL)
  {
    if (is.null(file_lines)) file_lines = readLines(filename)
    header_symbols = which(grepl("---", file_lines))
    return(file_lines[header_symbols[1]:header_symbols[2]])
  }
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
