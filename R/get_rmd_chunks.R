#' Get all the chunk names and the start/end line numbers for the corresponding source code.
#'
#' @export


get_rmd_chunks = function(filename = NULL, file_lines = NULL)
{
  if (is.null(file_lines)) file_lines = readLines(filename)

  chunk_indices = get_rmd_chunk_indices(file_lines = file_lines)
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

    chunk_name =
      trimws(gsub("r ", "", gsub("[,}=<>]", "", reg_match)))

    if(length(chunk_name) == 0) chunk_name = ""

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




#'
#'
#'
#' @export

source_rmd_chunks = function(filename, file_lines = NULL, chunk_names = NULL)
{


  if (FALSE)
  {
    filename = find_file("lab_05.Rmd", exact_match = TRUE)
    file_lines = NULL
    chunk_names = NULL
  }

  if (is.null(file_lines)) file_lines = readLines(filename)
  chunks = get_rmd_chunks(file_lines = file_lines)

  e <- environment() # current environment
  e = .GlobalEnv
  p <- parent.env(e)
  lapply(chunks, function(x) evaluate::evaluate(
    # textConnection(x), stop_on_error = 0, envir = globalenv()))
    textConnection(x), stop_on_error = 0, envir = e))
}



if (FALSE)
{
  require(rmd.utils)
  rm(list = ls())
  devtools::document()
  devtools::load_all()
  devtools::install()
  fn = find_file("lab_05.Rmd", exact_match = TRUE)

  source_rmd_chunks(fn)
  source_rmd_chunks(fn, chunk_names = "setup")

  file_lines = readLines(fn)
  chunk_indices = get_rmd_chunk_indices(file_lines = file_lines)

  chunks = get_rmd_chunks(filename = fn)

  chunks

  evaluate::evaluate(textConnection(chunks$setup))


  lapply(chunks, evaluate::evaluate)
  i = 1


  chunk_text_1 = file_lines[chunk_indices[i, 1]-1]

  chunk_text_1

  regmatches(chunk_text_1, regexpr("r .+?[\\s,}]", chunk_text_1))


  f = function()
  {
    evaluate::evalute("x <<- 2")
  }


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
