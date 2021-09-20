#' Wrapper for rmarkdown::render
#' Includes argument to check for specific version of pandoc or higher
#'
#'
#' @export
#'
#' @param filename_in Name of input file
#' @param filename_out Name of output file.  Given without extension: the extension will be added according to the output format specified in the header.
#' @param pandoc_version Required version of pandoc.  If this version (or higher) is not found, the page will not render.


render_page = function(filename_in, filename_out, dir_out, pandoc_version = "2.11")
{
  if (rmarkdown::pandoc_available(pandoc_version))
  {
    rmarkdown::find_pandoc(pandoc_version)
    rmarkdown::render(
      input = filename_in,
      output_file = filename_out,
      dir_out = dir_out
    )
    return(TRUE)
  }
  sprintf("Pandoc version %s or higher not found, not rendering document")
  return(FALSE)
}
