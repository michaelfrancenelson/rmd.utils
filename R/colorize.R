#' Change font color
#'
#' Stolen from: https://bookdown.org/yihui/rmarkdown-cookbook/font-color.html
#'
#' @export

colorize <- function(x, color, italic = FALSE, bold = FALSE)
{
  if (FALSE)
  {
    x = "abc"
    color = "blue"
    italic = TRUE
    bold = TRUE
  }

  if (knitr::is_latex_output())
  {
    out = sprintf("\\textcolor{%s}{%s}", color, x)
    if (italic) out = sprintf("\\textit{%s}", out)
    if (bold)   out = sprintf("\\textbf{%s}", out)

    sprintf(out)

  } else if (knitr::is_html_output())
  {
    out = sprintf("<span style='color: %s;", color)
    if (italic) out = sprintf("%s font-style: italic;", out)
    if (bold)   out = sprintf("%s font-weight:bold;", out)

    sprintf("%s'>%s</span>", out, x)
  }
}
