#'
#' Create html code for a dropdown menu.
#'
#' @param options a vector containing the elements to include int he dropdown
#' @param fmt_select the format for the html select element
#' @param fmt_option the format for the html option elemment
#' @param mchoice whether or not to display the multiple-choice question description
#' @param schoice_q the prompt text to display for single-choice
#' @param schoice_q the prompt text to display for multiple-choice
#' @param cat whether to cat() the results
#'
#' @export


make_html_dropdown = function(
  options,
  fmt_select = "<select>\n%s\n</select>",
  fmt_option = "<option>%s</option>",
  schoice_q = "select the best answer",
  mchoice_q = "select the best answer or answers",
  mchoice = FALSE,
  cat = TRUE
)
{

  out = (sprintf(
    fmt_select,
    paste0(
      sprintf(fmt_option, options),
      collapse = "\n")))

  out2 = paste0(
    ifelse(
      mchoice,
      mchoice_q,
      schoice_q),
    out
  )

  if (cat) cat(out)
  invisible(out2)
}

#'
#' Create substitute text for a cloze answer
#'
#' @param ans_num cloze answer number
#' @param check_var if an object with this name exists, the substitute text will be returned, otherwise the cloze answer is returned
#' @param sub_text text to substitute for the cloze code
#' @param cloze_fmt format for the cloze code
#' @param cat whether or not to cat() the output
#'
#' @export
sub_cloze_answer = function(
  ans_num,
  check_var,
  sub_text = "<span>______</span>",
  cloze_fmt = "\\#\\#ANSWER%s\\#\\#",
  cat = FALSE
)
{

  out = ifelse(
    exists(check_var),
    sub_text,
    sprintf(cloze_fmt, ans_num))
    # paste0("\\#\\#ANSWER", ans_num, "\\#\\#"))

  if (cat) cat(out)

  return(out)

}
