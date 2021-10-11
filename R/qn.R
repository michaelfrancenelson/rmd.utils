#' Format a questionnumber and point count for RMarkdown
#'
#' @param n Question number
#' @param pts Number of points
#' @param q_string Question abbreviation to show
#' @param pt_string Points text for a 1-point question
#' @param pts_string Points text for a question worth more than 2 pts
#' @param pt_brackets Brackets to surround the number of poitns
#' @param end_string Ending string
#'
#' @return Markdown formatted text for the question number and point count.
#' @export



qn = function(
  n, pts = 1,
  q_string = "Q",
  pt_string = " pt.",
  pts_string = " pts.",
  pt_brackets = c("(", ")"),
  end_string = ":")
{

  p_st = ifelse(pts == 1, pt_string, pts_string)

  pts_string = sprintf("%s%s%s%s", pt_brackets[1], pts, p_st, pt_brackets[2])

  sprintf("**%s%s %s%s**", q_string, n, pts_string, end_string)
}

