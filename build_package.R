# Instructions adapted from
# https://r-pkgs.org/whole-game.html#peek-at-the-finished-product

# create .Rd documentation files
devtools::document()

# Check for errors
devtools::check()

# Get the package ready to use
devtools::install()





if (FALSE)
{

  find_file("lab_05", extension = ".Rmd")

  filename = find_file("Q01", extension = ".Rmd")
  lines_i = readLines(filename)

  format_moodle_question_source(lines_i)
  format_moodle_question_source(lines_i, include_metadata = TRUE)
  format_moodle_question_source(lines_i, include_metadata = TRUE, include_solution = TRUE)
  format_moodle_question_source(lines_i, include_metadata = FALSE, include_solution = TRUE)



  find_file("lab_05", directory = TRUE)
  format_moodle_web_questions(find_file("Q01", extension = ".Rmd"))

    question_source_files = find_file("Q", search_path = find_file("lab_05", directory = TRUE), return_all = TRUE)
  format_moodle_web_questions(question_source_files)

}
