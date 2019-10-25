#' Lists problems in a directory.
#'
#' @param path character. Path to a directory that contains problems Rnw/Rmd, relative to basedir.
#'     This function assumes particular naming rules. For further details, see below.
#' @param group.sep character. Character to indicate groups. For further details, see below.
#' @param exclude character string containing a regular expression to be searched for.
#'   Files that match this pattern are excluded from the output.
#' @param basedir character. Base directory relative to which Path is specified. Defaults to
#'    problems_dir in course.yml
#'
#' @return List, which can be passed to exams function such as \code{exams::exams2pdf}.
#' @export
#'
#' @details Problem filen ames should be like \code{01-abc.Rmd}, \code{02-xyz.Rmd},
#'   where \code{-} is the character separating group (\code{01}, \code{02}) and
#'   keyword (\code{abc} and \code{xyz}). You can use different \code{group.sep}
#'   than \code{-} (default) by specifying \code{group.sep} argument.
#'
#'   When the directory specified by the \code{path} argument contains
#'   \code{01-abc.Rmd}, \code{01-def.Rmd} and \code{02-xyz.Rmd}, this function
#'   returns \code{list(c("01-abc.Rmd", "01-def.Rmd"), "02-xyz.Rmd")}.
#'
#' @examples
#' \dontrun{
#'   path <- system.file("inst", "samples", package = "examtools")
#'   problems_list(path)
#'}
problems_list <- function(path, group.sep = "-", exclude = NULL, basedir = NULL) {

  course <- course_check()
  if (is.null(basedir)) basedir <- course$problems_dir
  path <- file.path(basedir, path)

  files <- list.files(path, full.names = FALSE, pattern = "(Rmd|Rnw)$")
  if (!is.null(exclude)) {
    files <- files[!grepl(exclude, files)]
  }

  lst <- strsplit(files, split = group.sep, fixed = TRUE)

  if (sum(sapply(lst, length) == 1) > 0) {
    message("File names do not conform to the naming rule. Please check the result.")
  }



  groups <- unique(sapply(lst, `[[`, 1))
  problems <- list()
  for (i in seq_along(groups)){
    problems[[i]] <- files[startsWith(files, groups[[i]])]
  }
  problems
}

