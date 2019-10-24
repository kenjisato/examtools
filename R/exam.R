
#' Set up one exam.
#'
#' @param problems list of problems.
#' @param date character. Representing the date of exam. e.g.) "2019-10-10"
#' @param name character. Name of the exam. e.g.) "Final Exam"
#' @param shortname character. Short name for the exam. e.g.) "Day02" or "final"
#' @param versions integer. Number of versions created.
#' @param samples.from.group integer vector. a.k.a \code{nsamp} in R/exams world.
#'
#' @return list of exam information. Result is passed to \code{exam_create}
#' @export
#'
exam <- function(problems, date, name, shortname = date,
                 versions = 1, samples.from.group = 1) {
  course <- course_check()
  list(problems = problems,
       date = date,
       name = name,
       shortname = shortname,
       versions = versions,
       samples.from.group = samples.from.group,
       course = course)
}


#' Create NOPS written exam
#'
#' @param exam list of exam information. Created with \code{exam}
#' @param latex.cover character. Path to a tex file cover page or intro message.
#'    Passed to \code{intro} parameter of \code{exams2nops}
#' @param html.template character. Path to template html. Passed to \code{template}
#'    parameter of \code{exams2html}
#' @param encryptHTML logical. If TRUE, password-protected version of HTML problem set
#'    is also created.
#' @param course.as.pdf.title logical. If TRUE, use course title as PDF title. If FALSE,
#'    institution is used.
#' @param ..., other parameters passed to \code{exams2nops}
#'
#' @return invisible NULL
#' @export
#'
exam_create <- function(exam,
                        latex.cover = NULL,
                        html.template = NULL,
                        encryptHTML = TRUE,
                        course.as.pdf.title = TRUE,
                        ...){

  course <- exam$course
  shortname <- exam$shortname
  shortname.dash <- paste0(shortname, "-")

  # targetdir
  out.dir <- file.path(getwd(), course$nops_dir, exam$date)

  # Number of problem patterns
  n <- exam$versions

  # Randomness
  rseed <- as.integer(as.Date(exam$date)) + as.integer(course$seed)

  # Seeds matrix
  set.seed(rseed)
  seed_matrix <- matrix(sample(1:1000, length(exam$problems) * n), nrow = n)

  # HTML version
  set.seed(rseed)
  if (is.null(html.template)) {
    html.template <- system.file("xml", "plain.html", package = "examtools")
  }
  exams::exams2html(exam$problems, dir = out.dir, name = shortname.dash,
                    solution = FALSE, seed = seed_matrix,
                    template = html.template)

  if (encryptHTML) {
    # To be implemented..
  }

  # NOPS PDF
  set.seed(rseed)
  exams::exams2nops(
           exam$problems,
           n = n,
           dir = out.dir,
           name = shortname.dash,
           language = course$language,
           institution = if (course.as.pdf.title) course$course else course$institution,
           title = exam$title,
           date = exam$date,
           reglength = course$reglength,
           logo = file.path(getwd(), course$logo),
           seed = seed_matrix,
           blank = 0,
           intro = latex.cover,
           ...)

    # Rename metafile
    file.rename(file.path(out.dir, paste0(shortname.dash, ".rds")),
                file.path(out.dir, paste0(shortname, ".rds")))

    # Save Exam Information
    saveRDS(exam, file.path(out.dir, paste0(shortname, "-exam.rds")))
}



exam_scan <- function() {

}

exam_evaluate <- function() {

}
