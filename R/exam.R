
#' Step 0: Set up one exam.
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


#' Step 1: Create NOPS written exam
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
           title = exam$name,
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

# Step 2: Print
# Step 3: Exam

#' Step 4: Scan exam sheets
#'
#' @param exam character. path to saved RDS file.
#' @param images scanned images (PNG's or PDF file)
#' @param ... parameters passed on to \code{exams::nops_scan}. NB: \code{dir} and \code{file}
#'   are overwritten by default configurations
#'
#' @return contents of Daten.txt as a character vector. Returned invisibly.
#' @export
#'
exam_scan <- function(exam, images, ...){

  params <- list(...)
  params$dir <- dirname(tools::file_path_as_absolute(exam))
  examinfo <- readRDS(exam)

  params$file <- paste0(examinfo$shortname, "_scan")
  params$images <- tools::file_path_as_absolute(images)

  do.call(exams::nops_scan, params)

}


#' Step 5: Evaluate the exam
#'
#' @param exam character. path to saved RDS file.
#' @param flavor character. User nops_eval_write_<flavor>
#' @param report.template character. Passed on to nops_eval_write_<flavor>
#'    as an argument to template paramter
#' @param eval list specification of evaluation policy
#' @param ... Parameters passed on to \code{nops_eval} and \code{nops_eval_write_*}.
#'    As for \code{nops_eval}, respected parameters are \code{points}, \code{mark},
#'    \code{labels}, \code{results}, \code{file}, \code{interactive},
#'    \code{string_scans}, \code{string_points}. The other parameters are ignored.
#'
#' @return \code{data.frame} of marked result.
#' @export
#'
exam_evaluate <-
  function(exam, flavor = "examtools",
           report.template = system.file("xml", "report.html", package = "examtools"),
           eval = exams::exams_eval(partial = FALSE, negative = FALSE),
           ...){

  # Default parameters for nops_eval()
  dots <- list(...)
  points <- if (is.null(dots$points)) NULL else dots$points
  mark <- if (is.null(dots$mark)) c(0.5, 0.6, 0.75, 0.85) else dots$mark
  labels <- if (is.null(dots$labels)) NULL else dots$labels
  results <- if (is.null(dots$results)) "nops_eval" else dots$results
  file <- if (is.null(dots$file)) NULL else dots$file
  interactive <- if (is.null(dots$interactive)) TRUE else dots$interactive
  string_scans <-
    if (is.null(dots$string_scans)) character(0) else dots$string_scans
  string_points <-
    if (is.null(dots$string_points)) seq(0, 1, 0.25) else dots$string_points


  out.dir <- dirname(tools::file_path_as_absolute(exam))
  exam <- readRDS(exam)
  course <- exam$course

  if (is.null(dots$scans)){
    scans <- file.path(out.dir, paste0(exam$shortname, "_scan.zip"))
  }
  if (is.null(dots$solutions)){
    solutions <- file.path(out.dir, paste0(exam$shortname, ".rds"))
  }

  ev <- exams::nops_eval(
    register = course$register,
    solutions = solutions,
    scans = scans,
    points = points,
    eval = eval,
    mark = mark,
    labels = labels,
    dir = out.dir,
    results = results,
    file = file,
    flavor = flavor,
    language = course$language,
    interactive = interactive,
    string_scans = string_scans,
    string_points = string_points,
    template = report.template,
    ...)

  ## Omit this when the bug in nops_eval is fixed.
  for (file in c("nops_eval.csv", "nops_eval.zip")){
    file.copy(file, file.path(out.dir, file), overwrite = TRUE)
    unlink(file)
  }

  ev
}
