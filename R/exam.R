
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
                 versions = 1L, samples.from.group = NULL) {
  course <- course_check()
  examdir <- file.path(course$nops_dir, shortname)
  privdir <- file.path(course$private_dir, course$nops_dir, shortname)
  dir.create(examdir)
  dir.create(privdir)

  ex <- list(problems = problems,
             date = date,
             name = name,
             shortname = shortname,
             versions = versions,
             samples.from.group = samples.from.group,
             course = course)

  # Save Exam Information
  saveRDS(ex, file.path(examdir, "config.rds"))
}


#' Step 1: Create NOPS written exam
#'
#' @param exam exam short name, directory contains config.rds, created with \code{exam}
#' @param latex.cover character. Path to a tex file cover page or intro message.
#'    Passed to \code{intro} parameter of \code{exams2nops}
#' @param html.template character. Path to template html. Passed to \code{template}
#'    parameter of \code{exams2html}
#' @param solution.template character. Path to template tex. Passed to \code{template}
#'    parameter of \code{exams2pdf}
#' @param encryptHTML logical or character. If TRUE or passphrase is set, password-protected
#'    version of HTML problem set is also created.
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
                        solution.template = NULL,
                        encryptHTML = FALSE,
                        course.as.pdf.title = TRUE,
                        ...){

  course <- course_check()
  examdir <- file.path(course$nops_dir, exam)
  exam <- readRDS(file.path(examdir, "config.rds"))

  encrypt <- isTRUE(encryptHTML) ||
    if (is.character(encryptHTML)) as.logical(nchar(encryptHTML)) else FALSE

  if (encrypt && !requireNamespace("clientsideHtmlProtect")){
    stop("install clientsideHtmlProtect with: \n",
         "remotes::install_github('kenjisato/clientsideHtmlProtect')")
  }

  course <- exam$course
  shortname <- exam$shortname
  shortname.dash <- paste0(shortname, "-")

  # Number of problem patterns
  n <- exam$versions
  nsamp <- exam$samples.from.group

  # Randomness
  rseed <- as.integer(as.Date(exam$date)) + as.integer(course$seed)

  # HTML version
  set.seed(rseed)
  if (is.null(html.template)) {
    html.template <- system.file("xml", "plain.html", package = "examtools")
  }
  exams::exams2html(exam$problems, n = n, nsamp = nsamp, dir = examdir,
                    name = shortname.dash, solution = FALSE,
                    mathjax = TRUE, template = html.template)

  if (encrypt) {
    passphrase <- if (isTRUE(encryptHTML)) {
      readline(prompt = "passpharase: ")
    } else {
      encryptHTML
    }
    for (i in seq_len(n)){
      encrypted <- clientsideHtmlProtect::protect(
        file.path(examdir, paste0(shortname.dash, i, ".html")),
        passphrase = passphrase)
      writeLines(encrypted, file.path(examdir, paste0(shortname.dash, i, "-protected.html")))
    }
  }

  # params
  # NOPS PDF

  dots <- list(...)
  dots$file <- exam$problems
  dots$n <- n
  dots$nsamp <- nsamp
  dots$dir <- examdir
  dots$name <- shortname.dash
  dots$language <- course$language
  dots$institution <- if (course.as.pdf.title) course$course else course$institution
  dots$title <- exam$name
  dots$date <- exam$date
  dots$reglength <- course$reglength
  dots$logo <- if (is.null(dots$logo)) file.path(getwd(), course$logo) else dots$logo
  dots$blank <- 0
  dots$intro <- latex.cover

  set.seed(rseed)
  do.call(exams::exams2nops, dots)

  # Solution
  dots$template <- solution.template
  dots$header <- list(Date = exam$date)
  dots$name <- paste0(shortname.dash, "solution")

  set.seed(rseed)
  do.call(exams::exams2pdf, dots)

  # Rename metafile
  file.rename(file.path(examdir, paste0(shortname.dash, ".rds")),
              file.path(examdir, "metainfo.rds"))
}

# Step 2: Print
# Step 3: Exam

#' Step 4: Scan exam sheets
#'
#' @param shortname short name, directory contains config.rds, created with \code{exam}
#' @param images scanned images (PNG's or PDF file)
#' @param ... parameters passed on to \code{exams::nops_scan}. NB: \code{dir} and \code{file}
#'   are overwritten by default configurations
#'
#' @return contents of Daten.txt as a character vector. Returned invisibly.
#' @export
#'
exam_scan <- function(shortname, images, ...){

  course <- course_check()
  examdir <- file.path(course$nops_dir, shortname)
  privdir <- file.path(course$private_dir, course$nops_dir, shortname)
  exam <- readRDS(file.path(examdir, "config.rds"))

  params <- list(...)
  params$dir <- privdir

  if (is.null(params$string) || !params$string) {
    params$file <- "nops_scan"
  } else {
    params$file <- "nops_str_scan"
  }

  params$images <- tools::file_path_as_absolute(images)

  do.call(exams::nops_scan, params)

  z <- file.path(privdir, paste0(params$file))
  zip::unzip(paste0(z, ".zip"), exdir = z)

}


#' Step 5: Evaluate the exam
#'
#' @param shortname short name, directory contains config.rds, created with \code{exam}
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
  function(shortname, flavor = "template",
           report.template = system.file("nops", "eval.html", package = "exams"),
           eval = exams::exams_eval(partial = FALSE, negative = FALSE),
           ...){

    course <- course_check()
    examdir <- file.path(course$nops_dir, shortname)
    privdir <- file.path(course$private_dir, course$nops_dir, shortname)
    exam <- readRDS(file.path(examdir, "config.rds"))

    # Default parameters for nops_eval()
    params <- list(...)

    params$flavor <- flavor
    params$template <- report.template
    params$eval <- eval

    if (is.null(params$file)) params$file <- "report"
    if (is.null(params$mark)) params$mark <- FALSE

    # destination directory
    params$dir <- privdir

    # Use global setting from course.yml
    if (is.null(params$register)) params$register <- course$register

    if (is.null(params$scans)){
      nops_scan_dir <- file.path(privdir, "nops_scan")
      zipfile <- paste0(tempfile(), ".zip")
      on.exit(file.remove(zipfile))

      zip::zipr(zipfile, list.files(nops_scan_dir, full.names = TRUE))
      params$scans <- zipfile
    }
    if (is.null(params$solutions)){
      params$solutions <- file.path(examdir, "metainfo.rds")
    }

    ev <- do.call(exams::nops_eval, params)


    ## Omit this when the bug in nops_eval is fixed.
    for (file in c("nops_eval.csv", "nops_eval.zip")){
      file.copy(file, file.path(examdir, file), overwrite = TRUE)
      unlink(file)
    }

    ev
}
