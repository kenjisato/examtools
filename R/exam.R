
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

  encrypt <- isTRUE(encryptHTML) ||
    if (is.character(encryptHTML)) as.logical(nchar(encryptHTML)) else FALSE

  if (encrypt && !requireNamespace("clientsideHtmlProtect")){
    stop("install clientsideHtmlProtect with: \n",
         "remotes::install_github('kenjisato/clientsideHtmlProtect')")
  }

  course <- exam$course
  shortname <- exam$shortname
  shortname.dash <- paste0(shortname, "-")

  # targetdir
  out.dir <- file.path(getwd(), course$nops_dir, exam$date)

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
  exams::exams2html(exam$problems, n = n, nsamp = nsamp, dir = out.dir,
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
        file.path(out.dir, paste0(shortname.dash, i, ".html")),
        passphrase = passphrase)
      writeLines(encrypted, file.path(out.dir, paste0(shortname.dash, i, "-protected.html")))
    }
  }


  # NOPS PDF
  set.seed(rseed)
  exams::exams2nops(
    exam$problems,
    n = n,
    nsamp = nsamp,
    dir = out.dir,
    name = shortname.dash,
    language = course$language,
    institution = if (course.as.pdf.title) course$course else course$institution,
    title = exam$name,
    date = exam$date,
    reglength = course$reglength,
    logo = file.path(getwd(), course$logo),
    blank = 0,
    intro = latex.cover,
    ...
  )

  # Solution
  set.seed(rseed)
  exams::exams2pdf(
    exam$problems,
    n = n,
    nsamp = nsamp,
    dir = out.dir,
    template = solution.template,
    header = list(Date = exam$date),
    name = paste0(shortname.dash, "solution"),
    ...
  )

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

  if (is.null(params$string) || !params$string) {
    params$file <- paste0(examinfo$shortname, "_scan")
  } else {
    params$file <- paste0(examinfo$shortname, "_str_scan")
  }

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
  params <- list(...)

  params$flavor = flavor
  params$template = report.template
  params$eval = eval


  # destination directory
  out.dir <- dirname(tools::file_path_as_absolute(exam))
  params$dir <- out.dir

  exam <- readRDS(exam)
  course <- exam$course

  # Use global setting from course.yml
  if (is.null(params$register)) params$register <- course$register

  if (is.null(params$scans)){
    params$scans <- file.path(out.dir, paste0(exam$shortname, "_scan.zip"))
  }
  if (is.null(params$solutions)){
    params$solutions <- file.path(out.dir, paste0(exam$shortname, ".rds"))
  }

  ev <- do.call(exams::nops_eval, params)


  ## Omit this when the bug in nops_eval is fixed.
  for (file in c("nops_eval.csv", "nops_eval.zip")){
    file.copy(file, file.path(out.dir, file), overwrite = TRUE)
    unlink(file)
  }

  ev
}
