
#' @title Customized nops_eval_write
#'
#' @param results character or data.frame. Path to nops_eval.csv or data.frame for the same data.
#' @param name character. Output file name for individual reports, with extension.
#' @param template character. HTML template for output files.
#' @param ... list. \code{encoding}, \code{language}, \code{converter}, and \code{dir}, passed to
#'        \code{\link[exams]{nops_eval}}, are respected.
#'
#' @export
#'
#' @importFrom grDevices hcl
#' @importFrom utils read.csv2 zip
#' @importFrom whisker whisker.render rowSplit
nops_eval_write_custom <- function(
  results = "nops_eval.csv",
  name = "report.html",
  template = NULL,
  ...) {

  stopifnot(requireNamespace("base64enc"))

  # Explicitly Inherit Parameters
  dots <- list(...)
  encoding <- dots$encoding
  language <- dots$language
  converter <- dots$converter
  dir <- dots$dir

  out_zip <- paste0(tools::file_path_sans_ext(basename(results)), ".zip")
  results <- read.csv2(results, colClasses = "character")

  names(results)[1:3] <- c("registration", "name", "id")
  rownames(results) <- results$registration
  has_mark <- "mark" %in% names(results)
  m <- length(grep("answer.", colnames(results), fixed = TRUE))
  n <- nrow(results)
  format_mchoice <- function(x) {
    mchoice2print <- function(x) {
      paste(ifelse(x, letters[1L:5L], rep("_", 5L)),  collapse = "")
    }
    sapply(strsplit(x, ""),
           function(z) mchoice2print(as.logical(as.numeric(z))))
  }
  for (i in as.vector(outer(c("answer", "solution"), 1L:m, paste, sep = "."))) {
    results[[i]] <- format_mchoice(results[[i]])
  }
  nscans <- 1L + as.integer("scan2" %in% names(results))
  if (is.null(converter)) {
    converter <- if (language %in% c("hr", "ro", "sk", "tr")) "pandoc" else "tth"
  }
  if (!file.exists(language)) {
    language <- system.file(file.path("nops", paste0(language, ".dcf")),
                            package = "exams")
  }
  if (language == "") {
    language <- system.file(file.path("nops", "en.dcf"), package = "exams")
  }
  lang <- exams::nops_language(language, converter = converter)
  substr(lang$Points, 1L, 1L) <- toupper(substr(lang$Points, 1L, 1L))
  if (!is.null(lang$PointSum)) {
    lang$Points <- lang$PointSum
  }

  template <- if (!is.null(template)) {
    tools::file_path_as_absolute(template)
  } else {
    system.file(file.path("xml", "wide.html"), package = "examtools")
  }
  template <- readLines(template)

  commonDat <- list(
    language = basename(tools::file_path_sans_ext(language)),
    encoding = encoding,
    has_mark = has_mark,
    LANG_ExamResults = lang$ExamResults,
    LANG_Name = lang$Name,
    LANG_RegistrationNumber = lang$RegistrationNumber,
    LANG_DocumentID = lang$DocumentID,
    LANG_Points = lang$Points,
    LANG_Evaluation = lang$Evaluation,
    LANG_Mark = lang$Mark,
    LANG_Question = lang$Question,
    LANG_GivenAnswer = lang$GivenAnswer,
    LANG_CorrectAnswer = lang$CorrectAnswer,
    LANG_ExamSheet = lang$ExamSheet
  )
  checkClasses <- c("negative", "neutral", "positive", "full")

  odir <- getwd()
  dir.create(temp_dir <- tempfile())
  setwd(temp_dir)
  on.exit(setwd(odir))

  for (i in 1L:nrow(results)) {
    dat <- commonDat

    id <- rownames(results)[i]
    ac <- results[id, "id"]
    dir.create(file.path(temp_dir, ac))

    ## Exam Information
    dat$name <- results[id, "name"]
    dat$registration <- results[id, "registration"]
    dat$exam <- results[id, "exam"]
    dat$mark <- if (has_mark) results[id, "mark"] else ""
    dat$points <- results[id, "points"]

    ## Results for Individual Problems
    res <- data.frame(
      question = 1L:m,
      check = as.numeric(results[id, paste("check", 1L:m, sep = ".")]),
      answer = as.character(results[id, paste("answer", 1L:m, sep = ".")]),
      solution = as.character(results[id, paste("solution", 1L:m, sep = ".")]),
      points = format(as.numeric(results[id, paste("points", 1L:m, sep = ".")]))
    )
    res$check = checkClasses[cut(res$check,
                             breaks = c(-Inf, -1e-05, 1e-05, 0.99999, Inf))]
    dat$results <- unname(rowSplit(res))

    ## Images
    dat$image1 <- sprintf("<img src=\"%s\" />",
                      base64enc::dataURI(file = file.path(odir, results[id, "scan"]),
                                         mime = "image/png"))
    if (nscans > 1L && results[id, "scan2"] != "") {
      dat$image2 <-
        sprintf("<img src=\"%s\" />",
                base64enc::dataURI(file = file.path(odir, results[id, "scan2"]),
                                   mime = "image/png"))
    }

    template_i <- whisker.render(template, dat)
    writeLines(template_i, file.path(temp_dir, ac, name))
  }

  invisible(zip(file.path(dir, out_zip), c(results[, "id"])))

}
