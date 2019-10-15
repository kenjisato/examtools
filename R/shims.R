
insert_imports_shims <- function(){
  utils::assignInNamespace("nops_eval_write", shim_nops_eval_write,
                           ns="exams", pos="package:exams")
}

#' @importFrom grDevices hcl
#' @importFrom utils read.csv2 zip
#' @importFrom whisker whisker.render rowSplit
shim_nops_eval_write <- function(results = "nops_eval.csv",
                                 file = "nops_eval.zip",
                                 html = "exam_eval.html",
                                 encoding = "UTF-8",
                                 language = "en",
                                 converter = NULL,
                                 template = NULL,
                                 col = NULL
                                 ) {

  stopifnot(requireNamespace("base64enc"))

  if (is.character(results)) {
    results <- read.csv2(results, colClasses = "character")
  }

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
  lang <- exams:::nops_language(language, converter = converter)
  substr(lang$Points, 1L, 1L) <- toupper(substr(lang$Points, 1L, 1L))
  if (!is.null(lang$PointSum)) {
    lang$Points <- lang$PointSum
  }
  name <- html

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
  dir.create(dir <- tempfile())
  setwd(dir)
  on.exit(setwd(odir))

  for (i in 1L:nrow(results)) {
    dat <- commonDat

    id <- rownames(results)[i]
    ac <- results[id, "id"]
    dir.create(file.path(dir, ac))

    ## Exam Information
    dat$name <- results[id, "name"]
    dat$registration <- results[id, "registration"]
    dat$exam <- results[id, "exam"]
    dat$mark <- if (has_mark) results[id, "mark"] else ""
    dat$points <- results[id, "points"]

    ## Results for Individual Problems
    res <- data.frame(
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
    writeLines(template_i, file.path(dir, ac, name))
  }

  setwd(dir)

  invisible(zip(file.path(odir, file), c(results[, "id"])))

}
