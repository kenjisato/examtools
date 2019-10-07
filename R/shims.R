
insert_imports_shims <- function(){
  utils::assignInNamespace("nops_eval_write", shim_nops_eval_write,
                           ns="exams", pos="package:exams")

}

#' @importFrom grDevices hcl
#' @importFrom utils read.csv2 zip
shim_nops_eval_write <- function(results = "nops_eval.csv",
                                  file = "nops_eval.zip",
                                  html = "exam_eval.html",
                                  col = hcl(c(0, 0, 60, 120),
                                            c(70, 0, 70, 70),
                                             90),
                                  encoding = "latin1", language = "en",
                                  converter = NULL,
                                  template = NULL
                                 ) {

  stopifnot(requireNamespace("base64enc"))

  results <- if (is.character(results)) {
    read.csv2(results, colClasses = "character")
  } else {
    results
  }
  names(results)[1L:3L] <- c("registration", "name", "id")
  rownames(results) <- results$registration
  mark <- "mark" %in% names(results)
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
    system.file(file.path("templates", "examresult.html"), package = "examtools")
  }

  html <- readLines(template)

  language <-  basename(tools::file_path_sans_ext(language))

  common_plh <- matrix(c(
    "##Language##", language,
    "##LANG_ExamResults##", lang$ExamResults,
    "##LANG_Name##", lang$Name,
    "##LANG_RegistrationNumber##", lang$RegistrationNumber,
    "##LANG_DocumentID##", lang$DocumentID,
    "##LANG_Points##", lang$Points,
    "##LANG_Evaluation##", lang$Evaluation,
    "##LANG_Question##", lang$Question,
    "##LANG_GivenAnswer##", lang$GivenAnswer,
    "##LANG_CorrectAnswer##", lang$CorrectAnswer,
    "##LANG_ExamSheet##", lang$ExamSheet
  ), ncol = 2, byrow = TRUE)

  # Replace Placeholders Common to All Examinees
  for (j in seq_len(nrow(common_plh))){
    html <- gsub(common_plh[j, 1], common_plh[j, 2], html, fixed = TRUE)
  }

  odir <- getwd()
  dir.create(dir <- tempfile())
  setwd(dir)
  on.exit(setwd(odir))

  for (i in 1L:nrow(results)) {
    id <- rownames(results)[i]
    ac <- results[id, "id"]
    dir.create(file.path(dir, ac))
    chk <- as.numeric(results[id, paste("check", 1L:m, sep = ".")])
    ans <- as.character(results[id, paste("answer", 1L:m, sep = ".")])
    sol <- as.character(results[id, paste("solution", 1L:m, sep = ".")])
    pts <- format(as.numeric(results[id, paste("points", 1L:m, sep = ".")]))

    res <- paste(sprintf(
      "<tr valign=\"top\" bgcolor=\"%s\"><td align=\"right\">%s</td><td align=\"right\">%s</td><td>%s</td><td>%s</td></tr>",
      col[cut(chk, breaks = c(-Inf, -1e-05, 1e-05, 0.99999, Inf))], 1L:m, pts, ans, sol), collapse = "\n")

    mrk <- if (mark) {
      paste0("<tr><td>", lang$Mark, ":</td><td>", results[id, "mark"], "</td></tr>")
    } else {
      ""
    }
    image1 <- sprintf("<img src=\"%s\" />",
                      base64enc::dataURI(file = file.path(odir, results[id, "scan"]),
                                         mime = "image/png"))
    image2 <- if (nscans == 1L || results[id, "scan2"] == "") {
      ""
    } else {
      sprintf("<img src=\"%s\" />",
              base64enc::dataURI(file = file.path(odir, results[id, "scan2"]),
                                 mime = "image/png"))
    }

    st_plh <- matrix(c(
      #
      # Examinee Information
      #
      "##Name##", results[id, "name"],
      "##RegistrationNumber##", id,
      "##ExamID##", results[id, "exam"],
      "##Points##", round(as.numeric(results[id,"points"]), digits = 4),
      "##Results##", res,
      "##Image1##", image1,
      #
      # Navigation
      #
      "##Previous##", if (i == 1) "#" else file.path("..", results[i-1, "id"], name),
      "##Next##", if (i == n) "#" else file.path("..", results[i+1, "id"], name),
      #
      # Parameter-specific replacements
      #
      "##Mark?##", mrk,
      "##Image2?##",  image2
    ), ncol = 2, byrow = TRUE)


    # Replace Placeholders for an Examinee
    html_i <- html
    for (j in seq_len(nrow(st_plh))){
      html_i <- gsub(st_plh[j, 1], st_plh[j, 2], html_i, fixed = TRUE)
    }

    writeLines(html_i, file.path(dir, ac, name))
  }

  setwd(dir)

  # make index.html
  htmlfiles <- list.files(dir, recursive = TRUE)
  listview <- file.path(system.file("templates", package = "examtools"),
                            "listview.html")
  listview <- readLines(listview)
  idx_list <- paste(paste0("<li><a href='", htmlfiles, "'>",
                           htmlfiles,
                           "</a></li>"),
                    collapse = "\n")

  index.html <- gsub("##List##", idx_list, listview, fixed = TRUE)

  writeLines(index.html, file.path(dir, "index.html"))

  # copy default.css
  file.copy(file.path(system.file("templates", package = "examtools"),
                      "default.css"),
            "default.css")

  invisible(zip(file.path(odir, file),
                c(results[, "id"], "index.html", "default.css")))

}
