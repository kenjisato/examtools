
include_graphics <- function(image_file, md_opts = NULL, latex_opts = NULL) {

  sys_call <- sys.calls()
  use_knitr <- any(grepl('knitr::knit', sapply(sys_call, deparse)))
  pdf_file <- paste0(tools::file_path_sans_ext(image_file), '.pdf')

  latex_options <- if (!is.null(latex_opts)) {
    paste(paste(names(latex_opts), latex_opts, sep = '='), collapse = ', ')
  }

  if (exams::match_exams_device() == 'pdf' && file.exists(pdf_file)){
    out <- paste('\\includegraphics[', latex_options, ']{', pdf_file ,'}', sep = '')

    if (use_knitr) {
      return(structure(out, class = c('knit_asis')))
    } else {
      cat(out)
      return(invisible())
    }
  }

  if (!use_knitr){
    out <- paste('\\includegraphics[', latex_options, ']{', image_file ,'}', sep = '')
    cat(out)
    return(invisible())
  } else {
    alt <- if (!is.null(md_opts$alt)) md_opts$alt else ''
    width <- if (!is.null(md_opts$width))
      sprintf('{width=%s}', md_opts$width) else ''
    out <- sprintf('![%s](%s)%s', alt, image_file, width)
    return(structure(out, class = c('knit_asis')))
  }
}


