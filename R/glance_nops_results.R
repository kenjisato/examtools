
#' Serve listview of nops_eval.zip
#'
#' @param name Path to output zip file of nops_eval()
#'
#' @return Directory name to serve, invisibly
#' @export
#'
nops_eval_listview <- function(name){
  basename <- tools::file_path_sans_ext(name)
  zipfile <- paste0(basename, ".zip")

  if(!dir.exists(basename)){
    zip::unzip(zipfile)
  }

  servr::httd(dir = basename)

  message("To clean up, run",
           "\n",
           paste0("unlink('", basename, "', recursive = TRUE)"))

  invisible(basename)
}
