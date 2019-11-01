merge_moodle <- function(ws, ev, reg) {
  # default merge function
  # It is assumed that id column of register.csv corresponds
  # to email address of worksheet.
  ws_names <- names(ws)

  merged_df <- merge(ws, ev, by = "id", sort = FALSE)
  merged_df[["Grade"]] <- merged_df[["points"]]
  merged_df
}


merge_moodle_opu <- function(ws, ev, reg){
  # Remove prefix from submission ID column -> sid
  ws_names <- names(ws)
  ws$sid <- sub("^[^0-9]*([0-9]+)$", "\\1", ws[[1]])

  # Pick registration numbers from Full name column.
  reglength <- 10
  ws$registration <- sub(paste0(".*([0-9]{", reglength, "}).*"),
                         "\\1", ws[[2]])

  merged_df <- merge(ws, ev, by = "registration", sort = FALSE)

  merged_df[["Grade"]] <- merged_df[["points"]]
  merged_df
}


#' Convert structure of nops_eval.zip for bulk uploading to Moodle
#' Grading worksheet will also be produced.
#'
#' @param nops_zip character. Path to nops_eval.zip, output of nops_eval()
#' @param worksheet_csv character. Path to Moodle's grading worksheet.
#' @param merge closure. Function to merge nops_eval.csv and worksheet_csv
#' @param register character. Path to register.csv
#' @param nops_csv character. Path to nops_eval.csv, output of nops_eval().
#'                 If NULL, it is guessed from the argument to nop_zip.
#' @param suffix character. Suffix for output zip file.
#' @param pattern character. Pattern for output directory names.
#' @param quote logical, passed to write.csv
#'
#' @return None. New zip file is created as a side effect.
#' @export
#'
#' @importFrom utils read.csv read.table unzip write.csv
rewrite_for_moodle <- function(nops_zip,
                               worksheet_csv,
                               merge,
                               register = NULL,
                               nops_csv = NULL,
                               suffix = "-moodle",
                               pattern = "%s_%s_assignsubmission_file_",
                               quote = TRUE) {

  name <- tools::file_path_sans_ext(nops_zip)
  if (is.null(nops_csv)) {
    nops_csv <- paste0(name, ".csv")
  }

  nops_eval <- read.table(nops_csv, header = TRUE, sep = ";",
                          colClasses = "character")
  worksheet <- read.csv(worksheet_csv, fileEncoding = "UTF-8",
                        stringsAsFactors = FALSE, check.names = FALSE)
  worksheet_names <- names(worksheet)
  merged_df <- merge(worksheet, nops_eval, register)
  merged_df$dir_name <- sprintf(pattern,
                                merged_df[[worksheet_names[[2]]]],
                                merged_df$sid)

  # Write out updated Grading Worksheet
  write.csv(merged_df[worksheet_names],
            sub(".csv", paste0(suffix, ".csv"), worksheet_csv, fixed = TRUE),
            row.names = FALSE, na = "", quote = quote)

  # Process ZIP file
  temp_dir <- tempfile()
  dir.create(temp_dir); on.exit(unlink(temp_dir, recursive = TRUE))
  unzip(nops_zip, exdir = temp_dir)

  dirs <- list.files(temp_dir, full.names = TRUE)
  for (d in dirs) {
    student_dir <- basename(d)
    new_dir_name <- merged_df$dir_name[merged_df$registration == student_dir]
    if (length(new_dir_name) > 0){
      file.rename(d, file.path(temp_dir, new_dir_name))
    } else {
      message("ID ", student_dir, " is not registered. Skipped.")
    }
  }
  renamed_dirs <- list.files(temp_dir, full.names = TRUE)
  zip::zipr(paste0(name, suffix, ".zip"), renamed_dirs)
}
