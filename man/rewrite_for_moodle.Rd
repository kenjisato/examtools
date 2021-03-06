% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moodle.R
\name{rewrite_for_moodle}
\alias{rewrite_for_moodle}
\title{Convert structure of nops_eval.zip for bulk uploading to Moodle
Grading worksheet will also be produced.}
\usage{
rewrite_for_moodle(
  nops_zip,
  worksheet_csv,
  merge,
  register = NULL,
  nops_csv = NULL,
  suffix = "-moodle",
  pattern = "\%s_\%s_assignsubmission_file_",
  quote = TRUE
)
}
\arguments{
\item{nops_zip}{character. Path to nops_eval.zip, output of nops_eval()}

\item{worksheet_csv}{character. Path to Moodle's grading worksheet.}

\item{merge}{closure. Function to merge nops_eval.csv and worksheet_csv}

\item{register}{character. Path to register.csv}

\item{nops_csv}{character. Path to nops_eval.csv, output of nops_eval().
If NULL, it is guessed from the argument to nops_zip.}

\item{suffix}{character. Suffix for output zip file.}

\item{pattern}{character. Pattern for output directory names.}

\item{quote}{logical, passed to write.csv}
}
\value{
None. New zip file is created as a side effect.
}
\description{
Convert structure of nops_eval.zip for bulk uploading to Moodle
Grading worksheet will also be produced.
}
