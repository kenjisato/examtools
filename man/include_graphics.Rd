% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphics.R
\name{include_graphics}
\alias{include_graphics}
\title{Include image not produced by R}
\usage{
include_graphics(image_file, md_opts = NULL, latex_opts = NULL)
}
\arguments{
\item{image_file}{character. image file name. Typically, PNG or SVG}

\item{md_opts}{list. list of \code{alt} and \code{width}. Used as ![alt](image_file){width=width}}

\item{latex_opts}{list. list of parameters passed to \\includegraphics[]{image_file}}
}
\value{
LaTeX/Markdown code embedded in Rmd/Rnw file.
}
\description{
Include image not produced by R
}
