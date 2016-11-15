
#' Get output format
#'
#' This function returns the output file format that an rmarkdown file is being output to.
#' @return \code{character} name of output format (eg. 'html_document', or 'pdf_document').
output_format <- function() {
  rmarkdown::all_output_formats(knitr::current_input())
}