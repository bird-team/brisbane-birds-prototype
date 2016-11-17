#' Create dummy image
#'
#' This function creates a dummy image to represent that the species' picture is missing.
#' @param path \code{character} file path to save image.
#' @return None. This function is used for the side effect of creating the image.
dummy.image <- function(path='data/book-resources/missing.png') {
  png(path, height=100, width=100)
  par(bg='gray80', mar=c(0,0,0,0))
  plot(0, type='n', axes=FALSE, xlab='', ylab='', ylim=c(-1,1), xlim=c(-1,1))
  text(x=0, y=0, '?', cex=10, col='black')
  dev.off()
}

#' Deparse methods in R6 object
#'
#' This function deparses the methods in an R6 object.
#' @param x \code{R6} object.
#' @return \code{list} with deparsed methods.
deparse.methods <- function(x) {
  l <- as.list(x)
  ret <- list()
  for (x in names(l))
    if (inherits(l[[x]], 'function'))
      ret <- append(ret, structure(list(deparse(l[[x]])), .Names = x))
  return(ret)
}
