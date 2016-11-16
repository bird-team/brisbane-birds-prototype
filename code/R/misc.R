#' Create dummy image
#'
#' This function creates a dummy image to represent that the species' picture is missing.
#' @param parh \code{character} file path to save image.
#' @return None. This function is used for the side effect of creating the image.
dummy_image <- function(path='data/book-resources/missing.png') {
  png(path, height=100, width=100)
  par(bg='gray80', mar=c(0,0,0,0))
  plot(0, type='n', axes=FALSE, xlab='', ylab='', ylim=c(-1,1), xlim=c(-1,1))
  text(x=0, y=0, '?', cex=10, col='black')
  dev.off()
}
