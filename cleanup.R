cleanUp <- function(y){
  x <- y[,1]
  y <- as.data.frame(t(y[,-1]))
  colnames(y) <- x
  rownames(y) <- NULL
  if(length(which(rowSums(is.na(y)) > 0) > 0)) {
    stop("error, please remove NA-filled rows and try again")
  }
  return(y)
}