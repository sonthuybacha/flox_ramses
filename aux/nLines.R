nLines <- function(file){
  testcon <- file(file,open="r")
  readsizeof <- 20000
  nooflines <- 0
  while((linesread <- length(readLines(testcon,readsizeof))) > 0){
    nooflines <- nooflines+linesread
  }
  close(testcon)
  return(nooflines)
}