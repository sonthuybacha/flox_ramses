disect <- function(file){
  myValues <- list()
  myDetails <- list()
  dim <- nLines(file)/6
  for(i in 1:dim){
    a <- (6*i-5)
    b <- (6*i-6)
    myValues[[i]] <- read.csv(file, sep = ";", skip = a, nrows = 5, header = F, stringsAsFactors = FALSE)
    x <- myValues[[i]][,1]
    myValues[[i]] <- as.data.frame(t(myValues[[i]][,-1]))
    colnames(myValues[[i]]) <- x
    myValues[[i]] <- myValues[[i]][-which(rowSums(is.na(myValues[[i]])) == ncol(myValues[[i]])),]
    rownames(myValues[[i]]) <- NULL
    myDetails[[i]] <- read.csv(file, sep = ";", skip = b, nrows = 1, header = F, stringsAsFactors = FALSE)
    myDetails[[i]] <- as.data.frame(t(myDetails[[i]]), stringsAsFactors = FALSE)
    rownames(myDetails[[i]]) <- NULL
  }
  result <- list(myValues, myDetails)
  return(result)
}
