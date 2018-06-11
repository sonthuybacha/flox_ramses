myValues <- list()
myDetails <- list()

for(i in 1:5){
  a <- (6*i-5)
  myValues[[i]] <- read.csv("03_Lsky_F133403.CSV", sep = ";", skip = a, nrows = 5, header = F, stringsAsFactors = FALSE)
  x <- myValues[[i]][,1]
  myValues[[i]] <- as.data.frame(t(myValues[[i]][,-1]))
  colnames(myValues[[i]]) <- x
  myValues[[i]] <- myValues[[i]][-which(rowSums(is.na(myValues[[i]])) == ncol(myValues[[i]])),]
  rownames(myValues[[i]]) <- NULL
}

for(i in 1:5){
  a <- (6*i-6)
  myDetails[[i]] <- read.csv("03_Lsky_F133403.CSV", sep = ";", skip = a, nrows = 1, header = F, stringsAsFactors = FALSE)
  myDetails[[i]] <- as.data.frame(t(myDetails[[i]]), stringsAsFactors = FALSE)
  rownames(myDetails[[i]]) <- NULL
}

# define the following
p = NA
calc = NA

for(i in 1:5){
  for(j in 1:nrow(myValues[[i]])){
    myValues[[i]]$new[j] <- ((myValues[[i]][j,2]-myValues[[i]][j,5])/(as.numeric(myDetails[[i]][8,])*calc) - p*(myValues[[i]][j,2]-myValues[[i]][j,5])/(as.numeric(myDetails[[i]][8,])*calc))/
      ((myValues[[i]][j,1]-myValues[[i]][j,4])/(as.numeric(myDetails[[i]][6,])*calc))
  }
}

View(myValues[[1]])