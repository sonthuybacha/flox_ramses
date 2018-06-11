#### create layers ####

megalist <- list.files(paste(getwd(), "/Data", sep=""), full.names=TRUE)
megalist <- megalist[-c(1:3, 100)]
megalist <- megalist[-c(5,6,55,56)]
myList <- list()

for(i in 1:(length(megalist)/4)){
  myList[[i]] <- megalist[c((4*i-3):(4*i))]
}

#### create useful functions ####

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

disect <- function(file){
  
  myValues <- list()
  myDetails <- list()
  dim <- nLines(file)/6
  
  for(i in 1:dim){
    a <- (6*i-5)
    myValues[[i]] <- read.csv(file, sep = ";", skip = a, nrows = 5, header = F, stringsAsFactors = FALSE)
    x <- myValues[[i]][,1]
    myValues[[i]] <- as.data.frame(t(myValues[[i]][,-1]))
    colames(myValues[[i]]) <- x
    myValues[[i]] <- myValues[[i]][-which(rowSums(is.na(myValues[[i]])) == ncol(myValues[[i]])),]
    rownames(myValues[[i]]) <- NULL
  }
  
  for(i in 1:dim){
    a <- (6*i-6)
    myDetails[[i]] <- read.csv(file, sep = ";", skip = a, nrows = 1, header = F, stringsAsFactors = FALSE)
    myDetails[[i]] <- as.data.frame(t(myDetails[[i]]), stringsAsFactors = FALSE)
    rownames(myDetails[[i]]) <- NULL
  }
  
  result <- list(myValues, myDetails)
  return(result)
}

#### create additional variables ####

cal.fine <- read.csv(paste(getwd(),"/Data/00_CAL_Fine.csv", sep=""), sep = ";", header = F, stringsAsFactors = FALSE)
cal.fine <- cleanUp(cal.fine)

cal.full <- read.csv(paste(getwd(),"/Data/00_CAL_Full.csv", sep=""), sep = ";", header = F, stringsAsFactors = FALSE)
cal.full <- cleanUp(cal.full)

#### create workflows per 4 main entries in megalist ####

for(i in 1:length(myList[[1]])){
  
}


# for calculations, use grep and find entry after relevant text to get values

#### extra code ####

# define the following
# p = NA
# calc = NA
# 
# for(i in 1:5){
#   for(j in 1:nrow(myValues[[i]])){
#     myValues[[i]]$new[j] <- ((myValues[[i]][j,2]-myValues[[i]][j,5])/(as.numeric(myDetails[[i]][8,])*calc) - p*(myValues[[i]][j,2]-myValues[[i]][j,5])/(as.numeric(myDetails[[i]][8,])*calc))/
#       ((myValues[[i]][j,1]-myValues[[i]][j,4])/(as.numeric(myDetails[[i]][6,])*calc))
#   }
# }
# 
# View(myValues[[1]])