#### new concept ####

# use last lsky value or second lsky value of next session depending on what is better approx
# use only one lsky per session, either last one recorded in current session or second one of next session depending on the abs(delta(t)) value
# cannot use the next version of lsky for last session, because does not exist
# add some if statements to direct decisions, add an if.exists above normal if statement

#### create layers ####

megalist <- list.files(paste(getwd(), "/Data", sep=""), full.names=TRUE)
megalist <- megalist[-c(1:3, 100)]
megalist <- megalist[-c(5,6,55,56)]
myList <- list()
p = 0.0245

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
    colnames(myValues[[i]]) <- x
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

for(i in 1:length(myList)){
  
  result.fine <- data.frame(matrix(ncol = 7))
  names(result.fine) <- c("LuFrac", "LskyFrac", "EdFrac1", "EdFrac2", "result.fine.1", "result.fine.2", "wl")
  
  result.full <- data.frame(matrix(ncol = 7))
  names(result.full) <- c("LuFrac", "LskyFrac", "EdFrac1", "EdFrac2", "result.full.1", "result.full.2", "wl")
  
  # can add new lsky possibilities here, but exception of last case
  
  lsky.fine <- disect(myList[[i]][1])
  lsky.full <- disect(myList[[i]][2])
  lu.fine <- disect(myList[[i]][3])
  lu.full <- disect(myList[[i]][4])
  
  if(i<length(myList)){
    lsky.fine2 <- disect(myList[[i+1]][1])
    lsky.full2 <- disect(myList[[i+1]][2])
    
    # test for fine
    
    orig.fine <- as.numeric(lu.fine[[2]][[2]][which(lu.fine[[2]][[1]] == "GPS_TIME_UTC=")+1,])
    session.fine <- as.numeric(lsky.fine[[2]][[length(lsky.fine[[2]])]][which(lsky.fine[[2]][[1]] == "GPS_TIME_UTC=")+1,])
    session.fine2 <- as.numeric(lsky.fine2[[2]][[2]][which(lsky.fine2[[2]][[1]] == "GPS_TIME_UTC=")+1,])
    check.fine <- c(abs(orig.fine-session.fine), abs(orig.fine-session.fine2))
    checkIndex.fine <- which(check.fine == min(check.fine))
    
    # test for full
    
    orig.full <- as.numeric(lu.full[[2]][[2]][which(lu.full[[2]][[1]] == "GPS_TIME_UTC=")+1,])
    session.full <- as.numeric(lsky.full[[2]][[length(lsky.full[[2]])]][which(lsky.full[[2]][[1]] == "GPS_TIME_UTC=")+1,])
    session.full2 <- as.numeric(lsky.full2[[2]][[2]][which(lsky.full2[[2]][[1]] == "GPS_TIME_UTC=")+1,])
    check.full <- c(abs(orig.full-session.full), abs(orig.full-session.full2))
    checkIndex.full <- which(check.full == min(check.full))
    
  } else {
    checkIndex.fine <- 1
    checkIndex.full <- 1
  }
    
  # for fine
  for(j in 1:min(c(length(lu.fine[[1]]), length(lsky.fine[[1]])))){
    for(k in 1:1024){
      lufrac <- (lu.fine[[1]][[j]][k,2]-lu.fine[[1]][[j]][k,5])/(as.numeric(lu.fine[[2]][[j]][which(lu.fine[[2]][[j]] == "IT_VEG[us]=")+1,])*cal.fine[k,2])
      result.fine[k,1] <- lufrac
      
      if(checkIndex.fine == 1){
        lskyfrac <- ((p*(lsky.fine[[1]][[length(lsky.fine[[2]])]][k,2]-lsky.fine[[1]][[length(lsky.fine[[2]])]][k,5]))/(as.numeric(lsky.fine[[2]][[length(lsky.fine[[2]])]][which(lsky.fine[[2]][[length(lsky.fine[[2]])]] == "IT_VEG[us]=")+1,])*cal.fine[k,2]))
      } else if(checkIndex.fine == 2){
        lskyfrac <- ((p*(lsky.fine2[[1]][[2]][k,2]-lsky.fine2[[1]][[2]][k,5]))/(as.numeric(lsky.fine2[[2]][[2]][which(lsky.fine2[[2]][[2]] == "IT_VEG[us]=")+1,])*cal.fine[k,2]))
      }
      
      result.fine[k,2] <- lskyfrac
      
      edfrac1 <- (lu.fine[[1]][[j]][k,1]-lu.fine[[1]][[j]][k,4])/(as.numeric(lu.fine[[2]][[j]][which(lu.fine[[2]][[j]] == "IT_WR[us]=")+1,])*cal.fine[k,3])
      result.fine[k,3] <- edfrac1
      
      edfrac2 <- (lu.fine[[1]][[j]][k,3]-lu.fine[[1]][[j]][k,4])/(as.numeric(lu.fine[[2]][[j]][which(lu.fine[[2]][[j]] == "IT_WR[us]=")+1,])*cal.fine[k,3])
      result.fine[k,4] <- edfrac2
      
      result1 <- (lufrac-lskyfrac)/edfrac1
      result.fine[k,5] <- result1
      
      result2 <- (lufrac-lskyfrac)/edfrac2
      result.fine[k,6] <- result2
      
      result.fine[k,7] <- cal.fine[k,1]
    }
    write.csv(result.fine, file = paste(getwd(), "/Results/", "results_fine_", i ,"_", j, ".csv", sep=""), row.names = FALSE)
  }
  
  # for full
  for(j in 1:min(c(length(lu.full[[1]]), length(lsky.full[[1]])))){
    for(k in 1:1024){
      lufrac <- (lu.full[[1]][[j]][k,2]-lu.full[[1]][[j]][k,5])/(as.numeric(lu.full[[2]][[j]][which(lu.full[[2]][[j]] == "IT_VEG[us]=")+1,])*cal.full[k,2])
      result.full[k,1] <- lufrac
      
      if(checkIndex.full == 1){
        lskyfrac <- ((p*(lsky.full[[1]][[length(lsky.full[[2]])]][k,2]-lsky.full[[1]][[length(lsky.full[[2]])]][k,5]))/(as.numeric(lsky.full[[2]][[length(lsky.full[[2]])]][which(lsky.full[[2]][[length(lsky.full[[2]])]] == "IT_VEG[us]=")+1,])*cal.full[k,2]))
      } else if(checkIndex.full == 2){
        lskyfrac <- ((p*(lsky.full2[[1]][[2]][k,2]-lsky.full2[[1]][[2]][k,5]))/(as.numeric(lsky.full2[[2]][[2]][which(lsky.full2[[2]][[2]] == "IT_VEG[us]=")+1,])*cal.full[k,2]))
      }
      
      lskyfrac <- ((p*(lsky.full[[1]][[j]][k,2]-lsky.full[[1]][[j]][k,5]))/(as.numeric(lsky.full[[2]][[j]][which(lu.full[[2]][[j]] == "IT_VEG[us]=")+1,])*cal.full[k,2]))
      result.full[k,2] <- lskyfrac

      edfrac1 <- (lu.full[[1]][[j]][k,1]-lu.full[[1]][[j]][k,4])/(as.numeric(lu.full[[2]][[j]][which(lu.full[[2]][[j]] == "IT_WR[us]=")+1,])*cal.full[k,3])
      result.full[k,3] <- edfrac1

      edfrac2 <- (lu.full[[1]][[j]][k,3]-lu.full[[1]][[j]][k,4])/(as.numeric(lu.full[[2]][[j]][which(lu.full[[2]][[j]] == "IT_WR[us]=")+1,])*cal.full[k,3])
      result.full[k,4] <- edfrac2

      result1 <- (lufrac-lskyfrac)/edfrac1
      result.full[k,5] <- result1

      result2 <- (lufrac-lskyfrac)/edfrac2
      result.full[k,6] <- result2
      
      result.full[k,7] <- cal.full[k,1]
    }
    write.csv(result.full, file = paste(getwd(), "/Results/", "results_full_", i ,"_", j, ".csv", sep=""), row.names = FALSE)
  }
}

# for calculations, use grep and find entry after relevant text to get values

#### extra code ####

# define the following
# p = NA
# calc = NA
# 
# 
# View(myValues[[1]])