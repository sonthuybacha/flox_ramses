#!/usr/bin/env Rscript
# -*- coding: utf-8 -*-

###########################
# source key functions
###########################

source("./aux/cleanUp.R", encoding = "UTF-8")
source("./aux/nLines.R", encoding = "UTF-8")
source("./aux/disect.R", encoding = "UTF-8")

###########################
# create layers
# create source functions
###########################

# note: subjective to most recent run
megalist <- list.files(paste(getwd(), "/data/flox", sep=""), full.names=TRUE)
megalist <- megalist[-c(1:3)]
megalist <- megalist[-c(5,6,55,56)]
myList <- list()
final.results.fine <- list()
final.results.full <- list()
p = 0.0245
for(i in 1:(length(megalist)/4)){
  myList[[i]] <- megalist[c((4*i-3):(4*i))]
}

###########################
# create variables
###########################

cal.fine <- read.csv(paste(getwd(),"/data/flox/00_CAL_Fine.csv", sep=""), sep = ";", header = F, stringsAsFactors = FALSE)
cal.fine <- cleanUp(cal.fine)
cal.full <- read.csv(paste(getwd(),"/data/flox/00_CAL_Full.csv", sep=""), sep = ";", header = F, stringsAsFactors = FALSE)
cal.full <- cleanUp(cal.full)

###########################
# create workflows per 4 main entries in megalist
###########################

pb.overall <- txtProgressBar(min = 0, max = length(myList), initial = 0, char = "=",
                             width = options()$width, style = 3, file = "")
start.time <- Sys.time()
for(i in 1:length(myList)){
  result.fine <- data.frame(matrix(ncol = 13))
  names(result.fine) <- c("luFrac", "lskyFrac", "edFrac1", "edFrac2", "result.fine.1", "result.fine.2", "wl", "date", "time", "lat", "lon", "i", "j")
  result.full <- data.frame(matrix(ncol = 13))
  names(result.full) <- c("luFrac", "lskyFrac", "edFrac1", "edFrac2", "result.full.1", "result.full.2", "wl", "date", "time", "lat", "lon", "i", "j")
  # can add new lsky possibilities here, but exception of last case
  lsky.fine <- disect(myList[[i]][1])
  lsky.full <- disect(myList[[i]][2])
  lu.fine <- disect(myList[[i]][3])
  lu.full <- disect(myList[[i]][4])
  if(i < length(myList)){
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
  w <- length(final.results.fine)
  # rune pipeline for fine
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
      date <- lu.fine[[2]][[j]][which(lu.fine[[2]][[j]] == "GPS_date=")+1,]
      time <- lu.fine[[2]][[j]][which(lu.fine[[2]][[j]] == "GPS_TIME_UTC=")+1,]
      result.fine[k,8] <- date
      result.fine[k,9] <- time
      lat <- lu.fine[[2]][[j]][which(lu.fine[[2]][[j]] == "GPS_lat=")+1,]
      result.fine[k,10] <- lat
      lon <- lu.fine[[2]][[j]][which(lu.fine[[2]][[j]] == "GPS_lon=")+1,]
      result.fine[k,11] <- lon
      result.fine[k,12] <- i
      result.fine[k,13] <- j
    }
    m = j + w
    final.results.fine[[m]] <- result.fine
  }
  w <- length(final.results.full)
  # run pipeline for full
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
      date <- lu.full[[2]][[j]][which(lu.full[[2]][[j]] == "GPS_date=")+1,]
      time <- lu.full[[2]][[j]][which(lu.full[[2]][[j]] == "GPS_TIME_UTC=")+1,]
      result.full[k,8] <- date
      result.full[k,9] <- time
      lat <- lu.full[[2]][[j]][which(lu.full[[2]][[j]] == "GPS_lat=")+1,]
      result.full[k,10] <- lat
      lon <- lu.full[[2]][[j]][which(lu.full[[2]][[j]] == "GPS_lon=")+1,]
      result.full[k,11] <- lon
      result.full[k,12] <- i
      result.full[k,13] <- j
    }
    m = j + w
    final.results.full[[m]] <- result.full
  }
  Sys.sleep(1/1000)
  setTxtProgressBar(pb.overall, i, title = NULL, label = NULL)
}
end.time <- Sys.time()
end.time - start.time
close(pb.overall)

###########################
# aggregation and writing
###########################

# aggregate lists
aggregate.fine <- do.call("rbind", lapply(final.results.fine, function(x) return(x)))
aggregate.full <- do.call("rbind", lapply(final.results.full, function(x) return(x)))
aggregate.fine <- aggregate.fine[c(12,13,8:11,7,1:6)]
aggregate.full <- aggregate.full[c(12,13,8:11,7,1:6)]
# write to file
write.csv(aggregate.fine, "./out/aggregateFine.csv", row.names = FALSE)
write.csv(aggregate.full, "./out/aggregateFull.csv", row.names = FALSE)
