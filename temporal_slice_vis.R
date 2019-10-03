# load dependencies
library(ggplot2)
library(chron)
library(latex2exp)
library(extrafont)
# par(family = "LM Roman 10")

################################
# read bulk data into memory
################################

# read in file and prepare for processing
hold <- read.csv("./out/sample_20190710.csv",stringsAsFactors = FALSE,header = FALSE)
# hold <- read.csv("./out/sample.csv",stringsAsFactors = FALSE,header = FALSE)
hold <- hold[,c(1,2,3,4)]
hold <- hold[which(hold[,3] != 0),]
hold <- hold[which(hold[,4] != "NaN"),]
names(hold) <- c("Pond","DateTime","Wavelength","Intensity")
hold[,2] <- chron(times = gsub(".*\\s+","",hold[,2]))
# remove anomalous data points due to human error
# temporary fix for 20190716
# hold <- hold[which(hold[,2] != "12:14:16" & hold[,2] != "13:44:16"),]
# temporary fix for 20190710
# hold <- hold[which(hold[,2] != "10:59:20" & hold[,2] != "11:05:50" & hold[,2] != "10:59:22"),]

################################
# perform sanity alignment check
################################

# minimize amount of data lost while creating alignment
# get necessary wavelengths which will form x-axis
store <- hold[which(hold[,3] >= 630 & hold[,3] <= 750),]
any(store[which(store[,1] == "SAM_8623"),2]-store[which(store[,1] == "SAM_8622"),2] > "00:02:00")
any(store[which(store[,1] == "SAM_8624"),2]-store[which(store[,1] == "SAM_8622"),2] > "00:02:00")
any(abs(store[which(store[,1] == "SAM_8623"),3]-store[which(store[,1] == "SAM_8622"),3]) > 3)
any(abs(store[which(store[,1] == "SAM_8624"),3]-store[which(store[,1] == "SAM_8622"),3]) > 3)

################################
# perform necessary calcs
################################

rel <- store
rel <- rel[which(rel[,1]!="SAM_8622"),]
# find reflectances for SAM_8623
rel[which(rel[,1] == "SAM_8623"),4] <- store[which(store[,1] == "SAM_8623"),4]/store[which(store[,1] == "SAM_8622"),4]
# find reflectances for SAM_8624
rel[which(rel[,1] == "SAM_8624"),4] <- store[which(store[,1] == "SAM_8624"),4]/store[which(store[,1] == "SAM_8622"),4]
names(rel)[4] <- "Reflectance"
# find time closest to noon for SAM_8623
noon_closest_8623 <- unique(rel[which(rel[,1] == "SAM_8623"),][which(abs(rel[which(rel[,1] == "SAM_8623"),2]-"12:00:00") == min(abs(rel[which(rel[,1] == "SAM_8623"),2]-"12:00:00"))),2])
noon_closest_8624 <- unique(rel[which(rel[,1] == "SAM_8624"),][which(abs(rel[which(rel[,1] == "SAM_8624"),2]-"12:00:00") == min(abs(rel[which(rel[,1] == "SAM_8624"),2]-"12:00:00"))),2])
# divide each set of readings by maxmimum
# assume all measurements are of same length
rel[which(rel[,1] == "SAM_8623"),4] <- rel[which(rel[,1] == "SAM_8623"),4]/rel[which(rel[,2] == noon_closest_8623 & rel[,1] == "SAM_8623"),4]
rel[which(rel[,1] == "SAM_8624"),4] <- rel[which(rel[,1] == "SAM_8624"),4]/rel[which(rel[,2] == noon_closest_8624 & rel[,1] == "SAM_8624"),4]
names(rel)[4] <- "Factor"
rel[,2] <- as.character(rel[,2])

################################
# bin timeslots and plot
################################

png("test_20190710.png",width=2000,height=1000)
g <- ggplot(rel) + geom_line(aes(x = Wavelength, y = Factor, color = DateTime)) + facet_wrap(~Pond) + theme(text=element_text(size=20))
print(g)
dev.off()
