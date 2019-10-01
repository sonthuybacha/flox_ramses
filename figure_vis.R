# load dependencies
library(ggplot2)
library(chron)
library(latex2exp)
library(extrafont)
par(family = "LM Roman 10")

################################
# read bulk data into memory
################################

# read in file and prepare for processing
hold <- read.csv("./out/sample.csv",stringsAsFactors = FALSE,header = FALSE)
hold <- hold[,c(1,2,3,4)]
hold <- hold[which(hold[,3] != 0),]
hold <- hold[which(hold[,4] != "NaN"),]
names(hold) <- c("Pond","DateTime","Wavelength","Intensity")
hold[,2] <- chron(times = gsub(".*\\s+","",hold[,2]))
# remove anomalous data points due to human error
hold <- hold[which(hold[,2] != "12:14:16" & hold[,2] != "13:44:16"),]

################################
# process figure 1
################################

# get necessary values from time range
store <- hold[which(hold[,2] >= "11:30:00" & hold[,2] <= "12:30:00" & hold[,1] != "SAM_8622"),]
# create average of intensities across discrete wavelengths for main plot
spec <- lapply(unique(store[,1]),function(x){
  return(cbind("Pond"=x,aggregate(Intensity~Wavelength,data=store[which(store[,1] == x),],
                                  FUN = function(x) c(mean=mean(x),min=min(x),max=max(x)))))
})
spec <- do.call("rbind",spec)
spec <- cbind(spec,as.data.frame(spec$Intensity))
spec <- spec[-3]
png("./out/fig_1.png",width=1600,height=800)
# plot charts for comparison
g <- ggplot(data = spec) +
  geom_ribbon(aes(x=Wavelength,ymin=min,ymax=max,fill=Pond),alpha=0.4) +
  geom_line(data = spec[which(spec[,1]=="SAM_8624"),],aes(x=Wavelength,y=mean),color="black",size=0.3,alpha=0.6) +
  geom_line(data = spec[which(spec[,1]=="SAM_8623"),],aes(x=Wavelength,y=mean),color="black",size=0.3,alpha=0.6) +
  geom_point(aes(x=Wavelength,y=mean),colour="black",size =0.3,alpha=0.5) +
  ylab(TeX("L$_u$ $\\[mW$ $m^{-2}$ $nm^{-1}$ $sr^{-1}\\]$")) +
  xlab(TeX("$\\lambda$ $\\[nm\\]$")) +
  theme_bw() +
  theme(legend.title=element_text(face="bold"),
        legend.position = c(0.92,0.88),
        legend.background = element_rect(fill="transparent"),
        text = element_text(size=40),
        legend.text=element_text(size=35)) +
  guides(fill = guide_legend(override.aes = list(size=15))) +
  scale_fill_manual(name="Pond",
                      breaks=c("SAM_8623","SAM_8624"),
                      labels=c("9B","9D"),
                      values=c("green","blue")) +
  scale_x_continuous(breaks = round(seq(min(spec$Wavelength), max(spec$Wavelength), by = 100),-2)) +
  scale_y_continuous(breaks = round(seq(min(spec$min), max(spec$max), by = 2),1))
print(g)
dev.off()

################################
# process figure 2
################################

# convert date/time format to numeric
hold[,2] <- (as.numeric(as.POSIXct(paste("2014-01-01", hold[,2]))) -
        as.numeric(as.POSIXct("2014-01-01 0:0:0")))/60^2
# define variables to look through
s1 <- hold[which(hold[,3] <= 688 & hold[,3] >= 683),]
s2 <- hold[which(hold[,3] <= 705 & hold[,3] >= 695 & hold[,1] != "SAM_8624"),]
s3 <- hold[which(hold[,3] <= 710 & hold[,3] >= 700 & hold[,1] != "SAM_8623"),]
# start wit first variable
s1 <- s1[,-3]
s2 <- s2[,-3]
s3 <- s3[,-3]
# create average of intensities across discrete times for main plot
s1 <- lapply(unique(s1[,1]),function(x){
  return(cbind("Pond"=x,aggregate(Intensity~DateTime,data=s1[which(s1[,1] == x),],
                                  FUN = function(x) c(mean=mean(x),min=min(x),max=max(x)))))
})
s1 <- do.call("rbind",s1)
s1 <- cbind(s1,as.data.frame(s1$Intensity))
s1 <- s1[-3]
s1[which(s1[,1] != "SAM_8622"),c(3,4,5)] <- 100*s1[which(s1[,1] != "SAM_8622"),c(3,4,5)]/s1[which(s1[,1] == "SAM_8622"),3]
s1 <- s1[-which(s1[,1] == "SAM_8622"),]
s1$description <- "9B_685 vs. 9D_685"
# create average of intensities across discrete times for main plot
s2 <- lapply(unique(s2[,1]),function(x){
  return(cbind("Pond"=x,aggregate(Intensity~DateTime,data=s2[which(s2[,1] == x),],
                                  FUN = function(x) c(mean=mean(x),min=min(x),max=max(x)))))
})
s2 <- do.call("rbind",s2)
s2 <- cbind(s2,as.data.frame(s2$Intensity))
s2 <- s2[-3]
s2[which(s2[,1] != "SAM_8622"),c(3,4,5)] <- 100*s2[which(s2[,1] != "SAM_8622"),c(3,4,5)]/s2[which(s2[,1] == "SAM_8622"),3]
s2 <- s2[-which(s2[,1] == "SAM_8622"),]
s2[,1] <- "SAM_8623_700"
s2 <- rbind(s2,s1[which(s1[,1] == "SAM_8623"),c(1:5)])
s2$description <- "9B_685 vs. 9B_700"
# create average of intensities across discrete times for main plot
s3 <- lapply(unique(s3[,1]),function(x){
  return(cbind("Pond"=x,aggregate(Intensity~DateTime,data=s3[which(s3[,1] == x),],
                                  FUN = function(x) c(mean=mean(x),min=min(x),max=max(x)))))
})
s3 <- do.call("rbind",s3)
s3 <- cbind(s3,as.data.frame(s3$Intensity))
s3 <- s3[-3]
s3[which(s3[,1] != "SAM_8622"),c(3,4,5)] <- 100*s3[which(s3[,1] != "SAM_8622"),c(3,4,5)]/s3[which(s3[,1] == "SAM_8622"),3]
s3 <- s3[-which(s3[,1] == "SAM_8622"),]
s3[,1] <- "SAM_8624_705"
s3 <- rbind(s3,s1[which(s1[,1] == "SAM_8624"),c(1:5)])
s3$description <- "9D_685 vs. 9D_705"
# make combined dataframe
s <- rbind(s1,s2,s3)
s$description <- factor(s$description,levels=unique(s$description))
# plot figure
png("./out/fig_2.png",width=1600,height=800)
g <- ggplot(data = s) +
  geom_ribbon(aes(x=DateTime,ymin=min,ymax=max,fill=Pond),alpha=0.4) +
  geom_line(data = s[which(s[,1]=="SAM_8624"),],aes(x=DateTime,y=mean),color="black",size=0.3,alpha=0.6) +
  geom_line(data = s[which(s[,1]=="SAM_8623"),],aes(x=DateTime,y=mean),color="black",size=0.3,alpha=0.6) +
  geom_line(data = s[which(s[,1]=="SAM_8624_705"),],aes(x=DateTime,y=mean),color="black",size=0.3,alpha=0.6) +
  geom_line(data = s[which(s[,1]=="SAM_8623_700"),],aes(x=DateTime,y=mean),color="black",size=0.3,alpha=0.6) +
  geom_point(aes(x=DateTime,y=mean),colour="black",size =0.3,alpha=0.5) +
  ylab(TeX("Reflectance $\\[$sr^{-1}$ $\\%\\]$")) +
  xlab(TeX("Time of Day $\\[hrs\\]$")) +
  xlim(c(9,18.1)) +
  theme_bw() +
  theme(legend.title=element_text(face="bold"),
        #legend.position = c(0.12,0.8),
        legend.background = element_rect(fill="transparent"),
        text = element_text(size=35),
        strip.text = element_text(size=30),
        legend.text=element_text(size=30)) +
  guides(fill = guide_legend(override.aes = list(size=15))) +
  scale_fill_manual(name="Pond_Wavelength",
                    breaks=c("SAM_8623","SAM_8624","SAM_8623_700","SAM_8624_705"),
                    labels=c("9B_685","9D_685","9B_700","9D_705"),
                    values=c("blue","green","purple","darkgreen")) +
  # scale_x_continuous(breaks = c(round(seq(min(s$DateTime), max(s$DateTime),by=2),0),18)) +
  # scale_y_continuous(breaks = round(seq(min(s$min), max(s$max), by = 0.5),0)) +
  facet_wrap(~description)
print(g)
dev.off()
# additional figure 2a
png("./out/fig_2a.png",width=1200,height=1000)
g <- ggplot(data = s3) +
  geom_ribbon(aes(x=DateTime,ymin=min,ymax=max,fill=Pond),alpha=0.4) +
  geom_line(data = s[which(s[,1]=="SAM_8624"),],aes(x=DateTime,y=mean),color="black",size=0.3,alpha=0.6) +
  # geom_line(data = s[which(s[,1]=="SAM_8623"),],aes(x=DateTime,y=mean),color="black",size=0.3,alpha=0.6) +
  geom_line(data = s[which(s[,1]=="SAM_8624_705"),],aes(x=DateTime,y=mean),color="black",size=0.3,alpha=0.6) +
  # geom_line(data = s[which(s[,1]=="SAM_8623_700"),],aes(x=DateTime,y=mean),color="black",size=0.3,alpha=0.6) +
  geom_point(aes(x=DateTime,y=mean),colour="black",size =0.3,alpha=0.5) +
  ylab(TeX("Reflectance $\\[$sr^{-1}$ $\\%\\]$")) +
  xlab(TeX("Time of Day $\\[hrs\\]$")) +
  xlim(c(9,18.1)) +
  theme_bw() +
  theme(legend.title=element_text(face="bold"),
        #legend.position = c(0.12,0.8),
        legend.background = element_rect(fill="transparent"),
        text = element_text(size=35),
        strip.text = element_text(size=30),
        legend.text=element_text(size=30)) +
  guides(fill = guide_legend(override.aes = list(size=15))) +
  scale_fill_manual(name="Pond_Wavelength",
                    breaks=c("SAM_8624","SAM_8624_705"),
                    labels=c("9D_685","9D_705"),
                    values=c("green","darkgreen"))
  # scale_x_continuous(breaks = c(round(seq(min(s$DateTime), max(s$DateTime),by=2),0),18)) +
  # scale_y_continuous(breaks = round(seq(min(s$min), max(s$max), by = 0.5),0)) +
  # facet_wrap(~description)
print(g)
dev.off()
