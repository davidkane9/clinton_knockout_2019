# Clinton, Engelhardt, Trussler 2018 "Knock Out Blows"
# Purpose: This file produces Figure 1 in the paper displaying attention to the campaign
# via google trends data.


rm(list = ls())

#Load Google Trends Data
goog <- read.csv("D:/Google Drive/Momentum/Replication/Data/Google Trends.csv")
#Load Primary Results
dem.results <- read.csv("D:/Google Drive/Momentum/Replication/Data/Dem Primary results.csv")
rep.results <- read.csv("D:/Google Drive/Momentum/Replication/Data/Rep Primary results.csv")

#Define the dates of primaries and their labels. 
dem.dates <- c(63,71,82,92,99,106,113,127,141,148,155,162,169,190)
dprim.labels <- c("IA","NH","NV","ST","MI","FL","AZ","WI","NY","AC","IN","WV","OR","CA")
rep.dates <- c(63,71,82,92,96,103,113,127,141,148,155)
rprim.labels <- c("IA","NH","SC","ST","LA","FL","AZ","WI","NY","AC","IN")

#Define dates by type
goog$dem <- 0
goog$rep <- 0
goog$both <- 0
goog$neither <- 0
goog$around <- 0

dem.dates.placebo <- c(dem.dates+1, dem.dates+2,dem.dates-1, dem.dates-2)
rep.dates.placebo <- c(rep.dates+1, rep.dates+2,rep.dates-1, rep.dates-2)

goog$dem[goog$time %in% dem.dates & !(goog$time %in% rep.dates)]<- 1
goog$rep[goog$time %in% rep.dates & !(goog$time %in% dem.dates)]<- 1
goog$both[goog$time %in% rep.dates & goog$time %in% dem.dates]<- 1
goog$around[goog$time %in% rep.dates.placebo | goog$time %in% dem.dates.placebo]<- 1
goog$neither[goog$dem==0 & goog$rep==0 & goog$both==0 & goog$around==0] <- 1

#Create Figure 1
first.days <- c(1,32,63,92,123,153,184)
date.labels <- c("Dec 1", "Jan 1", "Feb 1", "Mar 1", "Apr 1", "May 1", "Jun 1")

pdf(file="D:/Google Drive/Momentum/Replication/Figures/Main/CET_Figure1.pdf", width=11, height=8.5)
plot(goog$time, goog$Volume, type="n", ylab="Search Volume", xlab="Time", axes=F, ylim=c(0,120))
points(goog$time[goog$neither!=1], goog$Volume[goog$neither!=1], pch=2)
points(goog$time[goog$neither==1], goog$Volume[goog$neither==1], pch=16)
#points(goog$time, goog$pred, type="l", col="gray80", lty=2)
legend("topleft", c("Election Days +/- 2", "Placebo Days"),
       pch=c(2,16), bty = "n")
axis(side=2, at=seq(0,100,20))
axis(side=1, at=first.days, labels=date.labels)
dev.off()


