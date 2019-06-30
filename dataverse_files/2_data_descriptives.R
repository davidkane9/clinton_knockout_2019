# Clinton, Engelhardt, Trussler 2018 "Knock Out Blows"
# Purpose: This file generates the daily response rate presented in the main text.

#----------------------------------------------------------------------------------------------------#
library(foreach)
setwd("~/google drive/momentum/Replication/")
load("./data/Survey Monkey Data.Rdata")
#----------------------------------------------------------------------------------------------------#
# The code below generates Section 2 Figure 1
#----------------------------------------------------------------------------------------------------#
DATE <- sort(unique(sm$Date))

N <- foreach(i = DATE, .combine = "c") %do% {
  table(sm$Date[which(sm$Date == i)])
}
LOESS <- loess.smooth(DATE, N)
# pdf("./figures/main/CET_Figure2.pdf", width = 8.75, height = 6.25)
plot(1:192, N, main = "", col = "grey", axes = F,
     xlab = "Date", ylab = "Number of Respondents", pch = 16)
lines(seq(1,192,length.out = 50), LOESS$y, lwd = 3, col = "black")
axis(1, at = c(1, 28, 59, 88, 119, 149, 180), 
     labels = c("December", "January", "February", "March", "April", "May", "June"))
axis(2, las = 1)
# dev.off()


#####################################################

#Clinton Trend

#####################################################
load("D:/Google Drive/Momentum/Replication/Data/Survey Monkey Data.Rdata")

days <- seq(1,192,1)
dprim.values <- c(1,63,71,82,92,99,106,113,127,141,148,155,162,169,190)
dprim.labels <- c("Dec 1, 2015","IA","NH","NV","ST","MI","FL","AZ","WI","NY","AC","IN","WV","OR","CA")


#Daily means from fitted model
all.days <- lm(clinton ~ factor(time), data=sm.dweighted, weights=DayWeights)

time <- unique(sm.dweighted$time)

time <- cbind.data.frame(time)
day.means <- predict(all.days, newdata=time,se.fit = T)

#Trend from 6th order model

trend <- lm(clinton ~ time + time2 + time3 + time4 + time5 + time6, data=sm.dweighted, weights = DayWeights)

#Just trend figure 
time <- seq(1,196,1)
smp <- as.data.frame(time)
smp$time2 <- smp$time^2
smp$time3 <- smp$time^3
smp$time4 <- smp$time^4
smp$time5 <- smp$time^5
smp$time6 <- smp$time^6
smp$time7 <- smp$time^7
pred <- predict(trend,newdata =smp,interval = "confidence")

time <- unique(sm.dweighted$time)


pdf(file="D:/Google Drive/Momentum/Replication/Figures/Main/CET_Figure3a.pdf")
plot(smp$time, pred[,1], type="l", ylim=c(0,100), axes=F, xlab="Time",ylab="Predicted Clinton Support")
lines(smp$time,pred[,2], lty=2)
lines(smp$time,pred[,3], lty=2)
points(time,day.means$fit,col="gray80")
segments(time,day.means$fit, time,day.means$fit+day.means$se.fit*1.96, col="gray90")
segments(time,day.means$fit, time,day.means$fit-day.means$se.fit*1.96, col="gray90")
legend("topleft",c("Daily Average % Supporting Clinton"),pch=c(1),col=c("gray80"))
axis(1,at=dprim.values,labels=dprim.labels,cex.axis=0.5)
axis(2,at=seq(0,100,25))
dev.off()



######################################################

### Trump Trend

#####################################################


days <- seq(1,192,1)
rprim.values <- c(1,63,71,82,92,96,103,113,127,141,148,155,162,190)
rprim.labels <- c("Dec 1, 2015","IA","NH","SC","ST","LA","FL","AZ","WI","NY","AC","IN","WV","CA")




#Daily means from fitted model
all.days <- lm(trump ~ factor(time), data=sm.rweighted, weights=DayWeights)

time <- unique(sm.rweighted$time)
time <- time[time<158]

time <- cbind.data.frame(time)
day.means <- predict(all.days, newdata=time,se.fit = T)

#Trend from 6th order model

trend <- lm(trump ~ time + time2 + time3 + time4 + time5 + time6, data=sm.rweighted, weights = DayWeights)

#Just trend figure 
time <- seq(1,162,1)
smp <- as.data.frame(time)
smp$time2 <- smp$time^2
smp$time3 <- smp$time^3
smp$time4 <- smp$time^4
smp$time5 <- smp$time^5
smp$time6 <- smp$time^6
smp$time7 <- smp$time^7
pred <- predict(trend,newdata =smp,interval = "confidence")

time <- unique(sm.rweighted$time)
time <- time[time<158]

pdf(file="D:/Google Drive/Momentum/Replication/Figures/Main/CET_Figure3b.pdf")
plot(smp$time, pred[,1], type="l", ylim=c(0,100), axes=F, xlab="Time",ylab="Predicted Trump Support")
lines(smp$time,pred[,2], lty=2)
lines(smp$time,pred[,3], lty=2)
points(time,day.means$fit,col="gray80")
segments(time,day.means$fit, time,day.means$fit+day.means$se.fit*1.96, col="gray90")
segments(time,day.means$fit, time,day.means$fit-day.means$se.fit*1.96, col="gray90")
legend("topleft",c("Daily Average % Supporting Trump"),pch=c(1),col=c("gray80"))
axis(1,at=rprim.values,labels=rprim.labels,cex.axis=0.5)
axis(2,at=seq(0,100,25))
dev.off()
