# Clinton, Engelhardt, Trussler 2018 "Knock Out Blows"
# Purpose: This file produces the over-time figures for the main ITS analysis

rm(list = ls())
library(foreign)
library(xtable)

#Load SM Data
load("D:/Google Drive/Momentum/Replication/Data/Survey Monkey Data.Rdata")

#Define the dates of primaries and their labels. 
dem.dates <- c(63,71,82,92,99,106,113,127,141,148,155,162,169,190)
dprim.labels <- c("IA","NH","NV","ST","MI","FL","AZ","WI","NY","AC","IN","WV","OR","CA")
rep.dates <- c(63,71,82,92,96,103,113,127,141,148,155)
rprim.labels <- c("IA","NH","SC","ST","LA","FL","AZ","WI","NY","AC","IN")

####DEMOCRATS#####

pdf(file="D:/Google Drive/Momentum/Replication/Figures/Appendix/FigureA13.pdf", height=30, width=25)
par(mfrow=c(7,2))
for(i in 1:length(dem.dates)){
  #Start
  sm.model <- sm.dweighted.pid
  #make day before primary 0
  sm.model$time <- sm.model$time - (dem.dates[i])
  #Get rid of those interviewed on primary day
  #sm.model$clinton[sm.model$time==0] <- NA
  #Create rho
  sm.model$rho[sm.model$time<0]<- 0
  sm.model$rho[sm.model$time>0] <-1 
  #Make polynomials 
  sm.model$time2 <- sm.model$time^2
  sm.model$time3 <- sm.model$time^3
  sm.model$time4 <- sm.model$time^4
  sm.model$time5 <- sm.model$time^5
  sm.model$time6 <- sm.model$time^6
  
  
  
  #Run model
  model <- lm(clinton ~ 
                time  + time2 + time3 + time4 + time5 + time6 + rho +
                factor(racethn4)*age6*factor(educ)*factor(gender)
              , data=sm.model, weights = DayWeights)
  
  time <- seq(-30,30,1)
  time2 <- time^2
  time3 <- time^3
  time4 <- time^4
  time5 <- time^5
  time6 <- time^6
  time7 <- time^7
  time8 <- time^8
  rho <- rep(NA,length(time))
  rho[time<0]<-0
  rho[time>0]<-1
  racethn4 <- rep("White", length(time))
  age6 <- rep("55-64", length(time))
  educ <- rep("College graduate", length(time))
  gender <- rep("Female", length(time))
  
  sm.pred <- cbind.data.frame(time,time2,time3,time4,time5,time6,time7,time8,rho,racethn4,age6,educ,gender)
  prediction <- predict(model, newdata=sm.pred, interval="confidence")
  
  model2<-lm(clinton ~ factor(time) + factor(racethn4)*age6*
               factor(educ)*factor(gender), 
             data=sm.model, weights = DayWeights) 
  
  time <- seq(-30,30,1)
  time <- time[time %in% unique(sm.model$time)]
  racethn4 <- rep("White", length(time))
  age6 <- rep("55-64", length(time))
  educ <- rep("College graduate", length(time))
  gender <- rep("Female", length(time))
  
  sm.pred <- cbind.data.frame(time,racethn4,age6,educ,gender)
  
  prediction2 <- predict(model2, newdata=sm.pred, interval="confidence")
  
  time.full <- seq(-30,30,1)
  
  plot(time.full,prediction[,1], type="l", ylim=c(0,100),
       xlab="Days", ylab="Predicted Clinton Support", main=dprim.labels[i])
  lines(time.full,prediction[,2],lty=2)
  lines(time.full,prediction[,3],lty=2)
  points(time, prediction2[,1],col="gray80" )
  segments(time, prediction2[,1],time, prediction2[,2],col="gray80")
  segments(time, prediction2[,1],time, prediction2[,3],col="gray80")
  abline(v=0, lty=2)
  legend("topleft","Average Daily Support", pch=1, col="gray80")
}
dev.off()


##REPUBLICANS

#Create figures
pdf(file="D:/Google Drive/Momentum/Replication/Figures/Appendix/FigureA14.pdf", height=30, width=25)
par(mfrow=c(7,2))
for(i in 1:length(rep.dates)){
  sm.model <- sm.rweighted.pid
  sm.model <- sm.model[sm.model$time<=157,]
  #make day before primary 0
  sm.model$time <- sm.model$time - (rep.dates[i])
  #Get rid of those interviewed on primary day
  #Create rho
  sm.model$rho[sm.model$time<0]<- 0
  sm.model$rho[sm.model$time>0] <-1 
  #Make polynomials 
  sm.model$time2 <- sm.model$time^2
  sm.model$time3 <- sm.model$time^3
  sm.model$time4 <- sm.model$time^4
  sm.model$time5 <- sm.model$time^5
  sm.model$time6 <- sm.model$time^6
  sm.model$time7 <- sm.model$time^7
  
  
  #Run model
  model <- lm(trump ~ 
                time  + time2 + time3 + time4 + time5 + time6 + rho +
                factor(racethn4)*age6*factor(educ)*factor(gender)
              , data=sm.model, weights = DayWeights)
  
  time <- seq(-30,30,1)
  time2 <- time^2
  time3 <- time^3
  time4 <- time^4
  time5 <- time^5
  time6 <- time^6
  time7 <- time^7
  time8 <- time^8
  rho <- rep(NA,length(time))
  rho[time<0]<-0
  rho[time>0]<-1
  racethn4 <- rep("White", length(time))
  age6 <- rep("55-64", length(time))
  educ <- rep("College graduate", length(time))
  gender <- rep("Female", length(time))
  
  sm.pred <- cbind.data.frame(time,time2,time3,time4,time5,time6,time7,time8,rho,racethn4,age6,educ,gender)
  prediction <- predict(model, newdata=sm.pred, interval="confidence")
  
  model2<-lm(trump ~ factor(time) + factor(racethn4)*age6*
               factor(educ)*factor(gender), 
             data=sm.model, weights = DayWeights) 
  
  time <- seq(-30,30,1)
  time <- time[time %in% unique(sm.model$time)]
  racethn4 <- rep("White", length(time))
  age6 <- rep("55-64", length(time))
  educ <- rep("College graduate", length(time))
  gender <- rep("Female", length(time))
  
  sm.pred <- cbind.data.frame(time,racethn4,age6,educ,gender)
  
  prediction2 <- predict(model2, newdata=sm.pred, interval="confidence")
  
  time.full <- seq(-30,30,1)
  
  plot(time.full,prediction[,1], type="l", ylim=c(0,100),
       xlab="Days", ylab="Predicted Trump Support", main=rprim.labels[i])
  lines(time.full,prediction[,2],lty=2)
  lines(time.full,prediction[,3],lty=2)
  points(time, prediction2[,1],col="gray80" )
  segments(time, prediction2[,1],time, prediction2[,2],col="gray80")
  segments(time, prediction2[,1],time, prediction2[,3],col="gray80")
  
  #points(clinton.daily$all.days - dem.dates[i], clinton.daily$clinton.average, col="gray80")
  #segments(clinton.daily$all.days - dem.dates[i], clinton.daily$clinton.average,clinton.daily$all.days - dem.dates[i], clinton.daily$up, col="gray80")
  #segments(clinton.daily$all.days - dem.dates[i], clinton.daily$clinton.average,clinton.daily$all.days - dem.dates[i], clinton.daily$down,col="gray80")
  abline(v=0, lty=2)
  legend("topleft","Average Daily Support", pch=1, col="gray80")
}
dev.off()


