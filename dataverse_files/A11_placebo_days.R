# Clinton, Engelhardt, Trussler 2018 "Knock Out Blows"
# Purpose: This file further investigates the magnitude and predictive power of shocks on non-primary days. 

rm(list = ls())
library(foreign)
library(xtable)

#Load SM Data
load("D:/Google Drive/Momentum/Replication/Data/Survey Monkey Data.Rdata")

#Load Primary Results
dem.results <- read.csv("D:/Google Drive/Momentum/Replication/Data/Dem Primary results.csv")
rep.results <- read.csv("D:/Google Drive/Momentum/Replication/Data/Rep Primary results.csv")


#Define the dates of primaries and their labels. 
dem.dates <- c(63,71,82,92,99,106,113,127,141,148,155,162,169,190)
dprim.labels <- c("IA","NH","NV","ST","MI","FL","AZ","WI","NY","AC","IN","WV","OR","CA")
rep.dates <- c(63,71,82,92,96,103,113,127,141,148,155)
rprim.labels <- c("IA","NH","SC","ST","LA","FL","AZ","WI","NY","AC","IN")

##########################################
### Inductive look at day imporance
##########################################

ITS.Model.Dem <- function(day){
#Start
sm.model <- sm.dweighted.pid
#make day before primary 0
sm.model$time <- sm.model$time - (day)
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


jump<- coef(model)["rho"]

sm.model <- sm.model[!is.na(sm.model$clinton) & !is.na(sm.model$rho),]
sm.model$prediction <- predict(model)
sm.model$vote.pred <- NA
sm.model$vote.pred[sm.model$prediction>=50]<- 100
sm.model$vote.pred[sm.model$prediction<50]<- 0
sm.model$error<- NA
sm.model$error[sm.model$clinton==sm.model$vote.pred] <- 0
sm.model$error[sm.model$clinton!=sm.model$vote.pred] <- 1

error <- mean(sm.model$error, na.rm=T)

estimate <- c(jump,error)
return(estimate)
 }
 

 
#Make vector of all days
days.placebo <- unique(sm.dweighted.pid$time)
#Truncate (model can't run with too few data in the tails)
days.placebo <- days.placebo[days.placebo>=17 & days.placebo <=195]
days.placebo <- days.placebo[!is.na(days.placebo)]
days.placebo <- sort(days.placebo)

jump <- rep(NA, length(days.placebo))
pred.error <- rep(NA, length(days.placebo))

training.data <- cbind.data.frame(days.placebo, jump, pred.error)

training.data$primary <- 0
training.data$primary[training.data$days.placebo %in% dem.dates]<- 1
 

#Loop across all days
 for(i in 1:nrow(training.data)){
training.data[i,2:3] <- ITS.Model.Dem(training.data$days.placebo[i]) 
}

#Save for Regression Tree Analysis
save(training.data, file="D:/Google Drive/Momentum/Replication/Data/DemTrainingData.Rdata")

training.data$pred.error  <- training.data$pred.error *100

pdf(file="D:/Google Drive/Momentum/Replication/Figures/Appendix/FigureA28a.pdf")
plot(training.data$days.placebo[training.data$primary==0], training.data$jump[training.data$primary==0], type="n",
     ylim=c(-10,10), ylab="Estimated Shock to Time Trend", xlab="Time")
abline(h=0, lty=2)
points(training.data$days.placebo[training.data$primary==0], training.data$jump[training.data$primary==0])
points(training.data$days.placebo[training.data$primary==1], training.data$jump[training.data$primary==1], pch=16) 
legend("topleft", "Primary Days", pch=16)
dev.off()

#Set naive model error rate
sm.model <- sm.dweighted.pid

model <- lm(clinton ~ 
              time  + time2 + time3 + time4 + time5 + time6  +
              factor(racethn4)*age6*factor(educ)*factor(gender)
            , data=sm.model, weights = DayWeights)

sm.model <- sm.model[!is.na(sm.model$clinton),]
sm.model$prediction <- predict(model)
sm.model$vote.pred <- NA
sm.model$vote.pred[sm.model$prediction>=50]<- 100
sm.model$vote.pred[sm.model$prediction<50]<- 0
sm.model$error<- NA
sm.model$error[sm.model$clinton==sm.model$vote.pred] <- 0
sm.model$error[sm.model$clinton!=sm.model$vote.pred] <- 1

naive.error <- mean(sm.model$error, na.rm=T)*100


pdf(file="D:/Google Drive/Momentum/Replication/Figures/Appendix/FigureA28b.pdf")
plot(training.data$days.placebo[training.data$primary==0], training.data$pred.error[training.data$primary==0],
     ylim=c(35.7, 36), type="n", ylab="Prediction Error Rate from Model", xlab="Time")
abline(h=naive.error, col="darkred", lty=2)
points(training.data$days.placebo[training.data$primary==0], training.data$pred.error[training.data$primary==0])
points(training.data$days.placebo[training.data$primary==1], training.data$pred.error[training.data$primary==1], pch=16) 
legend("topleft", c("Primary Days", "Error Rate from No Interruption Model"), pch=c(16, NA), lty=c(NA, 2), col=c("black", "darkred"))
dev.off()

 
#Largest 20 Democratic Placebo Days

#Merge in actual dates
date.time <- cbind.data.frame(unique(sm.dweighted.pid$time),unique(sm.dweighted.pid$Date))
names(date.time) <- c("time","Date")
placebo.shocks <- merge(training.data,date.time, by.x="days.placebo", by.y="time")

#Remove primary dates +-2
dem.dates.placebo <- sort(c(dem.dates, dem.dates+1, dem.dates+2, dem.dates-1, dem.dates-2))

placebo.shocks <- placebo.shocks[!(placebo.shocks$days.placebo %in% dem.dates.placebo),]
placebo.shocks$Date <- as.character(placebo.shocks$Date)
placebo.shocks <- placebo.shocks[order(abs(placebo.shocks$jump), decreasing=T),]

keep <- c("jump", "Date")
placebo.shocks <- placebo.shocks[keep]

#Create Table A14
print(xtable(placebo.shocks[1:20,]), include.rownames=F)

  
####REPUBLICANS#####
ITS.Model.Rep <- function(day){
  #Start
  sm.model <- sm.rweighted.pid
  #make day before primary 0
  sm.model$time <- sm.model$time - (day)
  #Get rid of those interviewed on primary day
  #sm.model$trump[sm.model$time==0] <- NA
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
  model <- lm(trump ~ 
                time  + time2 + time3 + time4 + time5 + time6 + rho +
                factor(racethn4)*age6*factor(educ)*factor(gender)
              , data=sm.model, weights = DayWeights)
  
  
  jump<- coef(model)["rho"]
  
  sm.model <- sm.model[!is.na(sm.model$trump) & !is.na(sm.model$rho),]
  sm.model$prediction <- predict(model)
  sm.model$vote.pred <- NA
  sm.model$vote.pred[sm.model$prediction>=50]<- 100
  sm.model$vote.pred[sm.model$prediction<50]<- 0
  sm.model$error<- NA
  sm.model$error[sm.model$trump==sm.model$vote.pred] <- 0
  sm.model$error[sm.model$trump!=sm.model$vote.pred] <- 1
  
  error <- mean(sm.model$error, na.rm=T)
  
  estimate <- c(jump,error)
  return(estimate)
}



#Make vector of all days
days.placebo <- unique(sm.rweighted.pid$time)
#Truncate (model can't run with too few data in the tails)
days.placebo <- days.placebo[days.placebo>=17 & days.placebo <=156]
days.placebo <- days.placebo[!is.na(days.placebo)]
days.placebo <- sort(days.placebo)

jump <- rep(NA, length(days.placebo))
pred.error <- rep(NA, length(days.placebo))

training.data <- cbind.data.frame(days.placebo, jump, pred.error)

training.data$primary <- 0
training.data$primary[training.data$days.placebo %in% rep.dates]<- 1


#Loop across all days
for(i in 1:nrow(training.data)){
  training.data[i,2:3] <- ITS.Model.Rep(training.data$days.placebo[i]) 
}

save(training.data, file="D:/Google Drive/Momentum/Replication/Data/RepTrainingData.Rdata")

training.data$pred.error  <- training.data$pred.error *100

pdf(file="D:/Google Drive/Momentum/Replication/Figures/Appendix/FigureA28a.pdf")
plot(training.data$days.placebo[training.data$primary==0], training.data$jump[training.data$primary==0], type="n",
     ylim=c(-10,10), ylab="Estimated Shock to Time Trend", xlab="Time")
abline(h=0, lty=2)
points(training.data$days.placebo[training.data$primary==0], training.data$jump[training.data$primary==0])
points(training.data$days.placebo[training.data$primary==1], training.data$jump[training.data$primary==1], pch=16) 
legend("topleft", "Primary Days", pch=16)
dev.off()

#Set naive model error rate
sm.model <- sm.rweighted.pid

model <- lm(trump ~ 
              time  + time2 + time3 + time4 + time5 + time6  +
              factor(racethn4)*age6*factor(educ)*factor(gender)
            , data=sm.model, weights = DayWeights)

sm.model <- sm.model[!is.na(sm.model$trump),]
sm.model$prediction <- predict(model)
sm.model$vote.pred <- NA
sm.model$vote.pred[sm.model$prediction>=50]<- 100
sm.model$vote.pred[sm.model$prediction<50]<- 0
sm.model$error<- NA
sm.model$error[sm.model$trump==sm.model$vote.pred] <- 0
sm.model$error[sm.model$trump!=sm.model$vote.pred] <- 1

naive.error <- mean(sm.model$error, na.rm=T)*100


pdf(file="D:/Google Drive/Momentum/Replication/Figures/Appendix/FigureA28b.pdf")
plot(training.data$days.placebo[training.data$primary==0], training.data$pred.error[training.data$primary==0],
     ylim=c(35.6, 36), type="n", ylab="Prediction Error Rate from Model", xlab="Time")
abline(h=naive.error, col="darkred", lty=2)
points(training.data$days.placebo[training.data$primary==0], training.data$pred.error[training.data$primary==0])
points(training.data$days.placebo[training.data$primary==1], training.data$pred.error[training.data$primary==1], pch=16) 
legend("topleft", c("Primary Days", "Error Rate from No Interruption Model"), pch=c(16, NA), lty=c(NA, 2), col=c("black", "darkred"))
dev.off()

#Largest 20 Republican Placebo Days

#Merge in actual dates
date.time <- cbind.data.frame(unique(sm.rweighted.pid$time),unique(sm.rweighted.pid$Date))
names(date.time) <- c("time","Date")
placebo.shocks <- merge(training.data,date.time, by.x="days.placebo", by.y="time")

#Remove primary dates +-2
rep.dates.placebo <- sort(c(rep.dates, rep.dates+1, rep.dates+2, rep.dates-1, rep.dates-2))

placebo.shocks <- placebo.shocks[!(placebo.shocks$days.placebo %in% rep.dates.placebo),]
placebo.shocks$Date <- as.character(placebo.shocks$Date)
placebo.shocks <- placebo.shocks[order(abs(placebo.shocks$jump), decreasing=T),]

keep <- c("jump", "Date")
placebo.shocks <- placebo.shocks[keep]

#Create Table A15
print(xtable(placebo.shocks[1:20,]), include.rownames=F)


