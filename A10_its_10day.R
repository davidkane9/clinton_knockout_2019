## Momentum analysis ##
rm(list = ls())
library(foreign)
library(readstata13)
library(stargazer)

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
#Function adjusts dates to center on primary day. 
# Individuals differ in their value for rho, 0 if before, 1 if after
#Rho (time=0, rho=1) is trend adjusted shift in opinion after the primary. 
#Function returns Rho, its standard error, CI, and p-value
ITS.Model.Dem <- function(day){
  #Start
  sm.model <- sm.dweighted.pid
  #make day before primary 0
  sm.model$time <- sm.model$time - (day)
  sm.model <- sm.model[sm.model$time>=-10 & sm.model$time <= 10,]
  
  
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
  jump.upper <- jump + sqrt(vcov(model)["rho","rho"])*1.96
  jump.lower <- jump- sqrt(vcov(model)["rho","rho"])*1.96
  jump.se <- sqrt(vcov(model)["rho","rho"])
  jump.p <- summary(model)$coefficients["rho",4] 
  estimate <- c(jump,jump.upper,jump.lower,jump.se,jump.p)
  return(estimate)
}


# Run for each dem dates, save estimates in matrix
dem.constant.estimates <- matrix(NA, ncol=5, nrow=length(dem.dates))
for( i in 1:length(dem.dates)){
  dem.constant.estimates[i,]<-ITS.Model.Dem(dem.dates[i])
}


#This code creates a pdf with all of the over-time figures for the k cutpoints
# Because their are covariates in the model, these are predicted effects for a white woman
#college graduate age 55-64. Similarly, the daily averages are what is predicted for such an 
#indivudal (these averages pulled from a model with daily fixed effects and covariates.)

pdf(file="D:/Google Drive/Momentum/Replication/Figures/FigureA22.pdf", height=30, width=25)
par(mfrow=c(7,2))
for(i in 1:length(dem.dates)){
  #Start
  sm.model <- sm.dweighted.pid
  #make day before primary 0
  sm.model$time <- sm.model$time - (dem.dates[i])
  sm.model <- sm.model[sm.model$time>=-10 & sm.model$time <= 10,]
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
  
  time <- seq(-10,10,1)
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
  
  time <- seq(-10,10,1)
  time <- time[time %in% unique(sm.model$time)]
  racethn4 <- rep("White", length(time))
  age6 <- rep("55-64", length(time))
  educ <- rep("College graduate", length(time))
  gender <- rep("Female", length(time))
  
  sm.pred <- cbind.data.frame(time,racethn4,age6,educ,gender)
  
  prediction2 <- predict(model2, newdata=sm.pred, interval="confidence")
  
  time.full <- seq(-10,10,1)
  
  plot(time.full,prediction[,1], type="l", ylim=c(0,100),
       xlab="Days", ylab="Predicted Clinton Support", main=dprim.labels[i])
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


####REPUBLICANS#####

rep.dates <- c(63,71,82,92,96,103,113,127,141,148,155)
rprim.labels <- c("IA","NH","SC","ST","LA","FL","AZ","WI","NY","AC","IN")

#Generate function for Republicans  
ITS.Model.Rep <- function(day){
  #Start
  sm.model <- sm.rweighted.pid
  #make day before primary 0
  sm.model$time <- sm.model$time - (day)
  sm.model <- sm.model[sm.model$time>=-10 & sm.model$time <= 10,]
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
                time +time2 +time3 +time4 + time5 + time6 + rho +
                factor(racethn4)*age6*factor(educ)*factor(gender)
              , data=sm.model, weights = DayWeights)
  
  jump <- coef(model)["rho"]
  jump.upper <- jump + sqrt(vcov(model)["rho","rho"])*1.96
  jump.lower <- jump- sqrt(vcov(model)["rho","rho"])*1.96
  jump.se <- sqrt(vcov(model)["rho","rho"])
  jump.p <- summary(model)$coefficients["rho",4] 
  estimate <- c(jump,jump.upper,jump.lower,jump.se,jump.p)
  return(estimate)
}

#Loop across Function for Republican dates  
rep.constant.estimates <- matrix(NA, ncol=5, nrow=length(rep.dates))
for( i in 1:length(rep.dates)){
  rep.constant.estimates[i,]<-ITS.Model.Rep(rep.dates[i])
}


#Create figures
pdf(file="D:/Google Drive/Momentum/Replication/Figures/FigureA23.pdf", height=30, width=25)
par(mfrow=c(7,2))
for(i in 1:length(rep.dates)){
  sm.model <- sm.rweighted.pid
  sm.model <- sm.model[sm.model$time<=157,]
  #make day before primary 0
  sm.model$time <- sm.model$time - (rep.dates[i])
  sm.model <- sm.model[sm.model$time>=-10 & sm.model$time <= 10,]
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
  
  time <- seq(-10,10,1)
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
  
  time <- seq(-10,10,1)
  time <- time[time %in% unique(sm.model$time)]
  racethn4 <- rep("White", length(time))
  age6 <- rep("55-64", length(time))
  educ <- rep("College graduate", length(time))
  gender <- rep("Female", length(time))
  
  sm.pred <- cbind.data.frame(time,racethn4,age6,educ,gender)
  
  prediction2 <- predict(model2, newdata=sm.pred, interval="confidence")
  
  time.full <- seq(-10,10,1)
  
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



###########################
## Plot shocks against primar results
###########################

#Loop across dem dates, getting the percentage of delegates won by clinton
dem.dates2 <- dem.dates
clinton.share <- rep(NA, length(dem.dates))
for(i in 1:length(dem.dates)){
  #primary.results[i] <- weighted.mean(dem.results$clinton[dem.results$date==dem.dates2[i]],
  #                                    weights=dem.results$delegates[dem.results$date==dem.dates2[i]],na.rm=T)
  clinton.share[i] <-(sum(dem.results$clinton.delegates[dem.results$date==dem.dates2[i]])/
                        sum(dem.results$delegates[dem.results$date==dem.dates2[i]]))*100
}

#What is Correlation between results and shocks?
cor(dem.constant.estimates[,1],clinton.share, use="pairwise.complete.obs")


###Load Republican Results
rep.results <- read.csv("D:/Google Drive/Momentum/Data/Rep Primary results.csv")

#Loop across rep dates, getting the percentage of delegates won by trump
trump.share <- rep(NA, length(rep.dates))
for(i in 1:length(rep.dates)){
  #primary.results[i] <- weighted.mean(rep.results$trump[rep.results$date==rep.dates2[i]],
  
  #                                    weights=rep.results$delegates[rep.results$date==rep.dates2[i]],na.rm=T)
  trump.share[i] <-(sum(rep.results$trump.delegates[rep.results$date==rep.dates[i]])/
                      sum(rep.results$delegates[rep.results$date==rep.dates[i]]))*100
}

#What is Correlation between results and shocks?
cor(rep.constant.estimates[,1],trump.share, use="pairwise.complete.obs")

################
### Main Placebo Test
##############
#Make vector of all days
days <- unique(sm.dweighted.pid$time)
#Make vector of days to exclude (primary days +- 2)
dem.dates.placebo <- sort(c(dem.dates, dem.dates+1, dem.dates+2, dem.dates-1, dem.dates-2))

#Keep only those dayes not in dem.dates
days.placebo <- days[days %in% dem.dates.placebo==F]
#Truncate (model can't run with too few data in the tails)
days.placebo <- days.placebo[days.placebo>=17 & days.placebo <=195]
days.placebo <- days.placebo[!is.na(days.placebo)]
#Run 1000 ITS models, each time randomly sampling from placebo days
estimates <- rep(NA,length(days.placebo))
for(i in 1:length(estimates)){
  estimates[i]<- ITS.Model.Dem(days.placebo[i])[1]
}

dem.placebo.estimates <- estimates


pdf(file="D:/Google Drive/Momentum/Replication/Figures/FigureA24a.pdf",  height=7, width=10)
plot(clinton.share,dem.constant.estimates[,1],type="n", ylim=c(-20,20), xlab="Clinton Delegate Share", 
     ylab="Gamma Coefficient")
abline(h=0,lty=2,col="gray80")
abline(v=50,lty=2,col="gray80")
segments(clinton.share,dem.constant.estimates[,1],clinton.share,dem.constant.estimates[,2])
segments(clinton.share,dem.constant.estimates[,1],clinton.share,dem.constant.estimates[,3])
text(clinton.share,dem.constant.estimates[,1], labels=dprim.labels)
abline(h=quantile(dem.placebo.estimates,.95,na.rm=T), lty=2, col="firebrick")
abline(h=quantile(dem.placebo.estimates,.05,na.rm=T), lty=2, col="firebrick")
legend("bottomright","95% of Placebo Tests", lty=2, col="firebrick")
dev.off()


### Republican Placebo

days <- unique(sm.rweighted.pid$time)

rep.dates.placebo <- sort(c(rep.dates, rep.dates+1, rep.dates+2, rep.dates-1, rep.dates-2))
days.placebo <- days[days %in% rep.dates.placebo==F]
#Truncate (model can't run with too few data in the tails)
days.placebo <- days.placebo[days.placebo>=17 & days.placebo <=156]
days.placebo <- days.placebo[!is.na(days.placebo)]
estimates <- rep(NA,length(days.placebo))
for(i in 1:length(estimates)){
  estimates[i]<- ITS.Model.Rep(days.placebo[i])[1]
}


rep.placebo.estimates <- estimates


pdf(file="D:/Google Drive/Momentum/Replication/Figures/FigureA24b.pdf",  height=7, width=10)
plot(trump.share,rep.constant.estimates[,1],type="n", ylim=c(-20,20), xlab="Trump Delegate Share", 
     ylab="Gamma Coefficient")
abline(h=0,lty=2,col="gray80")
abline(v=50,lty=2,col="gray80")
segments(trump.share,rep.constant.estimates[,1],trump.share,rep.constant.estimates[,2])
segments(trump.share,rep.constant.estimates[,1],trump.share,rep.constant.estimates[,3])
text(trump.share,rep.constant.estimates[,1], labels=rprim.labels)
abline(h=quantile(rep.placebo.estimates,.95,na.rm=T), lty=2, col="firebrick")
abline(h=quantile(rep.placebo.estimates,.05,na.rm=T), lty=2, col="firebrick")
legend("bottomright","95% of Placebo Tests", lty=2, col="firebrick")
dev.off()




