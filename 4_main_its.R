# Clinton, Engelhardt, Trussler 2018 "Knock Out Blows"
# Purpose: This file conducts the main ITS analysis in the paper
# producing figures 6a and 6b and Table 2

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

####DEMOCRATS#####

#Function adjusts dates to center on primary day. 
# Individuals differ in their value for rho, 0 if before, 1 if after
#Rho (time=0, rho=1) is trend adjusted shift in opinion after the primary. 
#Function returns Rho, its standard error, CI, and p-value
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
  
####REPUBLICANS#####
  

#Generate function for Republicans  
ITS.Model.Rep <- function(day){
    sm.model <- sm.rweighted.pid
    #make day before primary 0
    sm.model$time <- sm.model$time - (day)
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


#####Create Table 2#####
dem.table <-  cbind(dprim.labels,round(dem.constant.estimates[,1],3),round(dem.constant.estimates[,4:5],3))
rep.table <-  cbind(rprim.labels,round(rep.constant.estimates[,1],3),round(rep.constant.estimates[,4:5],3))
library(xtable)
dem.table[,-1]
rep.table[,-1]
 
print(xtable(dem.table),include.rownames=FALSE)
print(xtable(rep.table),include.rownames=FALSE)



##### Primary Results Data to Compare against Shocks#####

#Loop across dem dates, getting the percentage of delegates won by clinton

clinton.share <- rep(NA, length(dem.dates))
for(i in 1:length(dem.dates)){
  #primary.results[i] <- weighted.mean(dem.results$clinton[dem.results$date==dem.dates2[i]],
  #                                    weights=dem.results$delegates[dem.results$date==dem.dates2[i]],na.rm=T)
  clinton.share[i] <-(sum(dem.results$clinton.delegates[dem.results$date==dem.dates[i]])/
                         sum(dem.results$delegates[dem.results$date==dem.dates[i]]))*100
}


#What is correlation between shocks and percent of delegates won?
cor(dem.constant.estimates[,1],clinton.share, use="pairwise.complete.obs")
#.30




#Loop across rep dates, getting the percentage of delegates won by trump
trump.share <- rep(NA, length(rep.dates))
for(i in 1:length(rep.dates)){
  #primary.results[i] <- weighted.mean(rep.results$trump[rep.results$date==rep.dates2[i]],
  
  #                                    weights=rep.results$delegates[rep.results$date==rep.dates2[i]],na.rm=T)
  trump.share[i] <-(sum(rep.results$trump.delegates[rep.results$date==rep.dates[i]])/
                          sum(rep.results$delegates[rep.results$date==rep.dates[i]]))*100
}

cor(rep.constant.estimates[,1],trump.share, use="pairwise.complete.obs")
#.59


#Republican Drop outs?
#Were the magnitude of shocks larger in contests which caused individuals to drop out?
#IA,NH,SC,ST,FL,IN
sum(c(4.027,2.959,0.808,1.841,0.242,2.914))/6
#LA,AZ,WI,NY,AC
sum(c(3.823,0.178,2.379,0.625,4.708))/5


##### Main Placebo Test######

#Make vector of all days
days <- unique(sm.dweighted.pid$time)
#Make vector of days to exclude (primary days +- 2)
dem.dates.placebo <- sort(c(dem.dates, dem.dates+1, dem.dates+2, dem.dates-1, dem.dates-2))

#Keep only those dayes not in dem.dates
days.placebo <- days[days %in% dem.dates.placebo==F]
#Truncate (model can't run with too few data in the tails)
days.placebo <- days.placebo[days.placebo>=17 & days.placebo <=195]
days.placebo <- days.placebo[!is.na(days.placebo)]

#Run ITS model for each placebo day
estimates <- rep(NA,length(days.placebo))
for(i in 1:length(estimates)){
estimates[i]<- ITS.Model.Dem(days.placebo[i])[1]
}
dem.placebo.estimates <- estimates



#Ceate Figure 6a
pdf(file="D:/Google Drive/Momentum/Replication/Figures/Main/CET_Figure6a.pdf",  height=7, width=10)
plot(clinton.share,dem.constant.estimates[,1],type="n", ylim=c(-20,20), xlab="Clinton Delegate Share", 
     ylab="Estimated Impact on Clinton Support")
abline(h=0,lty=2,col="gray80")
abline(v=50,lty=2,col="gray80")
segments(clinton.share,dem.constant.estimates[,1],clinton.share,dem.constant.estimates[,2])
segments(clinton.share,dem.constant.estimates[,1],clinton.share,dem.constant.estimates[,3])
text(clinton.share,dem.constant.estimates[,1], labels=dprim.labels)
abline(h=quantile(dem.placebo.estimates,.95,na.rm=T), lty=2, col="gray60")
abline(h=quantile(dem.placebo.estimates,.05,na.rm=T), lty=2, col="gray60")
legend("bottomright","95% of Placebo Tests", lty=2, col="gray60")
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


#Create Figure 6b
pdf(file="D:/Google Drive/Momentum/Replication/Figures/Main/CET_Figure6b.pdf",  height=7, width=10)
plot(trump.share,rep.constant.estimates[,1],type="n", ylim=c(-20,20), xlab="Trump Delegate Share", 
     ylab="Estimated Impact on Trump Support")
abline(h=0,lty=2,col="gray80")
abline(v=50,lty=2,col="gray80")
segments(trump.share,rep.constant.estimates[,1],trump.share,rep.constant.estimates[,2])
segments(trump.share,rep.constant.estimates[,1],trump.share,rep.constant.estimates[,3])
text(trump.share,rep.constant.estimates[,1], labels=rprim.labels)
abline(h=quantile(rep.placebo.estimates,.95,na.rm=T), lty=2, col="gray60")
abline(h=quantile(rep.placebo.estimates,.05,na.rm=T), lty=2, col="gray60")
legend("bottomright","95% of Placebo Tests", lty=2, col="gray60")
dev.off()






