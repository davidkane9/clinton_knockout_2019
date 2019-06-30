# Clinton, Engelhardt, Trussler 2018 "Knock Out Blows"
# Purpose: This file conducts ITS analysis conditional on whether
# a respondent's state has voted or not. It produces Figures 7a-7d

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



##############################
#Whether the state voted or not
##############################

##### Democrat #####


table(sm.dweighted.pid$st.voted[sm.dweighted.pid$state=="Iowa"],sm.dweighted.pid$time[sm.dweighted.pid$state=="Iowa"])

#Reverse this coding:
sm.dweighted.pid$state.not.voted <- abs(sm.dweighted.pid$st.voted - 1)
table(sm.dweighted.pid$st.voted,sm.dweighted.pid$state.not.voted)

sm.rweighted.pid$state.not.voted <- abs(sm.rweighted.pid$st.voted - 1)
table(sm.rweighted.pid$st.voted,sm.rweighted.pid$state.not.voted)

table(sm.dweighted.pid$state.not.voted[sm.dweighted.pid$state=="Iowa"],sm.dweighted.pid$time[sm.dweighted.pid$state=="Iowa"])



#Same ITS Model as above, with an interaction between rho and state.not.voted
ITS.Model.Dem.Voted <- function(day){
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
                time  + time2 + time3 + time4 + time5 + time6 + rho*state.not.voted +
                factor(racethn4)*age6*factor(gender)*factor(educ)
              , data=sm.model, weights = DayWeights)
  
  
  jump0<- coef(model)["rho"]
  jump.upper0 <- jump0 + sqrt(vcov(model)["rho","rho"])*1.96
  jump.lower0 <- jump0- sqrt(vcov(model)["rho","rho"])*1.96
  
  jump1 <- coef(model)["rho"] + coef(model)["rho:state.not.voted"]
  jump1.se <- sqrt(vcov(model)["rho","rho"] + vcov(model)["rho:state.not.voted" , "rho:state.not.voted"]+
                     2*vcov(model)["rho","rho:state.not.voted"])
  jump.upper1 <- jump1 + jump1.se*1.96
  jump.lower1 <- jump1 - jump1.se*1.96
  
  diffp <- summary(model)$coefficients["rho:state.not.voted",4]
  estimate <- c(jump0,jump.upper0,jump.lower0, jump1, jump.upper1,jump.lower1,diffp)
  return(estimate)
}

dem.voted.estimates <- matrix(NA, ncol=7, nrow=length(dem.dates))
for( i in 2:length(dem.dates)){
  dem.voted.estimates[i,]<-ITS.Model.Dem.Voted(dem.dates[i])
}

#What is correlation between shocks for those that have and have not voted?
cor(dem.voted.estimates[,1], dem.voted.estimates[,4], use="pairwise.complete")
#.97

#Generate Clinton % Delegates won on each day

#Loop across dem dates, getting the percentage of delegates won by clinton

clinton.share <- rep(NA, length(dem.dates))
for(i in 1:length(dem.dates)){
  #primary.results[i] <- weighted.mean(dem.results$clinton[dem.results$date==dem.dates2[i]],
  #                                    weights=dem.results$delegates[dem.results$date==dem.dates2[i]],na.rm=T)
  clinton.share[i] <-(sum(dem.results$clinton.delegates[dem.results$date==dem.dates[i]])/
                        sum(dem.results$delegates[dem.results$date==dem.dates[i]]))*100
}



#Placebo

#Make vector of all days
days <- unique(sm.dweighted.pid$time)
#Make vector of days to exclude (primary days +- 2)
dem.dates.placebo <- sort(c(dem.dates, dem.dates+1, dem.dates+2, dem.dates-1, dem.dates-2))

#Keep only those dayes not in dem.dates
days.placebo <- days[days %in% dem.dates.placebo==F]
#Truncate (model can't run with too few data in the tails)
days.placebo <- days.placebo[days.placebo>=65 & days.placebo <=188]
days.placebo <- days.placebo[!is.na(days.placebo)]
estimates <- matrix(NA, nrow=length(days.placebo), ncol=2)



for(i in 1:nrow(estimates)){
  results<- ITS.Model.Dem.Voted(days.placebo[i])
  estimates[i,1]<- results[1]
  estimates[i,2]<- results[4]
}
dem.voted.placebo.estimates <- estimates

#This is for those that HAVE VOTED
pdf(file="D:/Google Drive/Momentum/Replication/Figures/Main/CET_Figure7a.pdf",  height=7, width=10)
plot(clinton.share,dem.voted.estimates[,1],type="n", ylim=c(-20,20), xlab="Clinton Delegate Share", 
     ylab="Estimated Impact on Clinton Support", col="darkblue")
abline(h=0,lty=2,col="gray80")
abline(v=50,lty=2,col="gray80")
segments(clinton.share,dem.voted.estimates[,1],clinton.share,dem.voted.estimates[,2])
segments(clinton.share,dem.voted.estimates[,1],clinton.share,dem.voted.estimates[,3])
text(clinton.share,dem.voted.estimates[,1], labels=dprim.labels)
abline(h=quantile(dem.voted.placebo.estimates[,1],.95), lty=2)
abline(h=quantile(dem.voted.placebo.estimates[,1],.05), lty=2)
dev.off()
#What is correlation?
cor(clinton.share,dem.voted.estimates[,1], use="pairwise.complete")
#.30

#This is for those that HAVE NOT VOTED

pdf(file="D:/Google Drive/Momentum/Replication/Figures/Main/CET_Figure7b.pdf",  height=7, width=10)
plot(clinton.share,dem.voted.estimates[,4],type="n", ylim=c(-20,20), xlab="Clinton Delegate Share", 
     ylab="Estimated Impact on Clinton Support", col="purple")
abline(h=0,lty=2,col="gray80")
abline(v=50,lty=2,col="gray80")
segments(clinton.share,dem.voted.estimates[,4],clinton.share,dem.voted.estimates[,5])
segments(clinton.share,dem.voted.estimates[,4],clinton.share,dem.voted.estimates[,6])
text(clinton.share,dem.voted.estimates[,4], labels=dprim.labels)
abline(h=quantile(dem.voted.placebo.estimates[,2],.95), lty=2)
abline(h=quantile(dem.voted.placebo.estimates[,2],.05), lty=2)
dev.off()
#What is correlation
cor(clinton.share,dem.voted.estimates[,4], use="pairwise.complete")
#.23


##### Republican #####

ITS.Model.Rep.Voted <- function(day){
  #Start
  sm.model <- sm.rweighted.pid
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
  model <- lm(trump ~ 
                time  + time2 + time3 + time4 + time5 + time6 + rho*state.not.voted +
                factor(racethn4)*age6*factor(gender)*factor(educ)
              , data=sm.model, weights = DayWeights)
  
  
  jump0<- coef(model)["rho"]
  jump.upper0 <- jump0 + sqrt(vcov(model)["rho","rho"])*1.96
  jump.lower0 <- jump0- sqrt(vcov(model)["rho","rho"])*1.96
  
  jump1 <- coef(model)["rho"] + coef(model)["rho:state.not.voted"]
  jump1.se <- sqrt(vcov(model)["rho","rho"] + vcov(model)["rho:state.not.voted" , "rho:state.not.voted"]+
                     2*vcov(model)["rho","rho:state.not.voted"])
  jump.upper1 <- jump1 + jump1.se*1.96
  jump.lower1 <- jump1 - jump1.se*1.96
  
  diffp <- summary(model)$coefficients["rho:state.not.voted",4]
  estimate <- c(jump0,jump.upper0,jump.lower0, jump1, jump.upper1,jump.lower1,diffp)
  return(estimate)
}

rep.voted.estimates <- matrix(NA, ncol=7, nrow=length(rep.dates))
for( i in 2:length(rep.dates)){
  rep.voted.estimates[i,]<-ITS.Model.Rep.Voted(dem.dates[i])
}
#What is correlation between sets of estimates?
cor(rep.voted.estimates[,1], rep.voted.estimates[,4], use="pairwise.complete")
#.57

#Loop across rep dates, getting the percentage of delegates won by trump
trump.share <- rep(NA, length(rep.dates))
for(i in 1:length(rep.dates)){
  #primary.results[i] <- weighted.mean(rep.results$trump[rep.results$date==rep.dates2[i]],
  
  #                                    weights=rep.results$delegates[rep.results$date==rep.dates2[i]],na.rm=T)
  trump.share[i] <-(sum(rep.results$trump.delegates[rep.results$date==rep.dates[i]])/
                      sum(rep.results$delegates[rep.results$date==rep.dates[i]]))*100
}

#Placebo
days <- unique(sm.rweighted.pid$time)

rep.dates.placebo <- sort(c(rep.dates, rep.dates+1, rep.dates+2, rep.dates-1, rep.dates-2))
days.placebo <- days[days %in% rep.dates.placebo==F]
#Truncate (model can't run with too few data in the tails)
days.placebo <- days.placebo[days.placebo>=65 & days.placebo <=156]
days.placebo <- days.placebo[!is.na(days.placebo)]
estimates <- matrix(NA, nrow=length(days.placebo), ncol=2)

for(i in 1:nrow(estimates)){
  results<- ITS.Model.Rep.Voted(days.placebo[i])
  estimates[i,1]<- results[1]
  estimates[i,2]<- results[4]
}

rep.voted.placebo.estimates <- estimates



pdf(file="D:/Google Drive/Momentum/Replication/Figures/Main/CET_Figure7c.pdf",  height=7, width=10)
plot(trump.share,rep.voted.estimates[,1],type="n", ylim=c(-20,20), xlab="Trump Delegate Share", 
     ylab="Estimated Impact on Trump Support", col="firebrick")
abline(h=0,lty=2,col="gray80")
abline(v=50,lty=2,col="gray80")
segments(trump.share,rep.voted.estimates[,1],trump.share,rep.voted.estimates[,2])
segments(trump.share,rep.voted.estimates[,1],trump.share,rep.voted.estimates[,3])
text(trump.share,rep.voted.estimates[,1], labels=rprim.labels)
abline(h=quantile(rep.voted.placebo.estimates[,1],.95), lty=2)
abline(h=quantile(rep.voted.placebo.estimates[,1],.05), lty=2)
dev.off()
cor(trump.share,rep.voted.estimates[,1], use="pairwise.complete")
#.37

pdf(file="D:/Google Drive/Momentum/Replication/Figures/Main/CET_Figure7d.pdf",  height=7, width=10)
plot(trump.share,rep.voted.estimates[,4],type="n", ylim=c(-20,20), xlab="Trump Delegate Share", 
     ylab="Estimated Impact on Trump Supportt", col="magenta")
abline(h=0,lty=2,col="gray80")
abline(v=50,lty=2,col="gray80")
segments(trump.share,rep.voted.estimates[,4],trump.share,rep.voted.estimates[,5])
segments(trump.share,rep.voted.estimates[,4],trump.share,rep.voted.estimates[,6])
text(trump.share,rep.voted.estimates[,4], labels=rprim.labels)
abline(h=quantile(rep.voted.placebo.estimates[,2],.95), lty=2)
abline(h=quantile(rep.voted.placebo.estimates[,2],.05), lty=2)
legend("bottomright","95% of Placebo Tests", lty=2)
dev.off()
cor(trump.share,rep.voted.estimates[,4], use="pairwise.complete")
#.61


