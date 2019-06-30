# Clinton, Engelhardt, Trussler 2018 "Knock Out Blows"
# Purpose: This file produces the estimates and figures for all of the heterogeneous treatment effects,
# for education, state voted, partisan leaners, and gender.

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



#Loop across dem dates, getting the percentage of delegates won by clinton

clinton.share <- rep(NA, length(dem.dates))
for(i in 1:length(dem.dates)){
  #primary.results[i] <- weighted.mean(dem.results$clinton[dem.results$date==dem.dates2[i]],
  #                                    weights=dem.results$delegates[dem.results$date==dem.dates2[i]],na.rm=T)
  clinton.share[i] <-(sum(dem.results$clinton.delegates[dem.results$date==dem.dates[i]])/
                        sum(dem.results$delegates[dem.results$date==dem.dates[i]]))*100
}



#Loop across rep dates, getting the percentage of delegates won by trump
trump.share <- rep(NA, length(rep.dates))
for(i in 1:length(rep.dates)){
  #primary.results[i] <- weighted.mean(rep.results$trump[rep.results$date==rep.dates2[i]],
  
  #                                    weights=rep.results$delegates[rep.results$date==rep.dates2[i]],na.rm=T)
  trump.share[i] <-(sum(rep.results$trump.delegates[rep.results$date==rep.dates[i]])/
                      sum(rep.results$delegates[rep.results$date==rep.dates[i]]))*100
}

################################
##### Heterogenous Effects #####
################################

########Education############

sm.dweighted.pid$college.graduate <- NA

sm.dweighted.pid$college.graduate[sm.dweighted.pid$educ=="College graduate" 
                              | sm.dweighted.pid$educ=="Post graduate degree"]<- 1

sm.dweighted.pid$college.graduate[sm.dweighted.pid$educ=="Did not complete high school" 
                              | sm.dweighted.pid$educ=="High school or G.E.D."
                              | sm.dweighted.pid$educ=="Associate?s degree"
                              | sm.dweighted.pid$educ=="Some college"]<- 0

sm.rweighted.pid$college.graduate <- NA

sm.rweighted.pid$college.graduate[sm.rweighted.pid$educ=="College graduate" 
                              | sm.rweighted.pid$educ=="Post graduate degree"]<- 1

sm.rweighted.pid$college.graduate[sm.rweighted.pid$educ=="Did not complete high school" 
                              | sm.rweighted.pid$educ=="High school or G.E.D."
                              | sm.rweighted.pid$educ=="Associate?s degree"
                              | sm.rweighted.pid$educ=="Some college"]<- 0


###For Democrats

ITS.Model.Dem.Educ <- function(day){
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
                time  + time2 + time3 + time4 + time5 + time6 + rho*college.graduate +
                factor(racethn4)*age6*factor(gender)
              , data=sm.model, weights = DayWeights)
  
  
  jump0<- coef(model)["rho"]
  jump.upper0 <- jump0 + sqrt(vcov(model)["rho","rho"])*1.96
  jump.lower0 <- jump0- sqrt(vcov(model)["rho","rho"])*1.96
  
  jump1 <- coef(model)["rho"] + coef(model)["rho:college.graduate"]
  jump1.se <- sqrt(vcov(model)["rho","rho"] + vcov(model)["rho:college.graduate" , "rho:college.graduate"]+
              2*vcov(model)["rho","rho:college.graduate"])
  jump.upper1 <- jump1 + jump1.se*1.96
  jump.lower1 <- jump1 - jump1.se*1.96
  diffp <- summary(model)$coefficients["rho:college.graduate",4]
 estimate <- c(jump0,jump.upper0,jump.lower0, jump1, jump.upper1,jump.lower1,diffp)
  return(estimate)
}

dem.educ.estimates <- matrix(NA, ncol=7, nrow=length(dem.dates))
for( i in 1:length(dem.dates)){
  dem.educ.estimates[i,]<-ITS.Model.Dem.Educ(dem.dates[i])
}

cor(dem.educ.estimates[,1], dem.educ.estimates[,4])
#.99

pdf(file="D:/Google Drive/Momentum/Replication/Figures/Appendix/FigureA17a.pdf")
plot(dem.educ.estimates[,1], dem.educ.estimates[,4], type="n", ylim=c(-15,15), xlim=c(-15,15),
     xlab="No College", ylab="College")
abline(0,1, lty=2)
segments(dem.educ.estimates[,1], dem.educ.estimates[,4], dem.educ.estimates[,2], dem.educ.estimates[,4], col="gray70")
segments(dem.educ.estimates[,1], dem.educ.estimates[,4], dem.educ.estimates[,3], dem.educ.estimates[,4], col="gray70")
segments(dem.educ.estimates[,1], dem.educ.estimates[,4], dem.educ.estimates[,1], dem.educ.estimates[,5], col="gray70")
segments(dem.educ.estimates[,1], dem.educ.estimates[,4], dem.educ.estimates[,1], dem.educ.estimates[,6], col="gray70")
text(dem.educ.estimates[,1], dem.educ.estimates[,4], label=dprim.labels)
legend("topleft","Cor=.99")
dev.off()


#Placebo Tests

#Make vector of all days
days <- unique(sm.dweighted.pid$time)
#Make vector of days to exclude (primary days +- 2)
dem.dates.placebo <- sort(c(dem.dates, dem.dates+1, dem.dates+2, dem.dates-1, dem.dates-2))

#Keep only those dayes not in dem.dates
days.placebo <- days[days %in% dem.dates.placebo==F]
#Truncate (model can't run with too few data in the tails)
days.placebo <- days.placebo[days.placebo>=17 & days.placebo <=195]
days.placebo <- days.placebo[!is.na(days.placebo)]
estimates <- matrix(NA, nrow=length(days.placebo), ncol=2)

for(i in 1:nrow(estimates)){
  results<- ITS.Model.Dem.Educ(days.placebo[i])
  estimates[i,1]<- results[1]
  estimates[i,2]<- results[4]
}

dem.educ.placebo.estimates <- estimates


pdf(file="D:/Google Drive/Momentum/Replication/Figures/Appendix/FigureA166a.pdf",  height=7, width=10)
plot(clinton.share,dem.educ.estimates[,1],type="n", ylim=c(-20,20), xlab="Clinton Delegate Share", 
     ylab="Gamma Coefficient", col="darkblue")
abline(h=0,lty=2,col="gray80")
abline(v=50,lty=2,col="gray80")
segments(clinton.share,dem.educ.estimates[,1],clinton.share,dem.educ.estimates[,2],col="darkblue")
segments(clinton.share,dem.educ.estimates[,1],clinton.share,dem.educ.estimates[,3],col="darkblue")
abline(h=quantile(dem.educ.placebo.estimates[,1],.95), lty=2)
abline(h=quantile(dem.educ.placebo.estimates[,1],.05), lty=2)
text(clinton.share,dem.educ.estimates[,1], labels=dprim.labels,col="darkblue")
legend("bottomright","95% of Placebo Tests", lty=2)
dev.off()
cor(clinton.share,dem.educ.estimates[,1], use="pairwise.complete")
#.34


pdf(file="D:/Google Drive/Momentum/Replication/Figures/Appendix/FigureA16b.pdf",  height=7, width=10)
plot(clinton.share,dem.educ.estimates[,4],type="n", ylim=c(-20,20), xlab="Clinton Delegate Share", 
     ylab="Gamma Coefficient", col="purple")
abline(h=0,lty=2,col="gray80")
abline(v=50,lty=2,col="gray80")
segments(clinton.share,dem.educ.estimates[,4],clinton.share,dem.educ.estimates[,5],col="purple")
segments(clinton.share,dem.educ.estimates[,4],clinton.share,dem.educ.estimates[,6],col="purple")
text(clinton.share,dem.educ.estimates[,4], labels=dprim.labels,col="purple")
abline(h=quantile(dem.educ.placebo.estimates[,2],.95), lty=2)
abline(h=quantile(dem.educ.placebo.estimates[,2],.05), lty=2)
legend("bottomright","95% of Placebo Tests", lty=2)
dev.off()
cor(clinton.share,dem.educ.estimates[,4], use="pairwise.complete")
#.30

#Republican Education

ITS.Model.Rep.Educ <- function(day){
  #Start
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
  
  
  
  #Run model
  model <- lm(trump ~ 
                time  + time2 + time3 + time4 + time5 + time6 + rho*college.graduate +
                factor(racethn4)*age6*factor(gender)
              , data=sm.model, weights = DayWeights)
  
  
  jump0<- coef(model)["rho"]
  jump.upper0 <- jump0 + sqrt(vcov(model)["rho","rho"])*1.96
  jump.lower0 <- jump0- sqrt(vcov(model)["rho","rho"])*1.96
  
  jump1 <- coef(model)["rho"] + coef(model)["rho:college.graduate"]
  jump1.se <- sqrt(vcov(model)["rho","rho"] + vcov(model)["rho:college.graduate" , "rho:college.graduate"]+
                     2*vcov(model)["rho","rho:college.graduate"])
  jump.upper1 <- jump1 + jump1.se*1.96
  jump.lower1 <- jump1 - jump1.se*1.96
  
  diffp <- summary(model)$coefficients["rho:college.graduate",4]
  estimate <- c(jump0,jump.upper0,jump.lower0, jump1, jump.upper1,jump.lower1,diffp)
  return(estimate)
}

rep.educ.estimates <- matrix(NA, ncol=7, nrow=length(rep.dates))
for( i in 1:length(rep.dates)){
  rep.educ.estimates[i,]<-ITS.Model.Rep.Educ(rep.dates[i])
}
cor(rep.educ.estimates[,1], rep.educ.estimates[,4])
#.90

pdf(file="D:/Google Drive/Momentum/Replication/Figures/Appendix/FigureA17b.pdf")
plot(rep.educ.estimates[,1], rep.educ.estimates[,4], type="n", ylim=c(-10,10), xlim=c(-10,10),
     xlab="No College", ylab="College")
abline(0,1, lty=2)
segments(rep.educ.estimates[,1], rep.educ.estimates[,4], rep.educ.estimates[,2], rep.educ.estimates[,4], col="gray70")
segments(rep.educ.estimates[,1], rep.educ.estimates[,4], rep.educ.estimates[,3], rep.educ.estimates[,4], col="gray70")
segments(rep.educ.estimates[,1], rep.educ.estimates[,4], rep.educ.estimates[,1], rep.educ.estimates[,5], col="gray70")
segments(rep.educ.estimates[,1], rep.educ.estimates[,4], rep.educ.estimates[,1], rep.educ.estimates[,6], col="gray70")
text(rep.educ.estimates[,1], rep.educ.estimates[,4], label=rprim.labels)
legend("topleft", "Cor=.90")
dev.off()

#Placebo
days <- unique(sm.rweighted.pid$time)

rep.dates.placebo <- sort(c(rep.dates, rep.dates+1, rep.dates+2, rep.dates-1, rep.dates-2))
days.placebo <- days[days %in% rep.dates.placebo==F]
#Truncate (model can't run with too few data in the tails)
days.placebo <- days.placebo[days.placebo>=17 & days.placebo <=156]
days.placebo <- days.placebo[!is.na(days.placebo)]
estimates <- matrix(NA, nrow=length(days.placebo), ncol=2)

for(i in 1:nrow(estimates)){
  results<- ITS.Model.Rep.Educ(days.placebo[i])
  estimates[i,1]<- results[1]
  estimates[i,2]<- results[4]
}
rep.educ.placebo.estimates <- estimates


pdf(file="D:/Google Drive/Momentum/Replication/Figures/Appendix/FigureA16c.pdf",  height=7, width=10)
plot(trump.share,rep.educ.estimates[,1],type="n", ylim=c(-20,20), xlab="Trump Delegate Share", 
     ylab="Gamma Coefficient", col="darkblue")
abline(h=0,lty=2,col="gray80")
abline(v=50,lty=2,col="gray80")
segments(trump.share,rep.educ.estimates[,1],trump.share,rep.educ.estimates[,2],col="firebrick")
segments(trump.share,rep.educ.estimates[,1],trump.share,rep.educ.estimates[,3],col="firebrick")
text(trump.share,rep.educ.estimates[,1], labels=rprim.labels,col="firebrick")
abline(h=quantile(rep.educ.placebo.estimates[,2],.95), lty=2)
abline(h=quantile(rep.educ.placebo.estimates[,2],.05), lty=2)
legend("bottomright","95% of Placebo Tests", lty=2)
dev.off()
cor(trump.share,rep.educ.estimates[,1], use="pairwise.complete")

pdf(file="D:/Google Drive/Momentum/Replication/Figures/Appendix/FigureA16d.pdf",  height=7, width=10)
plot(trump.share,rep.educ.estimates[,4],type="n", ylim=c(-20,20), xlab="Trump Delegate Share", 
     ylab="Gamma Coefficient", col="magenta")
abline(h=0,lty=2,col="gray80")
abline(v=50,lty=2,col="gray80")
segments(trump.share,rep.educ.estimates[,4],trump.share,rep.educ.estimates[,5],col="magenta")
segments(trump.share,rep.educ.estimates[,4],trump.share,rep.educ.estimates[,6],col="magenta")
text(trump.share,rep.educ.estimates[,4], labels=rprim.labels,col="magenta")
abline(h=quantile(rep.educ.placebo.estimates[,2],.95), lty=2)
abline(h=quantile(rep.educ.placebo.estimates[,2],.05), lty=2)
legend("bottomright","95% of Placebo Tests", lty=2)
dev.off()
cor(trump.share,rep.educ.estimates[,4], use="pairwise.complete")


#Create Table A6 and A7

dem.table <-  cbind(dprim.labels,round(dem.educ.estimates[,1],3),round(dem.educ.estimates[,4],3),round(dem.educ.estimates[,7],3))
rep.table <-  cbind(rprim.labels,round(rep.educ.estimates[,1],3),round(rep.educ.estimates[,4],3),round(rep.educ.estimates[,7],3))

library(xtable)

print(xtable(dem.table),include.rownames=FALSE)
print(xtable(rep.table),include.rownames=FALSE)



##############################
#Whether the state voted or not
##############################

#Note this analysis is also in the main text, see also file "4_hetero.its.R"

table(sm.dweighted.pid$st.voted[sm.dweighted.pid$state=="Iowa"],sm.dweighted.pid$time[sm.dweighted.pid$state=="Iowa"])

#Reverse this coding:
sm.dweighted.pid$state.not.voted <- abs(sm.dweighted.pid$st.voted - 1)
table(sm.dweighted.pid$st.voted,sm.dweighted.pid$state.not.voted)

sm.rweighted.pid$state.not.voted <- abs(sm.rweighted.pid$st.voted - 1)
table(sm.rweighted.pid$st.voted,sm.rweighted.pid$state.not.voted)

table(sm.dweighted.pid$state.not.voted[sm.dweighted.pid$state=="Iowa"],sm.dweighted.pid$time[sm.dweighted.pid$state=="Iowa"])


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
cor(dem.voted.estimates[,1], dem.voted.estimates[,4], use="pairwise.complete")
#.97



pdf(file="D:/Google Drive/Momentum/Replication/Figures/Appendix/FigureA15a.pdf")
plot(dem.voted.estimates[,1], dem.voted.estimates[,4], type="n", ylim=c(-10,10), xlim=c(-10,10),
     xlab="State Has Voted", ylab="State Has Not Voted")
abline(0,1, lty=2)
segments(dem.voted.estimates[,1], dem.voted.estimates[,4], dem.voted.estimates[,2], dem.voted.estimates[,4], col="gray70")
segments(dem.voted.estimates[,1], dem.voted.estimates[,4], dem.voted.estimates[,3], dem.voted.estimates[,4], col="gray70")
segments(dem.voted.estimates[,1], dem.voted.estimates[,4], dem.voted.estimates[,1], dem.voted.estimates[,5], col="gray70")
segments(dem.voted.estimates[,1], dem.voted.estimates[,4], dem.voted.estimates[,1], dem.voted.estimates[,6], col="gray70")
text(dem.voted.estimates[,1], dem.voted.estimates[,4], label=dprim.labels)
legend("topleft","Cor=.97")
dev.off()



## Republican Voted

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
cor(rep.voted.estimates[,1], rep.voted.estimates[,4], use="pairwise.complete")
#.57

pdf(file="D:/Google Drive/Momentum/Replication/Figures/Appendix/FigureA15b.pdf")
plot(rep.voted.estimates[,1], rep.voted.estimates[,4], type="n", ylim=c(-10,10), xlim=c(-10,10),
     xlab="State Has Voted", ylab="State Has Not Voted")
abline(0,1, lty=2)
segments(rep.voted.estimates[,1], rep.voted.estimates[,4], rep.voted.estimates[,2], rep.voted.estimates[,4], col="gray70")
segments(rep.voted.estimates[,1], rep.voted.estimates[,4], rep.voted.estimates[,3], rep.voted.estimates[,4], col="gray70")
segments(rep.voted.estimates[,1], rep.voted.estimates[,4], rep.voted.estimates[,1], rep.voted.estimates[,5], col="gray70")
segments(rep.voted.estimates[,1], rep.voted.estimates[,4], rep.voted.estimates[,1], rep.voted.estimates[,6], col="gray70")
text(rep.voted.estimates[,1], rep.voted.estimates[,4], label=rprim.labels)
legend("topleft","Cor=.57")
dev.off()


#Create Tables A4 and A5

dem.table <-  cbind(dprim.labels,round(dem.voted.estimates[,1],3),round(dem.voted.estimates[,4],3),round(dem.voted.estimates[,7],3))
rep.table <-  cbind(rprim.labels,round(rep.voted.estimates[,1],3),round(rep.voted.estimates[,4],3),round(rep.voted.estimates[,7],3))



print(xtable(dem.table),include.rownames=FALSE)
print(xtable(rep.table),include.rownames=FALSE)



##############################
#Strong/weak partisans versus leaners
##############################

table(sm.dweighted.pid$party5, sm.dweighted.pid$clinton)

sm.dweighted.pid$indy <- NA
sm.dweighted.pid$indy[sm.dweighted.pid$party5=="Lean Dem"]<-1
sm.dweighted.pid$indy[sm.dweighted.pid$party5=="Democrat"]<-0
table(sm.dweighted.pid$indy, sm.dweighted.pid$clinton)

sm.rweighted.pid$indy <- NA
sm.rweighted.pid$indy[sm.rweighted.pid$party5=="Lean Rep"]<-1
sm.rweighted.pid$indy[sm.rweighted.pid$party5=="Republican"]<-0
table(sm.rweighted.pid$indy, sm.rweighted.pid$trump)

ITS.Model.Dem.Indy <- function(day){
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
                time  + time2 + time3 + time4 + time5 + time6 + rho*indy +
                factor(racethn4)*age6*factor(gender)*factor(educ)
              , data=sm.model, weights = DayWeights)
  
  
  jump0<- coef(model)["rho"]
  jump.upper0 <- jump0 + sqrt(vcov(model)["rho","rho"])*1.96
  jump.lower0 <- jump0- sqrt(vcov(model)["rho","rho"])*1.96
  
  jump1 <- coef(model)["rho"] + coef(model)["rho:indy"]
  jump1.se <- sqrt(vcov(model)["rho","rho"] + vcov(model)["rho:indy" , "rho:indy"]+
                     2*vcov(model)["rho","rho:indy"])
  jump.upper1 <- jump1 + jump1.se*1.96
  jump.lower1 <- jump1 - jump1.se*1.96
  
  diffp <- summary(model)$coefficients["rho:indy",4]
  estimate <- c(jump0,jump.upper0,jump.lower0, jump1, jump.upper1,jump.lower1,diffp)
  return(estimate)
}

dem.indy.estimates <- matrix(NA, ncol=7, nrow=length(dem.dates))
for( i in 1:length(dem.dates)){
  dem.indy.estimates[i,]<-ITS.Model.Dem.Indy(dem.dates[i])
}
cor(dem.indy.estimates[,1], dem.indy.estimates[,4], use="pairwise.complete")
#.99

pdf(file="D:/Google Drive/Momentum/Replication/Figures/Appendix/FigureA19a.pdf")
plot(dem.indy.estimates[,1], dem.indy.estimates[,4], type="n", ylim=c(-10,10), xlim=c(-10,10),
     xlab="Partisan", ylab="Leaner")
abline(0,1, lty=2)
segments(dem.indy.estimates[,1], dem.indy.estimates[,4], dem.indy.estimates[,2], dem.indy.estimates[,4], col="gray70")
segments(dem.indy.estimates[,1], dem.indy.estimates[,4], dem.indy.estimates[,3], dem.indy.estimates[,4], col="gray70")
segments(dem.indy.estimates[,1], dem.indy.estimates[,4], dem.indy.estimates[,1], dem.indy.estimates[,5], col="gray70")
segments(dem.indy.estimates[,1], dem.indy.estimates[,4], dem.indy.estimates[,1], dem.indy.estimates[,6], col="gray70")
text(dem.indy.estimates[,1], dem.indy.estimates[,4], label=dprim.labels)
legend("topleft","Cor=.99")
dev.off()


#Placebo

#Make vector of all days
days <- unique(sm.dweighted.pid$time)
#Make vector of days to exclude (primary days +- 2)
dem.dates.placebo <- sort(c(dem.dates, dem.dates+1, dem.dates+2, dem.dates-1, dem.dates-2))

#Keep only those dayes not in dem.dates
days.placebo <- days[days %in% dem.dates.placebo==F]
#Truncate (model can't run with too few data in the tails)
days.placebo <- days.placebo[days.placebo>=17 & days.placebo <=195]
days.placebo <- days.placebo[!is.na(days.placebo)]
estimates <- matrix(NA, nrow=length(days.placebo), ncol=2)

for(i in 1:nrow(estimates)){
  results<- ITS.Model.Dem.Indy(days.placebo[i])
  estimates[i,1]<- results[1]
  estimates[i,2]<- results[4]
}
dem.indy.placebo.estimates <- estimates


pdf(file="D:/Google Drive/Momentum/Replication/Figures/Appendix/FigureA18a.pdf",  height=7, width=10)
plot(clinton.share,dem.indy.estimates[,1],type="n", ylim=c(-20,20), xlab="Clinton Delegate Share", 
     ylab="Gamma Coefficient", col="darkblue")
abline(h=0,lty=2,col="gray80")
abline(v=50,lty=2,col="gray80")
segments(clinton.share,dem.indy.estimates[,1],clinton.share,dem.indy.estimates[,2],col="darkblue")
segments(clinton.share,dem.indy.estimates[,1],clinton.share,dem.indy.estimates[,3],col="darkblue")
text(clinton.share,dem.indy.estimates[,1], labels=dprim.labels,col="darkblue")
abline(h=quantile(dem.indy.placebo.estimates[,1],.95), lty=2)
abline(h=quantile(dem.indy.placebo.estimates[,1],.05), lty=2)
dev.off()
cor(clinton.share,dem.indy.estimates[,1], use="pairwise.complete")

pdf(file="D:/Google Drive/Momentum/Replication/Figures/Appendix/FigureA18b.pdf",  height=7, width=10)
plot(clinton.share,dem.indy.estimates[,4],type="n", ylim=c(-20,20), xlab="Clinton Delegate Share", 
     ylab="Gamma Coefficient", col="purple")
abline(h=0,lty=2,col="gray80")
abline(v=50,lty=2,col="gray80")
segments(clinton.share,dem.indy.estimates[,4],clinton.share,dem.indy.estimates[,5],col="purple")
segments(clinton.share,dem.indy.estimates[,4],clinton.share,dem.indy.estimates[,6],col="purple")
text(clinton.share,dem.indy.estimates[,4], labels=dprim.labels,col="purple")
abline(h=quantile(dem.indy.placebo.estimates[,2],.95), lty=2)
abline(h=quantile(dem.indy.placebo.estimates[,2],.05), lty=2)
dev.off()
cor(clinton.share,dem.indy.estimates[,4], use="pairwise.complete")





#Republicans

ITS.Model.Rep.Indy <- function(day){
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
                time  + time2 + time3 + time4 + time5 + time6 + rho*indy +
                factor(racethn4)*age6*factor(gender)*factor(educ)
              , data=sm.model, weights = DayWeights)
  
  
  jump0<- coef(model)["rho"]
  jump.upper0 <- jump0 + sqrt(vcov(model)["rho","rho"])*1.96
  jump.lower0 <- jump0- sqrt(vcov(model)["rho","rho"])*1.96
  
  jump1 <- coef(model)["rho"] + coef(model)["rho:indy"]
  jump1.se <- sqrt(vcov(model)["rho","rho"] + vcov(model)["rho:indy" , "rho:indy"]+
                     2*vcov(model)["rho","rho:indy"])
  jump.upper1 <- jump1 + jump1.se*1.96
  jump.lower1 <- jump1 - jump1.se*1.96
  
  diffp <- summary(model)$coefficients["rho:indy",4]
  estimate <- c(jump0,jump.upper0,jump.lower0, jump1, jump.upper1,jump.lower1,diffp)
  return(estimate)
}

rep.indy.estimates <- matrix(NA, ncol=7, nrow=length(rep.dates))
for( i in 1:length(rep.dates)){
  rep.indy.estimates[i,]<-ITS.Model.Rep.Indy(rep.dates[i])
}
cor(rep.indy.estimates[,1], rep.indy.estimates[,4], use="pairwise.complete")
#.89

pdf(file="D:/Google Drive/Momentum/Replication/Figures/Appendix/FigureA19b.pdf")
plot(rep.indy.estimates[,1], rep.indy.estimates[,4], type="n", ylim=c(-10,10), xlim=c(-10,10),
     xlab="Partisan", ylab="Leaner")
abline(0,1, lty=2)
segments(rep.indy.estimates[,1], rep.indy.estimates[,4], rep.indy.estimates[,2], rep.indy.estimates[,4], col="gray70")
segments(rep.indy.estimates[,1], rep.indy.estimates[,4], rep.indy.estimates[,3], rep.indy.estimates[,4], col="gray70")
segments(rep.indy.estimates[,1], rep.indy.estimates[,4], rep.indy.estimates[,1], rep.indy.estimates[,5], col="gray70")
segments(rep.indy.estimates[,1], rep.indy.estimates[,4], rep.indy.estimates[,1], rep.indy.estimates[,6], col="gray70")
text(rep.indy.estimates[,1], rep.indy.estimates[,4], label=rprim.labels)
legend("topleft","Cor=.90")
dev.off()

#Placebo
days <- unique(sm.rweighted.pid$time)

rep.dates.placebo <- sort(c(rep.dates, rep.dates+1, rep.dates+2, rep.dates-1, rep.dates-2))
days.placebo <- days[days %in% rep.dates.placebo==F]
#Truncate (model can't run with too few data in the tails)
days.placebo <- days.placebo[days.placebo>=17 & days.placebo <=156]
days.placebo <- days.placebo[!is.na(days.placebo)]
estimates <- matrix(NA, nrow=length(days.placebo), ncol=2)

for(i in 1:nrow(estimates)){
  results<- ITS.Model.Rep.Indy(days.placebo[i])
  estimates[i,1]<- results[1]
  estimates[i,2]<- results[4]
}

rep.indy.placebo.estimates <- estimates

pdf(file="D:/Google Drive/Momentum/Replication/Figures/Appendix/FigureA18c.pdf",  height=7, width=10)
plot(trump.share,rep.indy.estimates[,1],type="n", ylim=c(-20,20), xlab="trump Delegate Share", 
     ylab="Gamma Coefficient", col="firebrick")
abline(h=0,lty=2,col="gray80")
abline(v=50,lty=2,col="gray80")
segments(trump.share,rep.indy.estimates[,1],trump.share,rep.indy.estimates[,2],col="firebrick")
segments(trump.share,rep.indy.estimates[,1],trump.share,rep.indy.estimates[,3],col="firebrick")
text(trump.share,rep.indy.estimates[,1], labels=rprim.labels,col="firebrick")
abline(h=quantile(rep.indy.placebo.estimates[,1],.95), lty=2)
abline(h=quantile(rep.indy.placebo.estimates[,1],.05), lty=2)
dev.off()
cor(trump.share,rep.indy.estimates[,1], use="pairwise.complete")

pdf(file="D:/Google Drive/Momentum/Replication/Figures/Appendix/FigureA18d.pdf",  height=7, width=10)
plot(trump.share,rep.indy.estimates[,4],type="n", ylim=c(-20,20), xlab="trump Delegate Share", 
     ylab="Gamma Coefficient", col="magenta")
abline(h=0,lty=2,col="gray80")
abline(v=50,lty=2,col="gray80")
segments(trump.share,rep.indy.estimates[,4],trump.share,rep.indy.estimates[,5],col="magenta")
segments(trump.share,rep.indy.estimates[,4],trump.share,rep.indy.estimates[,6],col="magenta")
text(trump.share,rep.indy.estimates[,4], labels=rprim.labels,col="magenta")
abline(h=quantile(rep.indy.placebo.estimates[,2],.95), lty=2)
abline(h=quantile(rep.indy.placebo.estimates[,2],.05), lty=2)
dev.off()
cor(trump.share,rep.indy.estimates[,4], use="pairwise.complete")

#Creation of Tables A8 and A9

dem.table <-  cbind(dprim.labels,round(dem.indy.estimates[,1],3),round(dem.indy.estimates[,4],3),round(dem.indy.estimates[,7],3))
rep.table <-  cbind(rprim.labels,round(rep.indy.estimates[,1],3),round(rep.indy.estimates[,4],3),round(rep.indy.estimates[,7],3))



print(xtable(dem.table),include.rownames=FALSE)
print(xtable(rep.table),include.rownames=FALSE)





##############################
#Gender
##############################

table(sm.dweighted.pid$gender, sm.dweighted.pid$clinton)

sm.dweighted.pid$female <- NA
sm.dweighted.pid$female[sm.dweighted.pid$gender=="Female"]<-1
sm.dweighted.pid$female[sm.dweighted.pid$gender=="Male"]<-0
table(sm.dweighted.pid$female, sm.dweighted.pid$clinton)

sm.rweighted.pid$female <- NA
sm.rweighted.pid$female[sm.rweighted.pid$gender=="Female"]<-1
sm.rweighted.pid$female[sm.rweighted.pid$gender=="Male"]<-0
table(sm.rweighted.pid$female, sm.rweighted.pid$trump)

ITS.Model.Dem.female <- function(day){
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
                time  + time2 + time3 + time4 + time5 + time6 + rho*female +
                factor(racethn4)*age6*factor(gender)*factor(educ)
              , data=sm.model, weights = DayWeights)
  
  
  jump0<- coef(model)["rho"]
  jump.upper0 <- jump0 + sqrt(vcov(model)["rho","rho"])*1.96
  jump.lower0 <- jump0- sqrt(vcov(model)["rho","rho"])*1.96
  
  jump1 <- coef(model)["rho"] + coef(model)["rho:female"]
  jump1.se <- sqrt(vcov(model)["rho","rho"] + vcov(model)["rho:female" , "rho:female"]+
                     2*vcov(model)["rho","rho:female"])
  jump.upper1 <- jump1 + jump1.se*1.96
  jump.lower1 <- jump1 - jump1.se*1.96
  
  diffp <- summary(model)$coefficients["rho:female",4]
  estimate <- c(jump0,jump.upper0,jump.lower0, jump1, jump.upper1,jump.lower1,diffp)
  return(estimate)
}

dem.female.estimates <- matrix(NA, ncol=7, nrow=length(dem.dates))
for( i in 1:length(dem.dates)){
  dem.female.estimates[i,]<-ITS.Model.Dem.female(dem.dates[i])
}
cor(dem.female.estimates[,1], dem.female.estimates[,4], use="pairwise.complete")
#.99

pdf(file="D:/Google Drive/Momentum/Replication/Figures/Appendix/FigureA21a.pdf")
plot(dem.female.estimates[,1], dem.female.estimates[,4], type="n", ylim=c(-10,10), xlim=c(-10,10),
     xlab="Male", ylab="Female")
abline(0,1, lty=2)
segments(dem.female.estimates[,1], dem.female.estimates[,4], dem.female.estimates[,2], dem.female.estimates[,4], col="gray70")
segments(dem.female.estimates[,1], dem.female.estimates[,4], dem.female.estimates[,3], dem.female.estimates[,4], col="gray70")
segments(dem.female.estimates[,1], dem.female.estimates[,4], dem.female.estimates[,1], dem.female.estimates[,5], col="gray70")
segments(dem.female.estimates[,1], dem.female.estimates[,4], dem.female.estimates[,1], dem.female.estimates[,6], col="gray70")
text(dem.female.estimates[,1], dem.female.estimates[,4], label=dprim.labels)
legend("topleft","Cor=.99")
dev.off()


#Placebo

#Make vector of all days
days <- unique(sm.dweighted.pid$time)
#Make vector of days to exclude (primary days +- 2)
dem.dates.placebo <- sort(c(dem.dates, dem.dates+1, dem.dates+2, dem.dates-1, dem.dates-2))

#Keep only those dayes not in dem.dates
days.placebo <- days[days %in% dem.dates.placebo==F]
#Truncate (model can't run with too few data in the tails)
days.placebo <- days.placebo[days.placebo>=17 & days.placebo <=195]
days.placebo <- days.placebo[!is.na(days.placebo)]
estimates <- matrix(NA, nrow=length(days.placebo), ncol=2)

for(i in 1:nrow(estimates)){
  results<- ITS.Model.Dem.female(days.placebo[i])
  estimates[i,1]<- results[1]
  estimates[i,2]<- results[4]
}
dem.female.placebo.estimates <- estimates


pdf(file="D:/Google Drive/Momentum/Replication/Figures/Appendix/FigureA20a.pdf",  height=7, width=10)
plot(clinton.share,dem.female.estimates[,1],type="n", ylim=c(-20,20), xlab="Clinton Delegate Share", 
     ylab="Gamma Coefficient", col="darkblue")
abline(h=0,lty=2,col="gray80")
abline(v=50,lty=2,col="gray80")
segments(clinton.share,dem.female.estimates[,1],clinton.share,dem.female.estimates[,2],col="darkblue")
segments(clinton.share,dem.female.estimates[,1],clinton.share,dem.female.estimates[,3],col="darkblue")
text(clinton.share,dem.female.estimates[,1], labels=dprim.labels,col="darkblue")
abline(h=quantile(dem.female.placebo.estimates[,1],.95), lty=2)
abline(h=quantile(dem.female.placebo.estimates[,1],.05), lty=2)
dev.off()
cor(clinton.share,dem.female.estimates[,1], use="pairwise.complete")

pdf(file="D:/Google Drive/Momentum/Replication/Figures/Appendix/FigureA20b.pdf",  height=7, width=10)
plot(clinton.share,dem.female.estimates[,4],type="n", ylim=c(-20,20), xlab="Clinton Delegate Share", 
     ylab="Gamma Coefficient", col="purple")
abline(h=0,lty=2,col="gray80")
abline(v=50,lty=2,col="gray80")
segments(clinton.share,dem.female.estimates[,4],clinton.share,dem.female.estimates[,5],col="purple")
segments(clinton.share,dem.female.estimates[,4],clinton.share,dem.female.estimates[,6],col="purple")
text(clinton.share,dem.female.estimates[,4], labels=dprim.labels,col="purple")
abline(h=quantile(dem.female.placebo.estimates[,2],.95), lty=2)
abline(h=quantile(dem.female.placebo.estimates[,2],.05), lty=2)
dev.off()
cor(clinton.share,dem.female.estimates[,4], use="pairwise.complete")





#Republicans

ITS.Model.Rep.female <- function(day){
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
                time  + time2 + time3 + time4 + time5 + time6 + rho*female +
                factor(racethn4)*age6*factor(gender)*factor(educ)
              , data=sm.model, weights = DayWeights)
  
  
  jump0<- coef(model)["rho"]
  jump.upper0 <- jump0 + sqrt(vcov(model)["rho","rho"])*1.96
  jump.lower0 <- jump0- sqrt(vcov(model)["rho","rho"])*1.96
  
  jump1 <- coef(model)["rho"] + coef(model)["rho:female"]
  jump1.se <- sqrt(vcov(model)["rho","rho"] + vcov(model)["rho:female" , "rho:female"]+
                     2*vcov(model)["rho","rho:female"])
  jump.upper1 <- jump1 + jump1.se*1.96
  jump.lower1 <- jump1 - jump1.se*1.96
  
  diffp <- summary(model)$coefficients["rho:female",4]
  estimate <- c(jump0,jump.upper0,jump.lower0, jump1, jump.upper1,jump.lower1,diffp)
  return(estimate)
}

rep.female.estimates <- matrix(NA, ncol=7, nrow=length(rep.dates))
for( i in 1:length(rep.dates)){
  rep.female.estimates[i,]<-ITS.Model.Rep.female(rep.dates[i])
}
cor(rep.female.estimates[,1], rep.female.estimates[,4], use="pairwise.complete")
#.98

pdf(file="D:/Google Drive/Momentum/Replication/Figures/Appendix/FigureA21b.pdf")
plot(rep.female.estimates[,1], rep.female.estimates[,4], type="n", ylim=c(-10,10), xlim=c(-10,10),
     xlab="Male", ylab="Female")
abline(0,1, lty=2)
segments(rep.female.estimates[,1], rep.female.estimates[,4], rep.female.estimates[,2], rep.female.estimates[,4], col="gray70")
segments(rep.female.estimates[,1], rep.female.estimates[,4], rep.female.estimates[,3], rep.female.estimates[,4], col="gray70")
segments(rep.female.estimates[,1], rep.female.estimates[,4], rep.female.estimates[,1], rep.female.estimates[,5], col="gray70")
segments(rep.female.estimates[,1], rep.female.estimates[,4], rep.female.estimates[,1], rep.female.estimates[,6], col="gray70")
text(rep.female.estimates[,1], rep.female.estimates[,4], label=rprim.labels)
legend("topleft","Cor=.98")
dev.off()

#Placebo
days <- unique(sm.rweighted.pid$time)

rep.dates.placebo <- sort(c(rep.dates, rep.dates+1, rep.dates+2, rep.dates-1, rep.dates-2))
days.placebo <- days[days %in% rep.dates.placebo==F]
#Truncate (model can't run with too few data in the tails)
days.placebo <- days.placebo[days.placebo>=17 & days.placebo <=156]
days.placebo <- days.placebo[!is.na(days.placebo)]
estimates <- matrix(NA, nrow=length(days.placebo), ncol=2)

for(i in 1:nrow(estimates)){
  results<- ITS.Model.Rep.female(days.placebo[i])
  estimates[i,1]<- results[1]
  estimates[i,2]<- results[4]
}

rep.female.placebo.estimates <- estimates

pdf(file="D:/Google Drive/Momentum/Replication/Figures/Appendix/FigureA20c.pdf",  height=7, width=10)
plot(trump.share,rep.female.estimates[,1],type="n", ylim=c(-20,20), xlab="trump Delegate Share", 
     ylab="Gamma Coefficient", col="firebrick")
abline(h=0,lty=2,col="gray80")
abline(v=50,lty=2,col="gray80")
segments(trump.share,rep.female.estimates[,1],trump.share,rep.female.estimates[,2],col="firebrick")
segments(trump.share,rep.female.estimates[,1],trump.share,rep.female.estimates[,3],col="firebrick")
text(trump.share,rep.female.estimates[,1], labels=rprim.labels,col="firebrick")
abline(h=quantile(rep.female.placebo.estimates[,1],.95), lty=2)
abline(h=quantile(rep.female.placebo.estimates[,1],.05), lty=2)
dev.off()
cor(trump.share,rep.female.estimates[,1], use="pairwise.complete")

pdf(file="D:/Google Drive/Momentum/Replication/Figures/Appendix/FigureA20d.pdf",  height=7, width=10)
plot(trump.share,rep.female.estimates[,4],type="n", ylim=c(-20,20), xlab="trump Delegate Share", 
     ylab="Gamma Coefficient", col="magenta")
abline(h=0,lty=2,col="gray80")
abline(v=50,lty=2,col="gray80")
segments(trump.share,rep.female.estimates[,4],trump.share,rep.female.estimates[,5],col="magenta")
segments(trump.share,rep.female.estimates[,4],trump.share,rep.female.estimates[,6],col="magenta")
text(trump.share,rep.female.estimates[,4], labels=rprim.labels,col="magenta")
abline(h=quantile(rep.female.placebo.estimates[,2],.95), lty=2)
abline(h=quantile(rep.female.placebo.estimates[,2],.05), lty=2)
dev.off()
cor(trump.share,rep.female.estimates[,4], use="pairwise.complete")

#Creation of Tables A10 and A11

dem.table <-  cbind(dprim.labels,round(dem.female.estimates[,1],3),round(dem.female.estimates[,4],3),round(dem.female.estimates[,7],3))
rep.table <-  cbind(rprim.labels,round(rep.female.estimates[,1],3),round(rep.female.estimates[,4],3),round(rep.female.estimates[,7],3))



print(xtable(dem.table),include.rownames=FALSE)
print(xtable(rep.table),include.rownames=FALSE)




