# Clinton, Engelhardt, Trussler 2018 "Knock Out Blows"
# Purpose: This file produces survey margins of error for specific subgroups proximate to each cutpoint.

rm(list = ls())
library(foreign)
library(xtable)

#Load SM Data
load("D:/Google Drive/Momentum/Replication/Data/Survey Monkey Data.Rdata")


#Define Dates
dem.dates <- c(63,71,82,92,99,106,113,127,141,148,155,162,169,190)
dprim.labels <- c("IA","NH","NV","ST","MI","FL","AZ","WI","NY","AC","IN","WV","OR","CA")
rep.dates <- c(63,71,82,92,96,103,113,127,141,148,155)
rprim.labels <- c("IA","NH","SC","ST","LA","FL","AZ","WI","NY","AC","IN")


################################
#Power Analysis#################
################################


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


table(sm.dweighted.pid$st.voted[sm.dweighted.pid$state=="Iowa"],sm.dweighted.pid$time[sm.dweighted.pid$state=="Iowa"])



table(sm.dweighted.pid$party5, sm.dweighted.pid$clinton)

sm.dweighted.pid$indy <- NA
sm.dweighted.pid$indy[sm.dweighted.pid$party5=="Lean Dem"]<-1
sm.dweighted.pid$indy[sm.dweighted.pid$party5=="Democrat"]<-0
table(sm.dweighted.pid$indy, sm.dweighted.pid$clinton)

sm.rweighted.pid$indy <- NA
sm.rweighted.pid$indy[sm.rweighted.pid$party5=="Lean Rep"]<-1
sm.rweighted.pid$indy[sm.rweighted.pid$party5=="Republican"]<-0
table(sm.rweighted.pid$indy, sm.rweighted.pid$trump)

table(sm.dweighted.pid$gender, sm.dweighted.pid$clinton)

sm.dweighted.pid$female <- NA
sm.dweighted.pid$female[sm.dweighted.pid$gender=="Female"]<-1
sm.dweighted.pid$female[sm.dweighted.pid$gender=="Male"]<-0
table(sm.dweighted.pid$female, sm.dweighted.pid$clinton)

sm.rweighted.pid$female <- NA
sm.rweighted.pid$female[sm.rweighted.pid$gender=="Female"]<-1
sm.rweighted.pid$female[sm.rweighted.pid$gender=="Male"]<-0
table(sm.rweighted.pid$female, sm.rweighted.pid$trump)



#Democrats: 

overall.n <- rep(NA, length(dem.dates))
notcollege.n <- rep(NA, length(dem.dates))
college.n <- rep(NA, length(dem.dates))
notvoted.n <- rep(NA, length(dem.dates))
voted.n <- rep(NA, length(dem.dates))
notindy.n <- rep(NA, length(dem.dates))
indy.n <- rep(NA, length(dem.dates))
male.n <- rep(NA, length(dem.dates))
female.n <- rep(NA, length(dem.dates))
for(i in 1:length(dem.dates)){
  overall.n[i] <- round(0.98 / sqrt(table(is.na(sm.dweighted.pid$clinton[sm.dweighted.pid$time>=dem.dates[i] - 3 
                                                                         & sm.dweighted.pid$time<=dem.dates[i] + 3]))[1]),4)*100
  notcollege.n[i] <- round(0.98 / sqrt(table(is.na(sm.dweighted.pid$clinton[sm.dweighted.pid$college.graduate==0 & 
                                                                              (sm.dweighted.pid$time>=dem.dates[i] - 3 
                                                                               & sm.dweighted.pid$time<=dem.dates[i] + 3)]))[1]),4)*100
  college.n[i] <-  round(0.98 / sqrt(table(is.na(sm.dweighted.pid$clinton[sm.dweighted.pid$college.graduate==1 & 
                                                                            (sm.dweighted.pid$time>=dem.dates[i] - 3 
                                                                             & sm.dweighted.pid$time<=dem.dates[i] + 3)]))[1]),4)*100
  notvoted.n[i] <- round(0.98 / sqrt(table(is.na(sm.dweighted.pid$clinton[sm.dweighted.pid$st.voted==0 & 
                                                                            (sm.dweighted.pid$time>=dem.dates[i] - 3 
                                                                             & sm.dweighted.pid$time<=dem.dates[i] + 3)]))[1]),4)*100
  voted.n[i] <-  round(0.98 / sqrt(table(is.na(sm.dweighted.pid$clinton[sm.dweighted.pid$st.voted==1 & 
                                                                          (sm.dweighted.pid$time>=dem.dates[i] - 3 
                                                                           & sm.dweighted.pid$time<=dem.dates[i] + 3)]))[1]),4)*100
  notindy.n[i] <- round(0.98 / sqrt(table(is.na(sm.dweighted.pid$clinton[sm.dweighted.pid$indy==0 & 
                                                                           (sm.dweighted.pid$time>=dem.dates[i] - 3 
                                                                            & sm.dweighted.pid$time<=dem.dates[i] + 3)]))[1]),4)*100
  indy.n[i] <-  round(0.98 / sqrt(table(is.na(sm.dweighted.pid$clinton[sm.dweighted.pid$indy==1 & 
                                                                         (sm.dweighted.pid$time>=dem.dates[i] - 3 
                                                                          & sm.dweighted.pid$time<=dem.dates[i] + 3)]))[1]),4)*100
  male.n[i] <- round(0.98 / sqrt(table(is.na(sm.dweighted.pid$clinton[sm.dweighted.pid$female==0 & 
                                                                        (sm.dweighted.pid$time>=dem.dates[i] - 3 
                                                                         & sm.dweighted.pid$time<=dem.dates[i] + 3)]))[1]),4)*100
  female.n[i] <-  round(0.98 / sqrt(table(is.na(sm.dweighted.pid$clinton[sm.dweighted.pid$female==1 & 
                                                                           (sm.dweighted.pid$time>=dem.dates[i] - 3 
                                                                            & sm.dweighted.pid$time<=dem.dates[i] + 3)]))[1]),4)*100
}


dem.moe <-cbind(dprim.labels, overall.n,notcollege.n,college.n, notvoted.n,voted.n,notindy.n,indy.n, male.n,female.n)

#Republicans
overall.n <- rep(NA, length(rep.dates))
notcollege.n <- rep(NA, length(rep.dates))
college.n <- rep(NA, length(rep.dates))
notvoted.n <- rep(NA, length(rep.dates))
voted.n <- rep(NA, length(rep.dates))
notindy.n <- rep(NA, length(rep.dates))
indy.n <- rep(NA, length(rep.dates))
male.n <- rep(NA, length(rep.dates))
female.n <- rep(NA, length(rep.dates))
for(i in 1:length(rep.dates)){
  overall.n[i] <- round(0.98 / sqrt(table(is.na(sm.rweighted.pid$trump[sm.rweighted.pid$time>=rep.dates[i] - 3 
                                                                       & sm.rweighted.pid$time<=rep.dates[i] + 3]))[1]),4)*100
  notcollege.n[i] <- round(0.98 / sqrt(table(is.na(sm.rweighted.pid$trump[sm.rweighted.pid$college.graduate==0 & 
                                                                            (sm.rweighted.pid$time>=rep.dates[i] - 3 
                                                                             & sm.rweighted.pid$time<=rep.dates[i] + 3)]))[1]),4)*100
  college.n[i] <-  round(0.98 / sqrt(table(is.na(sm.rweighted.pid$trump[sm.rweighted.pid$college.graduate==1 & 
                                                                          (sm.rweighted.pid$time>=rep.dates[i] - 3 
                                                                           & sm.rweighted.pid$time<=rep.dates[i] + 3)]))[1]),4)*100
  notvoted.n[i] <- round(0.98 / sqrt(table(is.na(sm.rweighted.pid$trump[sm.rweighted.pid$st.voted==0 & 
                                                                          (sm.rweighted.pid$time>=rep.dates[i] - 3 
                                                                           & sm.rweighted.pid$time<=rep.dates[i] + 3)]))[1]),4)*100
  voted.n[i] <-  round(0.98 / sqrt(table(is.na(sm.rweighted.pid$trump[sm.rweighted.pid$st.voted==1 & 
                                                                        (sm.rweighted.pid$time>=rep.dates[i] - 3 
                                                                         & sm.rweighted.pid$time<=rep.dates[i] + 3)]))[1]),4)*100
  notindy.n[i] <- round(0.98 / sqrt(table(is.na(sm.rweighted.pid$trump[sm.rweighted.pid$indy==0 & 
                                                                         (sm.rweighted.pid$time>=rep.dates[i] - 3 
                                                                          & sm.rweighted.pid$time<=rep.dates[i] + 3)]))[1]),4)*100
  indy.n[i] <-  round(0.98 / sqrt(table(is.na(sm.rweighted.pid$trump[sm.rweighted.pid$indy==1 & 
                                                                       (sm.rweighted.pid$time>=rep.dates[i] - 3 
                                                                        & sm.rweighted.pid$time<=rep.dates[i] + 3)]))[1]),4)*100
  male.n[i] <- round(0.98 / sqrt(table(is.na(sm.rweighted.pid$trump[sm.rweighted.pid$female==0 & 
                                                                      (sm.rweighted.pid$time>=rep.dates[i] - 3 
                                                                       & sm.rweighted.pid$time<=rep.dates[i] + 3)]))[1]),4)*100
  female.n[i] <-  round(0.98 / sqrt(table(is.na(sm.rweighted.pid$trump[sm.rweighted.pid$female==1 & 
                                                                         (sm.rweighted.pid$time>=rep.dates[i] - 3 
                                                                          & sm.rweighted.pid$time<=rep.dates[i] + 3)]))[1]),4)*100
}


rep.moe <-cbind(rprim.labels, overall.n,notcollege.n,college.n, notvoted.n,voted.n,notindy.n,indy.n, male.n,female.n)


#Print Tables A12 and A13
library(xtable)
print(xtable(dem.moe),include.rownames=FALSE)
print(xtable(rep.moe),include.rownames=FALSE)

