# Clinton, Engelhardt, Trussler 2018 "Knock Out Blows"
# Purpose: This file conducts the omnibus models analysis reported in Appendix 6

# Loading in data
setwd("~/google drive/momentum/Replication/")
load("./data/Survey Monkey Data.Rdata")

#----------------------------------------------------------------------------------------------------#
# The code below produces Appendix 6 Figure 11
# Note: the placebo distribution takes awhile to generate
#----------------------------------------------------------------------------------------------------#
primdates <- read.csv("./data/primdates.csv")
primdates$Date.dprim <- as.Date(primdates$Date.dprim)
primdates$Date.rprim <- as.Date(primdates$Date.rprim)

# Democrats
# Subset to Democratic election dates, ± 2
dates.dem <- unique(primdates$Date.dprim)
dates.dem <- c(dates.dem-2, dates.dem-1, dates.dem, dates.dem+1, dates.dem+2)
dates.dem <- sort(dates.dem)
dates <- unique(sm.dweighted.pid$Date)
# Placebo days
dates <- dates[dates %in% dates.dem == F]

#### Need to run not just single date. Draw same length vector?
dates.clinton.wins <- unique(primdates$Date.dprim[which(primdates$clinton.win == 1)]) 
dates.clinton.loss <- unique(primdates$Date.dprim[which(primdates$clinton.win == 0)]) 


set.seed(1968)
dem.estimates <- array(NA, c(1000,2))
for(i in 1:nrow(dem.estimates)){
  print(i)
  win.days <- sort(sample(dates, length(dates.clinton.wins)))
  
  sm.dweighted.pid$placebo.win <- NA
  for(j in 1:length(win.days)){
    sm.dweighted.pid$placebo.win[which(sm.dweighted.pid$Date - win.days[j] <= 3 & sm.dweighted.pid$Date - win.days[j] >= 1)] <- 1
  }
  sm.dweighted.pid$placebo.win[which(is.na(sm.dweighted.pid$placebo.win))] <- 0
  sm.dweighted.pid$placebo.win[which(is.na(sm.dweighted.pid$clinton))] <- NA
  
  m <- lm(clinton ~ placebo.win + time + time2 + time3 + time4 + time5 + time6 + 
            as.factor(age6) + as.factor(educ4) + as.factor(racethn4) + as.factor(gender) + 
            as.factor(state), 
          data = sm.dweighted.pid, weights = DayWeights)
  dem.estimates[i, 1] <- coef(m)["placebo.win"]
  
  loss.days <- sort(sample(dates, length(dates.clinton.loss)))
  
  
  sm.dweighted.pid$placebo.loss <- NA
  for(j in 1:length(loss.days)){
    sm.dweighted.pid$placebo.loss[which(sm.dweighted.pid$Date - loss.days[j] <= 3 & sm.dweighted.pid$Date - loss.days[j] >= 1)] <- 1
  }
  sm.dweighted.pid$placebo.loss[which(is.na(sm.dweighted.pid$placebo.loss))] <- 0
  sm.dweighted.pid$placebo.loss[which(is.na(sm.dweighted.pid$clinton))] <- NA
  
  m <- lm(clinton ~ placebo.loss + time + time2 + time3 + time4 + time5 + time6 + 
            as.factor(age6) + as.factor(educ4) + as.factor(racethn4) + as.factor(gender) + 
            as.factor(state), 
          data = sm.dweighted.pid, weights = DayWeights)
  dem.estimates[i, 2] <- coef(m)["placebo.loss"]
}

m1.pid.c <- lm(clinton ~ clinton_win_3day + time + time2 + time3 + time4 + time5 + time6 +
                 as.factor(age6) + as.factor(educ4) + as.factor(racethn4) + as.factor(gender) +
                 as.factor(state),
               data = sm.dweighted.pid, weights = DayWeights)
summary(m1.pid.c)

m1.pid.c.loss <- lm(clinton ~ clinton_loss_3day + time + time2 + time3 + time4 + time5 + time6 +
                      as.factor(age6) + as.factor(educ4) + as.factor(racethn4) + as.factor(gender) +
                      as.factor(state),
                    data = sm.dweighted.pid, weights = DayWeights)
summary(m1.pid.c.loss)


# Republicans
# Subset to Republican election dates, ± 2
dates.rep <- unique(primdates$Date.rprim)
dates.rep <- c(dates.rep-2, dates.rep-1, dates.rep, dates.rep+1, dates.rep+2)
dates.rep <- sort(dates.rep)
dates <- unique(sm.rweighted.pid$Date)
# Placebo days
dates <- dates[dates %in% dates.rep == F]

dates.trump.wins <- unique(primdates$Date.rprim[which(primdates$trump.win == 1)]) 
dates.trump.loss <- unique(primdates$Date.rprim[which(primdates$trump.win == 0)]) 


set.seed(1972)
rep.estimates <- array(NA, c(1000, 2))
for(i in 1:nrow(rep.estimates)){
  print(i)
  win.days <- sort(sample(dates, length(dates.trump.wins)))
  
  sm.rweighted.pid$placebo.win <- NA
  for(j in 1:length(win.days)){
    sm.rweighted.pid$placebo.win[which(sm.rweighted.pid$Date - win.days[j] <= 3 & sm.rweighted.pid$Date - win.days[j] >= 1)] <- 1
  }
  sm.rweighted.pid$placebo.win[which(is.na(sm.rweighted.pid$placebo.win))] <- 0
  sm.rweighted.pid$placebo.win[which(is.na(sm.rweighted.pid$trump))] <- NA
  
  m <- lm(trump ~ placebo.win + + time + time2 + time3 + time4 + time5 + time6 +
            as.factor(age6) + as.factor(educ4) + as.factor(racethn4) + as.factor(gender), 
          data = sm.rweighted.pid, weights = DayWeights)
  rep.estimates[i, 1] <- coef(m)["placebo.win"]
  
  loss.days <- sort(sample(dates, length(dates.trump.loss)))
  
  sm.rweighted.pid$placebo.loss <- NA
  for(j in 1:length(loss.days)){
    sm.rweighted.pid$placebo.loss[which(sm.rweighted.pid$Date - loss.days[j] <= 3 & sm.rweighted.pid$Date - loss.days[j] >= 1)] <- 1
  }
  sm.rweighted.pid$placebo.loss[which(is.na(sm.rweighted.pid$placebo.loss))] <- 0
  sm.rweighted.pid$placebo.loss[which(is.na(sm.rweighted.pid$trump))] <- NA
  
  m <- lm(trump ~ placebo.loss + + time + time2 + time3 + time4 + time5 + time6 +
            as.factor(age6) + as.factor(educ4) + as.factor(racethn4) + as.factor(gender), 
          data = sm.rweighted.pid, weights = DayWeights)
  rep.estimates[i, 2] <- coef(m)["placebo.loss"]
  
}

m1.pid.t <- lm(trump ~ trump_win_3day +  time + time2 + time3 + time4 + time5 + time6 +
                 as.factor(age6) + as.factor(educ4) + as.factor(racethn4) + as.factor(gender) +
                 as.factor(state),
               data = sm.rweighted.pid, weights = DayWeights)
summary(m1.pid.t)

m1.pid.t.loss <- lm(trump ~ trump_loss_3day +  time + time2 + time3 + time4 + time5 + time6 +
                      as.factor(age6) + as.factor(educ4) + as.factor(racethn4) + as.factor(gender) +
                      as.factor(state),
                    data = sm.rweighted.pid, weights = DayWeights)
summary(m1.pid.t.loss)



#### Plotting
# pdf("./figures/appendix/FigureA11.pdf", width = 12, height = 8)
# Modify to include losses
par(mfcol = c(2,2))
plot(density(dem.estimates[,1]), main = "Clinton Wins",
     xlab = "Effect Estimates", axes = F, lty = 2, col = "grey",
     ylim = c(0, .7), xlim = c(-4,4))
abline(v=coef(m1.pid.c)["clinton_win_3day"], lwd = 2)
abline(v=quantile(dem.estimates[,1], c(0.025,0.975)), 
       lwd = 2, lty = 2, col = "darkgrey")
axis(1); axis(2)
plot(density(dem.estimates[,2]), main = "Clinton Losses",
     xlab = "Effect Estimates", axes = F, lty = 2, col = "grey",
     ylim = c(0, .5), xlim = c(-4,4))
abline(v=coef(m1.pid.c.loss)["clinton_loss_3day"], lwd = 2, lty = 1)
abline(v=quantile(dem.estimates[,2], c(0.025,0.975)), 
       lwd = 2, lty = 2, col = "darkgrey")
axis(1); axis(2)
plot(density(rep.estimates[,1]), main = "Trump Wins",
     xlab = "Effect Estimates", axes = F, lty = 2, col = "grey",
     ylim = c(0, .5), xlim = c(-4,4))
abline(v=coef(m1.pid.t)["trump_win_3day"], lwd = 2)
abline(v=quantile(rep.estimates[,1], c(0.025,0.975)), 
       lwd = 2, lty = 2, col = "darkgrey")
axis(1); axis(2)
plot(density(rep.estimates[,2]), main = "Trump Losses",
     xlab = "Effect Estimates", axes = F, lty = 2, col = "grey",
     ylim = c(0, .5), xlim = c(-4,4))
abline(v=coef(m1.pid.t.loss)["trump_loss_3day"], lwd = 2, lty = 1)
abline(v=quantile(rep.estimates[,2], c(0.025,0.975)), 
       lwd = 2, lty = 2, col = "darkgrey")
axis(1); axis(2)
# dev.off()


#----------------------------------------------------------------------------------------------------#
# The code below produces Appendix 6 Figure 12
# Note: the placebo distribution takes awhile to generate
#----------------------------------------------------------------------------------------------------#
primdates <- read.csv("./data/primdates.csv")
primdates$Date.dprim <- as.Date(primdates$Date.dprim)
primdates$Date.rprim <- as.Date(primdates$Date.rprim)

m10.pid.c <- lm(clinton ~ FE.d.IA + FE.d.NH + FE.d.NV + FE.d.ST +
                  FE.d.MI + FE.d.FL + FE.d.AZ + FE.d.WI + FE.d.NY +
                  FE.d.AC + FE.d.IN + FE.d.WV + FE.d.OR + FE.d.CA +
                  time + time2 + time3 + time4 + time5 + time6 + 
                  as.factor(age6) + as.factor(educ4) + as.factor(racethn4) + as.factor(gender) +
                  as.factor(state), 
                data = sm.dweighted.pid, weights = DayWeights)
summary(m10.pid.c)
FEs.dem <- grep("FE", names(coef(m10.pid.c)), value = T)


#### Placebo Analyses
# Democrats
# Subset to Democratic election dates, ± 2
dates.dem <- unique(primdates$Date.dprim)
dates.dem <- c(dates.dem-2, dates.dem-1, dates.dem, dates.dem+1, dates.dem+2)
dates.dem <- sort(dates.dem)
dates <- unique(sm.dweighted.pid$Date)
# Placebo days
dates <- dates[dates %in% dates.dem == F]

# Index to start Placebo day addition
col.n <- ncol(sm.dweighted.pid)

# Contests
dem.contests <- unique(primdates$Date.dprim)


set.seed(1968)
dem.estimates.fe <- array(NA, c(1000, length(FEs.dem)))

for(i in 1:nrow(dem.estimates.fe)){
  print(i)
  days <- sort(sample(dates, length(FEs.dem))) # placebo days as many as FEs
  
  for(j in 1:length(days)){
    sm.dweighted.pid[,col.n + j] <- NA
    sm.dweighted.pid[,col.n + j][which(sm.dweighted.pid$Date > days[j])] <- 1
    sm.dweighted.pid[,col.n + j][which(sm.dweighted.pid$Date < days[j])] <- 0
  }
  
  names(sm.dweighted.pid)[(col.n + 1):(col.n + length(days))] <- paste0("p", 1:length(days))
  
  pd.vars <- paste0("p", 1:length(days))
  vars <- c(pd.vars, 
            "time",
            "time2", 
            "time3", 
            "time4", 
            "time5", 
            "time6", 
            "as.factor(age6)",
            "as.factor(educ4)", 
            "as.factor(racethn4)", 
            "as.factor(gender)",
            "as.factor(state)")
  fml <- as.formula(paste("clinton ~ ",
                          paste(vars, collapse = "+")))
  m <- lm(fml, 
          data = sm.dweighted.pid, weights = DayWeights)
  dem.estimates.fe[i,] <- coef(m)[grep("^p", names(coef(m)))]
  
}


# Republicans
m10.pid.t <- lm(trump ~ FE.r.IA + FE.r.NH + FE.r.SC + FE.r.ST +
                  FE.r.LA + FE.r.FL + FE.r.AZ + FE.r.WI + FE.r.NY +
                  FE.r.AC + FE.r.IN +
                  time + time2 + time3 + time4 + time5 + time6 +
                  as.factor(age6) + as.factor(educ4) + as.factor(racethn4) + as.factor(gender) +
                  as.factor(state), 
                data = sm.rweighted.pid, weights = DayWeights)
summary(m10.pid.t)
FEs.rep <- grep("FE", names(coef(m10.pid.t)), value = T)

# Subset to Republican election dates, ± 2
dates.rep <- unique(primdates$Date.rprim)
dates.rep <- c(dates.rep-2, dates.rep-1, dates.rep, dates.rep+1, dates.rep+2)
dates.rep <- sort(dates.rep)
dates <- unique(sm.rweighted.pid$Date)
# Placebo days
dates <- dates[dates %in% dates.rep == F]

# Index to start Placebo day addition
col.n <- ncol(sm.rweighted.pid)

# Contests
rep.contests <- unique(primdates$Date.rprim)

set.seed(1972)
rep.estimates.fe <- array(NA, c(1000, length(FEs.rep)))
for(i in 1:nrow(rep.estimates.fe)){
  print(i)
  days <- sort(sample(dates, length(FEs.rep)))
  
  for(j in 1:length(days)){
    sm.rweighted.pid[,col.n + j] <- NA
    sm.rweighted.pid[,col.n + j][which(sm.rweighted.pid$Date > days[j])] <- 1
    sm.rweighted.pid[,col.n + j][which(sm.rweighted.pid$Date < days[j])] <- 0
  }
  
  names(sm.rweighted.pid)[(col.n + 1):(col.n + length(days))] <- paste0("p", 1:length(days))
  
  pd.vars <- paste0("p", 1:length(days))
  vars <- c(pd.vars, 
            "time",
            "time2", 
            "time3", 
            "time4", 
            "time5", 
            "time6", 
            "as.factor(age6)",
            "as.factor(educ4)", 
            "as.factor(racethn4)", 
            "as.factor(gender)",
            "as.factor(state)")
  fml <- as.formula(paste("trump ~ ",
                          paste(vars, collapse = "+")))
  m <- lm(fml, 
          data = sm.rweighted.pid, weights = DayWeights)
  rep.estimates.fe[i,] <- coef(m)[grep("^p", names(coef(m)))]
}



#### Plotting
# pdf("./figures/appendix/FigureA12.pdf", width = 12, height = 6)
par(mfrow = c(1,2))
plot(density(dem.estimates.fe, na.rm = T), main = "Democrats",
     xlab = "Effect Estimates", axes = F, lty = 2,
     ylim = c(0, .13), xlim = c(-20,40))
abline(v=coef(m10.pid.c)[FEs.dem])
abline(v=quantile(dem.estimates.fe, c(0.025,0.975), na.rm = T), 
       lwd = 2, lty = 2, col = "darkgrey")
axis(1); axis(2)
plot(density(rep.estimates.fe, na.rm = T), main = "Republicans",
     xlab = "Effect Estimates", axes = F, lty = 2,
     ylim = c(0, .13), xlim = c(-20,40))
abline(v=coef(m10.pid.t)[FEs.rep])
abline(v=quantile(rep.estimates.fe, c(0.025,0.975), na.rm = T), 
       lwd = 2, lty = 2, col = "darkgrey")
axis(1); axis(2)
# dev.off()