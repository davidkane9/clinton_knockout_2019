# The code below

# Loading in data
setwd("~/google drive/momentum/Replication/")
load("./data/Survey Monkey Data.Rdata")

# Crearting Month Indicators
sm.dweighted.pid$dec <- NA
sm.dweighted.pid$dec <- ifelse(sm.dweighted.pid$Date >= "2015-12-01" & sm.dweighted.pid$Date <= "2015-12-31", 1, 0)
sm.dweighted.pid$jan <- NA
sm.dweighted.pid$jan <- ifelse(sm.dweighted.pid$Date >= "2016-01-01" & sm.dweighted.pid$Date <= "2016-01-31", 1, 0)
sm.dweighted.pid$feb <- NA
sm.dweighted.pid$feb <- ifelse(sm.dweighted.pid$Date >= "2016-02-01" & sm.dweighted.pid$Date <= "2016-02-29", 1, 0)
sm.dweighted.pid$march <- NA
sm.dweighted.pid$march <- ifelse(sm.dweighted.pid$Date >= "2016-03-01" & sm.dweighted.pid$Date <= "2016-03-31", 1, 0)
sm.dweighted.pid$april <- NA
sm.dweighted.pid$april <- ifelse(sm.dweighted.pid$Date >= "2016-04-01" & sm.dweighted.pid$Date <= "2016-04-30", 1, 0)
sm.dweighted.pid$may <- NA
sm.dweighted.pid$may <- ifelse(sm.dweighted.pid$Date >= "2016-05-01" & sm.dweighted.pid$Date <= "2016-05-31", 1, 0)
sm.dweighted.pid$june <- NA
sm.dweighted.pid$june <- ifelse(sm.dweighted.pid$Date >= "2016-06-01" & sm.dweighted.pid$Date <= "2016-06-30", 1, 0)

sm.rweighted.pid$dec <- NA
sm.rweighted.pid$dec <- ifelse(sm.rweighted.pid$Date >= "2015-12-01" & sm.rweighted.pid$Date <= "2015-12-31", 1, 0)
sm.rweighted.pid$jan <- NA
sm.rweighted.pid$jan <- ifelse(sm.rweighted.pid$Date >= "2016-01-01" & sm.rweighted.pid$Date <= "2016-01-31", 1, 0)
sm.rweighted.pid$feb <- NA
sm.rweighted.pid$feb <- ifelse(sm.rweighted.pid$Date >= "2016-02-01" & sm.rweighted.pid$Date <= "2016-02-29", 1, 0)
sm.rweighted.pid$march <- NA
sm.rweighted.pid$march <- ifelse(sm.rweighted.pid$Date >= "2016-03-01" & sm.rweighted.pid$Date <= "2016-03-31", 1, 0)
sm.rweighted.pid$april <- NA
sm.rweighted.pid$april <- ifelse(sm.rweighted.pid$Date >= "2016-04-01" & sm.rweighted.pid$Date <= "2016-04-30", 1, 0)
sm.rweighted.pid$may <- NA
sm.rweighted.pid$may <- ifelse(sm.rweighted.pid$Date >= "2016-05-01" & sm.rweighted.pid$Date <= "2016-05-31", 1, 0)
sm.rweighted.pid$june <- NA
sm.rweighted.pid$june <- ifelse(sm.rweighted.pid$Date >= "2016-06-01" & sm.rweighted.pid$Date <= "2016-06-30", 1, 0)


#----------------------------------------------------------------------------------------------------------#
# The following code produces Appendix 5 Figure 8
# Note: Baseline models take awhile to run due to data set size and interactions
#----------------------------------------------------------------------------------------------------------#
### Democratic Primary
# Baseline Model
m.dec.c.full <- glm(as.factor(clinton) ~ as.factor(age6)*as.factor(educ4)*as.factor(racethn4)*as.factor(region)*as.factor(gender)*as.factor(ideology3), 
                    family=binomial(link="probit"),
                    data = sm.dweighted.pid, weights = DayWeights, subset = dec == 1)
summary(m.dec.c.full)

correct.dem <- data.frame(array(NA, c(6, 2)))
correct.dem[,1] <- c("jan", "feb", "march", "april", "may", "june" )
colnames(correct.dem) <- c("month", "pct.c")
# January Predictions
jan.pred <- predict(m.dec.c.full, subset(sm.dweighted.pid, jan == 1 & !is.na(clinton)),  type = "response")
preds.jan <- cbind(subset(sm.dweighted.pid, jan == 1 & !is.na(clinton))$clinton, jan.pred,
                   ifelse(jan.pred > 0.5, 1, 0))
correct.dem[1,2] <- sum(diag(prop.table(table(preds.jan[,1], preds.jan[,3])))*100)
# February Predictions
feb.pred <- predict(m.dec.c.full, subset(sm.dweighted.pid, feb == 1 & !is.na(clinton)),  type = "response")
preds.feb <- cbind(subset(sm.dweighted.pid, feb == 1 & !is.na(clinton))$clinton, feb.pred,
                   ifelse(feb.pred > 0.5, 1, 0))
correct.dem[2,2] <- sum(diag(prop.table(table(preds.feb[,1], preds.feb[,3])))*100)
# March Predictions
mar.pred <- predict(m.dec.c.full, subset(sm.dweighted.pid, march == 1 & !is.na(clinton)),  type = "response")
preds.mar <- cbind(subset(sm.dweighted.pid, march == 1 & !is.na(clinton))$clinton, mar.pred,
                   ifelse(mar.pred > 0.5, 1, 0))
correct.dem[3,2] <- sum(diag(prop.table(table(preds.mar[,1], preds.mar[,3])))*100)
# April Predictions
apr.pred <- predict(m.dec.c.full, subset(sm.dweighted.pid, april == 1 & !is.na(clinton)),  type = "response")
preds.apr <- cbind(subset(sm.dweighted.pid, april == 1 & !is.na(clinton))$clinton, apr.pred,
                   ifelse(apr.pred > 0.5, 1, 0))
correct.dem[4,2] <- sum(diag(prop.table(table(preds.apr[,1], preds.apr[,3])))*100)
# May Predictions
may.pred <- predict(m.dec.c.full, subset(sm.dweighted.pid, may == 1 & !is.na(clinton)),  type = "response")
preds.may <- cbind(subset(sm.dweighted.pid, may == 1 & !is.na(clinton))$clinton, may.pred,
                   ifelse(may.pred > 0.5, 1, 0))
correct.dem[5,2] <- sum(diag(prop.table(table(preds.may[,1], preds.may[,3])))*100)
# June Predictions
jun.pred <- predict(m.dec.c.full, subset(sm.dweighted.pid, june == 1 & !is.na(clinton)),  type = "response")
preds.jun <- cbind(subset(sm.dweighted.pid, june == 1 & !is.na(clinton))$clinton, jun.pred,
                   ifelse(jun.pred > 0.5, 1, 0))
correct.dem[6,2] <- sum(diag(prop.table(table(preds.jun[,1], preds.jun[,3])))*100)


### Republicam Primary
# Baseline Model
m.dec.t.full <- glm(as.factor(trump) ~ as.factor(age6)*as.factor(educ4)*as.factor(racethn4)*as.factor(region)*as.factor(gender)*as.factor(ideology3), 
                    family=binomial(link="probit"),
                    data = sm.rweighted.pid, weights = DayWeights, subset = dec == 1)
summary(m.dec.t.full)

correct.rep <- data.frame(array(NA, c(6, 2)))
correct.rep[,1] <- c("jan", "feb", "march", "april", "may", "june" )
colnames(correct.rep) <- c("month", "pct.c")
# January
jan.pred <- predict(m.dec.t.full, subset(sm.rweighted.pid, jan == 1 & !is.na(trump)),  type = "response")
preds.jan <- cbind(subset(sm.rweighted.pid, jan == 1 & !is.na(trump))$trump, jan.pred,
                   ifelse(jan.pred > 0.5, 1, 0))
correct.rep[1,2] <- sum(diag(prop.table(table(preds.jan[,1], preds.jan[,3])))*100)
# February
feb.pred <- predict(m.dec.t.full, subset(sm.rweighted.pid, feb == 1 & !is.na(trump)),  type = "response")
preds.feb <- cbind(subset(sm.rweighted.pid, feb == 1 & !is.na(trump))$trump, feb.pred,
                   ifelse(feb.pred > 0.5, 1, 0))
correct.rep[2,2] <- sum(diag(prop.table(table(preds.feb[,1], preds.feb[,3])))*100)
# March
mar.pred <- predict(m.dec.t.full, subset(sm.rweighted.pid, march == 1 & !is.na(trump)),  type = "response")
preds.mar <- cbind(subset(sm.rweighted.pid, march == 1 & !is.na(trump))$trump, mar.pred,
                   ifelse(mar.pred > 0.5, 1, 0))
correct.rep[3,2] <- sum(diag(prop.table(table(preds.mar[,1], preds.mar[,3])))*100)
# April
apr.pred <- predict(m.dec.t.full, subset(sm.rweighted.pid, april == 1 & !is.na(trump)),  type = "response")
preds.apr <- cbind(subset(sm.rweighted.pid, april == 1 & !is.na(trump))$trump, apr.pred,
                   ifelse(apr.pred > 0.5, 1, 0))
correct.rep[4,2] <- sum(diag(prop.table(table(preds.apr[,1], preds.apr[,3])))*100)
# May
may.pred <- predict(m.dec.t.full, subset(sm.rweighted.pid, may == 1 & !is.na(trump)),  type = "response")
preds.may <- cbind(subset(sm.rweighted.pid, may == 1 & !is.na(trump))$trump, may.pred,
                   ifelse(may.pred > 0.5, 1, 0))
correct.rep[5,2] <- sum(diag(prop.table(table(preds.may[,1], preds.may[,3])))*100)
# June
jun.pred <- predict(m.dec.t.full, subset(sm.rweighted.pid, june == 1 & !is.na(trump)),  type = "response")
preds.jun <- cbind(subset(sm.rweighted.pid, june == 1 & !is.na(trump))$trump, jun.pred,
                   ifelse(jun.pred > 0.5, 1, 0))
correct.rep[6,2] <- sum(diag(prop.table(table(preds.jun[,1], preds.jun[,3])))*100)


# pdf("./figures/fig_A5_8.pdf", width = 8.75, height = 6.25)
plot(correct.dem[,2], type = "b", lwd = 2, col = "black", axes = F, pch = 16,
     main = "", 
     xlab = "Month", ylab = "Percent Correctly Predicted", ylim = c(0, 100))
lines(correct.rep[-6,2], type = "b", lwd = 2, col = "grey", pch = 15, lty = 2)
axis(1, at = 1:6, labels = c("January", "February", "March", "April", "May", "June"))
axis(2, las = 1)
legend("topleft", c("Democrat", "Republican"), pch = c(16, 15), col = c("black", "grey"), 
       bty = "n", lty = c(1, 2))
# dev.off()


#----------------------------------------------------------------------------------------------------------#
# The following code produces Appendix 5 Figure 8
# Note: Baseline models take awhile to run due to data set size and interactions
#----------------------------------------------------------------------------------------------------------#
# Democrats
# Baseline Model
m.dec.c.full <- glm(as.factor(clinton) ~ as.factor(age6)*as.factor(educ4)*as.factor(racethn4)*as.factor(region)*as.factor(gender)*as.factor(ideology3), 
                    family=binomial(link="probit"),
                    data = sm.dweighted.pid, weights = DayWeights, subset = dec == 1)
summary(m.dec.c.full)

DATES <- sort(unique(sm.dweighted.pid$Date))
DATES <- grep("2016", DATES, value = T)
correct.dem.day <- data.frame(array(NA, c(length(DATES), 5)))
correct.dem.day[,1] <- as.Date(DATES)
colnames(correct.dem.day) <- c("day", "pct.c.hrc", "pct.c.bs", "pct.w.hrc", "pct.w.bs")

for(i in 1:length(DATES)){
  preds <- predict(m.dec.c.full, subset(sm.dweighted.pid, Date == DATES[i] & !is.na(clinton)),
                   type = "response")
  preds.df <- cbind(subset(sm.dweighted.pid, Date == DATES[i] & !is.na(clinton))$clinton, preds,
                    ifelse(preds > 0.5, 1, 0))
  correct.dem.day[i,2] <- prop.table(table(preds.df[,1], preds.df[,3]))["100","1"]*100
  correct.dem.day[i,3] <- prop.table(table(preds.df[,1], preds.df[,3]))["0","0"]*100
  correct.dem.day[i,4] <- prop.table(table(preds.df[,1], preds.df[,3]))["0","1"]*100 # missclassifying BS as HRC
  correct.dem.day[i,5] <- prop.table(table(preds.df[,1], preds.df[,3]))["100","0"]*100
}

## Republicans
# Baseline Model
m.dec.t.full <- glm(as.factor(trump) ~ as.factor(age6)*as.factor(educ4)*as.factor(racethn4)*as.factor(region)*as.factor(gender)*as.factor(ideology3), 
                    family=binomial(link="probit"),
                    data = sm.rweighted.pid, weights = DayWeights, subset = dec == 1)
summary(m.dec.t.full)

DATES <- sort(unique(sm.rweighted.pid$Date))
TRUMP <- trump.mean.pid[grep("2016", DATES)]
DATES <- grep("2016", DATES, value = T)
correct.rep.day <- data.frame(array(NA, c(length(DATES), 5)))
correct.rep.day[,1] <- as.Date(DATES)
colnames(correct.rep.day) <- c("day", "pct.c.dt", "pct.c.notdt", "pct.w.dt", "pct.w.notdt")

for(i in 1:length(DATES)){
  preds <- predict(m.dec.t.full, subset(sm.rweighted.pid, Date == DATES[i] & !is.na(trump)),
                   type = "response")
  preds.df <- cbind(subset(sm.rweighted.pid, Date == DATES[i] & !is.na(trump))$trump, preds,
                    ifelse(preds > 0.5, 1, 0))
  correct.rep.day[i,2] <- prop.table(table(preds.df[,1], preds.df[,3]))["100","1"]*100
  correct.rep.day[i,3] <- prop.table(table(preds.df[,1], preds.df[,3]))["0","0"]*100
  correct.rep.day[i,4] <- prop.table(table(preds.df[,1], preds.df[,3]))["0","1"]*100 # missclassifying as DT
  correct.rep.day[i,5] <- prop.table(table(preds.df[,1], preds.df[,3]))["100","0"]*100
}


days <- sort(unique(sm.dweighted.pid$Date))
clinton.mean <- NULL
trump.mean <- NULL
# Calculating daily candidate support
for(i in 1:length(days)){
  clinton.mean[i] <- weighted.mean(sm.dweighted.pid$clinton[sm.dweighted.pid$Date == days[i]], 
                                   sm.dweighted.pid$DayWeights[sm.dweighted.pid$Date == days[i]], na.rm=T)
  trump.mean[i] <- weighted.mean(sm.rweighted.pid$trump[sm.rweighted.pid$Date == days[i]], 
                                 sm.rweighted.pid$DayWeights[sm.rweighted.pid$Date == days[i]], na.rm=T)
}

DATES <- sort(unique(sm.dweighted.pid$Date))
CLINTON <- clinton.mean[grep("2016", DATES)]
DATES <- grep("2016", DATES, value = T)

DATES <- sort(unique(sm.rweighted.pid$Date))
TRUMP <- trump.mean[grep("2016", DATES)]
DATES <- grep("2016", DATES, value = T)

# Calculating bias
hrc.bias <- (correct.dem.day$pct.c.hrc + correct.dem.day$pct.w.hrc) - CLINTON
dt.bias <- (correct.rep.day$pct.c.dt + correct.rep.day$pct.w.dt) - TRUMP


# Plotting
# pdf("./figures/fig_A5_9.pdf", width = 9.75, height = 6.25)
par(mfcol=c(1,2))
plot(correct.dem.day$day, hrc.bias, axes = F, xlim = c(16800, 16983),
     type = "p", main = "Clinton Support",
     xlab = "Date", ylab = "Percent Bias", col = "grey",
     ylim = c(-30, 30))
abline(h = 0, lty = 2, col = "grey", lwd = 2)
lines(loess.smooth(correct.dem.day$day, hrc.bias, span = 2/3)$x,
      loess.smooth(correct.dem.day$day, hrc.bias, span = 2/3)$y,
      col = "black", lwd = 3)
axis(1, at = c(16800, 16832, 16861, 16892, 16922, 16953, 16983),
     labels = c("Jan", "Feb", "March", "April",
                "May", "June", "July"), cex.axis = .9)
axis(2, las = 1)
plot(correct.rep.day$day, dt.bias, axes = F,
     type = "p", main = "Trump Support",
     xlab = "Date", ylab = "Percent Bias", col = "grey",
     ylim = c(-30, 30))
abline(h = 0, lty = 2, col = "grey", lwd = 2)
lines(loess.smooth(correct.rep.day$day, dt.bias, span = 2/3)$x,
      loess.smooth(correct.rep.day$day, dt.bias, span = 2/3)$y,
      col = "black", lwd = 3)
axis(1, at = c(16800, 16832, 16861, 16892, 16922, 16953),
     labels = c("Jan", "Feb", "March", "April",
                "May", "June"), cex.axis = .9)
axis(2, las = 1)
# dev.off()


#----------------------------------------------------------------------------------------------------------#
# The following code produces Appendix 5 Table 3
# Note: Baseline models take awhile to run due to data set size and interactions
#----------------------------------------------------------------------------------------------------------#
# Democrats
# Baseline Model
m.dec.c.full <- glm(as.factor(clinton) ~ as.factor(age6)*as.factor(educ4)*as.factor(racethn4)*as.factor(region)*as.factor(gender)*as.factor(ideology3), 
                    family=binomial(link="probit"),
                    data = sm.dweighted.pid, weights = DayWeights, subset = dec == 1)
summary(m.dec.c.full)

DATES <- sort(unique(sm.dweighted.pid$Date))
DATES <- grep("2016", DATES, value = T)
correct.dem.day <- data.frame(array(NA, c(length(DATES), 5)))
correct.dem.day[,1] <- as.Date(DATES)
colnames(correct.dem.day) <- c("day", "pct.c.hrc", "pct.c.bs", "pct.w.hrc", "pct.w.bs")

for(i in 1:length(DATES)){
  preds <- predict(m.dec.c.full, subset(sm.dweighted.pid, Date == DATES[i] & !is.na(clinton)),
                   type = "response")
  preds.df <- cbind(subset(sm.dweighted.pid, Date == DATES[i] & !is.na(clinton))$clinton, preds,
                    ifelse(preds > 0.5, 1, 0))
  correct.dem.day[i,2] <- prop.table(table(preds.df[,1], preds.df[,3]))["100","1"]*100
  correct.dem.day[i,3] <- prop.table(table(preds.df[,1], preds.df[,3]))["0","0"]*100
  correct.dem.day[i,4] <- prop.table(table(preds.df[,1], preds.df[,3]))["0","1"]*100 # missclassifying BS as HRC
  correct.dem.day[i,5] <- prop.table(table(preds.df[,1], preds.df[,3]))["100","0"]*100
}

## Republicans
# Baseline Model
m.dec.t.full <- glm(as.factor(trump) ~ as.factor(age6)*as.factor(educ4)*as.factor(racethn4)*as.factor(region)*as.factor(gender)*as.factor(ideology3), 
                    family=binomial(link="probit"),
                    data = sm.rweighted.pid, weights = DayWeights, subset = dec == 1)
summary(m.dec.t.full)

DATES <- sort(unique(sm.rweighted.pid$Date))
TRUMP <- trump.mean.pid[grep("2016", DATES)]
DATES <- grep("2016", DATES, value = T)
correct.rep.day <- data.frame(array(NA, c(length(DATES), 5)))
correct.rep.day[,1] <- as.Date(DATES)
colnames(correct.rep.day) <- c("day", "pct.c.dt", "pct.c.notdt", "pct.w.dt", "pct.w.notdt")

for(i in 1:length(DATES)){
  preds <- predict(m.dec.t.full, subset(sm.rweighted.pid, Date == DATES[i] & !is.na(trump)),
                   type = "response")
  preds.df <- cbind(subset(sm.rweighted.pid, Date == DATES[i] & !is.na(trump))$trump, preds,
                    ifelse(preds > 0.5, 1, 0))
  correct.rep.day[i,2] <- prop.table(table(preds.df[,1], preds.df[,3]))["100","1"]*100
  correct.rep.day[i,3] <- prop.table(table(preds.df[,1], preds.df[,3]))["0","0"]*100
  correct.rep.day[i,4] <- prop.table(table(preds.df[,1], preds.df[,3]))["0","1"]*100 # missclassifying as DT
  correct.rep.day[i,5] <- prop.table(table(preds.df[,1], preds.df[,3]))["100","0"]*100
}


days <- sort(unique(sm.dweighted.pid$Date))
clinton.mean <- NULL
trump.mean <- NULL
# Calculating daily candidate support
for(i in 1:length(days)){
  clinton.mean[i] <- weighted.mean(sm.dweighted.pid$clinton[sm.dweighted.pid$Date == days[i]], 
                                   sm.dweighted.pid$DayWeights[sm.dweighted.pid$Date == days[i]], na.rm=T)
  trump.mean[i] <- weighted.mean(sm.rweighted.pid$trump[sm.rweighted.pid$Date == days[i]], 
                                 sm.rweighted.pid$DayWeights[sm.rweighted.pid$Date == days[i]], na.rm=T)
}

DATES <- sort(unique(sm.dweighted.pid$Date))
CLINTON <- clinton.mean[grep("2016", DATES)]
DATES <- grep("2016", DATES, value = T)

DATES <- sort(unique(sm.rweighted.pid$Date))
TRUMP <- trump.mean[grep("2016", DATES)]
DATES <- grep("2016", DATES, value = T)

# Calculating bias
hrc.bias <- (correct.dem.day$pct.c.hrc + correct.dem.day$pct.w.hrc) - CLINTON
dt.bias <- (correct.rep.day$pct.c.dt + correct.rep.day$pct.w.dt) - TRUMP


errors <- data.frame(day = as.Date(DATES), 
                     hrc.bias = hrc.bias, 
                     dt.bias = dt.bias, 
                     stringsAsFactors = F)
errors$hrc.wins <- NA
errors$dt.wins <- NA
errors$dem.total <- NA
errors$rep.total <- NA


# Indicators for primary wins/losses
primdates <- read.csv("./data/primdates.csv")
primdates$Date.dprim <- as.Date(primdates$Date.dprim)
primdates$Date.rprim <- as.Date(primdates$Date.rprim)

tot.d <- tot.r <- wins.d <- wins.r <- 0
for(i in 1:length(errors$day)){
  if(errors$day[i] %in% primdates$Date.dprim){
    df <- subset(primdates, Date.dprim == errors$day[i])
    d.states <- df$state
    tot.d <- tot.d + length(d.states)
    for(j in 1:length(d.states)){
      df1 <- subset(df, state == d.states[j])
      wins.d <- ifelse(names(which.max(df1[,c("clinton.outcome", "sanders.outcome")])) == "clinton.outcome", 
                       wins.d + 1, wins.d)
    }
  }
  if(errors$day[i] %in% primdates$Date.rprim){
    df <- subset(primdates, Date.rprim == errors$day[i])
    r.states <- df$state
    tot.r <- tot.r + length(r.states)   
    for(j in 1:length(r.states)){
      df1 <- subset(df, state == r.states[j])
      if(!is.na(df1$trump.outcome)){
        wins.r <- ifelse(names(which.max(df1[, c("trump.outcome", "cruz.outcome",
                                                 "rubio.outcome", "kasich.outcome")])) == "trump.outcome", 
                         wins.r + 1, wins.r)
      }
    }
  }
  errors$hrc.wins[i] <- wins.d
  errors$dem.total[i] <- tot.d
  errors$dt.wins[i] <- wins.r
  errors$rep.total[i] <- tot.r
}


m.hrc.errors <- lm(hrc.bias ~ hrc.wins, data = errors)
summary(m.hrc.errors)

# off
m.dt.errors <- lm(dt.bias ~ dt.wins, data = errors)
summary(m.dt.errors)

## First differences model
errors.dif <- errors[-1,]
errors.dif[,-c(1,10:ncol(errors))] <- apply(errors[,-c(1,10:ncol(errors))], 2, diff)

m.hrc.errors.dif <- lm(hrc.bias ~ hrc.wins, data = errors.dif)
summary(m.hrc.errors.dif)

# Off
m.dt.errors.dif <- lm(dt.bias ~ dt.wins, data = errors.dif)
summary(m.dt.errors.dif)

# stargazer(m.hrc.errors, m.dt.errors, m.hrc.errors.dif, m.dt.errors.dif,
#           title = "Relationship between Primary Success and Daily Candidate Support Bias",
#           covariate.labels = c("Contests Won", "$\\Delta$Contests Won"),
#           dep.var.labels = c("Clinton Bias", "Trump Bias", "$\\Delta$Clinton Bias", "$\\Delta$Trump Bias"),
#           intercept.bottom = T, notes.align = "l",
#           no.space = T, digits = 3, label = c("bias_preds_withIdeo"),
#           #out = c("./figures/table_A5_3.tex"),
#           align = T, omit.stat = c("f", "adj.rsq"), df = F
# )

#----------------------------------------------------------------------------------------------------------#
# The following code creates Appendix 12 Figure 10
#----------------------------------------------------------------------------------------------------------#
# Clinton
m.dec.hrc <- glm(as.factor(clinton) ~ as.factor(age6) + as.factor(educ4) + as.factor(racethn4) 
                 + as.factor(region) + as.factor(gender) + as.factor(ideology3), 
                 data = sm.dweighted.pid, weights = DayWeights, subset = dec == 1, family = "binomial"(link = "probit"))
summary(m.dec.hrc)
m.may.hrc <- glm(as.factor(clinton) ~ as.factor(age6) + as.factor(educ4) + as.factor(racethn4) 
                 + as.factor(region) + as.factor(gender) + as.factor(ideology3), 
                 data = sm.dweighted.pid, weights = DayWeights, subset = may == 1, family = "binomial"(link = "probit"))
summary(m.may.hrc)


# Trump
m.dec.dt <- glm(as.factor(trump) ~ as.factor(age6) + as.factor(educ4) + as.factor(racethn4) 
                           + as.factor(region) + as.factor(gender) + as.factor(ideology3), 
                           data = sm.rweighted.pid, weights = DayWeights, subset = dec == 1, family = "binomial"(link = "probit"))
summary(m.dec.dt)
m.april.dt <- glm(as.factor(trump) ~ as.factor(age6) + as.factor(educ4) + as.factor(racethn4) 
                  + as.factor(region) + as.factor(gender) + as.factor(ideology3), 
                  data = sm.rweighted.pid, weights = DayWeights, subset = april == 1, family = "binomial"(link = "probit"))
summary(m.april.dt)

# Plotting Covariate Effects
d.pid.coefs <- cbind(coef(m.dec.hrc)[-1],
                     coef(m.may.hrc)[-1])
r.pid.coefs <- cbind(coef(m.dec.dt)[-1],
                     coef(m.april.dt)[-1])


PCH <- c(rep(16,5),
         rep(5,3),
         rep(15,3),
         rep(17,3),
         1,
         rep(3, 4))

# pdf("../figures/fig_A5_10.pdf", width = 9.75, height = 6.25)
par(mfcol = c(1,2))
plot(d.pid.coefs[,1], d.pid.coefs[,2], xlab = "December Effects", ylab = "May Effects",
     main = "Democratic Primary", ylim = c(-.8, 1.3), xlim = c(-.8, 1.3),
     pch = PCH)
abline(a = 0, b = 1)
mtext("(Probit Coefficients)", side = 3, line = 0.5, cex = .75)
# text(d.coefs["as.factor(racethn4)Other",1] + 8, d.coefs["as.factor(racethn4)Other",2], "Race: Other", cex = .75)
legend("topleft", pch = c(16, 5, 15, 17, 1, 3), bty = "n",
       c("Age", "Education", "Race", "Region", "Female", "Ideology"))
plot(r.pid.coefs[,1], r.pid.coefs[,2], xlab = "December Effects", ylab = "April Effects",
     main = "Republican Primary", ylim = c(-.8, 1.3), xlim = c(-.8, 1.3),
     pch = PCH)
abline(a = 0, b = 1)
mtext("(Probit Coefficients)", side = 3, line = 0.5, cex = .75)
legend("topleft", pch = c(16, 5, 15, 17, 1, 3), bty = "n",
       c("Age", "Education", "Race", "Region", "Female", "Ideology"))
# dev.off()