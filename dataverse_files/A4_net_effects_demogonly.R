# Clinton, Engelhardt, Trussler 2018 "Knock Out Blows"
# Purpose: This file conducts the analyses reported in appendix 3 looking at changes candidate support 
# prediction success by GOP field composition

# Loading in Data
setwd("~/google drive/momentum/Replication/")
load("./data/Survey Monkey Data.Rdata")

#----------------------------------------------------------------------------------------------------------#
# Creating Month Indicators
sm.dweighted$dec <- NA
sm.dweighted$dec <- ifelse(sm.dweighted$Date >= "2015-12-01" & sm.dweighted$Date <= "2015-12-31", 1, 0)
sm.dweighted$jan <- NA
sm.dweighted$jan <- ifelse(sm.dweighted$Date >= "2016-01-01" & sm.dweighted$Date <= "2016-01-31", 1, 0)
sm.dweighted$feb <- NA
sm.dweighted$feb <- ifelse(sm.dweighted$Date >= "2016-02-01" & sm.dweighted$Date <= "2016-02-29", 1, 0)
sm.dweighted$march <- NA
sm.dweighted$march <- ifelse(sm.dweighted$Date >= "2016-03-01" & sm.dweighted$Date <= "2016-03-31", 1, 0)
sm.dweighted$april <- NA
sm.dweighted$april <- ifelse(sm.dweighted$Date >= "2016-04-01" & sm.dweighted$Date <= "2016-04-30", 1, 0)
sm.dweighted$may <- NA
sm.dweighted$may <- ifelse(sm.dweighted$Date >= "2016-05-01" & sm.dweighted$Date <= "2016-05-31", 1, 0)
sm.dweighted$june <- NA
sm.dweighted$june <- ifelse(sm.dweighted$Date >= "2016-06-01" & sm.dweighted$Date <= "2016-06-30", 1, 0)

sm.rweighted$dec <- NA
sm.rweighted$dec <- ifelse(sm.rweighted$Date >= "2015-12-01" & sm.rweighted$Date <= "2015-12-31", 1, 0)
sm.rweighted$jan <- NA
sm.rweighted$jan <- ifelse(sm.rweighted$Date >= "2016-01-01" & sm.rweighted$Date <= "2016-01-31", 1, 0)
sm.rweighted$feb <- NA
sm.rweighted$feb <- ifelse(sm.rweighted$Date >= "2016-02-01" & sm.rweighted$Date <= "2016-02-29", 1, 0)
sm.rweighted$march <- NA
sm.rweighted$march <- ifelse(sm.rweighted$Date >= "2016-03-01" & sm.rweighted$Date <= "2016-03-31", 1, 0)
sm.rweighted$april <- NA
sm.rweighted$april <- ifelse(sm.rweighted$Date >= "2016-04-01" & sm.rweighted$Date <= "2016-04-30", 1, 0)
sm.rweighted$may <- NA
sm.rweighted$may <- ifelse(sm.rweighted$Date >= "2016-05-01" & sm.rweighted$Date <= "2016-05-31", 1, 0)
sm.rweighted$june <- NA
sm.rweighted$june <- ifelse(sm.rweighted$Date >= "2016-06-01" & sm.rweighted$Date <= "2016-06-30", 1, 0)

#----------------------------------------------------------------------------------------------------------#
# The following code produces Appendix Figure 4
# Note: Baseline models take awhile to run due to data set size and interactions
#----------------------------------------------------------------------------------------------------------#
### Democratic Primary
# Baseline Model
m.dec.c.full <- glm(as.factor(clinton) ~ as.factor(age6)*as.factor(educ4)*as.factor(racethn4)*as.factor(region)*as.factor(gender), 
                    family=binomial(link="probit"),
                    data = sm.dweighted, weights = DayWeights, subset = dec == 1)
summary(m.dec.c.full)

correct.dem <- data.frame(array(NA, c(6, 2)))
correct.dem[,1] <- c("jan", "feb", "march", "april", "may", "june" )
colnames(correct.dem) <- c("month", "pct.c")
# January Predictions
jan.pred <- predict(m.dec.c.full, subset(sm.dweighted, jan == 1 & !is.na(clinton)),  type = "response")
preds.jan <- cbind(subset(sm.dweighted, jan == 1 & !is.na(clinton))$clinton, jan.pred,
                   ifelse(jan.pred > 0.5, 1, 0))
correct.dem[1,2] <- sum(diag(prop.table(table(preds.jan[,1], preds.jan[,3])))*100)
# February Predictions
feb.pred <- predict(m.dec.c.full, subset(sm.dweighted, feb == 1 & !is.na(clinton)),  type = "response")
preds.feb <- cbind(subset(sm.dweighted, feb == 1 & !is.na(clinton))$clinton, feb.pred,
                   ifelse(feb.pred > 0.5, 1, 0))
correct.dem[2,2] <- sum(diag(prop.table(table(preds.feb[,1], preds.feb[,3])))*100)
# March Predictions
mar.pred <- predict(m.dec.c.full, subset(sm.dweighted, march == 1 & !is.na(clinton)),  type = "response")
preds.mar <- cbind(subset(sm.dweighted, march == 1 & !is.na(clinton))$clinton, mar.pred,
                   ifelse(mar.pred > 0.5, 1, 0))
correct.dem[3,2] <- sum(diag(prop.table(table(preds.mar[,1], preds.mar[,3])))*100)
# April Predictions
apr.pred <- predict(m.dec.c.full, subset(sm.dweighted, april == 1 & !is.na(clinton)),  type = "response")
preds.apr <- cbind(subset(sm.dweighted, april == 1 & !is.na(clinton))$clinton, apr.pred,
                   ifelse(apr.pred > 0.5, 1, 0))
correct.dem[4,2] <- sum(diag(prop.table(table(preds.apr[,1], preds.apr[,3])))*100)
# May Predictions
may.pred <- predict(m.dec.c.full, subset(sm.dweighted, may == 1 & !is.na(clinton)),  type = "response")
preds.may <- cbind(subset(sm.dweighted, may == 1 & !is.na(clinton))$clinton, may.pred,
                   ifelse(may.pred > 0.5, 1, 0))
correct.dem[5,2] <- sum(diag(prop.table(table(preds.may[,1], preds.may[,3])))*100)
# June Predictions
jun.pred <- predict(m.dec.c.full, subset(sm.dweighted, june == 1 & !is.na(clinton)),  type = "response")
preds.jun <- cbind(subset(sm.dweighted, june == 1 & !is.na(clinton))$clinton, jun.pred,
                   ifelse(jun.pred > 0.5, 1, 0))
correct.dem[6,2] <- sum(diag(prop.table(table(preds.jun[,1], preds.jun[,3])))*100)


### Republicam Primary
# Baseline Model
m.dec.t.full <- glm(as.factor(trump) ~ as.factor(age6)*as.factor(educ4)*as.factor(racethn4)*as.factor(region)*as.factor(gender), 
                    family=binomial(link="probit"),
                    data = sm.rweighted, weights = DayWeights, subset = dec == 1)
summary(m.dec.t.full)

correct.rep <- data.frame(array(NA, c(6, 2)))
correct.rep[,1] <- c("jan", "feb", "march", "april", "may", "june" )
colnames(correct.rep) <- c("month", "pct.c")
# January
jan.pred <- predict(m.dec.t.full, subset(sm.rweighted, jan == 1 & !is.na(trump)),  type = "response")
preds.jan <- cbind(subset(sm.rweighted, jan == 1 & !is.na(trump))$trump, jan.pred,
                   ifelse(jan.pred > 0.5, 1, 0))
correct.rep[1,2] <- sum(diag(prop.table(table(preds.jan[,1], preds.jan[,3])))*100)
# February
feb.pred <- predict(m.dec.t.full, subset(sm.rweighted, feb == 1 & !is.na(trump)),  type = "response")
preds.feb <- cbind(subset(sm.rweighted, feb == 1 & !is.na(trump))$trump, feb.pred,
                   ifelse(feb.pred > 0.5, 1, 0))
correct.rep[2,2] <- sum(diag(prop.table(table(preds.feb[,1], preds.feb[,3])))*100)
# March
mar.pred <- predict(m.dec.t.full, subset(sm.rweighted, march == 1 & !is.na(trump)),  type = "response")
preds.mar <- cbind(subset(sm.rweighted, march == 1 & !is.na(trump))$trump, mar.pred,
                   ifelse(mar.pred > 0.5, 1, 0))
correct.rep[3,2] <- sum(diag(prop.table(table(preds.mar[,1], preds.mar[,3])))*100)
# April
apr.pred <- predict(m.dec.t.full, subset(sm.rweighted, april == 1 & !is.na(trump)),  type = "response")
preds.apr <- cbind(subset(sm.rweighted, april == 1 & !is.na(trump))$trump, apr.pred,
                   ifelse(apr.pred > 0.5, 1, 0))
correct.rep[4,2] <- sum(diag(prop.table(table(preds.apr[,1], preds.apr[,3])))*100)
# May
may.pred <- predict(m.dec.t.full, subset(sm.rweighted, may == 1 & !is.na(trump)),  type = "response")
preds.may <- cbind(subset(sm.rweighted, may == 1 & !is.na(trump))$trump, may.pred,
                   ifelse(may.pred > 0.5, 1, 0))
correct.rep[5,2] <- sum(diag(prop.table(table(preds.may[,1], preds.may[,3])))*100)
# June
jun.pred <- predict(m.dec.t.full, subset(sm.rweighted, june == 1 & !is.na(trump)),  type = "response")
preds.jun <- cbind(subset(sm.rweighted, june == 1 & !is.na(trump))$trump, jun.pred,
                   ifelse(jun.pred > 0.5, 1, 0))
correct.rep[6,2] <- sum(diag(prop.table(table(preds.jun[,1], preds.jun[,3])))*100)


## Generating Figure 
# pdf("../figures/appendix/FigureA4.pdf", width = 8.75, height = 6.25)
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
# The following code creates appendix Figure 5
# Note: Baseline models take awhile to run due to data set size and interactions
#----------------------------------------------------------------------------------------------------------#
## Democratic Primary Prediction Error Decomposition
# Baseline Model
m.dec.c.full <- glm(as.factor(clinton) ~ as.factor(age6)*as.factor(educ4)*as.factor(racethn4)*as.factor(region)*as.factor(gender), 
                    family=binomial(link="probit"),
                    data = sm.dweighted, weights = DayWeights, subset = dec == 1)
summary(m.dec.c.full)
# Creating utility strings and empty arrays to save results
DATES <- sort(unique(sm.dweighted$Date))
DATES <- grep("2016", DATES, value = T)
correct.dem.day <- data.frame(array(NA, c(length(DATES), 5)))
correct.dem.day[,1] <- as.Date(DATES)
colnames(correct.dem.day) <- c("day", "pct.c.hrc", "pct.c.bs", "pct.w.hrc", "pct.w.bs")

# Saving prediction errors
for(i in 1:length(DATES)){
  preds <- predict(m.dec.c.full, subset(sm.dweighted, Date == DATES[i] & !is.na(clinton)),
                   type = "response")
  preds.df <- cbind(subset(sm.dweighted, Date == DATES[i] & !is.na(clinton))$clinton, preds,
                    ifelse(preds > 0.5, 1, 0))
  correct.dem.day[i,2] <- prop.table(table(preds.df[,1], preds.df[,3]))["100","1"]*100
  correct.dem.day[i,3] <- prop.table(table(preds.df[,1], preds.df[,3]))["0","0"]*100
  correct.dem.day[i,4] <- prop.table(table(preds.df[,1], preds.df[,3]))["0","1"]*100 # missclassifying BS as HRC
  correct.dem.day[i,5] <- prop.table(table(preds.df[,1], preds.df[,3]))["100","0"]*100
}

## Republican Primary Prediction Error Decomposition
# Baseline Model
m.dec.t.full <- glm(as.factor(trump) ~ as.factor(age6)*as.factor(educ4)*as.factor(racethn4)*as.factor(region)*as.factor(gender), 
                    family = binomial(link="probit"),
                    data = sm.rweighted, weights = DayWeights, subset = dec == 1)
summary(m.dec.t.full)

# Creating utility strings and empty arrays to save results
DATES <- sort(unique(sm.rweighted$Date))
DATES <- grep("2016", DATES, value = T)
correct.rep.day <- data.frame(array(NA, c(length(DATES), 5)))
correct.rep.day[,1] <- as.Date(DATES)
colnames(correct.rep.day) <- c("day", "pct.c.dt", "pct.c.notdt", "pct.w.dt", "pct.w.notdt")

# Saving prediction errors
for(i in 1:length(DATES)){
  preds <- predict(m.dec.t.full, subset(sm.rweighted, Date == DATES[i] & !is.na(trump)),
                   type = "response")
  preds.df <- cbind(subset(sm.rweighted, Date == DATES[i] & !is.na(trump))$trump, preds,
                    ifelse(preds > 0.5, 1, 0))
  correct.rep.day[i,2] <- prop.table(table(preds.df[,1], preds.df[,3]))["100","1"]*100
  correct.rep.day[i,3] <- prop.table(table(preds.df[,1], preds.df[,3]))["0","0"]*100
  correct.rep.day[i,4] <- prop.table(table(preds.df[,1], preds.df[,3]))["0","1"]*100 # missclassifying as DT
  correct.rep.day[i,5] <- prop.table(table(preds.df[,1], preds.df[,3]))["100","0"]*100
}

days <- sort(unique(sm.drweighted$Date))
clinton.mean <- NULL
trump.mean <- NULL
# Calculating daily candidate support
for(i in 1:length(days)){
  clinton.mean[i] <- weighted.mean(sm.dweighted$clinton[sm.dweighted$Date == days[i]], 
                                   sm.dweighted$DayWeights[sm.dweighted$Date == days[i]], na.rm=T)
  trump.mean[i] <- weighted.mean(sm.rweighted$trump[sm.rweighted$Date == days[i]], 
                                 sm.rweighted$DayWeights[sm.rweighted$Date == days[i]], na.rm=T)
}

DATES <- sort(unique(sm.dweighted$Date))
CLINTON <- clinton.mean[grep("2016", DATES)]
DATES <- grep("2016", DATES, value = T)

DATES <- sort(unique(sm.rweighted$Date))
TRUMP <- trump.mean[grep("2016", DATES)]
DATES <- grep("2016", DATES, value = T)
# Calculating bias
hrc.bias <- (correct.dem.day$pct.c.hrc + correct.dem.day$pct.w.hrc) - CLINTON
dt.bias <- (correct.rep.day$pct.c.dt + correct.rep.day$pct.w.dt) - TRUMP

# pdf("../figures/appendix/figureA5.pdf", width = 9.75, height = 6.25)
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
     ylim = c(-40, 30))
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
# The following code produces Appendix 4 Figure 6
#----------------------------------------------------------------------------------------------------------#
### Clinton
# Baseline Model
m.hrc.dec <- glm(as.factor(clinton) ~ as.factor(age6) + as.factor(educ4) + as.factor(racethn4) 
                 + as.factor(region) + as.factor(gender), 
                 data = sm.dweighted, weights = DayWeights, subset = dec == 1, family = "binomial"(link = "probit"))
summary(m.hrc.dec)
# Primary End Model
m.hrc.may <- glm(as.factor(clinton) ~ as.factor(age6) + as.factor(educ4) + as.factor(racethn4) 
                 + as.factor(region) + as.factor(gender), 
                 data = sm.dweighted, weights = DayWeights, subset = may == 1, family = "binomial"(link = "probit"))
summary(m.hrc.may)

### Trump
# Baseline End Model
m.dt.dec <- glm(as.factor(trump) ~ as.factor(age6) + as.factor(educ4) + as.factor(racethn4) 
                + as.factor(region) + as.factor(gender), 
                data = sm.rweighted, weights = DayWeights, subset = dec == 1, family = "binomial"(link = "probit"))
summary(m.dt.dec)
# Primary End Model
m.dt.april <- glm(as.factor(trump) ~ as.factor(age6) + as.factor(educ4) + as.factor(racethn4) 
                  + as.factor(region) + as.factor(gender), 
                  data = sm.rweighted, weights = DayWeights, subset = april == 1, family = "binomial"(link = "probit"))
summary(m.dt.april)


### Comparing Covariate Effects
# Storing Coefficients
d.coefs <- cbind(coef(m.hrc.dec)[-1],
                 coef(m.hrc.may)[-1])

r.coefs <- cbind(coef(m.dt.dec)[-1],
                 coef(m.dt.april)[-1])


PCH <- c(rep(16,5),
         rep(5,3),
         rep(15,3),
         rep(17,3),
         1)

# pdf("../figures/appendix/FigureA6.pdf", width = 9.75, height = 6.25)
par(mfcol = c(1,2))
plot(d.coefs[,1], d.coefs[,2], xlab = "December Effects", ylab = "May Effects",
     main = "Democratic Primary", ylim = c(-.5, 1.3), xlim = c(-.5, 1.3),
     pch = PCH)
abline(a = 0, b = 1)
mtext("(Probit Coefficients)", side = 3, line = 0.5, cex = .75)
# text(d.coefs["as.factor(racethn4)Other",1] + 8, d.coefs["as.factor(racethn4)Other",2], "Race: Other", cex = .75)
legend("topleft", pch = c(16, 5, 15, 17, 1), bty = "n",
       c("Age", "Education", "Race", "Region", "Female"))
plot(r.coefs[,1], r.coefs[,2], xlab = "December Effects", ylab = "April Effects",
     main = "Republican Primary", ylim = c(-.5, 1.3), xlim = c(-.5, 1.3),
     pch = PCH)
abline(a = 0, b = 1)
mtext("(Probit Coefficients)", side = 3, line = 0.5, cex = .75)
legend("topleft", pch = c(16, 5, 15, 17, 1), bty = "n",
       c("Age", "Education", "Race", "Region", "Female"))
# dev.off()


#----------------------------------------------------------------------------------------------------------#
# The following code produces Appendix 4 Figure 7
#----------------------------------------------------------------------------------------------------------#
### Trump vs. Field
m.dec.t.full <- glm(as.factor(trump) ~ as.factor(age6)*as.factor(educ4)*as.factor(racethn4)*as.factor(region)*as.factor(gender), 
                    family=binomial(link="probit"),
                    data = sm.rweighted, weights = DayWeights, subset = dec == 1)
summary(m.dec.t.full)

correct.rep <- data.frame(array(NA, c(6, 2)))
correct.rep[,1] <- c("jan", "feb", "march", "april", "may", "june" )
colnames(correct.rep) <- c("month", "pct.c")
# January
jan.pred <- predict(m.dec.t.full, subset(sm.rweighted, jan == 1 & !is.na(trump)),  type = "response")
preds.jan <- cbind(subset(sm.rweighted, jan == 1 & !is.na(trump))$trump, jan.pred,
                   ifelse(jan.pred > 0.5, 1, 0))
correct.rep[1,2] <- sum(diag(prop.table(table(preds.jan[,1], preds.jan[,3])))*100)
# February
feb.pred <- predict(m.dec.t.full, subset(sm.rweighted, feb == 1 & !is.na(trump)),  type = "response")
preds.feb <- cbind(subset(sm.rweighted, feb == 1 & !is.na(trump))$trump, feb.pred,
                   ifelse(feb.pred > 0.5, 1, 0))
correct.rep[2,2] <- sum(diag(prop.table(table(preds.feb[,1], preds.feb[,3])))*100)
# March
mar.pred <- predict(m.dec.t.full, subset(sm.rweighted, march == 1 & !is.na(trump)),  type = "response")
preds.mar <- cbind(subset(sm.rweighted, march == 1 & !is.na(trump))$trump, mar.pred,
                   ifelse(mar.pred > 0.5, 1, 0))
correct.rep[3,2] <- sum(diag(prop.table(table(preds.mar[,1], preds.mar[,3])))*100)
# April
apr.pred <- predict(m.dec.t.full, subset(sm.rweighted, april == 1 & !is.na(trump)),  type = "response")
preds.apr <- cbind(subset(sm.rweighted, april == 1 & !is.na(trump))$trump, apr.pred,
                   ifelse(apr.pred > 0.5, 1, 0))
correct.rep[4,2] <- sum(diag(prop.table(table(preds.apr[,1], preds.apr[,3])))*100)
# May
may.pred <- predict(m.dec.t.full, subset(sm.rweighted, may == 1 & !is.na(trump)),  type = "response")
preds.may <- cbind(subset(sm.rweighted, may == 1 & !is.na(trump))$trump, may.pred,
                   ifelse(may.pred > 0.5, 1, 0))
correct.rep[5,2] <- sum(diag(prop.table(table(preds.may[,1], preds.may[,3])))*100)
# June
jun.pred <- predict(m.dec.t.full, subset(sm.rweighted, june == 1 & !is.na(trump)),  type = "response")
preds.jun <- cbind(subset(sm.rweighted, june == 1 & !is.na(trump))$trump, jun.pred,
                   ifelse(jun.pred > 0.5, 1, 0))
correct.rep[6,2] <- sum(diag(prop.table(table(preds.jun[,1], preds.jun[,3])))*100)


### Trump vs. Cruz
# Binomial Outcome
m.dec.tc.full <- glm(as.factor(trump) ~ as.factor(age6)*as.factor(educ4)*as.factor(racethn4)*as.factor(region)*as.factor(gender), 
                     family=binomial(link="probit"),
                     data = sm.rweighted, weights = DayWeights, 
                     subset = dec == 1 & (repprim == "Donald Trump" | repprim == "Ted Cruz"))
summary(m.dec.tc.full)

correct.rep.tc <- data.frame(array(NA, c(6, 2)))
correct.rep.tc[,1] <- c("jan", "feb", "march", "april", "may", "june" )
colnames(correct.rep.tc) <- c("month", "pct.c")
# January
jan.pred <- predict(m.dec.tc.full, subset(sm.rweighted, jan == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz")),  type = "response")
preds.jan <- cbind(subset(sm.rweighted, jan == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz"))$trump, jan.pred,
                   ifelse(jan.pred > 0.5, 1, 0))
correct.rep.tc[1,2] <- sum(diag(prop.table(table(preds.jan[,1], preds.jan[,2])))*100)

# February
feb.pred <- predict(m.dec.tc.full, subset(sm.rweighted, feb == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz")),  type = "response")
preds.feb <- cbind(subset(sm.rweighted, feb == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz"))$trump, feb.pred,
                   ifelse(feb.pred > 0.5, 1, 0))
correct.rep.tc[2,2] <- sum(diag(prop.table(table(preds.feb[,1], preds.feb[,2])))*100)

# March
mar.pred <- predict(m.dec.tc.full, subset(sm.rweighted, march == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz")),  type = "response")
preds.mar <- cbind(subset(sm.rweighted, march == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz"))$trump, mar.pred,
                   ifelse(mar.pred > 0.5, 1, 0))
correct.rep.tc[3,2] <- sum(diag(prop.table(table(preds.mar[,1], preds.mar[,2])))*100)

# April
apr.pred <- predict(m.dec.tc.full, subset(sm.rweighted, april == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz")),  type = "response")
preds.apr <- cbind(subset(sm.rweighted, april == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz"))$trump, apr.pred,
                   ifelse(apr.pred > 0.5, 1, 0))
correct.rep.tc[4,2] <- sum(diag(prop.table(table(preds.apr[,1], preds.apr[,2])))*100)

# May
may.pred <- predict(m.dec.tc.full, subset(sm.rweighted, may == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz")),  type = "response")
preds.may <- cbind(subset(sm.rweighted, may == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz"))$trump, may.pred,
                   ifelse(may.pred > 0.5, 1, 0))
correct.rep.tc[5,2] <- sum(diag(prop.table(table(preds.may[,1], preds.may[,2])))*100)

# Juned
jun.pred <- predict(m.dec.tc.full, subset(sm.rweighted, june == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz")),  type = "response")
preds.jun <- cbind(subset(sm.rweighted, june == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz"))$trump, jun.pred,
                   ifelse(jun.pred > 0.5, 1, 0))
correct.rep.tc[6,2] <- sum(diag(prop.table(table(preds.jun[,1], preds.jun[,2])))*100)


#### Trump vs. Cruz/Rubio
m.dec.tcr.full <- glm(as.factor(trump) ~ as.factor(age6)*as.factor(educ4)*as.factor(racethn4)*as.factor(region)*as.factor(gender), 
                      family=binomial(link="probit"),
                      data = sm.rweighted, weights = DayWeights, 
                      subset = dec == 1 & (repprim == "Donald Trump" | repprim == "Ted Cruz" | repprim == "Marco Rubio"))
summary(m.dec.tcr.full)

correct.rep.tcr <- data.frame(array(NA, c(6, 2)))
correct.rep.tcr[,1] <- c("jan", "feb", "march", "april", "may", "june" )
colnames(correct.rep.tcr) <- c("month", "pct.c")
# January
jan.pred <- predict(m.dec.tcr.full, subset(sm.rweighted, jan == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz" | repprim == "Marco Rubio")),  type = "response")
preds.jan <- cbind(subset(sm.rweighted, jan == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz" | repprim == "Marco Rubio"))$trump, jan.pred,
                   ifelse(jan.pred > 0.5, 1, 0))
correct.rep.tcr[1,2] <- sum(diag(prop.table(table(preds.jan[,1], preds.jan[,2])))*100)

# February
feb.pred <- predict(m.dec.tcr.full, subset(sm.rweighted, feb == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz" | repprim == "Marco Rubio")),  type = "response")
preds.feb <- cbind(subset(sm.rweighted, feb == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz" | repprim == "Marco Rubio"))$trump, feb.pred,
                   ifelse(feb.pred > 0.5, 1, 0))
correct.rep.tcr[2,2] <- sum(diag(prop.table(table(preds.feb[,1], preds.feb[,2])))*100)

# March
mar.pred <- predict(m.dec.tcr.full, subset(sm.rweighted, march == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz" | repprim == "Marco Rubio")),  type = "response")
preds.mar <- cbind(subset(sm.rweighted, march == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz" | repprim == "Marco Rubio"))$trump, mar.pred,
                   ifelse(mar.pred > 0.5, 1, 0))
correct.rep.tcr[3,2] <- sum(diag(prop.table(table(preds.mar[,1], preds.mar[,2])))*100)

# April
apr.pred <- predict(m.dec.tcr.full, subset(sm.rweighted, april == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz" | repprim == "Marco Rubio")),  type = "response")
preds.apr <- cbind(subset(sm.rweighted, april == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz" | repprim == "Marco Rubio"))$trump, apr.pred,
                   ifelse(apr.pred > 0.5, 1, 0))
correct.rep.tcr[4,2] <- sum(diag(prop.table(table(preds.apr[,1], preds.apr[,2])))*100)

# May
may.pred <- predict(m.dec.tcr.full, subset(sm.rweighted, may == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz" | repprim == "Marco Rubio")),  type = "response")
preds.may <- cbind(subset(sm.rweighted, may == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz" | repprim == "Marco Rubio"))$trump, may.pred,
                   ifelse(may.pred > 0.5, 1, 0))
correct.rep.tcr[5,2] <- sum(diag(prop.table(table(preds.may[,1], preds.may[,2])))*100)

# June
jun.pred <- predict(m.dec.tcr.full, subset(sm.rweighted, june == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz" | repprim == "Marco Rubio")),  type = "response")
preds.jun <- cbind(subset(sm.rweighted, june == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz" | repprim == "Marco Rubio"))$trump, jun.pred,
                   ifelse(jun.pred > 0.5, 1, 0))
correct.rep.tcr[6,2] <- sum(diag(prop.table(table(preds.jun[,1], preds.jun[,2])))*100)

preds <- cbind(correct.rep, correct.rep.tc, correct.rep.tcr)

# pdf("../figures/Appendix/FigureA7.pdf", width = 8.75, height = 6.25)
plot(preds[-6,2], type = "b", lwd = 2, col = "black", axes = F,
     main = "", 
     xlab = "Month", ylab = "Percent Correctly Predicted", ylim = c(0, 100),
     pch = 16)
lines(preds[-6,4], type = "b", lwd = 2, col = "black", pch = 15, lty = 6)
lines(preds[-6,6], type = "b", lwd = 2, col = "black", pch = 18, lty = 2)
axis(1, at = 1:5, labels = c("January", "February", "March", "April", "May"))
axis(2, las = 1)
legend("topleft", c("Trump vs. Full Field", "Trump vs. Cruz", "Trump vs. Cruz and Rubio"), 
       # col = c("navy", "darkorange1", "firebrick"), 
       lty = c(1,6,2),
       pch = c(16, 15, 18), bty = "n")
# dev.off()