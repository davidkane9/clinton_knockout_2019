# Clinton, Engelhardt, Trussler 2018 "Knock Out Blows"
# Purpose: This file conducts the analyses reported in appendix 3 looking at changes candidate support 
# prediction success by GOP field composition

# Loading in data
setwd("~/google drive/momentum/Replication/")
load("./data/Survey Monkey Data.Rdata")

# Creating Month Indicators
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
# The Code Below Produces Appendix 3 Figure 3
#----------------------------------------------------------------------------------------------------------#
### Trump vs. Field
m.dec.t.full <- glm(as.factor(trump) ~ as.factor(age6)*as.factor(educ4)*as.factor(racethn4)*as.factor(region)*as.factor(gender), 
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


### Trump vs. Cruz
# Binomial Outcome
m.dec.tc.full <- glm(as.factor(trump) ~ as.factor(age6)*as.factor(educ4)*as.factor(racethn4)*as.factor(region)*as.factor(gender), 
                           family=binomial(link="probit"),
                           data = sm.rweighted.pid, weights = DayWeights, 
                           subset = dec == 1 & (repprim == "Donald Trump" | repprim == "Ted Cruz"))
summary(m.dec.tc.full)

correct.rep.tc <- data.frame(array(NA, c(6, 2)))
correct.rep.tc[,1] <- c("jan", "feb", "march", "april", "may", "june" )
colnames(correct.rep.tc) <- c("month", "pct.c")
# January
jan.pred <- predict(m.dec.tc.full, subset(sm.rweighted.pid, jan == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz")),  type = "response")
preds.jan <- cbind(subset(sm.rweighted.pid, jan == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz"))$trump, jan.pred,
                   ifelse(jan.pred > 0.5, 1, 0))
correct.rep.tc[1,2] <- sum(diag(prop.table(table(preds.jan[,1], preds.jan[,2])))*100)

# February
feb.pred <- predict(m.dec.tc.full, subset(sm.rweighted.pid, feb == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz")),  type = "response")
preds.feb <- cbind(subset(sm.rweighted.pid, feb == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz"))$trump, feb.pred,
                   ifelse(feb.pred > 0.5, 1, 0))
correct.rep.tc[2,2] <- sum(diag(prop.table(table(preds.feb[,1], preds.feb[,2])))*100)

# March
mar.pred <- predict(m.dec.tc.full, subset(sm.rweighted.pid, march == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz")),  type = "response")
preds.mar <- cbind(subset(sm.rweighted.pid, march == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz"))$trump, mar.pred,
                   ifelse(mar.pred > 0.5, 1, 0))
correct.rep.tc[3,2] <- sum(diag(prop.table(table(preds.mar[,1], preds.mar[,2])))*100)

# April
apr.pred <- predict(m.dec.tc.full, subset(sm.rweighted.pid, april == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz")),  type = "response")
preds.apr <- cbind(subset(sm.rweighted.pid, april == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz"))$trump, apr.pred,
                   ifelse(apr.pred > 0.5, 1, 0))
correct.rep.tc[4,2] <- sum(diag(prop.table(table(preds.apr[,1], preds.apr[,2])))*100)

# May
may.pred <- predict(m.dec.tc.full, subset(sm.rweighted.pid, may == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz")),  type = "response")
preds.may <- cbind(subset(sm.rweighted.pid, may == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz"))$trump, may.pred,
                   ifelse(may.pred > 0.5, 1, 0))
correct.rep.tc[5,2] <- sum(diag(prop.table(table(preds.may[,1], preds.may[,2])))*100)

# Juned
jun.pred <- predict(m.dec.tc.full, subset(sm.rweighted.pid, june == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz")),  type = "response")
preds.jun <- cbind(subset(sm.rweighted.pid, june == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz"))$trump, jun.pred,
                   ifelse(jun.pred > 0.5, 1, 0))
correct.rep.tc[6,2] <- sum(diag(prop.table(table(preds.jun[,1], preds.jun[,2])))*100)


#### Trump vs. Cruz/Rubio
m.dec.tcr.full <- glm(as.factor(trump) ~ as.factor(age6)*as.factor(educ4)*as.factor(racethn4)*as.factor(region)*as.factor(gender), 
                            family=binomial(link="probit"),
                            data = sm.rweighted.pid, weights = DayWeights, 
                            subset = dec == 1 & (repprim == "Donald Trump" | repprim == "Ted Cruz" | repprim == "Marco Rubio"))
summary(m.dec.tcr.full)

correct.rep.tcr <- data.frame(array(NA, c(6, 2)))
correct.rep.tcr[,1] <- c("jan", "feb", "march", "april", "may", "june" )
colnames(correct.rep.tcr) <- c("month", "pct.c")
# January
jan.pred <- predict(m.dec.tcr.full, subset(sm.rweighted.pid, jan == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz" | repprim == "Marco Rubio")),  type = "response")
preds.jan <- cbind(subset(sm.rweighted.pid, jan == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz" | repprim == "Marco Rubio"))$trump, jan.pred,
                   ifelse(jan.pred > 0.5, 1, 0))
correct.rep.tcr[1,2] <- sum(diag(prop.table(table(preds.jan[,1], preds.jan[,2])))*100)

# February
feb.pred <- predict(m.dec.tcr.full, subset(sm.rweighted.pid, feb == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz" | repprim == "Marco Rubio")),  type = "response")
preds.feb <- cbind(subset(sm.rweighted.pid, feb == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz" | repprim == "Marco Rubio"))$trump, feb.pred,
                   ifelse(feb.pred > 0.5, 1, 0))
correct.rep.tcr[2,2] <- sum(diag(prop.table(table(preds.feb[,1], preds.feb[,2])))*100)

# March
mar.pred <- predict(m.dec.tcr.full, subset(sm.rweighted.pid, march == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz" | repprim == "Marco Rubio")),  type = "response")
preds.mar <- cbind(subset(sm.rweighted.pid, march == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz" | repprim == "Marco Rubio"))$trump, mar.pred,
                   ifelse(mar.pred > 0.5, 1, 0))
correct.rep.tcr[3,2] <- sum(diag(prop.table(table(preds.mar[,1], preds.mar[,2])))*100)

# April
apr.pred <- predict(m.dec.tcr.full, subset(sm.rweighted.pid, april == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz" | repprim == "Marco Rubio")),  type = "response")
preds.apr <- cbind(subset(sm.rweighted.pid, april == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz" | repprim == "Marco Rubio"))$trump, apr.pred,
                   ifelse(apr.pred > 0.5, 1, 0))
correct.rep.tcr[4,2] <- sum(diag(prop.table(table(preds.apr[,1], preds.apr[,2])))*100)

# May
may.pred <- predict(m.dec.tcr.full, subset(sm.rweighted.pid, may == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz" | repprim == "Marco Rubio")),  type = "response")
preds.may <- cbind(subset(sm.rweighted.pid, may == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz" | repprim == "Marco Rubio"))$trump, may.pred,
                   ifelse(may.pred > 0.5, 1, 0))
correct.rep.tcr[5,2] <- sum(diag(prop.table(table(preds.may[,1], preds.may[,2])))*100)

# June
jun.pred <- predict(m.dec.tcr.full, subset(sm.rweighted.pid, june == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz" | repprim == "Marco Rubio")),  type = "response")
preds.jun <- cbind(subset(sm.rweighted.pid, june == 1 & !is.na(trump) & (repprim == "Donald Trump" | repprim == "Ted Cruz" | repprim == "Marco Rubio"))$trump, jun.pred,
                   ifelse(jun.pred > 0.5, 1, 0))
correct.rep.tcr[6,2] <- sum(diag(prop.table(table(preds.jun[,1], preds.jun[,2])))*100)

preds <- cbind(correct.rep, correct.rep.tc, correct.rep.tcr)

# pdf("../figures/Appendix/FigureA3.pdf", width = 8.75, height = 6.25)
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