# Clinton, Engelhardt, Trussler 2018 "Knock Out Blows"
# Purpose: This file conducts the analyses reported in appendix 2 looking at changes in candidate support by demographic strata

# Loading in data
setwd("~/google drive/momentum/Replication/")
load("./data/Survey Monkey Data.Rdata")


#----------------------------------------------------------------------------------------------------------#
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
# The following code produces Appendix 2 Figure 2
#----------------------------------------------------------------------------------------------------------#
### Clinton
# Baseline Model
m.hrc.dec <- glm(as.factor(clinton) ~ as.factor(age6) + as.factor(educ4) + as.factor(racethn4) 
                 + as.factor(region) + as.factor(gender), 
                 data = sm.dweighted.pid, weights = DayWeights, subset = dec == 1, family = "binomial"(link = "probit"))
summary(m.hrc.dec)
# Primary End Model
m.hrc.may <- glm(as.factor(clinton) ~ as.factor(age6) + as.factor(educ4) + as.factor(racethn4) 
                 + as.factor(region) + as.factor(gender), 
                 data = sm.dweighted.pid, weights = DayWeights, subset = may == 1, family = "binomial"(link = "probit"))
summary(m.hrc.may)

### Trump
# Baseline End Model
m.dt.dec <- glm(as.factor(trump) ~ as.factor(age6) + as.factor(educ4) + as.factor(racethn4) 
                + as.factor(region) + as.factor(gender), 
                data = sm.rweighted.pid, weights = DayWeights, subset = dec == 1, family = "binomial"(link = "probit"))
summary(m.dt.dec)
# Primary End Model
m.dt.april <- glm(as.factor(trump) ~ as.factor(age6) + as.factor(educ4) + as.factor(racethn4) 
                  + as.factor(region) + as.factor(gender), 
                  data = sm.rweighted.pid, weights = DayWeights, subset = april == 1, family = "binomial"(link = "probit"))
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

# pdf("../figures/appendix/FigureA2.pdf", width = 9.75, height = 6.25)
par(mfcol = c(1,2))
plot(d.coefs[,1], d.coefs[,2], xlab = "December Effects", ylab = "May Effects",
     main = "Democratic Primary", ylim = c(-.5, 1.3), xlim = c(-.5, 1.3),
     pch = PCH)
abline(a = 0, b = 1)
mtext("(Probit Coefficients)", side = 3, line = 0.5, cex = .75)
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