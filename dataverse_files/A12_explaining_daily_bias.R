# Clinton, Engelhardt, Trussler 2018 "Knock Out Blows"
# Purpose: This file conducts the analyses reported in appendix 12 explaining daily candidate bias estimates


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
# The following code creates Appendix 12 Table 16
# Note: Baseline models take awhile to run due to data set size and interactions
#----------------------------------------------------------------------------------------------------------#
## Democratic Primary Prediction Error Decomposition
# Baseline Model
m.dec.c.full.pidwt <- glm(as.factor(clinton) ~ as.factor(age6)*as.factor(educ4)*as.factor(racethn4)*as.factor(region)*as.factor(gender), 
                          family=binomial(link="probit"),
                          data = sm.dweighted.pid, weights = DayWeights, subset = dec == 1)
summary(m.dec.c.full.pidwt)
# Creating utility strings and empty arrays to save results
DATES <- sort(unique(sm.dweighted.pid$Date))
DATES <- grep("2016", DATES, value = T)
correct.dem.day <- data.frame(array(NA, c(length(DATES), 5)))
correct.dem.day[,1] <- as.Date(DATES)
colnames(correct.dem.day) <- c("day", "pct.c.hrc", "pct.c.bs", "pct.w.hrc", "pct.w.bs")

# Saving prediction errors
for(i in 1:length(DATES)){
  preds <- predict(m.dec.c.full.pidwt, subset(sm.dweighted.pid, Date == DATES[i] & !is.na(clinton)),
                   type = "response")
  preds.df <- cbind(subset(sm.dweighted.pid, Date == DATES[i] & !is.na(clinton))$clinton, preds,
                    ifelse(preds > 0.5, 1, 0))
  correct.dem.day[i,2] <- prop.table(table(preds.df[,1], preds.df[,3]))["100","1"]*100
  correct.dem.day[i,3] <- prop.table(table(preds.df[,1], preds.df[,3]))["0","0"]*100
  correct.dem.day[i,4] <- prop.table(table(preds.df[,1], preds.df[,3]))["0","1"]*100 # missclassifying BS as HRC
  correct.dem.day[i,5] <- prop.table(table(preds.df[,1], preds.df[,3]))["100","0"]*100
}

## Republican Primary Prediction Error Decomposition
# Baseline Model
m.dec.t.full.pidwt <- glm(as.factor(trump) ~ as.factor(age6)*as.factor(educ4)*as.factor(racethn4)*as.factor(region)*as.factor(gender), 
                          family=binomial(link="probit"),
                          data = sm.rweighted.pid, weights = DayWeights, subset = dec == 1)
summary(m.dec.t.full.pidwt)

# Creating utility strings and empty arrays to save results
DATES <- sort(unique(sm.rweighted.pid$Date))
DATES <- grep("2016", DATES, value = T)
correct.rep.day <- data.frame(array(NA, c(length(DATES), 5)))
correct.rep.day[,1] <- as.Date(DATES)
colnames(correct.rep.day) <- c("day", "pct.c.dt", "pct.c.notdt", "pct.w.dt", "pct.w.notdt")

# Saving prediction errors
for(i in 1:length(DATES)){
  preds <- predict(m.dec.t.full.pidwt, subset(sm.rweighted.pid, Date == DATES[i] & !is.na(trump)),
                   type = "response")
  preds.df <- cbind(subset(sm.rweighted.pid, Date == DATES[i] & !is.na(trump))$trump, preds,
                    ifelse(preds > 0.5, 1, 0))
  correct.rep.day[i,2] <- prop.table(table(preds.df[,1], preds.df[,3]))["100","1"]*100
  correct.rep.day[i,3] <- prop.table(table(preds.df[,1], preds.df[,3]))["0","0"]*100
  correct.rep.day[i,4] <- prop.table(table(preds.df[,1], preds.df[,3]))["0","1"]*100 # missclassifying as DT
  correct.rep.day[i,5] <- prop.table(table(preds.df[,1], preds.df[,3]))["100","0"]*100
}

# Calculating daily candidate support
days <- sort(unique(sm.dweighted.pid$Date))
clinton.mean <- rep(NA, length(days))
trump.mean <- rep(NA, length(days))
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

# Combining Error Calculations
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

m.dt.errors <- lm(dt.bias ~ dt.wins, data = errors)
summary(m.dt.errors)

## First differences model
errors.dif <- errors[-1,]
errors.dif[,-c(1,10:ncol(errors))] <- apply(errors[,-c(1,10:ncol(errors))], 2, diff)

m.hrc.errors.dif <- lm(hrc.bias ~ hrc.wins, data = errors.dif)
summary(m.hrc.errors.dif)

m.dt.errors.dif <- lm(dt.bias ~ dt.wins, data = errors.dif)
summary(m.dt.errors.dif)

# Printing Table
# stargazer(m.hrc.errors, m.dt.errors, m.hrc.errors.dif, m.dt.errors.dif,
#           title = "Relationship between Primary Success and Daily Candidate Support Bias",
#           covariate.labels = c("Contests Won", "$\\Delta$Contests Won"),
#           dep.var.labels = c("Clinton Bias", "Trump Bias", "$\\Delta$Clinton Bias", "$\\Delta$Trump Bias"),
#           intercept.bottom = T, notes.align = "l",
#           no.space = T, digits = 3, label = c("bias_preds"),
#           out = c("../figures/table_A12_16.tex"),
#           align = T, omit.stat = c("f", "adj.rsq")
# )
