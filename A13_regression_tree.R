# Clinton, Engelhardt, Trussler 2018 "Knock Out Blows"
# Purpose: This file conducts a flexible categorization of placebo estimates by day characteristic
# Results reported in Appendix 13

library(rpart)

setwd("~/Google Drive/Momentum/Replication")
# Loading in Data
REP <- read.csv("./Data/rep_prim_estimates.csv")
DEM <- read.csv("./Data/dem_prim_estimates.csv")


# Republican Primary
mod.rep <- rpart(estimate ~ primary + week + month + debate_forum + candidates,
                 data = REP,
                 method = "anova",
                 minsplit = 5)
printcp(mod.rep)
plotcp(mod.rep)
summary(mod.rep)


# Pruning model based on cross-validation results
mod.rep.pruned <- prune(mod.rep, cp=.038410) 

### Democratic Primary
mod.dem <- rpart(estimate ~ primary + week + month + debate_forum,
                 data = DEM,
                 method = "anova",
                 minsplit = 5)
printcp(mod.dem) 
plotcp(mod.dem)
summary(mod.dem)


# Pruning model based on cross-validation results
mod.dem.pruned <- prune(mod.dem, cp=.027)


# pdf("./figures/appendix/FigureA30.pdf", width = 10, height = 8)
par(mfrow=c(1,2), mar = rep(1.1, 4))
plot(mod.rep.pruned, uniform=T, 
     main="Republican Primary")
text(mod.rep.pruned, use.n=T, cex=1)
plot(mod.dem.pruned, uniform=T, 
     main="Democratic Primary")
text(mod.dem.pruned, use.n=T, cex=1)
# dev.off()