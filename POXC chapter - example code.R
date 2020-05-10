library(dplyr)
library(plyr)

## LOAD IN DATA ##
example.data <- read.csv("Active Projects/POXC chapter/POXC chapter - example data.csv")
# order treatments for better readability (not mandatory) #
example.data$Treatment <- ordered(example.data$Treatment, levels = c("0x", "1x", "3x"))

## REGRESSIONS AND EXTRACTING RESIDUALS #
# Site 1: East badger (EB) #
data.EB <- subset(example.data, Site.ID=="EB") # subset only this site
EB.model <- lm(POXC ~ Min.C, data=data.EB, na.action=na.exclude)
data.EB$residual <- resid(EB.model, na.action=na.exclude) # extract residuals and add to data frame #
shapiro.test(res.EB) # check for normality of residuals (yes, they still need to be normal) #
plyr::ddply(data.EB, .(Treatment), summarise, avg.res=mean(residual, na.rm=TRUE)) # Treatment-level averages #

# Site 2: Western (We) #
data.We <- subset(example.data, Site.ID=="We")
We.model <- lm(POXC ~ Min.C, data=data.We, na.action=na.exclude)
data.We$residual <- resid(We.model, na.action=na.exclude) # extract residuals and add to data frame #
shapiro.test(res.We) # check for normality of residuals (yes, they still need to be normal) #
plyr::ddply(data.We, .(Treatment), summarise, avg.res=mean(residual, na.rm=TRUE)) # Treatment-level averages #

# Site 3: Northwest (NW) #
data.NW <- subset(example.data, Site.ID=="NW")
NW.model <- lm(POXC ~ Min.C, data=data.NW, na.action=na.exclude)
data.NW$residual <- resid(NW.model, na.action=na.exclude) # extract residuals and add to data frame #
shapiro.test(res.NW) # check for normality of residuals (yes, they still need to be normal) #
plyr::ddply(data.NW, .(Treatment), summarise, avg.res=mean(residual, na.rm=TRUE)) # Treatment-level averages #

# NOTE: ddply can get masked if package Hmisc is also loaded #