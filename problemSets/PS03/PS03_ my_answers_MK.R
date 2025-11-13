#####################
# load libraries
# set wd
# clear global .envir
#####################

setwd("~/Documents/PhD/03. Classes/Quantitative Methods I/PS03")


# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}
library(ggplot2)
library(stargazer)

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/incumbents_subset.csv")

#Check structure of data: 
head(inc.sub)
str(inc.sub)

#Regress outcome variable on explanatory variable: 
regression1 <- lm(voteshare ~ difflog, data = inc.sub)
#show Output:
summary(regression1)

ggplot(data = inc.sub, aes(x = difflog, y = voteshare)) + 
  geom_point(size = 2, shape = 22) + 
  geom_smooth(method = lm, color = "blue") + 
  labs(title = "Difference in Campaign spending and incumbents vote share", 
       x = "Campaign spending", 
       y = "Incumbents vote share") + 
  theme_bw()

# Add description of scatterplot 

residuals1 <- regression1$residuals
residuals1

#Prediction equation: 
#Find coefficient
summary(regression1)

# explain with variables AND with words, i.e. what variable names mean
# incumbent vote share = Intercept + slope * difflog
# Incumbent vote share = 0.57903 + 0.04167*difflog

# Question 2
regression2 <- lm(presvote ~ difflog, data = inc.sub)
ggplot(data = inc.sub, aes(x = difflog, y = presvote)) + 
  geom_point(size = 2, shape = 23) + 
  geom_smooth(method = lm, color = "orange") + 
  labs(title = "Difference in Campaign spending and presidential candidate vote share", 
       x = "Campaign spending", 
       y = "Voteshare presidential candidate") + 
  theme_bw()

residuals2 <- regression2$residuals
residuals2

summary(regression2)
# Voteshare presidential candidate = Intercept + slope * difflog
# Voteshare presidential candidate = 0.50758 + 0.02384*difflog

regression3 <- lm(voteshare ~ presvote, data = inc.sub)
ggplot(data = inc.sub, aes(x = presvote, y = voteshare)) + 
  geom_point(size = 2, shape = 2) + 
  geom_smooth(method = lm, color = "pink") + 
  labs(title = "TBC", 
       x = "Voteshare presidential candidate", 
       y = "Voteshare incumbent presidents' party") + 
  theme_bw()

regression3

# Voteshare incumbent presidents' party = Intercept + slope * voteshare presidential candidate
# Voteshare incumbent presidents' party = 0.4413 + 0.3880*Voteshare presidential candidate

#create new dataframe for regression: 
dataframe <- data.frame(residuals_1 = residuals1, residuals_2 = residuals2)

#make scatterplot
regression4 <- lm(residuals_1 ~ residuals_2, data = dataframe)
ggplot(data = dataframe, aes(x = residuals_2, y = residuals_1)) + 
  geom_point(size = 2, shape = 2) + 
  geom_smooth(method = lm, color = "green") + 
  labs(title = "Residuals2 regressed on residuals1", 
       x = "residuals 2", 
       y = "residuals 1") + 
  theme_bw()

#Prediction equation
regression5 <- lm(voteshare ~ difflog + presvote, data = inc.sub)
summary(regression5)

summary(regression4)
summary(regression5)

# coefficient of presvote in reg5 and regression 4 is the same bc both 
# regressions answer same question in 2 different ways; how much presidents' voteshare influences 
# voteshare for incumbent if account for spending differences
