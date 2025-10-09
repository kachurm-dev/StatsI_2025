###############################
# Project: Problem Set 1
# Author Name: Mairi Kachur
# Date last modified: 8.10.2025
###############################
# load libraries
# set wd
# clear global .envir
#####################
setwd("~/Documents/PhD/04. Classes/Quantitative Methods I/PS01")

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
lapply(c("paletteer"),  pkgTest)

#####################
# Problem 1
#####################
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# Task 1: Short version: n<30: t-test for CI
t.test(y, conf.level = 0.90)

# Task 1: Long version: 
n <- length(y)
mean_y <- mean(y)
sd_y <- sd(y)
se_y <- sd_y / sqrt(n)

# using critical value from t-distribution
t90 <- qt(1 - (0.10/2), df = n - 1)
ci_90_lower_t <- mean_y - t90 * se_y
ci_90_upper_t <- mean_y + t90 * se_y

# Results: mean of 98.44, confidence interval of (94;103). 

###

# Task 2:  
# Hypothesis testing, one sided hypothesis test.
# H0 = school mean is 100. 
# H1 = school mean is greater than 100. 
# a = 0.05 > 1-0.05 > 95%

t.test(y, mu = 100, alternative = "greater", conf.level = 0.95)

# test statistics (TS)
TS <- (mean_y-100)/se_y
pt(q=TS, df = 24)

# variant 1
1-pt(q=TS, df = 24)
# variant 2
pt(q=TS, df = 24, lower.tail = FALSE)

# Interpretation: The results give us the p-value is 0.72, we do not reject the H0. 
# The school's average IQ is not greater than the national average of 100. 

#####################
# Problem 2
#####################

# Please plot the relationships among Y, X1, X2, and X3 ? What are the correlations
# among them (you just need to describe the graph and the relationships among them)?

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)
head(expenditure)
str(expenditure)
pairs(expenditure[,c(2, 3, 4, 5)])

# Answer: We observe positive correlations for the plots (X1, Y), (X1, X3). 
# For (X3, Y), correlation not as clear.
# For (X2, Y), (X2, X1), (X2, X3) no clear linear relation, but rather some form of
# relation (can see curvature). 

####

# Please plot the relationship between Y and Region? On average, which region has the
# highest per capita expenditure on housing assistance?

str(expenditure)
expenditure$Region = factor(expenditure$Region,
                               levels = c(1, 2, 3, 4),
                               labels = c("Northeast", "North Central", "South", "West"))
boxplot(formula = expenditure$Y ~ expenditure$Region,
        ylab = "Spending on housing",
        xlab = "Regions")

# Answer: It appears that the Region "West" has the highest spending, while Region 
# "South" has the lowest spending. 

###

# Please plot the relationship between Y and X1 ? Describe this graph 
# and the relationship. 

plot(x = expenditure$X1, y = expenditure$Y,
     xlab = "per capita income", 
     ylab = "per capita expenditure on housing/shelters") 

# Answer: There appears to be a positive relationship (correlation) between X1 (per capita income)
# and Y (spending on housing).  

# Reproduce the above graph including one more variable Region and display
# different regions with different types of symbols and colors.

# Assigning shapes and colours to our observations!

colors <- paletteer_c("scico::berlin", n=4)
shapes <- c(17, 18, 15, 19)

plot(x = expenditure$X1, 
     y = expenditure$Y,
     col = colors[expenditure$Region],
     pch = shapes[expenditure$Region], 
     cex = 1.5,
     main = "Spending on housing against per capita income
     (by region)",
     xlab = "per capita income", 
     ylab = "per capita expenditure on housing/shelters")



