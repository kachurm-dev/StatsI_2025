# Name: PS02
# Autor: Mairi Kachur
# Last modified: 14 October 2025

# a) Calculate the χ2 test statistic by hand/manually 
# (even better if you can do ”by hand”in R).

# create matrix with observed values
data <- matrix(NA, nrow = 2, ncol = 3)
rownames(data) <- c("Upper class", "Lower Class")
colnames(data) <- c("Not stopped", "Bribe requested", "Stopped/given warning")
data[1,] <- c(14, 6, 7)
data[2,] <- c(7, 7, 1)
CSum <- colSums(data) 
RSum <- rowSums(data)
grand_total <- sum(data)

#create matrix with expected values
expected <- outer(RSum, CSum)/grand_total

chi_square <- sum((data - expected)^2/expected)

# Interpretation: The Chi-value is 3.79. This is a low value, meaning that we will 
# probably not be able to reject the upcoming H0. 

# b) Now calculate the p-value from the test statistic you just created (in R).2 What do you
# conclude if α= 0.1?
# H0 = Variables are statistically independent. 

df <- (nrow(data)-1)*(ncol(data)-1)
pchisq(chi_square, df = 2, lower.tail = FALSE)

# P-Value = 0.15, is > 0.1. We do not reject H0/the fact that variables are statistically 
# independent. As such, we assume that variables are statistically independent. 

# c) Calculate the standardized residuals for each cell and put them in the table below.
se <- sqrt(expected*(1-RSum/grand_total)*(1-CSum/grand_total))
z <- (data-expected)/se
z

# d) How might the standardized residuals help you interpret the results?

# 3 out of 6 cells have a higher value than expected (upper class/not stopped, 
# lower class/Bribe requested, upper class/stopped), with the remaining 3 cells 
# having a lower value than expected. However, the observed counts are not of 
# great magnitude, they don't have a strong deviation. This aligns with our observation 
# that the variables are statistically independent. 


# Question 2

data <- read.csv("women.csv")

# Having a look at data: 
head(data)
str(data)
summary(data)

# State a null and alternative (two-tailed) hypothesis.

# Run a bivariate regression to test this hypothesis in R (include your code!).
# Running a  regression of  Y ("water") on X ("reserved")

reg <- lm(water ~ reserved, data = data)
summary(reg)

# Coefficients:
 # (Intercept)    x  
 # 14.738        9.252  

# We find that a = 14.73 (= Y-intercept) and B = 9.25 (= slope of the line)






