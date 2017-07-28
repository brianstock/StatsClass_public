# Brian Stock
# 03.28.17
# Linear regression

# load packages
library(FSAdata)
library(dplyr)

# load Walleye dataset, only data from 2014
dat <- filter(WalleyeErie2,year==2014)
head(dat)

# quick look at length and weight
summary(dat$tl) # tl = total length (mm)
summary(dat$w) # w = weight (g)

# Plot length vs weight
plot(w~tl,data=dat,
     pch=19,
     col="black",
     xlab="Total Length (mm)",
     ylab="Weight (g)")

# Check assumptions of linear model
# Look at residual plot
mod1 <- lm(w ~ tl, data=dat)
plot(fitted(mod1), residuals(mod1)) # method 1
abline(h=0)
plot(mod1, which=1) # method 2

# conclusion from residual plot
# there is a relationship but NOT LINEAR
# 1. not shotgun/random = there is a pattern
# 2. variance not equal for different X

# Let's transform the data and see if it's better
dat$logW <- log(dat$w) # log(weight)
dat$logL <- log(dat$tl) # log(length)
# Plot log(weight) vs. log(length)
plot(logW ~ logL, data=dat, pch=19,
     xlab = "log(Length)",
     ylab = "log(Weight)")
# Now check the residual plot of transformed data
mod2 <- lm(logW ~ logL,data=dat)
plot(mod2, which=1)
# Residual plot looks better
# no pattern in residuals (red line is flat)
# equal variance for all X values

# Look at model results
summary(mod2)

# p-value for logL (slope) <2e-16
# reject null (there is no relationship between weight and length)
# accept alternate, there IS a relationship between weight and length

# R2 = 0.9787
# log(length) explains 97.9% of the variance in log(weight)

# print estimates of intercept and slope
coef(mod2)
# get 95% confidence intervals for intercept and slope
confint(mod2)
# Predict the weight of a fish length = 500 mm
p1 <- predict(mod2,
              data.frame(logL=log(500)),
              interval="prediction")
# p1 is LOG weight, not weight!
# how do I get the weight from p1?
exp(p1)

# Plot our fitted length-weight relationship
# Add 95% prediction interval
# 95% of data points should be inside interval
minX <- min(dat$tl)
maxX <- max(dat$tl)
L <- seq(minX, maxX, length.out=500)
# 1. turn length into log(length)
# 2. predict log(weight) from log(length) using our line ('predict' function)
# 3. turn log(weight) into weight using 'exp'
W <- exp(predict(mod2, data.frame(logL=log(L)),
                 interval="prediction"))
# fit = mean/prediction
# lwr = 2.5% lower bound of prediction interval
# upr = 97.5% upper bound of prediction interval
head(W)

# Plot our length-weight relationship
plot(w ~ tl, data=dat, pch=19, col="black",
     xlab="Total Length (mm)",
     ylab="Weight (g)")
# 'lines' adds a line to an existing plot
# add the solid red line (mean prediction, 'fit' column of W)
lines(L, W[,"fit"], lwd=2, col="red")
# add dashed red line (lower bound, 'lwr' column of W)
lines(L, W[,"lwr"], lwd=2, lty=2, col="red")
# add dashed red line (upper bound, 'upr' column of W)
lines(L, W[,"upr"], lwd=2, lty=2, col="red")






