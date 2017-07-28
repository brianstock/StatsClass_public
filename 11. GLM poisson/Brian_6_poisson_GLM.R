# Brian Stock
# 03.29.17
# Poisson GLM

# Poisson GLM estimates density from count data (surveys)

# load packages
library(dplyr)
library(multcomp)

# read in data
setwd("/home/brian/Documents/Workshops/Mexico fisheries/StatsClass")
dat <- read.csv("poissonGLM.csv", header=TRUE)

# look at data
head(dat)
summary(dat)
names(dat)
dim(dat)

# boxplot of counts by site
boxplot(counts ~ site, data=dat)

# make histograms of counts by site
bahia <- filter(dat, site=="bahia")$counts
costa <- filter(dat, site=="costa")$counts
marietas <- filter(dat, site=="marietas")$counts

hist(bahia)
hist(costa)
hist(marietas)

# ANOVA assumes each site is normal
# QQ plots
par(mfrow = c(2,2), mar=c(2, 4, 1, 1) + 0.1)
qqnorm(bahia, main="bahia")
qqline(bahia)
qqnorm(costa, main="costa")
qqline(costa)
qqnorm(marietas, main="marietas")
qqline(marietas)
# QQ plots have steps ==> we have discrete data
# not normal

# Count/discrete data ==> fit Poisson GLM
pois.mod <- glm(counts ~ site, data=dat, family=poisson)

# Look at model summary
summary(pois.mod)

# what does this mean?
# sitecosta has Estimate = -0.50418 and p-value <2e-16
# The density of mantas on the costa is significantly lower than in bahia.

# sitemarietas has Estimate = -0.44121 and p-value <2e-16
# The density of mantas at the marietas is significantly lower than in bahia.

# What if we want to test if there is a difference between Costa and Marietas?
# Tukey test (same as ANOVA)
pois.tukey <- glht(pois.mod, mcp(site="Tukey"))
summary(pois.tukey)

# Conclusion
# Costa - Bahia: significant difference
# Marietas - Bahia: significant difference
# Marietas - Costa: NOT significantly different

# Calculate mean density by site
newdata <- data.frame(site=c("bahia","costa","marietas"))
predict(pois.mod, newdata, type="response")

# Calculate 95% CI for the mean estimates
pred <- predict(pois.mod, newdata, type="response", se.fit=TRUE)
# pred$fit has the means
lwr <- pred$fit - 1.96*pred$se.fit # lower bound (2.5%)
upr <- pred$fit + 1.96*pred$se.fit # upper bound (97.5%)

# Bahia = 10.1 (9.6, 10.5)
# Costa = 6.1 (5.7, 6.4)
# Marietas = 6.5 (6.1, 6.8)

# The Normal/gaussian GLM is identical to an ANOVA in this case.
# fit an ANOVA
aov.mod <- aov(counts ~ site, data=dat)
# fit a normal/gaussian GLM
norm.mod <- glm(counts ~ site, data=dat, family=gaussian)
# compare models using AIC
AIC(pois.mod, aov.mod, norm.mod)

# See that ANOVA = normal GLM
# Conclusion: Poisson model is the best because it has the lowest AIC




























