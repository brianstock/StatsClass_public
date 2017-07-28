# ANOVA
# Brian Stock
# 03.27.17

# Load packages
library(multcomp) # for Tukey multiple comparisons
library(dplyr)    # for summarizing data

# Set your working directory to where your workshop files are
setwd("/home/brian/Documents/Workshops/Mexico fisheries/StatsClass")
dat <- read.csv("ANOVA_CPUE.csv")

# Look at dat
names(dat) # column names (variables)
dim(dat) # number rows and columns
head(dat) # first 6 lines
summary(dat) # summarize each column
table(dat$site) # get sample size of each site

# Boxplot of CPUE by site
# want y ~ x, explain change in y using x
boxplot(CPUE ~ site, data = dat,
        xlab = "Site",
        ylab = "Catch/day")

# Use dplyr to create summary table
# %>% means 'pass result on' or 'pipe'
# n() gets the sample size of each group
# mean() gets the mean of each group
dat.summary <- dat %>% 
  group_by(site) %>%
  summarize(N=n(), CPUE.mean=mean(CPUE))

dat.summary

# Check assumptions
# 1. each group is normal
# make QQ plots for each group
# par(mfrow) puts more than one graph in same plot
# mar adjusts plot margins (bot,left,top,right)
par(mfrow = c(2,2), mar=c(2, 4, 1, 1) + 0.1)

# filter is from dplyr. it selects rows (like 'which')
qqnorm(filter(dat,site=="B.Asuncion")$CPUE, main="B.Asuncion")
qqline(filter(dat,site=="B.Asuncion")$CPUE)
qqnorm(filter(dat,site=="Bocana")$CPUE, main="Bocana")
qqline(filter(dat,site=="Bocana")$CPUE)
qqnorm(filter(dat,site=="B.Tortugas")$CPUE, main="B.Tortugas")
qqline(filter(dat,site=="B.Tortugas")$CPUE)
qqnorm(filter(dat,site=="P.Abreojos")$CPUE, main="P.Abreojos")
qqline(filter(dat,site=="P.Abreojos")$CPUE)

# Check 2. each group have equal variance? (boxplot is good enough)
boxplot(CPUE~site,data=dat)

# Assumptions are ok. Do an ANOVA
fit <- aov(CPUE~site, data=dat) # create an 'aov' object
class(fit) # check class of fit ('aov' 'lm')
summary(fit) # see ANOVA table

# Null: all sites have same mean CPUE
# p < 0.05 so we reject the null
# At least one site has a different mean CPUE

# Now do Tukey test. Which sites are different?
# Using 'multcomp' package
# glht = general linear hypothesis test
# mcp = multiple comparisons
# 'site' is the group variable in 'dat'
fit.tukey <- glht(fit, mcp(site="Tukey"))
summary(fit.tukey)

# add Tukey letters to boxplot
par(mfrow=c(1,1)) # reset your plot to have only 1 graph
boxplot(CPUE~site,
        data=dat,
        xlab="Site",
        ylab="Catch/day (# fish)") # same boxplot
# get Tukey letters from 'fit.tukey' using 'cld' function
letters <- cld(fit.tukey)$mcletters$Letters
n.sites <- length(unique(dat$site)) # number of sites
for(i in 1:n.sites) mtext(letters[i],side=3,at=i)












