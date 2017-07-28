# Multiple Regression
# Josh Stewart
# March 29, 2017

#Read in data:
data<-read.csv("MultipleRegression.csv")

#Look at our variables
head(data)
names(data)

#Three continuous variables! What do we do?

#Plotting temperature & size:
plot(x=data$temp,y=data$size)

#with() attaches a data frame so we don't have to use $
with(data,plot(x=temp,y=size))

#Plot chl-a & size:
with(data,plot(x=chla,y=size))

#Are Chl-a and Temperature related?:
with(data,plot(x=temp,y=chla))


#Regressions (review):
c.lm<-lm(size~chla,data=data)
summary(c.lm)
with(data,plot(x=chla,y=size))
abline(c.lm,col="red")
# size = a + b*chla


t.lm<-lm(size~temp,data=data)
summary(t.lm)
with(data,plot(x=temp,y=size))
abline(t.lm,col="red")
# size = a + b*temp

# But perhaps size is a factor not only of temperature and chl a
# independently, but a combination of the two. 
# Let's move on to Multiple Regression.

# Multiple Regression with (+):
ct.lm<-lm(size~chla+temp,data=data)
summary(ct.lm)

# chla+temp gives us 2 slope coefficients 
# size = a + b1*chla + b2*temp

# Multiple Regression with (:):
ct.lm.int<-lm(size~chla:temp,data=data)
summary(ct.lm.int)

# chla:temp gives us one slope coefficient for 
# the interaction of chla & temp
# size = a + b * (chla * temp)

# Multiple regression with (*):
ct.lm.all<-lm(size~chla*temp,data=data)
summary(ct.lm.all)

# chla*temp gives us 3 slopes for:
# Chl-a, Temp, and (Chl-a * Temp)
# size = a + b1*chla + b2*temp + b3*(chla*temp)


# Compare R2 values:
summary(t.lm)$adj.r.squared
summary(c.lm)$adj.r.squared
summary(ct.lm)$adj.r.squared
summary(ct.lm.int)$adj.r.squared
summary(ct.lm.all)$adj.r.squared


# Compare AIC:
AIC(t.lm, c.lm, ct.lm, ct.lm.int, ct.lm.all)

# Best model (selected by AIC) has the SMALLEST
# AIC value, even if they are all negative. 
# NOT the closest to zero
# AIC Balances the number of parameters and the model fit
