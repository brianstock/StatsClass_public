#T-test/Normality Lecture Notes
#By Lynn Waterhouse
#March 26, 2017 
#Stats Class


## Checking for Normality
#There are 5 major tests used:
#  -Shapiro-Wilk W test 
#  -Anderson-Darling test 
#  -Martinez-Iglewicz test 
#  -Kolmogorov-Smirnov test 
#  -D'Agostino Omnibus test

#Note: Power of all is weak if N < 10 

### Shapiro Wilk W Test
#Recall we fail to reject the null hypothesis that the distribution is normal if the 
#calculated p-value is >0.05.

set.seed(1234)
a<-floor(rnorm(30,500,20))
a
hist(a)
shapiro.test(a)
qqnorm(a)
qqline(a)


### Log and Square Root Transformations
#Imagine we have collected measurements on the circumference of trees in a forest. 
#There are many young trees, so we have small circumferences. There are a few older 
#trees, with larger circumferences. If we look at the data in R we can look at a 
#histogram of the circumferences. The data is saved in a vector 'dat1'

set.seed(1245)
dat1<-round(exp(rnorm(100,3,.5))) #create fake tree data
hist(dat1, main="Histogram", xlab="Tree Circumference (cm)")
mean(dat1)
mode(dat1)
median(dat1)
sd(dat1)

#We can also look at the mean, mode, median, and standard deviation.
mean(dat1)
mode(dat1)
median(dat1)
sd(dat1)

#We can visually (qalitatively) evaluate normality by looking at the QQ plot.
qqnorm(dat1)
qqline(dat1)

#What do you think? Doesn't look so great, right?

#So we can try a log transformation.
hist(log(dat1))
qqnorm(log(dat1))
qqline(log(dat1))

#We can also try a square root transformation.
hist(sqrt(dat1))
qqnorm(sqrt(dat1))
qqline(sqrt(dat1))


#Which did you think looks better? Why?

#In fisheries, log transformations and square root transformations tend 
#to be the most common transformations. This is because with catch data we 
#often have lots of low catch days and then some very high catches.

#################################################
### Cautionary warning when using normality tests
#Null is that distribution is normal.

#When the sample size is small, even big departures from normality are not detected, 
#and when your sample size is large, even the smallest deviation from normality will 
#lead to a rejected null.

#Look at a small sample size, non-normal.
set.seed(123)
bb<-runif(8,1,100)
#look at shapiro.test
shapiro.test(bb)
#look at historgram
hist(bb)
#check out the qq-plots
qqnorm(bb)
qqline(bb)

# T-tests

## One Sample T-test
#A one sample t-test can be used to compare a sample average to some constant.

#Example one sample T-test problem:
#The mean weight of abalone sold at market in a package is listed as 500g, 
#you want to check if the abalone sold in the market have a mean of 500g. 
#You have a sample of 30 abalone. The mean is 493.6667g and the sample standard 
#deviation 18.02552g.

#Now in R look at the data. We will look at the mean and standard deviation.
#We will look at a histogram to qualitatively evaluate normality.

set.seed(1234)
a<-floor(rnorm(30,500,20))
a

mean(a)
sd(a)
hist(a)

#What are the hypotheses?
#Null: average weight = 500
#Alternative: average weigh not equal to 500

### Conduct the T-test
?t.test
t.test(a,mu=500)


#Say I am worried about being ripped off by the store. I am only concerned with 
#if the store is giving me less than 500g of abalone.
#H0-Null- : mean >=500
#Ha- Alternative: mean < 500
t.test(a,mu=500,alternative="less")


### Calculating the 95% Confidence interval by hand
#Using the mean, standard deviation, and sample size we can also calculate the 
#95% Confidence interval by hand. This is for the 2 sided hypotheses.
#Null: mean = 500
#Alternative: mean not equal 500

#calculate 95% confidence by hand
lower.ci<-mean(a)-qt(.975, df=29)*sd(a)/sqrt(length(a))
upper.ci<-mean(a)+qt(.975, df=29)*sd(a)/sqrt(length(a))
lower.ci
upper.ci

#calculate 95% CI with rule of thumb
lower.ci<-mean(a)-2*sd(a)/sqrt(length(a))
upper.ci<-mean(a)+2*sd(a)/sqrt(length(a))
lower.ci
upper.ci

#If the confidence interval contains 500 it is like having a p-value >0.05.
#If the confidence interval does not contain 500 it is like having a p-value <=0.05

## Paired T-tests
#We are curious if white seabass have a deep tail. We can tell measure the "deepness" 
#of the tail by looking at the difference between total length of the fish and 
#standard length. We consider a tail deep if it is more than 1/6 of the body length 
#of the fish. 

#Look at the data
#read in the dataset
wsb.dat<-read.csv("wsb.dat.csv") #read in the data
head(wsb.dat) #look at first 6 rows
summary(wsb.dat)
hist(wsb.dat$wsb.SL)
hist(wsb.dat$wsb.TL)


#Make a new variable for the difference between Total length and Standard length. 
diff<-wsb.dat$wsb.TL-wsb.dat$wsb.SL

#In order to use the paired t-test the data (differences) need to be normal. 
#Let's evaluate the normality. We can do this 3 ways. Using the qqnorm plot 
#and also with shapiro.test and histogram
qqnorm(diff)
qqline(diff)
shapiro.test(diff)
hist(diff)

#we are interested in if the tail is longer than 1/6
#multiplied by the average body length (deep tail)

# null hypothesis- difference <= 1/6*avg body length
# alternative hyp-  difference > 1/6*avg body length

#Now, let's do the t-test. We treat it the same as the one sample t-test. 
#We are interested in if the differences is more than 1/6 the body length. 
#In order to keep things simple we will calculate 1/6 body length using the 
#mean total length times 1/6.  This means the be the null hypothesis will be: 
#that diff < 1/6*mean(TL).

null.diff<-(1/6)*mean(wsb.dat$wsb.TL)
t.test(diff,mu=null.diff,alternative="greater")

#What do we conclude?
#We fail to reject the null hypothesis. We do not have enough statistical evidence 
#to claim that the white seabass have a "deep" tail.

#alternative way to conduct this paried t-test
t.test(wsb.dat$wsb.TL,wsb.dat$wsb.SL,alternative="greater",paired=TRUE)

#################################################################
#################################################################
## Two sample T-test
#There are two types of two sample t-tests. You can easily choose which one is 
#appropriate by looking at the variances of the two samples. If the variances 
#are homogeneous, you can use the two sample t-test with homogeneous vraiances. 
#If the variances are not homogeneous, you use the two sample t-test for 
#heterogeneous variances.

### Testing homogeneity of variances
#We test for homogeneity of variances in R using 'var.test(x,y)', where x and y 
#are the two samples.  It tests the null hypothesis that they are equal (actually 
#that the ratio of the two variances is equal to 1.
                                                                                                                                                     
#Example Homogeneous variances
#We want to compare the weights of rays in two sites, B and G.  We have the weights 
#recorded in kg in the file "ray.weightskg.csv".
ray.weightskg<-read.csv("ray.weightskg.csv")
head(ray.weightskg)
#make 2 vectors
B<-ray.weightskg$B
G<-ray.weightskg$G

#Look at the data and evaluate normality.
qqnorm(B)
qqline(B)

qqnorm(G)
qqline(G)

hist(B)

hist(G)

shapiro.test(B)
shapiro.test(G)
#Check if we have homoegeneity of variances.
var.test(B,G)

#p-value>0.05 unable to reject null that we have homogeneous variances
#Now, let's do the two sample t-test with homogeneity of variances.
t.test(B,G, var.equal=TRUE)

#So what do we conclude?
#p-value <0.05, so we reject the null- they are difference
#We say that there is a statistically significant difference in the weights 
#of rays from sites B and G.


### Two Sample T-test with Non-Homogeneous Variances
#We want to see if wild caught salmon weigh more than captive salmon. We catch 
#100 salmon of age 10 from the wild and 100 that are captive, and weigh them all 
#in kg. The data are in "salmon.weight.csv". Read in the data and evaluate the 
#normality.

#read in the data
salmon<-read.csv("salmon.weight.csv")

#make new vectors
Captive<-salmon$Captive
Wild<-salmon$Wild

#evaluate normality 3 ways
hist(Captive)
hist(Wild)

shapiro.test(Captive)
shapiro.test(Wild)

qqnorm(Captive)
qqline(Captive)

qqnorm(Wild)
qqline(Wild)

#What are the Hypotheses?
#Null: wild <= captive
#Alternative: wild > captive

#Evaluate the homoegeneity of the variances.
var.test(Captive,Wild)

#The p-value < 0.05, so now we can perform the two-sample t-test for 
#non-homoegeneous (or heterogeneous) variances.

#Conduct the t-test. What do you conclude?
t.test(Wild, Captive, var.equal=F, alternative="greater")


#The p-value is <0.05, so we reject the null hypothesis. 
#We conclude that wild salmon weigh a statistically significant more amount than 
#captive salmon.