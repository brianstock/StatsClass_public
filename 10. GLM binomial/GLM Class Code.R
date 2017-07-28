# Generalized Linear Models (GLMs)
# A class of models that allow us to fit linear 
# relationships with data that are not normally distributed

TurtleData<-read.csv("TurtleData.csv")

head(TurtleData)

with(TurtleData,plot(x=Temperature,y=observations,pch=19))

# Fit our GLM:

TurtleGLM <- glm(observations~Temperature, 
                 family = binomial(link = "logit"),
                 data = TurtleData)

summary(TurtleGLM)

# Summary still gives us a slope and intercept
# But how did we fit a linear model to binomial data?

# Extract the coefficients
t.c<-coef(TurtleGLM)

# Manually calculating response based on Temp & Coefficients
# Response = Explanatory * b + a  (Explanatory is Temp)
# t.c[2] is slope; t.c[1] is intercept
GLM_y <- TurtleData$Temperature*t.c[2]+t.c[1]


#Let's plot the predicted values from the GLM:

plot(x=TurtleData$Temperature,y=GLM_y,pch=19)

# Transform our GLM predictions using the Logistic Distribution:
hist(plogis(GLM_y))

# Plot our transformed values:
plot(x=TurtleData$Temperature,y=plogis(GLM_y),pch=19)


#Plot results of GLM with turtle observations:

with(TurtleData,plot(x=Temperature,y=observations,pch=19))
points(x=TurtleData$Temperature,y=plogis(GLM_y),pch=19,col="red")


# And now for the easy (and better-looking) way of plotting this:

with(TurtleData,plot(x=Temperature,y=observations,pch=19))

Temp<-data.frame(seq(24,31,length.out = 100)) #Make vector of Temp values

#IMPORTANT: This colname must be the same as your GLM variable
colnames(Temp)<-"Temperature" 

#Create your predicted probabilities using the predict.glm() function
Prob<-as.vector(predict.glm(TurtleGLM,newdata=Temp,type="response"))

#Add the probability curve using lines()
lines(x=Temp$Temperature,y=Prob,pch=19,col="red")


# Add confidence intervals:

with(TurtleData,plot(x=Temperature,y=observations,pch=19))

# Get your un-transformed predicted values from the GLM
link<-predict.glm(TurtleGLM,newdata=Temp,type="link")

# Find the standard error (se) of those predicted values
se<-predict.glm(TurtleGLM,newdata=Temp,type="link",se.fit=T)$se.fit

# Calculate the upper and lower limits of your confidence intervals
upper<-link+se*1.96
lower<-link-se*1.96

# Use plogis() to convert back to probabilities, plot smooth lines
lines(x=Temp$Temperature,y=plogis(link),col="red")
lines(x=Temp$Temperature,y=plogis(upper),col="red",lty=2)
lines(x=Temp$Temperature,y=plogis(lower),col="red",lty=2)


# Calculate the value at which you have a 50% probability of your response:

Prob<-predict.glm(TurtleGLM,newdata=Temp,type="response")
Prob

# Search through Prob until we find 0.5 values, or use which()
# to find the surrounding indices (in this case that's 60 & 61)
ind <- which(Prob > 0.49 & Prob < 0.51)
Temp[ind,]
Temp[60:61,] #60 is 28.17, 61 is 28.24

# If you want, get more precise: 
Temp<-data.frame(seq(28.17,28.24,length.out = 100))
colnames(Temp)<-"Temperature"
predict.glm(TurtleGLM,newdata=Temp,type="response")

