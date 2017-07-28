# Data wrangling in R
# Brian Stock
# 03.27.17

# set working directory to workshop folder with data files
setwd("/home/brian/Documents/Workshops/Mexico fisheries/StatsClass")
# check folder contents
list.files()
# load haul dataset
HAUL <- read.csv("wcann_haul.csv",header=TRUE)
# load fish dataset
FISH <- read.csv("wcann_fish.csv",header=TRUE)

# Look at HAUL
dim(HAUL)
names(HAUL)

# for some reason the hauls are duplicated
# only half are unique
length(unique(HAUL$Trawl.Id))

# So only keep the first half of the rows
n.unique <- length(unique(HAUL$Trawl.Id))
HAUL <- HAUL[1:n.unique,]

# Which vessel/boat has done the most hauls?
# How many vessels have been used?
table(HAUL$Vessel)

# When was the first haul?
sort(HAUL$Trawl.Date)[1]
# When was the most recent haul?
sort(HAUL$Trawl.Date)[6453]
tail(sort(HAUL$Trawl.Date),1)

# What depths do they normally survey?
# Calculate mean depth
mean(HAUL$Best.Depth..m.)
# Histogram of depth
hist(HAUL$Best.Depth..m., xlab="Depth (m)", main = "")

# Now look at FISH data
dim(FISH)
names(FISH)
head(FISH)

# Combine HAUL and FISH
# Create empty data frame where each row will be a unique haul
cols <- c("HAUL_ID","YEAR","DATE","TIME","LAT","LON","DEPTH","TEMP","DBRK","PHLB","YEYE")
dat <- matrix(NA, nrow=n.unique, ncol=length(cols))
dat <- as.data.frame(dat)
names(dat) <- cols # add column names to 'dat'
head(dat)

# Save workspace so we can come back to it later
save.image(file="hauls_fish.RData")
# show all objects we created
ls()
# delete all objects
rm(list=ls())
# see that everything is gone
ls()
# load your saved objects
load("hauls_fish.RData")
# see that everything is back
ls()










