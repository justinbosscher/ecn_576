# This is ECN 576 Homework #4

# This is the path name of the hw 4 folder
setwd("/home/optimus/Documents/Education/URI/Academics/Economics/ECN_576_Econometrics/HW/hw4")

# This opens the data set
data <- read.csv("ri_housing_2013.csv", header=TRUE)

# regress sales price on distance from coastline. price is dependent
# variable and distance is independent variable.
reg <- lm(price~coastline_mi, data=data)
reg
summary(reg)

# calculate average distance from coastline
mean(data$coastline_mi)

# calculate average house price
mean(data$price)

# using a different specification to find the elasticity of price with
# respect to distance from the coastline
data$logPrice <- log(data$price)
data$logCoastline_mi <- log(data$coastline_mi)
reg <- lm(logPrice~coastline_mi, data=data)
reg
summary(reg)

# regress price on distance, living area, bedrooms, bathrooms,
# half bathrooms, fireplaces, air conditioning, pool, water view,
# year built, and condition (bad, good, excellent)
data.goodBad <- subset(data, condition == "bad" | condition == "good")
reg <-lm(price ~ coastline_mi + livingarea + bedrooms + bathrooms + halfbath + pool + numfirepl + water_view + aircon + yearbuilt + condition + condition, data=data.goodBad)
reg
summary(reg)
# run same regression with excellent condition included
reg <-lm(price ~ coastline_mi + livingarea + bedrooms + bathrooms + halfbath + pool + numfirepl + water_view + aircon + yearbuilt + condition + condition, data=data)
reg
summary(reg)

# create variable for distance^2 and run same regression
data$distSquared <- (data$coastline_mi^2)
reg <-lm(price ~ coastline_mi + distSquared + livingarea + bedrooms + bathrooms + halfbath + pool + numfirepl + water_view + aircon + yearbuilt + condition + condition, data=data)
reg
summary(reg)
plot(price~distSquared, data=data)
plot(price~coastline_mi, data=data)

# create dummy var's for distance
 

# Create new dataset of housing prices < $1,000,000
newData <- data[ which(data$price > 1000000), ]

# regress price on living area where price > 1000000
reg <- lm(price~livingarea, data=data)
reg
summary(reg)

# regress log price on log living area where price > 1000000
data$logLivingArea <- log(data$livingarea)
data$logPrice <- log(data$price)
reg <- lm(logPrice~logLivingArea, data=data)
reg
summary(reg)

# calculate the proportion of living area coefficient of mean house price
proportion <- 213.47/mean(data$price)

