# This is ECN 576 Homework #6

# This is the path name of the hw 6 folder
setwd("/home/optimus/Documents/Education/URI/Academics/Economics/ECN_576_Econometrics/HW/hw6")

library(foreign)    # To read a STATA .dta file
library(ggplot2)    # For plotting

# This opens the data set
data <- read.dta("oil_gdp.dta")

# Create variables for gdp per capita and oil consumption per capita
# Rescale gdp per capita in terms of units of $10,000
data$gdp_capita <- (data$gdp / data$pop) / 10000
data$oil_capita <- (data$oil / data$pop)
write.csv(data,"oil_gdp_units10K.csv")

# Regress oil per capita on gdp per capita
reg <- lm(oil_capita~gdp_capita, data=data)


# Plot residuals from model above against gdp per capita
e_hat <- resid(reg)
ggplot(data, aes(x = data$gdp_capita, y = e_hat)) +
    geom_point() +
    ggtitle("Residuals vs GDP", "Oil Regressed On GDP: Linear Specifiacation")

dev.copy(jpeg, filename="data_linear_eHat-gdp.jpg")
dev.off()

# Polynomial specification of the above model
data$gdp_capita2 <- (data$gdp_capita)^2    # per capita gdp squared
data$gdp_capita3 <- (data$gdp_capita)^3    # per capita gdp cubed
reg1 <- lm(oil_capita~gdp_capita3 + gdp_capita2 + gdp_capita, data=data)
reg1
summary(reg1)

# Plot residuals from model above against gdp per capita
e_hat1 <- resid(reg1)
ggplot(data, aes(x = data$gdp_capita, y = e_hat1)) +
    geom_point() +
    ggtitle("Residuals vs GDP", "Oil Regressed on GDP") +
    xlab("per capita GDP") + ylab("Residuals")




#### TRIM DATA ####

# Remove the obvious outlier from the polynomial specification above
# Create dataset with e_hat1 and gdp_capita
gdp_capita <- data$gdp_capita
residData_reg1 <- data.frame(gdp_capita, e_hat1)

# To find the largest value of e_hat1
# Tried to pull the info and move it around without relying on the copy/paste method
# which would increase the liklihood of an error
e_hat1Max <- max(e_hat1)

# To find the gdp_capita that corresponds to e_hat1Max
outlier_gdp <- residData_reg1$gdp_capita[residData_reg1$e_hat1 == e_hat1Max]

# To find the country that corresponds to countryOutlier
countryOutlier <- data[ which(data$gdp_capita == outlier_gdp), ]

# Modify the data to exclude the country with the outlier
trimmed_data <- subset(data, !(gdp_capita == outlier_gdp))
write.csv(trimmed_data,"oil_gdp_units10K_lessOutlier.csv")


#### POLYNOMIAL SPECIFICATION using trimmed data ####

# Polynomial specification of the polynomial model with outlier removed
reg2 <- lm(oil_capita~gdp_capita3 + gdp_capita2 + gdp_capita, data=trimmed_data)
reg2
summary(reg2)
plot(reg2)

# Plot residuals from model above against gdp per capita
e_hat2 <- resid(reg2)
ggplot(trimmed_data, aes(x = trimmed_data$gdp_capita, y = e_hat2)) + 
    geom_point() +
    ggtitle("Residuals vs GDP", "Oil Regressed on GDP: Polynomial Specification") +
    xlab("per capita GDP") + ylab("Residuals")
    
dev.copy(jpeg, filename="trimData_poly_eHat-gdp.jpg")
dev.off()    


#### GOLDFELD-QUANDT TEST ####

# For the above polynomial specification (reg2)
# Divide the dataset into the first 50% of obs. taking the floor for odd # obs.
partition <- floor(97 / 2)
gq_data1 <- trimmed_data[1:partition, ]
gq_data2 <- trimmed_data[(partition + 1):97, ]

# Regress oil on gdp for the 1st half
gq_data1Reg1 <- lm(oil_capita~gdp_capita3 + gdp_capita2 + gdp_capita, data=gq_data1)
gq_data1Reg1
summary(gq_data1Reg1)

# Regress oil on gdp for the 2nd half
gq_data1Reg2 <- lm(oil_capita~gdp_capita3 + gdp_capita2 + gdp_capita, data=gq_data2)
gq_data1Reg2
summary(gq_data1Reg2)

# Extract residuals from each regression above and calculate GQ, store in gq
sig_hat1 <- sigma(gq_data1Reg1)
sig_hat2 <- sigma(gq_data1Reg2)
gq <- sig_hat2^2 / sig_hat1^2

# Compare GQ to F Stat, print result
result <- (gq > qf(0.95, gq_data1Reg1$df, gq_data1Reg2$df))
print(result) # Prints FALSE


#### BREUSCH-PAGAN TEST ####

# For the polynomical specification (reg2)
library("lmtest") # For the BP test

# Square residuals
e_hatSquare <- e_hat2^2

# Regress square of e_hat from polynomial specification using trimmed dataset (reg2)
e_hatReg <- lm(e_hatSquare~gdp_capita3 + gdp_capita2 + gdp_capita, data=trimmed_data)
e_hatReg
summary(e_hatReg)

# Run BP test, studentize to false
bptest(e_hatReg, ~ gdp_capita3 + gdp_capita2 + gdp_capita, data=trimmed_data, studentize = FALSE)

# Check to see if p-value is below 95% significance level
result2 <- 0.025 < 5.048e-14 # Find a way to pull the p-value out w/out having to do it m(anually
print(result2) # Prints FALSE


#### TRANSFORMATIONS ####

# Multiplying all terms by 1/square root of gdp_capita from trimmed dataset; Store term in T
trimmed_data$T <- 1/sqrt(trimmed_data$gdp_capita)

# Transformation of linear model using no constant option and trimmed dataset
regT_linear <- lm(I(T * oil_capita)~I(T * gdp_capita) + I(T * 1) -1, data=trimmed_data)
regT_linear
summary(regT_linear)

# Store residuals and plot against gdp_capita
e_hatT_lin <- resid(regT_linear)
ggplot(trimmed_data, aes(x = trimmed_data$gdp_capita, y = e_hatT_lin)) + 
    geom_point() +
    ggtitle("Residuals vs GDP", "Oil Regressed on GDP: Transformed Linear Specification") +
    xlab("per capita GDP") + ylab("Residuals")

dev.copy(jpeg, filename="trimData_T_linear_eHat-gdp.jpg")
dev.off() 

# Transform the data and re-estimate the polynomial model (reg2) using 
# no constant option and trimmed dataset
regT_poly <- lm(I(T * oil_capita)~I(T * gdp_capita3) + I(T * gdp_capita2) + 
        I(T * gdp_capita) + I(T * 1) -1, data=trimmed_data)
regT_poly
summary(regT_poly)

# Store residuals to plot against gdp_capita
e_hatT_ply <- resid(regT_poly)
ggplot(trimmed_data, aes(x = trimmed_data$gdp_capita, y = e_hatT_ply)) + 
    geom_point() +
    ggtitle("Oil vs GDP", "Oil Regressed on GDP: Transformed Polynomial Specification") +
    xlab("per capita GDP") + ylab("Residuals")

dev.copy(jpeg, filename="trimData_T_poly_oil-gdp.jpg")
dev.off() 

# Transformation of model regressing e_hat squared on the polynomial specification above
e_hatRegT <- lm(I(T * e_hatSquare)~I(T * gdp_capita3) + I(T * gdp_capita2) + 
        I(T * gdp_capita) + I(T * 1) -1, data=trimmed_data)
e_hatRegT
summary(e_hatRegT)
plot(e_hatRegT)

# Store residuals and plot against gdp_capita
residuals_e_hatRegT <- resid(e_hatRegT)
ggplot(trimmed_data, aes(x = trimmed_data$gdp_capita * T, y = residuals_e_hatRegT)) + 
    geom_point() +
    ggtitle("Residuals vs GDP", "Residuals Regressed On GDP: Transformed Polynomial Specification") +
    xlab("per capita GDP") + ylab("Residuals")

dev.copy(jpeg, filename="trimData_T_poly_eHat-gdp.jpg")
dev.off()


#### 2nd PARTITION ####

# Partition the data based on gdp so that they have similar heteroskadasticities at 0.5 gdp
gdp_part <- 0.5
gdp_part1 <- subset(trimmed_data, gdp_capita <= 0.5)
gdp_part2 <- subset(trimmed_data, gdp_capita > 0.5)

# Estimate linear model on both sets of partitioned data (reg)
gdp_part1_reg_lin <- lm(oil_capita~gdp_capita, data=gdp_part1)
gdp_part1_reg_lin
summary(gdp_part1_reg_lin)

gdp_part2_reg_lin <- lm(oil_capita~gdp_capita, data=gdp_part2)
gdp_part2_reg_lin
summary(gdp_part2_reg_lin)

# Estimate polynomial model on both sets of partitioned data (reg1)
gdp_part1_reg_poly <- lm(oil_capita~gdp_capita3 + gdp_capita2 + gdp_capita, data=gdp_part1)
gdp_part1_reg_poly
summary(gdp_part1_reg_poly)

gdp_part2_reg_poly <- lm(oil_capita~gdp_capita3 + gdp_capita2 + gdp_capita, data=gdp_part2)
gdp_part2_reg_poly
summary(gdp_part2_reg_poly)

# Estimate the Transformed Polynomial Model (reg2)
gdp_part1_regT_poly <- lm(I(T * oil_capita)~I(T * gdp_capita3) + I(T * gdp_capita2) + 
        I(T * gdp_capita), data=gdp_part1)
gdp_part1_regT_poly
summary(gdp_part1_regT_poly)

gdp_part2_regT_poly <- lm(I(T * oil_capita)~I(T * gdp_capita3) + I(T * gdp_capita2) + 
        I(T * gdp_capita), data=gdp_part1)
gdp_part2_regT_poly
summary(gdp_part2_regT_poly)

# Store residuals from partion 1 regression w/o transform and plot against gdp_capita
eHatT_poly_part1 <- resid(gdp_part1_reg_poly)
ggplot(gdp_part1, aes(x = gdp_part1$gdp_capita, y = eHatT_poly_part1)) + 
    geom_point() +
    ggtitle("Residuals vs GDP", "Oil Regressed on GDP: Transformed Polynomial Specification on Partitioned GDP") +
    xlab("per capita GDP Partition 1") + ylab("Residuals")

dev.copy(jpeg, filename="trimData_T_poly_part1_eHat-gdp.jpg")
dev.off() 

# Store residuals from partion 2 regression w/o transform and plot against gdp_capita
eHatT_poly_part2 <- resid(gdp_part2_reg_poly)
ggplot(gdp_part2, aes(x = gdp_part2$gdp_capita, y = eHatT_poly_part2)) + 
    geom_point() +
    ggtitle("Residuals vs GDP", "Oil Regressed on GDP: Transformed Polynomial Specification on Partitioned GDP") +
    xlab("per capita GDP Partition 2") + ylab("Residuals")

dev.copy(jpeg, filename="trimData_T_poly_part2_eHat-gdp.jpg")
dev.off() 


#### DERIVATIVES ####

# Function for 1st derivative of polynomial model w/o transform from trimmed dataset
f_prime_poly <- function (gdp){
    oil <- 15.36 - 10.40 * (gdp) + 1.83 * (gdp)^2
    return (oil);
}

# Function for the first derivative of the polynomial model and gdp partition 1
f_prime_part1 <- function(gdp){
    oil <- 27.23 - 55.68 * (gdp) - 53.10 * (gdp)^2
    return(oil)
}

# Function for the first derivative of the polynomial model and gdp partition 2
f_prime_part2 <- function(gdp){
    oil <- 13.75 - 8.06 * (gdp) + 0.06 * (gdp)^2
    return(oil)
}

# Solve each derivative function for levels of gdp of 2000 8000 & 20000
roc_poly_2k <- f_prime_poly(2000)
roc_part1_2k <- f_prime_part1(2000)
roc_part2_2k <- f_prime_part2(2000)

roc_poly_8k <- f_prime_poly(8000)
roc_part1_8 <- kf_prime_part1(8000)
roc_part2_8k <- kf_prime_part2(8000)

roc_poly_20k <- f_prime_poly(20000)
roc_part1_20k <- f_prime_part1(20000)
roc_part2_20k <- f_prime_part2(20000)


#### Huber/White Sandwhich Estimator ####

library(sandwich)   # For the estimator

# Run the polynomial specification with the full daata
regHW <- coeftest(reg1, vcov=sandwich)
regHW
summary(regHW)



