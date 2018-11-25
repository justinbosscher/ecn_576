########################### Homework 8 #########################################
#    Justin Bosscher    University of Rhode Island    ECN 576 Econometrics     #

# Set the working directory
wd <- paste("/home/optimus/Documents/Education/URI/Academics/Economics/ECN_576_Econometrics/HW/hw8") 
            
            
setwd(wd)

# Load libraries ###############################################################
library("plm")      # For panel data regressions
library(foreign)    # To read a STATA .dta file
library(ggplot2)    # For plotting
library("dplyr")    # For F-test
library(psych)      # For data summary stats
library(car)        # For scatterplot()
library("lmtest")   # For Hausman Test

# Data #########################################################################
# Open datasets
value <- read.dta("value.dta")
profitYield <- read.dta("profit_and_yield.dta")
weather <- read.dta("weather.dta")
soil <- read.dta("soil.dta")
climate <- read.dta("climate.dta")
total_less_yield <- read.csv("totalLessYield.csv")
total <- read.csv("total.csv")

# Merge dataframes two at a time by fips and year; include only 1987-2002
value_87_02 <- subset(value, year >= 1987)
total_less_yield <- merge(value_87_02, weather, by = c("year","fips"))
total_less_yield <- merge(total_less_yield, climate, by = "fips")
total_less_yield <- merge(total_less_yield, soil, by = "fips")

# Set any blank cells to NA
total_less_yield[total_less_yield==""] <- NA

# Check for missing data
sum(is.na(total_less_yield))    # Prints 0


# Test for NA's and inf's; place any column names that fail in indx
indx_c <- apply(total_less_yield, 2, function(x) any(is.na(x) | is.infinite(x)))
print(indx_c)     # All come back FALSE

# By row
indx_r <- apply(total_less_yield, 1, function(x) any(is.na(x) | is.infinite(x)))
print(indx_r)     # All come back FALSE

# Sort total_less_yield by year and fips
total_less_yield[order("year", "fips"), ]

# Create polynomial values for use in regressions
total_less_yield$c_dday8_32_square <- (total_less_yield$c_dday8_32)^2
total_less_yield$c_dday34_sqrt <- sqrt(total_less_yield$c_dday34)
total_less_yield$c_prec_square <- (total_less_yield$c_prec)^2
total_less_yield$prec_square <- (total_less_yield$prec)^2
total_less_yield$dday8_32_square <- (total_less_yield$dday8_32)^2
total_less_yield$dday34_sqrt <- sqrt(total_less_yield$dday34)
total_less_yield$prec_square <- (total_less_yield$prec)^2

# Save the dataframe so I don't have to load all of this 1000X
write.csv(total_less_yield, file="totalLessYield.csv")

# Question A ###################################################################
# Regress per acre farm value on climate, soil quality indicators, population
# density and income per capita for each year from 1987 to 2002 using a for loop

# Create vetor, years, containing the years 1987-2002 for iteration
years <- c(1987, 1992, 1997, 2002)

# Create a list of 4 elements to store the results of the regressions
year.reg <- vector("list",4)

# Run regression for each year using a for loop
year.reg <- NULL
for (i in 1:4) {     # Vector of years is 4 elements long
    year.reg[[i]] <- lm(farmvalue ~ popDens + incomeCapita + prec + 
            prec_square + dday8_32 + dday8_32_square + dday34 + dday34_sqrt +
                c_prec + c_prec_square + c_dday8_32 + c_dday8_32_square +
                c_dday34 + c_dday34_sqrt + pctClay + topKfact + minPerm +
                waterCapacity + bestSoil, data=subset(total_less_yield,
                year==years[i]), na.action = na.omit)
}

year.reg
summary(year.reg[[4]])
write.file(year.reg[[1]], file = "simple_reg_yearly_1987.txt") #
write.file(year.reg[[2]], file = "simple_reg_yearly_1992.txt") ## NO WORKY!!
write.file(year.reg[[3]], file = "simple_reg_yearly_1997.txt") #
write.file(year.reg[[4]], file = "simple_reg_yearly_2002.txt") #


# Question B ###################################################################
# Run F-tests to test whether or not the the following groups of variables
# matter: 1) climate, 2) soil, 3) population density & income per capita

# 2002 subset of data
data_2002 <- subset(total_less_yield, year==2002)

# F-tests
# Equality of variances for population density and per capita income
ftest02_a <- var.test(popDens ~ incomeCapita, data = data_2002)

var.test(data_2002$popDens, data_2002$incomeCapita)     # Check for equal var

var.test(lm(data_2002 ~ 1), lm(data_2002$ ~ 1))


# Question C ###################################################################
# Create vector of climate varibles for use in subset
c_vars <- c("c_prec", "c_prec_square", "c_dday34", "c_dday34_sqrt",
            "c_dday8_32", "c_dday8_32_square")

# 2002 subset of climate data
climate_2002 <- data_2002[c_vars]

# Create summary statistics for climate variables
describe(climate_2002)

# Check for NA's
sum(is.na(climate_2002))    # Prints 0

# Create histograms and boxplots of 2002 climate data using for loop
for(i in 1:6) {
    hist(climate_2002[,i], main=names(climate_2002[i]))
    boxplot(climate_2002[,i], main=names(climate_2002[i]), type = "l")
}


# Question E ###################################################################
# Function takes an integer and returns the first two highest order digits as
# an integer with an unknown number of digits
fips2state <- function(x) {
    # For the number of digits for each fips
    divisor_exp <- (floor(log10(x)) - 1)
    # Divide by 10^(# of digits - 2), drop the decimals
    floor((x) / (10^(divisor_exp)) - 2)
}

# Find out what data type the columns are
apply(total_less_yield, 2, is.vector)   # Returns vectors

# Call apply function to iterate thru fips column in total less yield df, 
# calls fips2state function to evaluate each cell with the fips2state function,
# and creates new column, state, in df
system.time(
    total_less_yield$state <- apply(total_less_yield[,c('fips'),drop = F], 1,
                                    function(x) fips2state(x))
)

# Not sure if I have to, but save the file again after changing it
write.csv(total_less_yield, file = "totalLessYield.csv")

# FINALLY, run state fixed effects regressions for each year using a loop
year.reg_state_fe <- NULL

for (i in 1:4) {     # Vector of years is 4 elements long
    year.reg_state_fe[[i]] <- lm(farmvalue ~ popDens + incomeCapita + prec + 
        prec_square + dday8_32 + dday8_32_square + dday34 + dday34_sqrt +
        c_prec + c_prec_square + c_dday8_32 + c_dday8_32_square +
        c_dday34 + c_dday34_sqrt + pctClay + topKfact + minPerm +
        waterCapacity + bestSoil + factor(state),
        data = subset(total_less_yield, year==years[i]), na.action=na.omit)
}

year.reg_state_fe
summary(year.reg_state_fe[[4]])
str(year.reg_state_fe[[4]])


# Question H ###################################################################
# First merge the profitYield dataframe with the total_less_yield dataframe
total <- merge(total_less_yield, profitYield, by = c("year","fips"))

# Remove duplicate year column
drop <- c("year.y")
total = total[,!(names(total) %in% drop)]

# Remove the weird X column; but, don't know where that came from so save it
x_files <- total$X
drop1 <- c("X")
total = total[,!(names(total) %in% drop1)]

# Rename year.x to year and try not to make this mistake again
colnames(total)[3] <- "year"    # Changing the 3rd column

# Set any blank cells to NA
total[total==""] <- NA

# Check for missing data
sum(is.na(total))    # Prints 3,259

# Find out if NA's are in profit per acre column
colnames(total)[colSums(is.na(total)) > 0]      # Returns cornYield & soyYield

# Test object type
is.data.frame(total)    # TRUE

# Find out what data types are in the dataframe
sapply(total, class)   # Returns integer for year, fips, & X. Numeric for rest

# Save the dataset
write.csv(total, file = "total.csv")

# Random Effects model of profit per acre
panel_re <- plm(profit_per_acre ~ popDens + incomeCapita + prec + prec_square +
        dday8_32 + dday8_32_square + dday34 + dday34_sqrt + pctClay + topKfact +
        minPerm + waterCapacity + bestSoil, data = total,
        index = c("fips", "year"), na.action = na.omit, model = "random")
panel_re
summary(panel_re)

# Error running reg above:
# Error in solve.default(M[therows, therows], quad[therows]) : 
# Lapack routine dgesv: system is exactly singular: U[2,2] = 0

# Cluster-robust standard errors for RE model by county
coeftest(panel_re, vcov = vcovHC(panel_re, cluster = "group"))

# Error in solve.default(crossprod(demX)) : 
# system is computationally singular: reciprocal condition number = 5.58987e-17

# I have no idea what this is, but I played around with it
svd(total)

# Question I ###################################################################
# Run FE model with same dependent and independent variables as the RE model
# above using the same years
panel_fe <- plm(profit_per_acre ~ popDens + incomeCapita + prec + prec_square +
                    dday8_32 + dday8_32_square + dday34 + dday34_sqrt +
                    pctClay + topKfact + minPerm + waterCapacity + bestSoil,
                    index = c("fips", "year"), data = total,
                    na.action = na.omit, model = "within")
panel_fe
summary(panel_fe)

# Not necessary, for posterity
# Convert fips from integer to numeric
as.numeric(total$fips)
# Test type
is.numeric(total$fips)

# Cluster-robust standard errors for RE model by county
coeftest(panel_fe, vcov = vcovHC(panel_fe, cluster = "fips"))

# Question N ###################################################################
# Regress corn & soy yields on weather, soil, population density, and income per
# capita using RE and FE models for each

# Corn Models
# Corn FE model indexed by fips, or county
corn_fe <- plm(corn_yield ~ popDens + incomeCapita + prec + prec_square +
                    dday8_32 + dday8_32_square + dday34 + dday34_sqrt +
                    pctClay + topKfact + minPerm + waterCapacity + bestSoil,
                    data = total, index = c("fips", "year"), 
                    na.action = na.omit, model = "within")
corn_fe
summary(corn_fe)
plot_model(corn_fe, transform = NULL)
# Cluster-robust standard errors for corn FE model by county
coeftest(corn_fe, vcov = vcovHC(corn_fe, cluster = "fips"))

# Corn RE model
corn_re <- plm(corn_yield ~ popDens + incomeCapita + prec + prec_square +
                    dday8_32 + dday8_32_square + dday34 + dday34_sqrt +
                    pctClay + topKfact + minPerm + waterCapacity + bestSoil, 
                    data = total, index = c("fips", "year"),
                    na.action = na.omit, model = "random")
corn_re
summary(corn_re)

# Error in solve.default(crossprod(ZBeta)) : 
# system is computationally singular: reciprocal condition number = 3.41726e-17

# Too many NA's in corn_yield?
# Check for missing data
sum(is.na(total$corn_yield))    # Prints 1,231

# How many observations are there after model excludes NA's?
length(total$corn_yield) - sum(is.na(total$corn_yield))     # Prints 5,703

# Is it a problem with multicolinearity?

# Cluster-robust standard errors for corn RE model by county
coeftest(corn_re, vcov = vcovHC(corn_re, cluster = "fips"))

# Soy Models
# Soy FE model indexed by fips, or county
soy_fe <- plm(soy_yield ~ popDens + incomeCapita + prec + prec_square +
                   dday8_32 + dday8_32_square + dday34 + dday34_sqrt +
                   pctClay + topKfact + minPerm + waterCapacity + bestSoil,
                   data = total, index = c("fips", "year"), na.action = na.omit,
                   model = "within")
soy_fe
summary(soy_fe)

# Cluster-robust standard errors for soy FE model by county
coeftest(soy_fe, vcov = vcovHC(soy_fe, cluster = "fips"))

# Soy RE model
soy_re <- plm(soy_yield ~ popDens + incomeCapita + prec + prec_square +
                   dday8_32 + dday8_32_square + dday34 + dday34_sqrt +
                   pctClay + topKfact + minPerm + waterCapacity + bestSoil, 
                   data = total, index = c("fips", "year"),
                   na.action = na.omit, model = "random")
soy_re
summary(soy_re)

# Error in solve.default(crossprod(ZBeta)) :
# system is computationally singular: reciprocal condition number = 2.43322e-17

# Too many NA's in corn_yield?
# Check for missing data
sum(is.na(total$soy_yield))    # Prints 2,028

# How many observations are there after model excludes NA's?
length(total$soy_yield) - sum(is.na(total$soy_yield))     # Prints 4,908

# Check if total df is balanced
is.pbalanced(total)     # Prints TRUE (But, is that with NA's?)

soy_lm <- lm(soy_yield ~ popDens + incomeCapita + prec + prec_square +
                  dday8_32 + dday8_32_square + dday34 + dday34_sqrt +
                  pctClay + topKfact + minPerm + waterCapacity + bestSoil,
                  na.action = na.omit, data = total)
soy_lm
summary(soy_lm)
# Cluster-robust standard errors for soy RE model by county
coeftest(soy_fe, vcov = vcovHC(soy_fe, cluster = "fips"))


# Question O ###################################################################
# Test whether RE or FE is best using the Hausman Test for soy and corn models
# Corn model Hausman test
phtest(corn_fe, corn_re)

# Soy model Hausman test
phtest(soy_fe, soy_re)



