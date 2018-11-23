## Justin K. Bosscher
## ECN 576 Mini Research Project
## University of Rhode Island
## December 7, 2017

####### WORKSPACE ##############################################################


# Working Directory ----
setwd("/home/optimus/Documents/School_and_Training/University_of_Rhode_Island/Academics/Economics_Courses/ECN_576_Econometrics/researchProject") 


# Load libraries
library(ggplot2)    # For plotting
library(stargazer)  # For creating summary tables




###### DATA ####################################################################

# Load and manipulate data ----
data <- read.csv("data.csv", na.strings = "..")         # Original data
data1 <- read.csv("data1.csv")                          # Altered data
data1T <- read.csv("data1T.csv")                        # Transformed data

# Print data
dput(data)

# Copy data to new dataframe
data1 <- data

# Convert the character Months column to integer
# Mod by 12 to return an integer for each month: 1-11, 0=12
data1$Month <- as.integer(data1$Month)%%12

# Remove commas from mi_month data
data1$miles <- as.numeric(gsub(",", "", data1$mi_month))

# Find columns that are not numeric or integer types
sapply(data1, class)

# Simplify column names
data1$miles <- data1$mi_month
data1$miles.s <- data1$mi_month_s
data1$pop <- data1$population
data1$jobs <- data1$job_monthly_total

# Create new variable for job search query average, jobs.avg
data1$jobs.avg <- as.numeric(data1$jobs / 7)

# Change scale of variables
# Divide mi_month, mi_month_s, and population by 1,000
data1$miles.S <- data1$mi_month / 1000
data1$miles.sS <- data1$mi_month_s / 1000
data1$pop.S <- data1$population / 1000

# Find the missing values
which(! complete.cases(data1))  # Prints 165

# Delete row 165
data1 <- data1[-165,]

# Remove Month.1 column from data1
data1 <- subset(data1, select = -c(4))

# Save altered data to data1 dataframe
write.csv(data1, file = "data1.csv")

# Transform the data in data1 and create new dataframe, data1T
data1T <- sqrt(data1)

# Save transformed data to data1T dataframe
write.csv(data1T, file = "data1T.csv")


### Examine data ###

# Summaries of data
summary(data1)
summary(data1T)

# Correlations of jobs with miles, miles.s, and miles.sS
jobs.miles <- subset(data1, select=c("jobs", "miles"))
cor(jobs.miles)             # -0.2943
jobs.miles.s <- subset(data1, select=c("jobs", "miles.s"))
cor(jobs.miles.s)           # -0.8175
jobs.miles.sS <- subset(data1, select=c("jobs", "miles.sS"))
cor(jobs.miles.sS)          # -0.8175, naturally




###### KEY #####################################################################

# Nomenclature for regressions =================================================
# swStc
# where:
# s -> seasonally adjusted miles driven data
# w -> weather variables included
# S -> scaled miles and population data
# t -> transformed data; all dependent var's and independent var
# c -> constant used in regression
#===============================================================================

###### REGRESSIONS #############################################################

# Regressions: yearly models ===================================================
# NO seasonal adjustment, NO weather, NO scale, NO transform; constant =========

# Create vector containing years evaluated, 2004-2017 for use in reg for loop
years <- c(2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,
           2015, 2016, 2017)

year.reg <- NULL
for (i in 1:14) {     # Vector of years is 14 elements long
    year.reg[[i]] <- lm(miles ~ population + conv_gas + reform_gas +
                        num2_diesel + job_monthly_total,
                        data=subset(data1, Year==years[i]))
}
year.reg
# How do I print all of these summaries?


#### Regressions Using Years 2004-2017 ####

# Regression: c model ==========================================================
# NO: seasonal adjustment, weather, scale, transform; constant =================

reg.c <- lm(miles ~ pop + conv_gas + reform_gas + num2_diesel + jobs,
            data=data1)
reg.c
sum.c <- summary(reg.c)
print(sum.c)
data1$yhat.c <- fitted(reg.c)
data1$ehat.c <- resid(reg.c)

# Plot: Residuals v Jobs; c model
plot.rVjobs.c <- ggplot(data1, aes(x = data1$jobs, y = ehat.c)) +
                    geom_point() +
                    ggtitle("Residuals vs Jobs") +
                    xlab("Jobs") + ylab("Residuals")
print(plot.rVjobs.c)

# Plot: Residuals v Actuals; c model
plot.rVa.c <- ggplot(data1, aes(x = data1$miles, y = yhat.c)) +
                    geom_point() +
                    ggtitle("Actual vs Fitted") +
                    xlab("Actuals") + ylab("Fitted")
print(plot.rVa.c)

# Plot: Residuals v Population; c model
plot.rVpop.c <- ggplot(data1, aes(x = data1$pop, y = ehat.c)) +
                       geom_point() +
                       ggtitle("Residuals vs Population") +
                       xlab("Population") + ylab("Residuals")
print(plot.rVpop.c)


# Regression: sc model =========================================================
# seasonal adjustment, NO weather, NO scale, NO transform, constant ============

reg.sc <- lm(miles.s ~ pop + conv_gas + reform_gas + num2_diesel + jobs,
             data=data1)
reg.sc
sum.sc <- summary(reg.sc)
print(sum.sc)
ehat.sc <- resid(reg.sc)
yhat.sc <- fitted(reg.sc)

# Plot: Residuals v Population; sc model
plot.rVpop.sc <- ggplot(data1, aes(x = data1$pop, y = ehat.sc)) +
                        geom_point() +
                        ggtitle("Residuals vs Population") +
                        xlab("Population") + ylab("Residuals")
print(plot.rVpop.sc)

# Plot:Residuals v Actuals; sc model
plot.rVa.sc <- ggplot(data = data1, aes(x = miles.s, y = yhat.sc)) +
                    geom_point() + 
                    geom_point(aes(y = yhat.sc), shape = 1) +
                    ggtitle("Fitted vs Actuals") +
                    xlab("Actuals") + ylab("Fitted")
print(plot.rVa.sc)


# Regression: sSc model ========================================================
# seasonally adjusted, scaled, NO weather, NO transform, constant =========

reg.sSc <- lm(miles.sS ~ pop.S + conv_gas + reform_gas + num2_diesel + jobs,
             data=data1)
reg.sSc
sum.sSc <- summary(reg.sSc)
print(sum.sSc)
ehat.sSc <- resid(reg.sSc)
yhat.sSc <- fitted(reg.sSc)

# Plot: Residuals v Population; sSc model
plot.rVpop.sSc <- ggplot(data1, aes(x = data1$pop.S, y = ehat.sSc)) +
                        geom_point() +
                        ggtitle("Residuals vs Population") +
                        xlab("Population") + ylab("Residuals")
print(plot.rVpop.sSc)

# Plot: Residuals v Population; sSc model
plot.rVjob.sSc <- ggplot(data1, aes(x = data1$jobs, y = ehat.sSc)) +
                        geom_point() +
                        ggtitle("Residuals vs Jobs") +
                        xlab("Jobs") + ylab("Residuals")
print(plot.rVjob.sSc)

# Plot:Residuals v Actuals; sSc model
plot.rVa.sSc <- ggplot(data = data1, aes(x = miles.sS, y = yhat.sSc)) +
                    geom_point() + 
                    geom_point(aes(y = yhat.sSc), shape = 1) +
                    ggtitle("Fitted vs Actuals") +
                    xlab("Actuals") + ylab("Fitted")
print(plot.rVa.sSc)


# Regression: swc model ========================================================
# seasonal adjust, NO scale, weather, NO transform, constant ===================

reg.swc <- lm(miles.s ~ pop + conv_gas + reform_gas + num2_diesel + jobs + 
              very_wet + very_dry + very_cold + very_warm, 
              data=data1, na.action = na.omit)
reg.swc
sum.swc <- summary(reg.swc)
print(sum.swc)
ehat.swc <- resid(reg.swc)
yhat.swc <- fitted(reg.swc)

# Plot: Residuals v Population; swc model
plot.rVpop.swc <- ggplot(data1, aes(x = data1$pop, y = ehat.swc)) +
                         geom_point() +
                         ggtitle("Residuals vs Population") +
                         xlab("Population") + ylab("Residuals")
print(plot.rVpop.swc)

# Plot:Residuals v Actuals; swc model
plot.rVa.swc <- ggplot(data = data1, aes(x = miles.s, y = yhat.swc)) +
                    geom_point() + 
                    geom_point(aes(y = yhat.swc), shape = 1) +
                    ggtitle("Fitted vs Actuals") +
                    xlab("Actuals") + ylab("Fitted")
print(plot.rVa.swc)


# Regression: swSc model =======================================================
# seasonally adjusted, weather, scale, NO transform, constant ==================

reg.swSc <- lm(miles.sS ~ pop.S + conv_gas + reform_gas + num2_diesel + jobs +
                   very_wet + very_dry + very_cold + very_warm,
                   data = data1, na.action = na.omit)
reg.swSc
sum.swSc <- summary(reg.swSc)
print(sum.swSc)
data1$ehat.swSc <- resid(reg.swSc)
data1$yhat.swSc <- fitted(reg.swSc)

# Plot: Residuals v Population; swSc model
plot.rVpop.swSc <- ggplot(data = data1, aes(x = pop.S, y = ehat.swSc)) +
                        geom_point() +
                        geom_point(aes(y = ehat.swSc), shape = 1) +
                        ggtitle("Residuals vs Population") +
                        xlab("Population") + ylab("Residuals")
print(plot.rVpop.swSc)

# Plot Jobs v Resisduals
plot.rVj.swSc <- ggplot(data = data1, aes(x = jobs, y = ehat.swSc)) +
                     geom_point() +
                     geom_point(aes(y = ehat.swSc), shape = 1) +
                     ggtitle("Residuals vs Job Searches") +
                     xlab("Job Searches") + ylab("Residuals")
print(plot.rVj.swSc)

# Plot:Residuals v Actuals; swSc model
plot.rVa.swSc <- ggplot(data = data1, aes(x = miles.sS, y = yhat.swSc)) +
                    geom_point() + 
                    geom_point(aes(y = yhat.swSc), shape = 1) +
                    ggtitle("Fitted vs Actuals") +
                    xlab("Actuals") + ylab("Fitted")
print(plot.rVa.swSc)



# Regression: swSc.mod.wf model ================================================
# seasonal adjust, mod weather, mod fuel, scaled, constant, NO transform =======

# This model is a modification of the swSc spec; it removes reformed gas, as
# well as, the very_wet and very_dry variables; none were sig. at 99% level

reg.swSc.mod.wf <- lm(miles.sS ~ pop.S + conv_gas + num2_diesel + jobs +
                          very_cold + very_warm,
                          data = data1, na.action = na.omit)
reg.swSc.mod.wf
sum.swSc.mod.wf <- summary(reg.swSc.mod.wf)
print(sum.swSc.mod.wf)
data1$ehat.swSc.mod.wf <- resid(reg.swSc.mod.wf)
data1$yhat.swSc.mod.wf <- fitted(reg.swSc.mod.wf)
summary(reg.swSc.mod.wf)$fstatistic

# Plot: Residuals v Population; swSc.mod.wf model
plot.rVpop.swSc.mod.wf <- ggplot(data1, aes(x = data1$pop.S,
                                             y = ehat.swSc.mod.wf)) +
                            geom_point() +
                            ggtitle("Residuals vs Population") +
                            xlab("Population") + ylab("Residuals")
print(plot.rVpop.swSc.mod.wf)


# Regression: swSc.mod.f model =================================================
# seasonal adjust, mod fuel, scaled, constant, NO transform =======

# This model is a modification of the swSc spec. it excludes reform_gas which
# was not significant at the 99% level

reg.swSc.mod.f <- lm(miles.sS ~ pop.S + conv_gas + num2_diesel + jobs +
                          very_wet + very_dry + very_cold + very_warm,
                          data = data1, na.action = na.omit)
reg.swSc.mod.f
sum.swSc.mod.f <- summary(reg.swSc.mod.f)
print(sum.swSc.mod.f)
data1$ehat.swSc.mod.f <- resid(reg.swSc.mod.f)
data1$yhat.swSc.mod.f <- fitted(reg.swSc.mod.f)
summary(reg.swSc.mod.f)$fstatistic

# Plot: Residuals v Population; swSc.mod.f model
plot.rVpop.swSc.mod.f <- ggplot(data1, aes(x = data1$pop.S,
                                             y = ehat.swSc.mod.f)) +
                            geom_point() +
                            ggtitle("Residuals vs Population") +
                            xlab("Population") + ylab("Residuals")
print(plot.rVpop.swSc.mod.f)

# Regression: swSc.mod2.wf =====================================================
# seasonal adjust, mod weather, mod fuel, scaled, constant, NO transform =======

# This model is a modification of the swSc spec; it removes reformed gas, the
# very_dry, and very_warm variables;

reg.swSc.mod2.wf <- lm(miles.sS ~ pop.S + conv_gas + num2_diesel + jobs +
                           very_wet + very_cold,
                           data = data1, na.action = na.omit)
reg.swSc.mod2.wf
sum.swSc.mod2.wf <- summary(reg.swSc.mod2.wf)
print(sum.swSc.mod2.wf)
data1$ehat.swSc.mod2.wf <- resid(reg.swSc.mod2.wf)
data1$yhat.swSc.mod2.wf <- fitted(reg.swSc.mod2.wf)
summary(reg.swSc.mod2.wf)$fstatistic

# Plot: Residuals v Population; swSc.mod2.wf model
plot.rVpop.swSc.mod2.wf <- ggplot(data1, aes(x = data1$pop.S,
                                             y = ehat.swSc.mod2.wf)) +
                                geom_point() +
                                ggtitle("Residuals vs Population") +
                                xlab("Population") + ylab("Residuals")
print(plot.rVpop.swSc.mod2.wf)


# Regression: swSc.mod3.wf =====================================================
# seasonal adjust, mod weather, mod fuel, scaled, constant, NO transform =======

# This model is a modification of the swSc spec; it removes reformed gas, the
# very_dry, and very_warm variables;

reg.swSc.mod3.wf <- lm(miles.sS ~ pop.S + conv_gas + num2_diesel + jobs +
                           very_cold,
                           data = data1, na.action = na.omit)
reg.swSc.mod3.wf
sum.swSc.mod3.wf <- summary(reg.swSc.mod3.wf)
print(sum.swSc.mod3.wf)
data1$ehat.swSc.mod3.wf <- resid(reg.swSc.mod3.wf)
data1$yhat.swSc.mod3.wf <- fitted(reg.swSc.mod3.wf)
summary(reg.swSc.mod3.wf)$fstatistic

# Plot: Residuals v Population; swSc.mod.wf model
plot.rVpop.swSc.mod3.wf <- ggplot(data1, aes(x = data1$pop.S,
                                             y = ehat.swSc.mod3.wf)) +
                                geom_point() +
                                ggtitle("Residuals vs Population") +
                                xlab("Population") + ylab("Residuals")
print(plot.rVpop.swSc.mod3.wf)


# Regression: swSc.mod4.wf =====================================================
# seasonal adjust, mod weather, mod fuel, scaled, constant, NO transform =======

# This model is a modification of the swSc spec; it removes reformed gas, the
# very_dry, and very_wet variables;

reg.swSc.mod4.wf <- lm(miles.sS ~ pop.S + conv_gas + num2_diesel + jobs +
                           very_cold + very_warm,
                       data = data1, na.action = na.omit)
reg.swSc.mod4.wf
sum.swSc.mod4.wf <- summary(reg.swSc.mod4.wf)
print(sum.swSc.mod4.wf)
data1$ehat.swSc.mod4.wf <- resid(reg.swSc.mod4.wf)
data1$yhat.swSc.mod4.wf <- fitted(reg.swSc.mod4.wf)
summary(reg.swSc.mod4.wf)$fstatistic

# Plot: Residuals v Population; swSc.mod.wf model
plot.rVpop.swSc.mod4.wf <- ggplot(data1, aes(x = data1$pop.S,
                                             y = ehat.swSc.mod4.wf)) +
    geom_point() +
    ggtitle("Residuals vs Population") +
    xlab("Population") + ylab("Residuals")
print(plot.rVpop.swSc.mod4.wf)

# To write equation on plot
equation = function(x) {
    lm_coef <- list(a = round(coef(x)[1], digits = 1),
                    b = round(coef(x)[2], digits = 1),
                    c = round(coef(x)[3], digits = 1),
                    d = round(coef(x)[4], digits = 1),
                    e = round(coef(x)[5], digits = 1),
                    f = round(coef(x)[6], digits = 1),
                    f = round(coef(x)[7], digits = 1),
                    r2 = round(summary(x)$r.squared, digits = 2));
    lm_eq <- substitute(italic(y) == a + b ~italic(x) + c ~italic(x) d ~italic(x) e ~italic(x) f ~italic(x) + g ~italic(x), lm_coef)
    as.character(as.expression(lm_eq));                 
}


# Plot: Residual v Population; swStc.mod4.wf model
plot.aVf.swSc.mod4.wf <- ggplot(data=data1, aes(x=miles.sS, y=yhat.swSc.mod4.wf)) +
                            ggtitle("Miles Driven OLS ",
                                    subtitle="Preferred Model (4)") +
                            geom_tile(fill="transparent", colour="black") +
                            geom_point(aes(x=yhat.swSc.mod4.wf, y=miles.sS,
                                           colour=jobs)) +
                            labs(color = " # of Jobs \n Searches") +
                            geom_smooth(method="lm", colour="#660000") +
                            scale_x_continuous(name="Predicted Miles (billions)") +
                            scale_y_continuous(name="Actual Miles (billions)") +
                            annotate("text", x=266, y=247,
                                     label="italic(R)^2 == 0.849", parse=T) + 
                            annotate("text", x=266, y=245,
                                     label="italic(alpha) == 184.43", parse=T) +
                            annotate("text", x=266, y=243,
                                     label="italic(beta) == 0.857" , parse=T)
print(plot.aVf.swSc.mod4.wf)
ggsave(filename="preferredPlot-swSc-mod4-wf.jpeg", plot=plot.aVf.swSc.mod4.wf)

# Find slope of line of best fit for display on plot
(max(data1$yhat.swSc.mod4.wf) - min(data1$yhat.swSc.mod4.wf)) / ((max(data1$miles.sS)) - min(data1$miles.sS))


# Regression: swSc.p2.mod3.wf ==================================================
# seas adjust, mod weather, mod fuel, scaled, const, pop^2, NO transform =======

# This model is a modification of the swSc.mod3.wf spec; it squares population
# in an attempt to reduce heteroskadsticity

reg.swSc.p2.mod3.wf <- lm(miles.sS ~ (pop.S)^2 + conv_gas + num2_diesel + jobs +
                          very_cold,
                          data = data1, na.action = na.omit)
reg.swSc.p2.mod3.wf
sum.reg.swSc.p2.mod3.wf <- summary(reg.swSc.p2.mod3.wf)
print(sum.swSc.mod3.wf)
data1$ehat.reg.swSc.p2.mod3.wf <- resid(reg.swSc.p2.mod3.wf)
data1$yhat.reg.swSc.p2.mod3.wf <- fitted(reg.swSc.p2.mod3.wf)
summary(reg.swSc.p2.mod3.wf)$fstatistic

# Plot: Residuals v Population; swSc.mod.wf model
plot.reg.swSc.p2.mod3.wf <- ggplot(data1, aes(x = (data1$pop.S),
                                             y = ehat.reg.swSc.p2.mod3.wf)) +
                                geom_point() +
                                ggtitle("Residuals vs Population") +
                                xlab("Population") + ylab("Residuals")
print(plot.reg.swSc.p2.mod3.wf)

# Plot: Residuals v Actuals; swSc.mod.p2.wf model
plot.reg.swSc.p2.mod3.wf <- ggplot(data1, aes(x = (data1$pop.S)^2,
                                             y = ehat.reg.swSc.p2.mod3.wf)) +
                                geom_point() +
                                ggtitle("Residuals vs Population") +
                                xlab("Population") + ylab("Residuals")
print(plot.reg.swSc.p2.mod3.wf)


# Regression: wSt model ========================================================
# NO seasonal adjustment, weather, scaled, transformed, NO constant ============

reg.wSt <- lm(I(T * miles.S)~I(T * pop.S) + I(T * conv_gas) +
                  I(T * reform_gas) + I(T * num2_diesel) -1 + I(T * jobs) +
                  I(T * very_wet) + I(T * very_dry) + I(T * very_cold) +
                  I(T * very_warm),
                  data=data1T)
reg.wSt               
sum.wSt <- summary(reg.wSt)               
print(sum.wSt)
data1T$yhat.wSt <- fitted(reg.wSt)
data1T$ehat.wSt <- resid(reg.wSt)

# Plot: Residuals v Population; wSt model
plot.rVpop.wSt <- ggplot(data = data1T,  aes(x = pop.S, y = ehat.wSt)) +
                      geom_point() +
                      geom_point(aes(y = ehat.wSt), shape = 1) +
                      ggtitle("Residuals vs Population") +
                      xlab("Population") + ylab("Residuals")
print(plot.rVpop.wSt)

# Plot: Actual v Fitted; wSt model
plot.aVf.wSt <- ggplot(data = data1T,  aes(x = miles.S, y = yhat.wSt)) +
                    geom_point() +
                    geom_point(aes(y = yhat_reg.swt), shape = 1) +
                    ggtitle("Acutals vs Fitted") +
                    xlab("Miles") + ylab("Fitted")
print(plot.aVf.wSt)


# Regression: swSt model =======================================================
# seasonally adjusted, weather, scaled, transformed, NO constant ===============

reg.swSt <- lm(I(T * miles.sS)~I(T * pop.S) + I(T * conv_gas) + 
               I(T * reform_gas) + I(T * num2_diesel) + I(T * jobs) 
               + I(T * very_wet) + I(T * very_dry) + I(T * very_cold) +
               I(T * very_warm) - 1,
               data=data1T)
reg.swSt
sum.swSt <- summary(reg.swSt)
print(sum.swSt)
data1T$ehat.swSt <- resid(reg.swSt)
data1T$yhat.swSt <- fitted(reg.swSt)

# Plot: Residual v Population; swSt model
plot.rVpop.swSt <- ggplot(data = data1T, aes(x = pop.S, y = ehat.swSt)) +
                        geom_point() +
                        geom_point(aes(y = ehat.swSt), shape = 1) +
                        ggtitle("Residuals vs Population") +
                        xlab("Population") + ylab("Residuals")
print(plot.rVpop.swSt)

# Plot: Fitted v Actual; swSt model
plot.fVa.swSt <- ggplot(data = data1T,  aes(x = miles.sS, y = yhat.swSt)) +
                    geom_point() +
                    geom_point(aes(y = yhat.swSt), shape = 1) +
                    ggtitle("Fitted vs Actual") +
                    xlab("Actual") + ylab("Fitted")
print(plot.fVa.swSt)


# Regression: swStc model ======================================================
# seasonally adjusted, weather, scaled, transformed, constant ==================

reg.swStc <- lm(I(T * miles.sS)~I(T * pop.S) + I(T * conv_gas) + 
                    I(T * reform_gas) + I(T * num2_diesel) +
                    I(T * jobs) + I(T * very_wet) + I(T * very_dry) +
                    I(T * very_cold) + I(T * very_warm),
                    data=data1T)
reg.swStc
sum.swStc <- summary(reg.swStc)
print(sum.swStc)
data1T$ehat.swStc <- resid(reg.swStc)
data1T$yhat.swStc <- fitted(reg.swStc)

# Plot: Residual v Population; swSt model
plot.rVpop.swStc <- ggplot(data = data1T, aes(x = pop.S, y = ehat.swStc)) +
                          geom_point() +
                          geom_point(aes(y = ehat.swStc), shape = 1) +
                          ggtitle("Residuals vs Population") +
                          xlab("Population") + ylab("Residuals")
print(plot.rVpop.swStc)

# Plot: Fitted v Actual; swSt model
plot.fVa.swStc <- ggplot(data = data1T,  aes(x = miles.sS, y = yhat.swStc)) +
                        geom_point() +
                        geom_point(aes(y = yhat.swStc), shape = 1) +
                        ggtitle("Fitted vs Actual") +
                        xlab("Actual") + ylab("Fitted")
print(plot.fVa.swStc)


# Regression: swSct.mod3.wf ====================================================
# seasonal adjust, mod weather, mod fuel, scaled, transform, constant ==========

# This model is a modification of the swStc spec; it removes reformed gas, the
# very_dry, very_wet, and very_warm variables;

reg.swStc.mod3.wf <-  lm(I(T * miles.sS)~I(T * pop.S) + I(T * conv_gas) + 
                           I(T * num2_diesel) + I(T * jobs) + I(T * very_cold),
                           data=data1T)
reg.swStc.mod3.wf
sum.swStc.mod3.wf <- summary(reg.swStc.mod3.wf)
print(sum.swStc.mod3.wf)
data1T$ehat.swStc.mod3.wf <- resid(reg.swStc.mod3.wf)
data1T$yhat.swStc.mod3.wf <- fitted(reg.swStc.mod3.wf)
summary(reg.swStc.mod3.wf)$fstatistic

# Plot: Residual v Population; swStc.mod3.wf model
plot.rVpop.swStc.mod3.wf <- ggplot(data = data1T, aes(x = pop.S,
                                    y = ehat.swStc.mod3.wf)) +
                                    geom_point() +
                                    ggtitle("Residuals vs Population") +
                                    xlab("Population") + ylab("Residuals")
print(plot.rVpop.swStc.mod3.wf)


# Regression: swSct.mod4.wf ====================================================
# seasonal adjust, mod weather, mod fuel, scaled, transform, constant ==========

# This model is a modification of the swStc spec; it removes reformed gas, the
# very_dry and very_wet variables;

reg.swStc.mod4.wf <-  lm(I(T * miles.sS)~I(T * pop.S) + I(T * conv_gas) + 
                        I(T * num2_diesel) + I(T * jobs)
                        + I(T * very_cold) + I(T * very_warm),
                        data=data1T)
reg.swStc.mod4.wf
sum.swStc.mod4.wf <- summary(reg.swStc.mod4.wf)
print(sum.swStc.mod4.wf)
data1T$ehat.swStc.mod4.wf <- resid(reg.swStc.mod4.wf)
data1T$yhat.swStc.mod4.wf <- fitted(reg.swStc.mod4.wf)
summary(reg.swStc.mod4.wf)$fstatistic

# Plot: Residual v Population; swStc.mod4.wf model
plot.rVpop.swStc.mod4.wf <- ggplot(data = data1T, aes(x = pop.S,
                                                      y = ehat.swStc.mod4.wf)) +
                                geom_point() +
                                ggtitle("Residuals vs Population") +
                                xlab("Population") + ylab("Residuals")
print(plot.rVpop.swStc.mod4.wf)


# Regression: wtc model ========================================================
# NO seasonal adjustment, weather, NO scale, transform, constant ===============

reg.wtc <- lm(I(T * miles)~I(T * pop) + I(T * conv_gas) + 
                  I(T * reform_gas) + I(T * num2_diesel) +
                  I(T * jobs) + I(T * very_wet) + I(T * very_dry) +
                  I(T * very_cold) + I(T * very_warm),
                  data=data1T)
reg.wtc               
sum.wtc <- summary(reg.wtc)
print(sum.wtc)
data1T$yhat.wtc <- fitted(reg.wtc)
data1T$ehat.wtc <- resid(reg.wtc)

# Plot: Fitted v Actual; wtc model
plot.fVa.wtc <- ggplot(data = data1T,  aes(x = miles, y = yhat.wtc)) +
                    geom_point() +
                    geom_point(aes(y = yhat.wtc), shape = 1) +
                    ggtitle("Fitted vs Actual") +
                    xlab("Actual") + ylab("Fitted")
print(plot.fVa.wtc)


# Regression: sSt model ========================================================
# seasonally adjusted, NO weather, scaled, transformed, NO constant ============

reg.sSt <- lm(I(T * miles.sS)~I(T * pop.S) + I(T * conv_gas) +
                  I(T * reform_gas) + I(T * num2_diesel) + I(T * jobs) - 1,
                  data=data1T)
reg.sSt               
sum.sSt <- summary(reg.sSt)
print(sum.sSt)
data1T$ehat.sSt <- resid(reg.sSt)
data1T$yhat.sSt <- fitted(reg.sSt)

# Plot: Fitted v Actual; sSt model
plot.fVa.sSt <- ggplot(data = data1T,  aes(x = miles.sS, y = yhat.sSt)) +
                    geom_point() +
                    geom_point(aes(y = yhat.sSt), shape = 1) +
                    ggtitle("Fitted vs Actual") +
                    xlab("Actual") + ylab("Fitted")
print(plot.fVa.sSt)


# Regression: sStc model  ======================================================
# seasonally adjusted, NO weather, scaled, transformed, constant =============

reg.sStc <- lm(I(T * miles.sS)~I(T * pop.S) + I(T * conv_gas) + 
                  I(T * reform_gas) + I(T * num2_diesel) + I(T * jobs),
                  data=data1T)
reg.sStc
sum.sStc <- summary(reg.sStc)
print(sum.sStc)
data1T$ehat.sStc <- resid(reg.sStc)
data1T$yhat.sStc <- fitted(reg.sStc)

# Plot: Actual v Fitted; sStc model
plot.aVf.sStc <- ggplot(data = data1T,  aes(x = miles.sS, y = yhat.sStc)) +
                    geom_point() +
                    geom_point(aes(y = yhat.sStc), shape = 1) +
                    ggtitle("Fitted vs Actual") +
                    xlab("Actual") + ylab("Fitted")
print(plot.aVf.sStc)



##### TABLES ###################################################################

# Aggregate all model stats to tables ==========================================

# Table of selected model summaries
stargazer(reg.sSc, reg.swSc, reg.sStc, reg.swStc, title="Table 1: OLS results",
          type="html",
          dep.var.caption="Miles Driven",
          align=TRUE,
          no.space=TRUE,
          dep.var.labels.include=FALSE,
          column.labels=c("sSc", "swSc", "sStc", "swStc"),
          covariate.labels=c("Intercept", "Population", "Conv Gas",
                             "Reform Gas","Diesel", "Job Searches", "Very Wet",
                             "Very Dry", "Very Cold", "Very Warm",
                             "~Population", "~Conv Gas", "~Reform Gas",
                             "~Diesel", "~Job Searches", "~Very Wet",
                             "~Very Dry", "~Very Cold", "~Very Warm"),
          model.names=FALSE,  digits=4, initial.zero=FALSE, intercept.bottom=FALSE,
          single.row=TRUE, ci=TRUE, ci.level=0.99,
          notes="c = constant, s = seasonally adjusted, w = weather, S = scaled, t = transformed; ~ transformed independent variable; p<0.1; p<0.05; p<0.01",
          notes.align="r",
          notes.append=FALSE,
          out="selectedModels.html")


# Table of modified models summaries
stargazer(reg.swSc.mod.f,reg.swSc.mod.wf, reg.swSc.mod2.wf, reg.swSc.mod3.wf,
          reg.swStc.mod3.wf, title="Table 1: OLS results",
          type="html",
          dep.var.caption="Miles Driven",
          align=TRUE,
          no.space=TRUE,
          dep.var.labels.include=FALSE,
          column.labels=c("swSc.mod.f", "swSc.mod.wf", "swSc.mod2.wf",
                          "swSc.mod3.wf", "swStc.mod3.wf"),
          covariate.labels=c("Intercept", "Population", "Conv Gas", "Reform Gas","Diesel", 
                             "Job Searches", "Very Wet","Very Dry", "Very Cold",
                             "Very Warm", "~Population", "~Conv Gas",
                             "~Reform Gas", "~Diesel", "~Job Searches",
                             "~Very Wet", "~Very Dry", "~Very Cold", 
                             "~Very Warm"),
          model.names=FALSE,  digits=4, initial.zero=FALSE, intercept.bottom=FALSE,
          single.row=TRUE, ci=TRUE, ci.level=0.99,
          notes="c = constant, s = seasonally adjusted, w = weather, S = scaled,t = transformed; ~ transformed independent variable; p<0.1; p<0.05; p<0.01",
          notes.align="r",
          notes.append=FALSE,
          out="modifiedModels.html")


# Table of selected model summaries including the modified models
stargazer(reg.sSc, reg.swSc, reg.swSc.mod3.wf, reg.swSc.mod4.wf,
          reg.swStc.mod3.wf, reg.swStc.mod4.wf, title="Table 1: OLS results",
          type="html",
          dep.var.caption="Miles Driven",
          align=TRUE,
          no.space=TRUE,
          dep.var.labels.include=FALSE,
          column.labels=c("sSc", "swSc", "swSc.mod3.wf", "swSc.mod4.wf",
                          "swStc.mod3.wf", "swStc.mod4.wf"),
          covariate.labels=c("Intercept", "Population", "Conv Gas", "Reform Gas","Diesel", 
                             "Job Searches", "Very Wet","Very Dry", "Very Cold",
                             "Very Warm", "~Population", "~Conv Gas",
                             "~Diesel", "~Job Searches", "~Very Cold", 
                             "~Very Warm"),
          model.names=FALSE,  digits=4, initial.zero=FALSE, intercept.bottom=FALSE,
          single.row=TRUE, ci=TRUE, ci.level=0.99,
          notes="c = constant, s = seasonally adjusted, w = weather, f = fuel, S = scaled, t = transformed; ~ transformed independent variable; * p<0.1; ** p<0.05; *** p<0.01",
          notes.align="l",
          notes.append=FALSE,
          out="selectedModels1.html")


# Table of descriptive stats on data1
stargazer(data1, type="html", title="Descriptive Statistics", digits=1,
          out="descriptive_data1.html")


# Table of descriptive stats on data1T
stargazer(data1, type="html", title="Descriptive Statistics", digits=1,
          out="descriptive_data1T.html")


# Put all job slopes from scaled models into vector, jobsB =====================
jobsB <- c(reg$coefficients[6], reg.s$coefficients[6], reg.sw$coefficients[6],
                 reg.swS$coefficients[6], reg.swt$coefficients[6], 
                 reg.swSt$coefficients[5], reg.sSt$coefficients[5])



0.0797/mean(data1T$miles.sS)

2.5272/mean(data1$miles.sS)







