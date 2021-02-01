# Master Thesis
# Factor models
# v1.0 - basic layout, test of YieldCurve package

rm(list = ls())

# Packages
if (!require("pacman")) install.packages("pacman") # installs pacman package if not installed yet
pacman::p_load(pacman, dplyr, tidyr, ggplot2, scales, Cairo, zoo, xtable, tibble, forecast, naturalsort) # load packages TBD e.g. these, edit when u determine which needed/used..
# p_load(reticulate, tensorflow, keras)

# Workspace
setwd("E:/Google_Drive/Diploma_Thesis/Code")
# load(file = "Workspaces/Data_03_trimmed-small_US.RData") # 0min
load(file = "Workspaces/Data_04_split-small_US_tick_05-2019.RData") # 0min
print("Workspace rdy set go!")


#############################
###### Factor models ########
#############################

# We run the preprocessed data through the factor models (DNS) before training the NNs.
p_load(YieldCurve)

# https://www.rdocumentation.org/packages/YieldCurve/versions/4.1/topics/Nelson.Siegel
# Nelson.Siegel(rate, maturity)


###############################################
###### Example from package YieldCurve ########
###############################################

data(FedYieldCurve) # Fed data set
str(FedYieldCurve) # 8 maturities in columns, 372 months in rows as observations
head(FedYieldCurve)
maturity.Fed <- c(3/12, 1/2, 1, 2, 3, 5, 7, 10)

# first() from xts package can have number of periods in character string, n = 'n period.type', where period.type can be: secs, seconds, mins, minutes, hours, days, weeks, months, quarters, and years
# the first() is not necessary there, it is just to calculate the NS parameters for the first few observations to run faster.
# Nelson.Siegel(rate, maturity) explained:
# - rate is a matrix with i.r.
# - maturity is a vector of maturities of rate (in months) - TBD why does the documentation say in months and the example has it in years?
(NSParameters <- Nelson.Siegel(rate = first(FedYieldCurve, '10 month'), maturity = maturity.Fed))
# NSrates(Coeff, maturity) explained:
# - https://www.rdocumentation.org/packages/YieldCurve/versions/4.1/topics/NSrates
(y <- NSrates(Coeff = NSParameters[5,], maturity = maturity.Fed))

# Plot
plot(maturity.Fed, first(FedYieldCurve, '10 month')[5,], main = "Fitting Nelson-Siegel yield curve", xlab = c("Pillars in months"), type = "o") # original observed data
lines(maturity.Fed, y, col = 2, type = "o") # add NS fitted rates
legend("topleft", legend = c("observed yield curve", "fitted yield curve"),u1)
grid()



####### NOT MY CODE, FUNCTIONS FROM PACKAGE YieldCurve ########
# Testing one by one for understanding
Nelson.Siegel <- function (rate, maturity) 
{
    rate <- try.xts(rate, error = as.matrix)
    if (ncol(rate) == 1) 
        rate <- matrix(as.vector(rate), 1, nrow(rate))
    pillars.number <- length(maturity)
    lambdaValues <- seq(maturity[1], maturity[pillars.number], 
        by = 0.5)
    FinalResults <- matrix(0, nrow(rate), 4)
    colnames(FinalResults) <- c("beta_0", "beta_1", "beta_2", 
        "lambda")
    j <- 1
    while (j <= nrow(rate)) {
        InterResults <- matrix(0, length(lambdaValues), 5)
        colnames(InterResults) <- c("beta0", "beta1", "beta2", 
            "lambda", "SSR")
        for (i in 1:length(lambdaValues)) {
            lambdaTemp <- optimize(.factorBeta2, interval = c(0.001, 
                1), maturity = lambdaValues[i], maximum = TRUE)$maximum
            InterEstimation <- .NS.estimator(as.numeric(rate[j, 
                ]), maturity, lambdaTemp)
            BetaCoef <- InterEstimation$Par
            if (BetaCoef[1] > 0 & BetaCoef[1] < 20) {
                SSR <- sum(InterEstimation$Res^2)
                InterResults[i, ] <- c(BetaCoef, lambdaTemp, 
                  SSR)
            }
            else {
                InterResults[i, ] <- c(BetaCoef, lambdaValues[i], 
                  1e+05)
            }
        }
        BestRow <- which.min(InterResults[, 5])
        FinalResults[j, ] <- InterResults[BestRow, 1:4]
        j <- j + 1
    }
    reclass(FinalResults, rate)
}

NSrates <- function (Coeff, maturity)  # takes in the NS parameters (betas), plugs into the NS equation to calculate the yields
{
    Curve <- xts(matrix(0, nrow(Coeff), length(maturity)), order.by = time(Coeff))
    colnames(Curve) <- make.names(maturity)
    Coeff <- as.matrix(Coeff)
    for (i in 1:nrow(Curve)) {
        # The main NS model equation, y_t(tau) = beta_0 + beta_1 * (exp fraction..) + beta_2 * (exp fractions..)
        Curve[i, ] <- as.numeric(Coeff[i, 1]) * rep(1, length(maturity)) + # beta_0 * 1 (~ vector of length equal to number of maturities)
            as.numeric(Coeff[i, 2]) * as.numeric(.factorBeta1(Coeff[i, 4], maturity)) + # beta_1 * the NS model's fraction after beta_1
            as.numeric(Coeff[i, 3]) * as.numeric(.factorBeta2(Coeff[i, 4], maturity)) # beta_2 * the NS model's fraction after beta_2
    }
    return(Curve)
}

# test one by one:
crv <- xts(matrix(0, 1, 8), order.by = time(NSParameters[5, ]))
make.names(maturity.Fed)
nrow(crv)
crv[1, ]
ceff <- NSParameters[5, ]
ceff[1, 1] # beta_0
rep(1, length(maturity.Fed))
ceff[1, 2] # beta_1
ceff[1, 3] # beta_2
ceff[1, 4] # lambda

.factorBeta1(ceff[1, 4], maturity.Fed) # NS model's fraction after beta_1
.factorBeta2(ceff[1, 4], maturity.Fed) # NS model's fraction after beta_2


.factorBeta1 <- function (lambda, maturity) # maturity is a vector of length = number of maturities, while lambda is a single number for the particular observation
{
    as.numeric((1 - exp(-lambda * maturity))/(lambda * maturity)) # factor loading (fraction) of beta_1
}

.factorBeta2 <- function (lambda, maturity) 
{
    as.numeric((1 - exp(-lambda * maturity))/(lambda * maturity) - exp(-lambda * maturity)) # factor loading (fraction) of beta_2
}



#####################
####### Notes #######
#####################


# Work with: dataFutures_train, probably window the data after the NS, as is the case currently in Model_fit.R

# We need to change our prices data into a different shape, with columns representing maturities.
# What are our maturities? Do we load up the 2, 5, 10, 30Y maturity prices as columns?
# For this we need to go back to Data_conversion.R and load up more datasets.
# Do we then add maturity interpolations?





# TBD:
	# go thru the Nelson.Siegel function source code
	# factor models documentation, find packages inspiration and test out

