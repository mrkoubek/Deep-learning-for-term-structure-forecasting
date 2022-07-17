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

# Returns the estimated coefficients of the Nelson-Siegel's model.
# https://www.rdocumentation.org/packages/YieldCurve/versions/4.1/topics/Nelson.Siegel
# Nelson.Siegel(rate, maturity)


###############################################
################ My real data #################
###############################################

str(yield_curve) # 4 maturities in columns, 372 months in rows as observations
head(yield_curve)
maturity_bonds <- c(2, 5, 10, 30)


###############################################
###### Example from package YieldCurve ########
###############################################

data(FedYieldCurve) # Fed data set
str(FedYieldCurve) # 8 maturities in columns, 372 months in rows as observations
head(FedYieldCurve)
maturity.Fed <- c(3/12, 1/2, 1, 2, 3, 5, 7, 10)

# first() from xts package can have number of periods in character string, n = 'n period.type', where period.type can be: secs, seconds, mins, minutes, hours, days, weeks, months, quarters, and years
# the first() is not necessary there, it is just to calculate the NS parameters only for the first few (10 months, so 10 observations for these monthly data) observations (instead of the whole Fed 1981-2012 dataset), to run faster for the example (only 10 months of data).
str(first(FedYieldCurve, '10 months'))
first(FedYieldCurve, '10 months') # the data the example uses, 10 obresvations

# Nelson.Siegel(rate, maturity) explained:
# - rate is a matrix with i.r.
# - maturity is a vector of maturities of rate (in months) - TBD why does the documentation say in months and the example has it in years?
# And the YieldCurve authors' example of '10 month' seems to work for the xts first() function, as well as the more correct plural syntax version '10 months'.
(NSParameters <- Nelson.Siegel(rate = first(FedYieldCurve, '10 month'), maturity = maturity.Fed))
# NSrates(Coeff, maturity), returns the interest rates by Nelson-Siegel's model:
# - https://www.rdocumentation.org/packages/YieldCurve/versions/4.1/topics/NSrates
(y <- NSrates(Coeff = NSParameters[5,], maturity = maturity.Fed))

# Plot
plot(maturity.Fed, first(FedYieldCurve, '10 month')[5,], main = "Fitting Nelson-Siegel yield curve", xlab = c("Pillars in months"), type = "o") # original observed data
lines(maturity.Fed, y, col = 2, type = "o") # add NS fitted rates
legend("topleft", legend = c("observed yield curve", "fitted yield curve"), col=c(1,2), lty=1)
grid()


(lambdaValues <- seq(maturity.Fed[1], maturity.Fed[length(maturity.Fed)], by = 0.5))
length(lambdaValues)
(InterResults <- matrix(0, length(lambdaValues), 5)) # matrix(data, nrow, ncol)



####### NOT MY CODE, FUNCTIONS FROM PACKAGE YieldCurve ########
# Testing one by one for understanding
# Initialise the function's variables
rate <- FedYieldCurve
maturity <- maturity.Fed

# My data
rate <- 
maturity <- maturity_bonds

# Run the inside of the function row by row
Nelson.Siegel <- function (rate, maturity) 
{
    rate <- try.xts(rate, error = as.matrix) # converts the data into xts, idk how the error works tho
    str(rate)
    if (ncol(rate) == 1) # one maturity column
        rate <- matrix(as.vector(rate), 1, nrow(rate)) # makes a matrix of 1xN, matrix(data, nrow = 1, ncol = number of original xts rows/observations)
    (pillars.number <- length(maturity)) # number of maturities
    (lambdaValues <- seq(maturity[1], maturity[pillars.number], by = 0.5)) # sequence from smallest maturity, by 0.5 increments, to largest, TBD weird
    FinalResults <- matrix(0, nrow(rate), 4) # prepare an empty matrix of 0s, ncol = 4 for 4 coefficients (3 betas and 1 lambda), nrow # of observations
    colnames(FinalResults) <- c("beta_0", "beta_1", "beta_2", "lambda")
    str(FinalResults)
    head(FinalResults)

    j <- 1
    while (j <= nrow(rate)) { # loop thru all the observations (j-th observation)
        InterResults <- matrix(0, length(lambdaValues), 5) # (re)initialise an empty matrix for fitting results, matrix(data, nrow, ncol)
        colnames(InterResults) <- c("beta0", "beta1", "beta2", "lambda", "SSR")
        rownames(InterResults) <- c(lambdaValues)

        for (i in 1:length(lambdaValues)) { # for each fractional maturity/lambda (for the InterResults matrix rows)
            lambdaTemp <- optimize(.factorBeta2, interval = c(0.001, 1), maturity = lambdaValues[i], maximum = TRUE)$maximum
            InterEstimation <- .NS.estimator(as.numeric(rate[j, ]), maturity, lambdaTemp) # !!!! main estimation !!!! TBD
            BetaCoef <- InterEstimation$Par # extract betas
            if (BetaCoef[1] > 0 & BetaCoef[1] < 20) {
                SSR <- sum(InterEstimation$Res^2)
                InterResults[i, ] <- c(BetaCoef, lambdaTemp, SSR) # log fitting results into 5 columns
            }
            else {
                InterResults[i, ] <- c(BetaCoef, lambdaValues[i], 1e+05) # prolly logging a weirdly low SSR error if first beta (i.e. constant Beta_0) too weird (negative, or >=20)
            }
        }

        BestRow <- which.min(InterResults[, 5]) # pick lowest SSR row from all fractional maturity/lambda fitting rows
        FinalResults[j, ] <- InterResults[BestRow, 1:4] # log the best result info j-th observation/row
        j <- j + 1
    }

    FinalResults_reclassed <- reclass(FinalResults, rate) # reclasses the FinalResults matrix into the original rate format, e.g. xts (with same attributes, like row dates etc) if it was passed as xts, reclass(x, match.to), match.to - xts object whose attributes will be passed to x
}

for (i in 1:length(lambdaValues)) {
    print(paste("Maturity", lambdaValues[i]))
    print(optimize(.factorBeta2, interval = c(0.001, 1), maturity = lambdaValues[i], maximum = TRUE)$maximum)
}

# Plot the estimated coefficients:
# melt xts format into a ggplot compatible dataframe format, exclude lambda
meltbetas <- fortify(FinalResults_reclassed[, ! colnames(FinalResults_reclassed) %in% "lambda"], melt = TRUE)
meltlambda <- fortify(FinalResults_reclassed[, "lambda"], melt = TRUE)

# Plot single series
ggplot(data = meltbetas[meltbetas$Series == "beta_0", ], aes(x = Index, y = Value)) +
    geom_line() + xlab("Index") + ylab("beta_0")

ggplot(data = meltbetas[meltbetas$Series == "beta_1", ], aes(x = Index, y = Value)) +
    geom_line() + xlab("Index") + ylab("beta_1")

ggplot(data = meltbetas[meltbetas$Series == "beta_2", ], aes(x = Index, y = Value)) +
    geom_line() + xlab("Index") + ylab("beta_2")

ggplot(data = meltlambda, aes(x = Index, y = Value)) +
    geom_line() + xlab("Index") + ylab("lambda")

# adding series one at a time
last_plot() + geom_line(data = meltbetas[meltbetas$Series == "beta_1", ], aes(x = Index, y = Value), colour = "red")

# MAIN GRAPH - multivariate plotting
loadings_graph <- ggplot(data = meltbetas, aes(x = Index, y = Value, group = Series, colour = Series)) +
    geom_line() +
    geom_line(data = meltlambda, aes(x = Index, y = Value)) +
    xlab("Index") + ylab("loadings")
loadings_graph

# Save the main graph
ggsave(loadings_graph, filename = "Graphs/Model_factor/factor_loadings_estimated.pdf", device = cairo_pdf,
    width = plots_width, height = plots_height, units = "in")

# Separate plots
autoplot(FinalResults_reclassed, geom = "line")

# Manually without melting
ggplot() + 
    geom_line(data = FinalResults_reclassed$beta_0, aes(x = Index, y = beta_0), color = "grey") +
    geom_line(data = FinalResults_reclassed$beta_1, aes(x = Index, y = beta_1), color = "blue") +
    geom_line(data = FinalResults_reclassed$beta_2, aes(x = Index, y = beta_2), color = "red") +
    xlab('Date') +
    ylab('Beta loadings')


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

.NS.estimator <- function (rate, maturity, lambda) 
{
    beta <- lm(rate ~ 1 + .factorBeta1(lambda, maturity) + .factorBeta2(lambda, maturity))
    betaPar <- coef(beta)
    NaValues <- na.omit(betaPar)
    if (length(NaValues) < 3) 
        betaPar <- c(0, 0, 0)
    names(betaPar) <- c("beta_0", "beta_1", "beta_2")
    EstResults <- list(Par = betaPar, Res = resid(beta))
    return(EstResults)
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

