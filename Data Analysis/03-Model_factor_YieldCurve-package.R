# Master Thesis
# Factor models - YieldCurve package examples
# v1.0 - basic formatting



###############################################
###### Workspace setup ########################
###############################################

    rm(list = ls())

    # Packages
    if (!require("pacman")) install.packages("pacman") # installs pacman package if not installed yet
    pacman::p_load(pacman, YieldCurve, dplyr, tidyr, ggplot2, scales, Cairo, zoo, xtable, tibble, forecast, naturalsort, data.table) # load packages TBD e.g. these, edit when u determine which needed/used..
    # p_load(reticulate, tensorflow, keras)

    # Workspace
    setwd("E:/Google_Drive/Diploma_Thesis/Code")
    load(file = "Workspaces/Data_04_aggregated_TUFVTYUS.RData") # 0min
    print("Workspace rdy set go!")



###############################################
###### Factor models ##########################
###############################################

    # Returns the estimated coefficients of the Nelson-Siegel's model.
    # https://www.rdocumentation.org/packages/YieldCurve/versions/5.1/topics/Nelson.Siegel
    # Nelson.Siegel(rate, maturity)



###############################################
###### Example from package YieldCurve ########
###############################################

    data(FedYieldCurve) # Fed data set

    str(FedYieldCurve) # 8 maturities in columns, 372 months in rows as observations
    head(FedYieldCurve)
    tail(FedYieldCurve)

    maturity.Fed <- c(3/12, 0.5, 1, 2, 3, 5, 7, 10)

    # first() from xts package can have a number of periods in character string, n = 'n period.type', where period.type can be: secs, seconds, mins, minutes, hours, days, weeks, months, quarters, and years
    # the first() is not necessary there, it is just to calculate the NS parameters only for the first few (10 months, so 10 observations for these monthly data) observations (instead of the whole Fed 1981-2012 dataset), to run faster for the example (only 10 months of data).
    str(first(FedYieldCurve, '10 months'))
    first(FedYieldCurve, '10 months') # the data the example uses, 10 observations

    # Nelson.Siegel(rate, maturity) explained:
    # - rate is a matrix with i.r.
    # - maturity is a vector of maturities of the rates (in months) - TBD why does the documentation say in months and the example has it in years?
    # And the YieldCurve authors' example of '10 month' seems to work for the xts first() function, as well as the more correct plural syntax version '10 months'.
    (NSParameters <- Nelson.Siegel(rate = first(FedYieldCurve, '10 month'), maturity = maturity.Fed))

    # NSrates(Coeff, maturity), returns the interest rates by Nelson-Siegel's model:
    # - https://www.rdocumentation.org/packages/YieldCurve/versions/5.1/topics/Nelson.Siegel
    (y <- NSrates(Coeff = NSParameters[5,], maturity = maturity.Fed)) # NS fitted rates
    plot(maturity.Fed, FedYieldCurve[5, ], main = "Fitting Nelson-Siegel yield curve", xlab = c("Pillars in months"), type = "o") # original observed data rates
    lines(maturity.Fed, y, col = 2, type = "o") # add NS fitted rates
    legend("topleft", legend = c("observed yield curve", "fitted yield curve"), col = c(1, 2), lty = 1)
    grid()

    ####### FUNCTIONS FROM PACKAGE YieldCurve ########
    # Testing one by one for understanding
    # Initialise the function's variables
    rate <- FedYieldCurve
    maturity <- maturity.Fed

    # My data
    # rate <- 
    # maturity <- maturity_bonds

    # Analyse the Nelson.Siegel function. Run the inside of the function row by row.
    Nelson.Siegel <- function (rate, maturity)
    {
        rate <- try.xts(rate, error = as.matrix) # converts the data into xts, if it comes across an error, tries to convert to a matrix
        str(rate)
        if (ncol(rate) == 1) # one maturity column
            rate <- matrix(as.vector(rate), 1, nrow(rate)) # makes a matrix of 1xN, matrix(data, nrow = 1, ncol = number of original xts rows/observations)
        (pillars.number <- length(maturity)) # number of maturities
        (lambdaValues <- seq(maturity[1], maturity[pillars.number], by = 0.5)) # sequence from smallest maturity, by 0.5 increments, to largest, in this example omits 10Y maturity (last one 9.75 since start at 0.25+0.5*n)
        length(lambdaValues)
        FinalResults <- matrix(0, nrow(rate), 4) # prepare an empty matrix of 0s, ncol = 4 for 4 coefficients (3 betas and 1 lambda), nrow # of observations
        colnames(FinalResults) <- c("beta_0", "beta_1", "beta_2", "lambda")
        str(FinalResults)
        head(FinalResults)

        j <- 1
        while (j <= nrow(rate)) { # loop thru all the observations (j-th observation)
            InterResults <- matrix(0, length(lambdaValues), 5) # (re)initialise an empty matrix for fitting results, matrix(data, nrow, ncol)
            colnames(InterResults) <- c("beta0", "beta1", "beta2", "lambda", "SSR")
            rownames(InterResults) <- c(lambdaValues)
            # head(InterResults)
            # tail(InterResults)

            i <- 1
            for (i in 1:length(lambdaValues)) { # for each fractional maturity/lambda (for the InterResults matrix rows)
                # The following calculates an optimal lambda for each maturity
                optimize(.factorBeta2, interval = c(0.001, 1), maturity = lambdaValues[i], maximum = TRUE)
                lambdaTemp <- optimize(.factorBeta2, interval = c(0.001, 1), maturity = lambdaValues[i], maximum = TRUE)$maximum
                # To fix the lambda across time, overwrite it with the fixed one
                fixed_lambda_maturity <- c(2.5) # TBD could edit the whole function to accept this as an argument)
                optimize(.factorBeta2, interval = c(0.001, 1), maturity = fixed_lambda_maturity, maximum = TRUE)$maximum
                lambdaTemp <- optimize(.factorBeta2, interval = c(0.001, 1), maturity = fixed_lambda_maturity, maximum = TRUE)$maximum
                # print(lambdaTemp)
                # as.numeric(rate[j, ]) # j-th observation
                InterEstimation <- .NS.estimator(as.numeric(rate[j, ]), maturity, lambdaTemp) # !!!! main estimation of betas !!!!
                BetaCoef <- InterEstimation$Par # extract betas
                if (BetaCoef[1] > 0 & BetaCoef[1] < 20) {
                    SSR <- sum(InterEstimation$Res^2)
                    InterResults[i, ] <- c(BetaCoef, lambdaTemp, SSR) # log fitting results into 5 columns
                    # head(InterResults)
                }
                else {
                    InterResults[i, ] <- c(BetaCoef, lambdaValues[i], 1e+05) # logging a weirdly high error SSR if first beta (i.e. constant Beta_0) too weird (negative, or >=20)
                }
            }

            BestRow <- which.min(InterResults[, 5]) # pick lowest SSR row from all fractional maturity/lambda fitting rows
            FinalResults[j, ] <- InterResults[BestRow, 1:4] # log the best result info for the j-th observation/row
            # head(FinalResults)
            j <- j + 1
        }

        FinalResults_reclassed <- reclass(FinalResults, rate) # reclasses the FinalResults matrix into the original rate format, e.g. xts (with same attributes, like row dates etc) if it was passed as xts, reclass(x, match.to), match.to - xts object whose attributes will be passed to x
        head(FinalResults_reclassed)
    }

    # Optimal lambda for each fractional maturity
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

    # Save the graphs to file
        custom_scale <- 1.5
        GoldenRatio <- (1 + sqrt(5)) / 2
        # plots_width <- 5.55226 # width in inches of thesis template textwidth
        plots_width <- 10 * custom_scale # 10 inches plus have nicely smallish graph elements, adjust custom scale for each graph type what looks nice
        plots_height <- plots_width / GoldenRatio

    # Save the main graph
    ggsave(loadings_graph, filename = "Graphs/Model_factor/factor_loadings_estimated_lambda-fixed.pdf", device = cairo_pdf,
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


    NSrates(Coeff = NSParameters[5,], maturity = maturity.Fed) # NS fitted rates
    (Coeff <- NSParameters[5, ])
    (maturity <- maturity.Fed)
    NSrates <- function (Coeff, maturity)  # takes in the NS parameters (betas), plugs into the NS equation to calculate the yields
    {
        Curve <- xts(matrix(0, nrow(Coeff), length(maturity)), order.by = time(Coeff)) # prepare an empty xts
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

    .factorBeta1 <- function (lambda, maturity) # maturity is a vector of length = number of maturities, while lambda is a single number for the particular observation
    {
        as.numeric((1 - exp(-lambda * maturity))/(lambda * maturity)) # factor loading (fraction) of beta_1
    }

    .factorBeta2 <- function (lambda, maturity) 
    {
        as.numeric((1 - exp(-lambda * maturity))/(lambda * maturity) - exp(-lambda * maturity)) # factor loading (fraction) of beta_2
    }

    # !!!! main estimation of betas !!!!
    j <- 1
    (InterEstimation <- .NS.estimator(as.numeric(rate[j, ]), maturity, lambdaTemp))
    (rate <- as.numeric(rate[j, ]))
    (lambda <- lambdaTemp)
    rate
    .NS.estimator <- function (rate, maturity, lambda)  # !!!! main estimation of betas !!!!
    {
        beta <- lm(rate ~ 1 + .factorBeta1(lambda, maturity) + .factorBeta2(lambda, maturity))
        # beta
        betaPar <- coef(beta)
        # betaPar
        NaValues <- na.omit(betaPar) # if there's an NA in one of the coefficients, the NaValues will drop it
        if (length(NaValues) < 3) # and then it's length will decrease below 3 coefficients
            betaPar <- c(0, 0, 0)
        names(betaPar) <- c("beta_0", "beta_1", "beta_2")
        EstResults <- list(Par = betaPar, Res = resid(beta))
        # EstResults
        return(EstResults)
    }






    ### Svensson function and ECB data-set ###
    data(ECBYieldCurve)
    rate.ECB = ECBYieldCurve[1:5,]
    maturity.ECB = c(0.25,0.5,seq(1,30,by=1))
    SvenssonParameters <- Svensson(rate.ECB, maturity.ECB)
    Svensson.rate <- Srates( SvenssonParameters ,maturity.ECB,"Spot")

    plot(maturity.ECB, rate.ECB[5,],main="Fitting Svensson yield curve", type="o")
    lines(maturity.ECB, Svensson.rate[5,], col=2)
    legend("topleft",legend=c("observed yield curve","fitted yield curve"),
    col=c(1,2),lty=1)



###############################################
####### Notes #################################
###############################################

    # Work with: dataFutures_train, probably window the data after the NS, as is the case currently in Model_fit.R

    # We need to change our prices data into a different shape, with columns representing maturities.
    # What are our maturities? Do we load up the 2, 5, 10, 30Y maturity prices as columns?
    # For this we need to go back to Data_conversion.R and load up more datasets.
    # Do we then add maturity interpolations?


    # TBD:
    	# go thru the Nelson.Siegel function source code
    	# factor models documentation, find packages inspiration and test out


