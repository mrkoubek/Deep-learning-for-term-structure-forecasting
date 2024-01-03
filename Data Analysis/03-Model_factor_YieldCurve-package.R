# Master Thesis
# Factor models - YieldCurve package examples
# v1.1 - optimised the Nelson.Siegel_custom_lambda function to run faster



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
    # Loading basic environment functions
    load(file = "Workspaces/00_Environment_functions.RData")
    # Loading data
    # load(file = "Workspaces/Data_04_aggregated_TUFVTYUS.RData") # 0min
    load(file = "Workspaces/Data_05_pricestoyields_TUFVTYUS-small-amonth.RData") # 0min
    load(file = "Workspaces/Data_05_pricestoyields_TUFVTYUS-small-ayear.RData") # 0min
    load(file = "Workspaces/Data_05_pricestoyields_TUFVTYUS.RData") # 0min
    load(file = "Workspaces/Data_06_yields-to-NSparameters_TUFVTYUS.RData") # 0min
    print("Workspace rdy set go!")

###############################################
###### Factor models ##########################
###############################################

    # Returns the estimated coefficients of the Nelson-Siegel's model.
    # https://www.rdocumentation.org/packages/YieldCurve/versions/5.1/topics/Nelson.Siegel
    # Nelson.Siegel(rate, maturity)



###############################################
###### Fixed lambda custom function ###########
###############################################


lambda <- 7
optimize(.factorBeta2, interval = c(0.001, 1), maturity = lambda, maximum = TRUE)$maximum


    # OPTIMISING - PROFILING THE FUNCTION
    Rprof("Profiling/profile_output_Nelson-Siegel-custom-lambda.txt")  # start the profiling, output to a text file

    # Passing the full dataset length
    start <- time_start()
    NSParameters_lambda_varying <- Nelson.Siegel_custom_lambda(rate = data, maturity = maturities)
    time_end(start)
    NSParameters_lambda_fixed <- Nelson.Siegel_custom_lambda(rate = data, maturity = maturities, lambda = 7)

    NSParameters <- NSParameters_lambda_varying
    NSParameters <- NSParameters_lambda_fixed

    NSParameters



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
        # return(EstResults)



    Rprof(NULL)  # Stop profiling

    summary <- summaryRprof("Profiling/profile_output_Nelson-Siegel-custom-lambda.txt")
    print(summary)







    maturities <- c(2, 5, 10, 30) # in years
    data <- yields
    str(data)
    data <- data[1:10000, ]
    # Passing the full dataset length
    # Takes 31.8min for the full dataset but w seq(by=1) instead of by=0.5 in the lambda maturities inside the function
    start <- time_start()
    NSParameters_lambda_varying <- Nelson.Siegel_custom_lambda(rate = data, maturity = maturities)
    # NSParameters_lambda_varying <- Nelson.Siegel(rate = data, maturity = maturities)
    time_end(start)

    # Takes 1min for the full 1H 80k obs. dataset for the fixed lambda:
    start <- time_start()
    NSParameters_lambda_fixed <- Nelson.Siegel_custom_lambda(rate = data, maturity = maturities, lambda = 7.5)
    time_end(start)

    NSParameters <- NSParameters_lambda_varying
    NSParameters <- NSParameters_lambda_fixed

    NSParameters
    
    # unique(NSParameters$lambda)
        #  For the time-varying: 0.05976451 0.06181972 0.06404860 0.06640777 0.06900377 0.07171477 0.07470026 0.07797724
    length(unique(NSParameters$lambda))

    # NSrates(Coeff, maturity), returns the interest rates by Nelson-Siegel's model:
    # - https://www.rdocumentation.org/packages/YieldCurve/versions/5.1/topics/Nelson.Siegel
    (y <- NSrates(Coeff = NSParameters[1000,], maturity = maturities)) # NS fitted rates
    plot(maturities, try.xts(data[1000, ]), main = "Fitting Nelson-Siegel yield curve", xlab = c("Pillars in months"), type = "o") # original observed data rates
    lines(maturities, y, col = 2, type = "o") # add NS fitted rates
    legend("topleft", legend = c("observed yield curve", "fitted yield curve"), col = c(1, 2), lty = 1)
    grid()

    # melt xts format into a ggplot compatible dataframe format, exclude lambda
    meltbetas <- fortify(NSParameters[, !colnames(NSParameters) %in% "lambda"], melt = TRUE)
    meltlambda <- fortify(NSParameters[, "lambda"], melt = TRUE)

    # MAIN GRAPH - multivariate plotting
    loadings_graph <- ggplot(data = meltbetas, aes(x = Index, y = Value, group = Series, colour = Series)) +
        geom_line() +
        geom_line(data = meltlambda, aes(x = Index, y = Value)) +
        xlab("Index") + ylab("loadings")
    loadings_graph

    # TBD edit for yield graph
    # melt xts format into a ggplot compatible dataframe format, exclude lambda
    meltbetas <- fortify(try.xts(data), melt = TRUE)

    # MAIN GRAPH - multivariate plotting
    # TBD save this graph!!
    yields_graph <- ggplot(data = meltbetas, aes(x = Index, y = Value, group = Series, colour = Series)) +
        geom_line() +
        xlab("Time") + ylab("Yields (in percent)")
    yields_graph


    # Save the graphs to file
        custom_scale <- 1.5
        GoldenRatio <- (1 + sqrt(5)) / 2
        # plots_width <- 5.55226 # width in inches of thesis template textwidth
        plots_width <- 10 * custom_scale # 10 inches plus have nicely smallish graph elements, adjust custom scale for each graph type what looks nice
        plots_height <- plots_width / GoldenRatio

    # Save the main graph
    # ggsave(loadings_graph, filename = "Graphs/Model_factor/factor_loadings_estimated_my-all-data_lambda-fixed.pdf", device = cairo_pdf,
    #     width = plots_width, height = plots_height, units = "in")
    # ggsave(loadings_graph, filename = "Graphs/Model_factor/factor_loadings_estimated_my-all-data_lambda-varying.pdf", device = cairo_pdf,
    #     width = plots_width, height = plots_height, units = "in")




    # Delete the variables we won't be needing
    rm(list = setdiff(ls(), c("NSParameters_lambda_varying", "NSParameters_lambda_fixed", "yields", "dataFutures", "dataFutures_M5", "dataFutures_H1", "dataFutures_H4", "dim", "futurenames",
                                "size_objects", "time_start", "time_end")))

    # Save the workspace
    # The following takes 0min to save and is 16MB
    # save.image(file = "Workspaces/Data_06_yields-to-NSparameters_TUFVTYUS.RData")





#### A parallel approach:
# 20s to 6s on the 444 obs. month excerpt, with 11 cores, works well
# 1min on the 5.5k obs. year excerpt
    p_load(parallel, beepr)

    # Set up the cluster
    (numCores <- detectCores() - 1)
    cl <- makeCluster(numCores)
    clusterExport(cl, c(".NS.estimator", ".factorBeta1", ".factorBeta2"))

    # Takes about 6sec for the 444obs amonth dataset
    start <- time_start()
    NSParameters_lambda_varying <- Nelson.Siegel_custom_lambda_parallel(rate = data, maturity = maturities)
    time_end(start)
    beep(3) # make a sound once it finishes

    # Takes about 1min for the 80k obs. full dataset
    start <- time_start()
    NSParameters_lambda_fixed <- Nelson.Siegel_custom_lambda_parallel(rate = data, maturity = maturities, lambda = 7)
    time_end(start)
    beep(3) # make a sound once it finishes

    str(NSParameters_lambda_varying)
    head(NSParameters_lambda_varying)
    tail(NSParameters_lambda_varying)
    rm(NSParameters_lambda_varying)

    str(NSParameters_lambda_fixed)
    head(NSParameters_lambda_fixed)
    tail(NSParameters_lambda_fixed)
    rm(NSParameters_lambda_fixed)

    gc()

    # Stop the cluster
    stopCluster(cl)


    # A custom DNS function for use with a time fixed lambda (feeding a number), or a time-varying lambda (feeding a default, or a character "time_varying")
    # Adapted from the YieldCurve package, Nelson.Siegel function.
    Nelson.Siegel_custom_lambda_parallel <- function (rate, maturity, lambda = "time_varying") {
        rate <- try.xts(rate, error = as.matrix) # converts the data into xts, if it comes across an error, tries to convert to a matrix
        if (ncol(rate) == 1) rate <- matrix(as.vector(rate), 1, nrow(rate)) # makes a matrix of 1xN, matrix(data, nrow = 1, ncol = number of original xts rows/observations)
        pillars.number <- length(maturity) # number of maturities
        lambdaValues <- seq(maturity[1], maturity[pillars.number], by = 0.5) # sequence from smallest maturity, by 0.5 increments, to largest, in this example omits 10Y maturity (last one 9.75 since start at 0.25+0.5*n)
        FinalResults <- matrix(0, nrow(rate), 5) # prepare an empty matrix of 0s, ncol = 4 for 4 coefficients (3 betas and 1 lambda), nrow # of observations
        print(paste("Analysing", nrow(rate), "observations."))
        colnames(FinalResults) <- c("beta_0", "beta_1", "beta_2", "lambda", "SSR")
        result_lists <- array(list(NULL), dim = nrow(rate)) # an empty array

        j <- 1
        # Apply the computation in parallel
        result_lists <- parLapply(cl, 1:nrow(rate), function(j) { # :nrow(rate)
            InterResults <- matrix(0, length(lambdaValues), 5) # (re)initialise an empty matrix for fitting results, matrix(data, nrow, ncol)
            colnames(InterResults) <- c("beta0", "beta1", "beta2", "lambda", "SSR")
            rownames(InterResults) <- c(lambdaValues)

            if (lambda == "time_varying") {
                i <- 1
                for (i in 1:length(lambdaValues)) { # for each fractional maturity/lambda (for the InterResults matrix rows)
                    # The following picks an optimal lambda for each maturity
                    lambdaTemp <- optimize(.factorBeta2, interval = c(0.001, 1), maturity = lambdaValues[i], maximum = TRUE)$maximum
                    InterEstimation <- .NS.estimator(as.numeric(rate[j, ]), maturity, lambdaTemp) # !!!! main estimation of betas !!!!

                    BetaCoef <- InterEstimation$Par # extract betas
                    if (BetaCoef[1] > 0 & BetaCoef[1] < 20) { # reasonable coefficients
                        SSR <- sum(InterEstimation$Res^2)
                        InterResults[i, ] <- c(BetaCoef, lambdaTemp, SSR) # log fitting results into 5 columns
                    }
                    else { # unreasonable coefficients
                        InterResults[i, ] <- c(BetaCoef, lambdaTemp, 1e+05) # logging a weirdly high error SSR if first beta (i.e. constant Beta_0) too weird (negative, or >=20)
                    }
                }

                BestRow <- which.min(InterResults[, 5]) # pick lowest SSR row from all fractional maturity/lambda fitting rows
                FinalResults[j, ] <- InterResults[BestRow, 1:5] # log the best result info for the j-th observation/row

            } else {
                # To fix the lambda across time, we feed just the one value of medium term maturity to the optimization
                lambdaTemp <- optimize(.factorBeta2, interval = c(0.001, 1), maturity = lambda, maximum = TRUE)$maximum
                InterEstimation <- .NS.estimator(as.numeric(rate[j, ]), maturity, lambdaTemp) # !!!! main estimation of betas !!!!
                BetaCoef <- InterEstimation$Par # extract betas

                if (BetaCoef[1] > 0 & BetaCoef[1] < 20) { # reasonable coefficients
                    SSR <- sum(InterEstimation$Res^2)
                    InterResults[1, ] <- c(BetaCoef, lambdaTemp, SSR) # log fitting results into 5 columns
                }
                else { # unreasonable coefficients
                    InterResults[1, ] <- c(BetaCoef, lambdaTemp, 1e+05) # logging a weirdly high error SSR if first beta (i.e. constant Beta_0) too weird (negative, or >=20)
                }

                BestRow <- c(1) # pick lowest SSR row from all fractional maturity/lambda fitting rows
                FinalResults[j, ] <- InterResults[BestRow, 1:5] # log the best result info for the j-th observation/row
            }

            return(FinalResults[j, ])
        })

        result <- do.call(rbind, result_lists)
        FinalResults_reclassed <- reclass(result, rate) # reclasses the FinalResults matrix into the original rate format, e.g. xts (with same attributes, like row dates etc) if it was passed as xts, reclass(x, match.to), match.to - xts object whose attributes will be passed to x
    }



# A gpuLm() approach:
    # https://rdrr.io/cran/gputools/man/gpuLm.html
    # seems deprecated old package, doesn't work with newer R versions, skip for now, the parallel approach is enough

    p_load(gputools)




#### A sapply() instead of the for loop:
# Doesn't speed things up at all.

    # A custom DNS function for use with a time fixed lambda (feeding a number), or a time-varying lambda (feeding a default, or a character "time_varying")
    # Adapted from the YieldCurve package, Nelson.Siegel function.
    Nelson.Siegel_custom_lambda_sapply <- function (rate, maturity, lambda = "time_varying") {
        rate <- try.xts(rate, error = as.matrix) # converts the data into xts, if it comes across an error, tries to convert to a matrix
        if (ncol(rate) == 1) rate <- matrix(as.vector(rate), 1, nrow(rate)) # makes a matrix of 1xN, matrix(data, nrow = 1, ncol = number of original xts rows/observations)
        pillars.number <- length(maturity) # number of maturities
        lambdaValues <- seq(maturity[1], maturity[pillars.number], by = 0.5) # sequence from smallest maturity, by 0.5 increments, to largest, in this example omits 10Y maturity (last one 9.75 since start at 0.25+0.5*n)
        FinalResults <- matrix(0, nrow(rate), 5) # prepare an empty matrix of 0s, ncol = 4 for 4 coefficients (3 betas and 1 lambda), nrow # of observations
        print(paste("Analysing", nrow(rate), "observations."))
        colnames(FinalResults) <- c("beta_0", "beta_1", "beta_2", "lambda", "SSR")

        j <- 1
        while (j <= nrow(rate)) { # loop thru all the observations (j-th observation)
            InterResults <- matrix(0, length(lambdaValues), 5) # (re)initialise an empty matrix for fitting results, matrix(data, nrow, ncol)
            colnames(InterResults) <- c("beta0", "beta1", "beta2", "lambda", "SSR")
            rownames(InterResults) <- c(lambdaValues)

            if (lambda == "time_varying") {
                InterResults <- t(sapply(lambdaValues, function(lamVal) {
                    lambdaTemp <- optimize(.factorBeta2, interval = c(0.001, 1), maturity = lamVal, maximum = TRUE)$maximum
                    InterEstimation <- .NS.estimator(as.numeric(rate[j, ]), maturity, lambdaTemp)
                    BetaCoef <- InterEstimation$Par

                    if (BetaCoef[1] > 0 & BetaCoef[1] < 20) {
                        SSR <- sum(InterEstimation$Res^2)
                        return(c(BetaCoef, lambdaTemp, SSR))
                    } else {
                        return(c(BetaCoef, lambdaTemp, 1e+05))
                    }
                }))

                BestRow <- which.min(InterResults[, 5]) # pick lowest SSR row from all fractional maturity/lambda fitting rows
                FinalResults[j, ] <- InterResults[BestRow, 1:5] # log the best result info for the j-th observation/row

                # i <- 1
                    # for (i in 1:length(lambdaValues)) { # for each fractional maturity/lambda (for the InterResults matrix rows)
                    #     # The following picks an optimal lambda for each maturity
                    #     lambdaTemp <- optimize(.factorBeta2, interval = c(0.001, 1), maturity = lambdaValues[1], maximum = TRUE)$maximum
                    #     InterEstimation <- .NS.estimator(as.numeric(rate[j, ]), maturity, lambdaTemp) # !!!! main estimation of betas !!!!
                    #     BetaCoef <- InterEstimation$Par # extract betas

                    #     if (BetaCoef[1] > 0 & BetaCoef[1] < 20) { # reasonable coefficients
                    #         SSR <- sum(InterEstimation$Res^2)
                    #         InterResults[i, ] <- c(BetaCoef, lambdaTemp, SSR) # log fitting results into 5 columns
                    #     }
                    #     else { # unreasonable coefficients
                    #         InterResults[i, ] <- c(BetaCoef, lambdaTemp, 1e+05) # logging a weirdly high error SSR if first beta (i.e. constant Beta_0) too weird (negative, or >=20)
                    #     }

                    #     BestRow <- which.min(InterResults[, 5]) # pick lowest SSR row from all fractional maturity/lambda fitting rows
                    #     FinalResults[j, ] <- InterResults[BestRow, 1:5] # log the best result info for the j-th observation/row
                    # }
            } else {
                # To fix the lambda across time, we feed just the one value of medium term maturity to the optimization
                lambdaTemp <- optimize(.factorBeta2, interval = c(0.001, 1), maturity = lambda, maximum = TRUE)$maximum
                InterEstimation <- .NS.estimator(as.numeric(rate[j, ]), maturity, lambdaTemp) # !!!! main estimation of betas !!!!
                BetaCoef <- InterEstimation$Par # extract betas

                if (BetaCoef[1] > 0 & BetaCoef[1] < 20) { # reasonable coefficients
                    SSR <- sum(InterEstimation$Res^2)
                    InterResults[1, ] <- c(BetaCoef, lambdaTemp, SSR) # log fitting results into 5 columns
                }
                else { # unreasonable coefficients
                    InterResults[1, ] <- c(BetaCoef, lambdaTemp, 1e+05) # logging a weirdly high error SSR if first beta (i.e. constant Beta_0) too weird (negative, or >=20)
                }

                BestRow <- c(1) # pick lowest SSR row from all fractional maturity/lambda fitting rows
                FinalResults[j, ] <- InterResults[BestRow, 1:5] # log the best result info for the j-th observation/row
            }

            j <- j + 1
        }

        FinalResults_reclassed <- reclass(FinalResults, rate) # reclasses the FinalResults matrix into the original rate format, e.g. xts (with same attributes, like row dates etc) if it was passed as xts, reclass(x, match.to), match.to - xts object whose attributes will be passed to x
    }





    # A custom DNS function for use with a time fixed lambda (feeding a number), or a time-varying lambda (feeding a default, or a character "time_varying")
    # Adapted from the YieldCurve package, Nelson.Siegel function.
    Nelson.Siegel_custom_lambda <- function (rate, maturity, lambda = "time_varying") {
        rate <- try.xts(rate, error = as.matrix) # converts the data into xts, if it comes across an error, tries to convert to a matrix
        if (ncol(rate) == 1) rate <- matrix(as.vector(rate), 1, nrow(rate)) # makes a matrix of 1xN, matrix(data, nrow = 1, ncol = number of original xts rows/observations)
        pillars.number <- length(maturity) # number of maturities
        lambdaValues <- seq(maturity[1], maturity[pillars.number], by = 0.5) # sequence from smallest maturity, by 0.5 increments, to largest, in this example omits 10Y maturity (last one 9.75 since start at 0.25+0.5*n)
        FinalResults <- matrix(0, nrow(rate), 5) # prepare an empty matrix of 0s, ncol = 4 for 4 coefficients (3 betas and 1 lambda), nrow # of observations
        print(paste("Analysing", nrow(rate), "observations."))
        colnames(FinalResults) <- c("beta_0", "beta_1", "beta_2", "lambda", "SSR")

        j <- 1
        while (j <= nrow(rate)) { # loop thru all the observations (j-th observation)
            InterResults <- matrix(0, length(lambdaValues), 5) # (re)initialise an empty matrix for fitting results, matrix(data, nrow, ncol)
            colnames(InterResults) <- c("beta0", "beta1", "beta2", "lambda", "SSR")
            rownames(InterResults) <- c(lambdaValues)

            if (lambda == "time_varying") {
                i <- 1
                for (i in 1:length(lambdaValues)) { # for each fractional maturity/lambda (for the InterResults matrix rows)
                    # The following picks an optimal lambda for each maturity
                    lambdaTemp <- optimize(.factorBeta2, interval = c(0.001, 1), maturity = lambdaValues[i], maximum = TRUE)$maximum
                    InterEstimation <- .NS.estimator(as.numeric(rate[j, ]), maturity, lambdaTemp) # !!!! main estimation of betas !!!!

                    BetaCoef <- InterEstimation$Par # extract betas
                    if (BetaCoef[1] > 0 & BetaCoef[1] < 20) { # reasonable coefficients
                        SSR <- sum(InterEstimation$Res^2)
                        InterResults[i, ] <- c(BetaCoef, lambdaTemp, SSR) # log fitting results into 5 columns
                    }
                    else { # unreasonable coefficients
                        InterResults[i, ] <- c(BetaCoef, lambdaTemp, 1e+05) # logging a weirdly high error SSR if first beta (i.e. constant Beta_0) too weird (negative, or >=20)
                    }

                    BestRow <- which.min(InterResults[, 5]) # pick lowest SSR row from all fractional maturity/lambda fitting rows
                    FinalResults[j, ] <- InterResults[BestRow, 1:5] # log the best result info for the j-th observation/row
                }
            } else {
                    # To fix the lambda across time, we feed just the one value of medium term maturity to the optimization
                    lambdaTemp <- optimize(.factorBeta2, interval = c(0.001, 1), maturity = lambda, maximum = TRUE)$maximum

                    InterEstimation <- .NS.estimator(as.numeric(rate[j, ]), maturity, lambdaTemp) # !!!! main estimation of betas !!!!
                    BetaCoef <- InterEstimation$Par # extract betas
                    if (BetaCoef[1] > 0 & BetaCoef[1] < 20) { # reasonable coefficients
                        SSR <- sum(InterEstimation$Res^2)
                        InterResults[1, ] <- c(BetaCoef, lambdaTemp, SSR) # log fitting results into 5 columns
                    }
                    else { # unreasonable coefficients
                        InterResults[1, ] <- c(BetaCoef, lambdaTemp, 1e+05) # logging a weirdly high error SSR if first beta (i.e. constant Beta_0) too weird (negative, or >=20)
                    }

                    BestRow <- c(1) # pick lowest SSR row from all fractional maturity/lambda fitting rows
                    FinalResults[j, ] <- InterResults[BestRow, 1:5] # log the best result info for the j-th observation/row
            }

            j <- j + 1
        }

        FinalResults_reclassed <- reclass(FinalResults, rate) # reclasses the FinalResults matrix into the original rate format, e.g. xts (with same attributes, like row dates etc) if it was passed as xts, reclass(x, match.to), match.to - xts object whose attributes will be passed to x
    }

    # Passing the first 10 observations/months
    NSParameters_lambda_varying <- Nelson.Siegel_custom_lambda(rate = first(FedYieldCurve, '10 month'), maturity = maturity.Fed)
    NSParameters_lambda_fixed <- Nelson.Siegel_custom_lambda(rate = first(FedYieldCurve, '10 month'), maturity = maturity.Fed, lambda = 7)

    # Passing the full dataset length
    start <- time_start()
    NSParameters_lambda_varying <- Nelson.Siegel_custom_lambda(rate = FedYieldCurve, maturity = maturity.Fed)
    time_end(start)
    NSParameters_lambda_fixed <- Nelson.Siegel_custom_lambda(rate = FedYieldCurve, maturity = maturity.Fed, lambda = 7)

    head(NSParameters_lambda_varying)
    tail(NSParameters_lambda_varying)
    head(NSParameters_lambda_fixed)
    tail(NSParameters_lambda_fixed)

    # Plot the estimated coefficients:
    # Pick if time varying or fixed to plot
    NSParameters <- NSParameters_lambda_varying
    NSParameters <- NSParameters_lambda_fixed
    # melt xts format into a ggplot compatible dataframe format, exclude lambda
    meltbetas <- fortify(NSParameters[, !colnames(NSParameters) %in% "lambda"], melt = TRUE)
    meltlambda <- fortify(NSParameters[, "lambda"], melt = TRUE)

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



    # On our datasets
    maturities <- c(2, 5, 10, 30) # in years
        maturity <- c(2, 5, 10, 30)
    str(dataFutures_H1)
    data <- dataFutures_H1
    data <- yields
    str(data)

    # TBD CLEAN UP I put this to 02-Prices_to_yields.R
        # data.table playground joining columns
            # Observations from TU, that are not in FV
            test <- data$TU[!data$FV, on = .(Hour)]
                # 95 obs.
            test <- data$FV[!data$TU, on = .(Hour)]
                # 341 obs.

            # Observations from TU, that are not in TY
            test <- data$TU[!data$TY, on = .(Hour)]
                # 0 obs.
            test <- data$TY[!data$TU, on = .(Hour)]
                # 355 obs.

            # Observations from TU, that are not in US
            test <- data$TU[!data$US, on = .(Hour)]
                # 1: 2008-11-27 17:00:00   108
            test <- data$US[!data$TU, on = .(Hour)]
                # 354 obs.



        str(data)

        # Set "Hour" as the key for each data.table
        data <- lapply(data, function(x) setkey(x, Hour))

        # Merge all data.tables using Reduce
        result <- Reduce(function(x, y) merge(x, y, by = "Hour", all = TRUE), data)

        # Rename the "Close" columns w maturity suffixes
        setnames(result, c("Hour", paste0("Close_", names(data))))

        str(result)
        head(result)


        # Subset the data to contain just a month for testing the NS function
        data_amonth <- result[Hour >= as.POSIXct("2006-01-01", tz = "GMT") & Hour < as.POSIXct("2006-02-01", tz = "GMT")]
        head(data_amonth)
        str(data_amonth)
            # 443 obs.

        # Quick NAs check
        sum(is.na(data_amonth))
            # 14
        NA_rows <- data_amonth[rowSums(is.na(data_amonth)) > 0, ]
        str(NA_rows)
        head(NA_rows)
        NA_rows


        # We use linear interpolation for the few NAs (missing values, mostly in the TU maturity)
        # From the zoo package, na.approx() function:
        data_amonth[, (2:ncol(data_amonth)) := lapply(.SD, na.approx, na.rm = FALSE), .SDcols = 2:ncol(data_amonth)]
        str(data_amonth)
        sum(is.na(data_amonth))
            # 0


    # Passing the first month of our data, it has 443 observations
    start <- time_start()
    NSParameters_lambda_varying <- Nelson.Siegel_custom_lambda(rate = data, maturity = maturities)
    time_end(start)
        # 20s for 1 month of hourly data, would be 60min total, needs slight optimising

    start <- time_start()
    NSParameters_lambda_fixed <- Nelson.Siegel_custom_lambda(rate = data, maturity = maturities, lambda = 7)
    time_end(start)

    NSParameters <- NSParameters_lambda_fixed

    NSParameters

    # We're getting too large Beta_0 (115), and negative Beta_1 (-17), Beta_2 (-3.5). TBD DELVE

    NSParameters <- Nelson.Siegel(rate = data, maturity = maturities)
    NSParameters


    data_amonth_yieldsdiv100 <- data[, (2:ncol(data)) := lapply(.SD, function(x) x / 100), .SDcols = 2:ncol(data)]
    str(data_amonth_yieldsdiv100)
    head(data_amonth_yieldsdiv100)

    start <- time_start()
    NSParameters_lambda_fixed <- Nelson.Siegel_custom_lambda(rate = data_amonth_yieldsdiv100, maturity = maturities, lambda = 7)
    time_end(start)

    NSParameters <- NSParameters_lambda_fixed

    NSParameters



    # Yields data
    # 2015-12-31 14:00
        str(result)
        result[Hour == as.POSIXct("2015-12-31 14:00:00", tz = "GMT")]
            #                Hour Close_TU Close_FV Close_TY Close_US
            # 2015-12-31 14:00:00 108.6406 118.3594 125.8906 153.4688


    # Passing the full dataset length
    start <- time_start()
    # NSParameters_lambda_varying <- Nelson.Siegel_custom_lambda(rate = data, maturity = maturities)
    NSParameters_lambda_varying <- Nelson.Siegel(rate = data, maturity = maturities)
    time_end(start)

    start <- time_start()
    NSParameters_lambda_fixed <- Nelson.Siegel_custom_lambda(rate = data, maturity = maturities, lambda = 7)
    time_end(start)

    NSParameters <- NSParameters_lambda_varying
    NSParameters <- NSParameters_lambda_fixed

    NSParameters

    length(unique(NSParameters$lambda))



###############################################
###### Example from package YieldCurve ########
###############################################

    data(FedYieldCurve) # Fed data set
    str(FedYieldCurve) # 8 maturities in columns, 372 months in rows as observations
    head(FedYieldCurve)
    tail(FedYieldCurve)


    # Play
    # subset the dataset to start from 2006:
    FedYieldCurve_compare <- FedYieldCurve[index(FedYieldCurve) >= as.POSIXct("2006-01-01", tz = "GMT")]
    head(FedYieldCurve_compare)

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
    (NSParameters <- Nelson.Siegel_custom_lambda_parallel(rate = first(FedYieldCurve, '10 month'), maturity = maturity.Fed))
    NSParameters <- Nelson.Siegel(rate = FedYieldCurve, maturity = maturity.Fed)
    NSParameters_custom <- Nelson.Siegel_custom_lambda_parallel(rate = FedYieldCurve, maturity = maturity.Fed)


    # Play
    # The 2012-11-30 observation is way off in the fed yield data and our data
    last(FedYieldCurve)
    data[Hour > as.POSIXct("2012-11-29 23:00:00", tz = "GMT") & Hour <= as.POSIXct("2012-11-30 23:00:00", tz = "GMT"), ]
    NSParameters_custom <- Nelson.Siegel_custom_lambda_parallel(rate = last(FedYieldCurve), maturity = maturity.Fed, lambda = 7)
    NSParameters_custom <- Nelson.Siegel_custom_lambda_parallel(rate = last(FedYieldCurve), maturity = maturity.Fed, lambda = 7)


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

    rate <- data
    maturity <- c(2, 5, 10, 30)

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
            head(InterResults)

            i <- 1
            for (i in 1:length(lambdaValues)) { # for each fractional maturity/lambda (for the InterResults matrix rows)
                # The following calculates an optimal lambda for each maturity
                optimize(.factorBeta2, interval = c(0.001, 1), maturity = lambdaValues[i], maximum = TRUE)
                lambdaTemp <- optimize(.factorBeta2, interval = c(0.001, 1), maturity = lambdaValues[i], maximum = TRUE)$maximum
                # To fix the lambda across time, overwrite it with the fixed one
                # fixed_lambda_maturity <- c(2.5) # TBD could edit the whole function to accept this as an argument)
                # optimize(.factorBeta2, interval = c(0.001, 1), maturity = fixed_lambda_maturity, maximum = TRUE)$maximum
                # lambdaTemp <- optimize(.factorBeta2, interval = c(0.001, 1), maturity = fixed_lambda_maturity, maximum = TRUE)$maximum
                # print(lambdaTemp)
                # as.numeric(rate[j, ]) # j-th observation
                InterEstimation <- .NS.estimator(as.numeric(rate[j, ]), maturity, lambdaTemp) # !!!! main estimation of betas !!!!
                BetaCoef <- InterEstimation$Par # extract betas
                if (BetaCoef[1] > 0 & BetaCoef[1] < 20) {
                    SSR <- sum(InterEstimation$Res^2)
                    InterResults[i, ] <- c(BetaCoef, lambdaTemp, SSR) # log fitting results into 5 columns
                    head(InterResults)
                }
                else {
                    InterResults[i, ] <- c(BetaCoef, lambdaValues[i], 1e+05) # logging a weirdly high error SSR if first beta (i.e. constant Beta_0) too weird (negative, or >=20)
                }
            }

            BestRow <- which.min(InterResults[, 5]) # pick lowest SSR row from all fractional maturity/lambda fitting rows
            FinalResults[j, ] <- InterResults[BestRow, 1:4] # log the best result info for the j-th observation/row
            head(FinalResults)
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
    meltbetas <- fortify(NSParameters[, ! colnames(NSParameters) %in% "lambda"], melt = TRUE)
    meltlambda <- fortify(NSParameters[, "lambda"], melt = TRUE)

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
    lambdaTemp <- optimize(.factorBeta2, interval = c(0.001, 1), maturity = 7, maximum = TRUE)$maximum
    (InterEstimation <- .NS.estimator(rate, maturity, lambdaTemp))
    (rate <- as.numeric(rate[j, ]))
    (lambda <- lambdaTemp)
    rate

    rate <- try.xts(data)
    str(rate)
    head(rate)
    rate[1, ]
    as.numeric(rate[1, ])
    maturities
    lambdaTemp <- optimize(.factorBeta2, interval = c(0.001, 1), maturity = 7, maximum = TRUE)$maximum
    lambdaTemp
    .factorBeta1(lambda = lambdaTemp, maturity = maturities)
    lm(as.numeric(rate[1, ]) ~ 1 + .factorBeta1(lambda = lambdaTemp, maturity = maturities) + .factorBeta2(lambda = lambdaTemp, maturity = maturities))
    .NS.estimator(rate = as.numeric(rate[1, ]), maturity = maturities, lambda = lambdaTemp)

    .NS.estimator <- function (rate, maturity, lambda)  # !!!! main estimation of betas !!!!
    {
        beta <- lm(rate ~ 1 + .factorBeta1(lambda, maturity) + .factorBeta2(lambda, maturity))
        betaPar <- coef(beta)
        NaValues <- na.omit(betaPar) # if there's an NA in one of the coefficients, the NaValues will drop it
        if (length(NaValues) < 3) # and then it's length will decrease below 3 coefficients
            betaPar <- c(0, 0, 0)
        names(betaPar) <- c("beta_0", "beta_1", "beta_2")
        EstResults <- list(Par = betaPar, Res = resid(beta))
        return(EstResults)
    }




    ### Svensson function and ECB data-set ###
    data(ECBYieldCurve)
    rate.ECB = ECBYieldCurve[1:5,]
    rate.ECB
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


