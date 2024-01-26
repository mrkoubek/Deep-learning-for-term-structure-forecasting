# Master Thesis
# Factor models
# v1.3 - WIP - code cleanup and revive



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
    # load(file = "Workspaces/Data_05_pricestoyields_TUFVTYUS.RData") # 0min
    load(file = "Workspaces/Data_06_yields-to-NSparameters_TUFVTYUS.RData") # 0min
    print("Workspace rdy set go!")



###############################################
###### Factor models ##########################
###############################################

    # We run the preprocessed data through the factor models (DNS) before training the NNs.
    # Returns the estimated coefficients of the Nelson-Siegel's model.
    # https://www.rdocumentation.org/packages/YieldCurve/versions/4.1/topics/Nelson.Siegel
    # Nelson.Siegel(rate, maturity)



###############################################
###### A parallel approach ####################
###############################################

    # Nelson.Siegel function adapted from the YieldCurve package.
    # A custom DNS function for use with a time-fixed lambda (feeding a number),
    # or a time-varying lambda (feeding a default, or a character lambda = "time_varying").
    Nelson.Siegel_custom_lambda_parallel <- function (rate, maturity, lambda = "time_varying") {
        # TBC
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
    
    # We define and empty variable with the structure we need to save all of our NS parameters in this variable.
    # It has two lists, one for the fixed and one for the varying lambdas.
    # Each of these two lists contains our various data excerpts or formats/frequencies:
    # data_amonth, data_ayear, data_hourly_all, data_daily_Fedcompare, data_daily_all
    NS_parameters <- array(list(NULL), dim = 2, dimnames = list(c("lambda_fixed", "lambda_varying"))) # an empty array of 2 lists
    NS_parameters$lambda_fixed <- array(list(NULL), dim = 5, dimnames = list(names(yields))) # 5 empty lists
    NS_parameters$lambda_varying <- array(list(NULL), dim = 5, dimnames = list(names(yields))) # 5 empty lists
    NS_parameters

    # Explore our datasets
    ls()
    str(yields)
    str(yields$data_hourly_all) # our main focus is on this hourly data frequency

    # Using the parallel package to spead out the workload on all CPU cores:
        # 20s to 6s on the 444 obs. amonth data excerpt, with 11 cores, works well
        # 1min on the 5.5k obs. year excerpt
        # 2min on 10k obs. TBD
        # 16min on 80k obs.

        p_load(parallel, beepr)

        # Set up the cluster
        (numCores <- detectCores() - 1)
        cl <- makeCluster(numCores)
        clusterExport(cl, c(".NS.estimator", ".factorBeta1", ".factorBeta2")) # prepares the most important functions into memory TBD

        # Fixed lambda
        # Takes about 1min for the full hourly dataset of 80k obs.
        start <- time_start()
        # 2024-01-XX: ran the function call with "lambda = 7"
        # TBD maybe use mapply() to simplify the following lapply()?
        NS_parameters$lambda_fixed <- lapply(yields, function(data) {
            Nelson.Siegel_custom_lambda_parallel(rate = data, maturity = maturities, lambda = 7)
            })
        time_end(start)
        beep(3) # make a sound once it finishes

        str(NS_parameters)
        head(NS_parameters)

        # Time varying lambda
        # !!! WARNING LONG !!! Takes about 13min for the full hourly dataset of 80k obs., and 17min for all the dataset variations
        start <- time_start()
        # TBD maybe use mapply() to simplify the following lapply()?
        NS_parameters$lambda_varying <- lapply(yields, function(data) {
            Nelson.Siegel_custom_lambda_parallel(rate = data, maturity = maturities)
            })
        time_end(start)
        beep(3) # make a sound once it finishes

        str(NS_parameters)
        head(NS_parameters)

        # Clear the memory TBD
        gc()

        # Stop the cluster
        stopCluster(cl)

# TBC, polish graphs
    # Plot the estimated coefficients
    str(NS_parameters)
    head(NS_parameters)
    # Need for the two following categories to compare them:
    # NS_parameters$lambda_varying
    # NS_parameters$lambda_fixed

    # Rewrite the one dataset code into a multi dataset code: WIP

    # We define and empty variable with the structure we need to save all of our NS parameters in this variable.
    # It has two lists, one for the fixed and one for the varying lambdas.
    # Each of these two lists contains our various data excerpts or formats/frequencies:
    # data_amonth, data_ayear, data_hourly_all, data_daily_Fedcompare, data_daily_all
    NS_parameters_melted <- array(list(NULL), dim = 2, dimnames = list(c("lambda_fixed", "lambda_varying"))) # an empty array of 2 lists
    NS_parameters_melted$lambda_fixed <- array(list(NULL), dim = 5, dimnames = list(names(yields))) # 5 empty lists
    NS_parameters_melted$lambda_varying <- array(list(NULL), dim = 5, dimnames = list(names(yields))) # 5 empty lists
    NS_parameters_melted

    NS_parameters_melted$lambda_fixed <- lapply(NS_parameters$lambda_fixed, function(data) {
        fortify(data, melt = TRUE)
        })
    str(NS_parameters_melted)

    # Prepare an empty variable
    loadings_graphs <- array(list(NULL), dim = 2, dimnames = list(c("lambda_fixed", "lambda_varying"))) # an empty array of 2 lists
    loadings_graphs$lambda_fixed <- array(list(NULL), dim = 5, dimnames = list(names(yields))) # 5 empty lists
    loadings_graphs$lambda_varying <- array(list(NULL), dim = 5, dimnames = list(names(yields))) # 5 empty lists
    loadings_graphs

    # MAIN LOADINGS GRAPH - multivariate plotting
    # Use ggplot for all the datasets at once
    # For now just for the lambda_fixed:
    loadings_graphs$lambda_fixed <- lapply(NS_parameters_melted$lambda_fixed, function(data) {
        ggplot(data = data, aes(x = Index, y = Value, group = Series, colour = Series)) +
        geom_line() +
        # geom_line(data = meltlambda, aes(x = Index, y = Value)) + # we melted the lambdas too above, so prolly don't need this
        xlab("Index") + ylab("loadings")
        })
    loadings_graphs$lambda_fixed[[1]]
    loadings_graphs$lambda_fixed[[2]]

    # Prepare an empty variable
    yields_graphs <- array(list(NULL), dim = 2, dimnames = list(c("lambda_fixed", "lambda_varying"))) # an empty array of 2 lists
    yielda_graphs$lambda_fixed <- array(list(NULL), dim = 5, dimnames = list(names(yields))) # 5 empty lists
    yields_graphs$lambda_varying <- array(list(NULL), dim = 5, dimnames = list(names(yields))) # 5 empty lists
    yields_graphs

#TBC
    # MAIN LOADINGS GRAPH - multivariate plotting
    # Use ggplot for all the datasets at once
    # For now just for the lambda_fixed:
    yielda_graphs$lambda_fixed <- lapply(NS_parameters_melted$lambda_fixed, function(data) {
        ggplot(data = data, aes(x = Index, y = Value, group = Series, colour = Series)) +
        geom_line() +
        xlab("Time") + ylab("Yields (in percent)")
        })
    yields_graphs$lambda_fixed[[1]]
    yields_graphs$lambda_fixed[[2]]


    # Yield graph
    meltyields <- fortify(try.xts(data), melt = TRUE)

    # MAIN YIELDS GRAPH - multivariate plotting
    # TBD save this graph!!
    yields_graph <- ggplot(data = meltyields, aes(x = Index, y = Value, group = Series, colour = Series)) +
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
    # ggsave(loadings_graph, filename = "Graphs/Model_factor/WIP/factor_loadings_estimated_my-all-data_lambda-varying.pdf", device = cairo_pdf,
    #     width = plots_width, height = plots_height, units = "in")
    # ggsave(loadings_graph, filename = "Graphs/Model_factor/WIP/factor_loadings_estimated_my-all-data_lambda-fixed.pdf", device = cairo_pdf,
    #     width = plots_width, height = plots_height, units = "in")
    # ggsave(yields_graph, filename = "Graphs/Model_factor/Yields/yields_percent_data_hourly_all.pdf", device = cairo_pdf,
    #     width = plots_width, height = plots_height, units = "in")

    # Delete the variables we won't be needing
    rm(list = setdiff(ls(), c("NS_parameters",
                                "yields",
                                "dataFutures", "dataFutures_M5", "dataFutures_H1", "dataFutures_H4", "dataFutures_D1",
                                "maturities", "dim", "futurenames",
                                "size_objects", "time_start", "time_end")))

    # Save the workspace
    # The following takes 0min to save and is 16MB
    save.image(file = "Workspaces/Data_06_yields-to-NSparameters_TUFVTYUS.RData")



###############################################
####### FedYieldCurve dataset comparison ######
###############################################

    # TBD do we want this here?
    # For comparison of the lambda development, fit the YieldCurve package example dataset FedYieldCurve.
    # Passing the full FedYieldCurve dataset length
        # start <- time_start()
        # NSParameters_lambda_varying <- Nelson.Siegel_custom_lambda(rate = FedYieldCurve, maturity = maturity.Fed)
        # time_end(start)
        # NSParameters_lambda_fixed <- Nelson.Siegel_custom_lambda(rate = FedYieldCurve, maturity = maturity.Fed, lambda = 7)

        # head(NSParameters_lambda_varying)
        # tail(NSParameters_lambda_varying)
        # head(NSParameters_lambda_fixed)
        # tail(NSParameters_lambda_fixed)



###############################################
####### Notes #################################
###############################################

    # 
    