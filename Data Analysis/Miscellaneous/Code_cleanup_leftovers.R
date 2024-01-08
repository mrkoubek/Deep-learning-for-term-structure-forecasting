# Master Thesis
# Code cleaup leftovers file, in case something's useful
# v1.0 - pasted prolly unused stuff





################################
### 01 - Load up data ##########
################################


	###### TBC
		###### TBD CLEANUP??

				print(paste("Loading up dataset", i, "/", dim, futurenames[i]))		
				dataFutures[i] <- load_RData_dataFutures(paste0("Workspaces/Data_01_loaded_", futurenames[i], ".RData"))[i]

				print(paste("Size before cleaning", futurenames[i]))		
				size_objects("dataFutures")

				print(paste("Cleaning the dataset", futurenames[i]))
				dataFutures[[i]] <- clean_data(dataFutures[[i]])

				print(paste("Size after cleaning", futurenames[i]))
				size_objects("dataFutures")

				print(paste("Dataset", i, "/", dim, futurenames[i], "loaded up."))


		################### TBD old loading code from here

			# US - 2Y (TICKER:TU)
			# dataFutures[[1]] <- read.csv(filePaths[[1]], header = TRUE) # takes 4min for TU, 7GB of RAM, dataFutures object is then 3.3Gb
			# The following workspace image takes 1min to save and is 120MB
			time_start()
			save.image(file = "Workspaces/Data_01_loaded_TU.RData") # save the workspace
			time_end()

			# US - 5Y (TICKER:FY)
			# dataFutures[[2]] <- read.csv(filePaths[[2]], header = TRUE) # takes 12min for FV, all of RAM and some HDD swapping, dataFutures 8.8Gb
			# The following workspace image takes 2min to save and is 400MB
			save.image(file = "Workspaces/Data_01_loaded_FV.RData") # save the workspace

			# US - 10Y (TICKER:TY)
			# dataFutures[[3]] <- read.csv(filePaths[[3]], header = TRUE) # takes 21min for TY, all of RAM and a lot of HDD swapping
			# dataFutures[[3]] <- fread(filePaths[[3]], header = TRUE, data.table = TRUE) # takes 7min for TY, but dataFutures 15.7Gb (like 1.5Gb larger than w read.csv which factorises a few variables)
			# dataFutures[[3]] <- fread(filePaths[[3]], select = columns_to_select, header = TRUE, data.table = TRUE) # takes 4min for TY (c("Date", "Time", "Close") columns cherrypicked), and dataFutures just 8Gb
			# dataFutures[[3]] <- fread(filePaths[[3]], select = columns_to_select, header = TRUE, data.table = TRUE) # takes 4.5min for TY (c("Date", "Time", "Open", "Close") columns cherrypicked), and dataFutures 10Gb
			# (had to increase HDD RAM limit to +20GB, but better do +30GB next time cause I think it was at the limit), and alto needed to tell R memory.limit(size = 50000) to actually use be able to ask for more, dataFutures 14Gb
			# The following workspace image takes 3min to save and is 570MB
			save.image(file = "Workspaces/Data_01_loaded_TY.RData") # save the workspace

			# US - 30Y (TICKER:US)
			# dataFutures[[4]] <- read.csv(filePaths[[4]], header = TRUE) # takes 12min for US, all of RAM and some HDD swapping, dataFutures 8.9Gb
			# The following workspace image takes 2min to save and is 380MB
			save.image(file = "Workspaces/Data_01_loaded_US.RData") # save the workspace



			################################
			### 01.02 - Merge maturities ###
			################################

			# Load one maturity to then clean it
			time_start()
			load(file = "Workspaces/Data_01_loaded_TU.RData")
			time_end()

			size_objects()

			str(dataFutures)

			rm(x)


			# Load up only the needed variable from a RData file
			# https://stackoverflow.com/questions/5577221/how-can-i-load-an-object-into-a-variable-name-that-i-specify-from-an-r-data-file
			load_RData_variable <- function(fileName) {
			  #loads an RData file, and returns it
			  load(fileName)
			  print(ls())
			  n <- readline(prompt="Which name of the variable to load? \n") # choose a specific variable name
			  get(ls()[ls() == n])
			}

			# Loads an RData file, and returns the variable dataFutures
			load_RData_dataFutures <- function(fileName) {
			  load(fileName)
			  get(ls()[ls() == "dataFutures"])
			}



				# testclean <- dataFutures[[2]]

				print(paste("Size before cleaning", "testclean"))		
				size_objects("testclean")

				print(paste("Cleaning the dataset", "testclean"))
				testclean <- clean_data(dataFutures[[2]])

				print(paste("Size after cleaning", "testclean"))
				size_objects("testclean")





			dataFutures <- array(list(NULL), dim = dim, dimnames = list(futurenames)) # an array in which we'll save all our data
			for (i in 1:3) {
				print(paste("Loading up dataset", i, "/", dim, futurenames[i]))		
				dataFutures[i] <- load_RData_dataFutures(paste0("Workspaces/Data_01_loaded_", futurenames[i], ".RData"))[i]

				print(paste("Size before cleaning", futurenames[i]))		
				size_objects("dataFutures")

				print(paste("Cleaning the dataset", futurenames[i]))
				dataFutures[[i]] <- clean_data(dataFutures[[i]])

				print(paste("Size after cleaning", futurenames[i]))
				size_objects("dataFutures")

				print(paste("Dataset", i, "/", dim, futurenames[i], "loaded up."))
			}

			test <- load_RData_dataFutures("Workspaces/Data_01_loaded_TU.RData")

			dataFutures[1] <- load_RData_dataFutures(paste0("Workspaces/Data_01_loaded_", futurenames[1], ".RData"))[1]



			# Explore US data
			data_US <- dataFutures[[4]]
			rm(dataFutures)
			ls()
			str(data_US)
			head(data_US)

			# Save the env
			ifelse(!dir.exists("Workspaces"), dir.create("Workspaces"), "Directory already exists.") # creates a "Workspaces" directory if it doesn't exist yet
			# The following workspace image takes 5?min to save and is 380MB
			# save.image(file = "Workspaces/Data_01_loaded_US.RData") # save the workspace

		################### TBD old loading code till here





################################
### 02 - Clean data ############
################################

	# Doesn't work for the larger maturities, cannot do the as.POSIXct, no RAM room to work with, gotta do it one by one in
	# the previous 01 - load chapter, while loading up each maturity straight away clean it
	# TBD cleaning the datetime columns saves so much space GB, maybe I could do that for all and keep all the other columns we couldn't load up (like High and Low), but if we don't need them w/e
	# TBD test with one US maturity, then rewrite to run on all 5 lists of dataFutures
	size_objects("dataFutures") # 25Gb
	# data.table does not support POSIXlt (which is result of only the strptime()) data types for performance reason, use POSIXct or ITime
	# https://stackoverflow.com/questions/21487614/error-creating-r-data-table-with-date-time-posixlt
	dataFutures$TU$Date <- as.POSIXct(strptime(dataFutures$TU$Date, format = "%m/%d/%Y %H:%M:%OS", tz = "GMT")) # 5min
	size_objects("dataFutures") # 2Gb
	op_backup <- options(digits.secs = 3) # set seconds digits, by default digits.secs = 0, save orig options, TBD
	tail(dataFutures$US) # prints with milliseconds
	dataFutures$TU[, c("Time")] <- NULL # getting rid of redundant columns
	size_objects("dataFutures") # 1Gb
	str(dataFutures$US)
	head(dataFutures$US)
	summary(dataFutures$US$Close)
	(dataFuturesUS_str <- str(dataFutures$US))
	(dataFuturesUS_summary <- summary(dataFutures$US))
	dataFuturesUS_str <- glimpse(dataFutures$US)
	dataFuturesUS_head <- head(dataFutures$US)
	dataFuturesUS_head$Date <- as.character(dataFuturesUS_head$Date) # in order for xtable to work, it doesn't work with date classes
	options(op_backup) # reset options, only after the above as.character is performed, else it writes the data_head w/o milliseconds



	##################################
	### 03 - Trim data ###############
	##################################

		# Small data sample of all maturities
		# A day
		data_small_aday <- array(list(NULL), dim = dim, dimnames = list(futurenames)) # an array in which we subset the small sample
		# Loop through all four maturities
		for (i in 1:dim) {
			print(paste("Trimming", names(data[i]), "to a smaller one day span."))
			data_small_aday[[i]] <- subset(data[[i]], Time >= as.POSIXct("2019-05-20", tz = "GMT") & Time < as.POSIXct("2019-05-21", tz = "GMT"))
		}
		str(data_small_aday)
		head(data_small_aday)



###########################################################
### File: 03-Model_factor_YieldCurve-package.R ############
###########################################################

# Line 278+
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