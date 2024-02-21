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



###########################################################
### File: 03-Model_factor.R ############
###########################################################

	######################################
	### Previous code - TBD go through ###
	######################################

	    # This function needs the yield rates of different maturities to be in one variable in columns. Say rows are observations (yield rates) in time, and columns are maturities. However, I have my maturities in separate lists and of a "data.table" not an xts. How do I transform them to the needed format?

	    # To transform your data from separate lists in a "data.table" format into a single matrix or data frame where each column represents a different maturity, you can follow these steps. I'll provide a general guide, assuming you have a list of data.tables, each representing a different maturity:

	    # Combine Data.tables:

	    # First, you need to combine these separate data.tables into one. Since each data.table represents a different maturity, you'll want to join them by the observation dates (rows).
	    # Ensure Consistent Row Ordering:

	    # Before joining, make sure that each data.table has the same row ordering, typically based on dates. This ensures that each row across these tables corresponds to the same observation time.
	    # Joining Data.tables:

	    # You can use a join function, like merge in base R or join functions in the data.table package. When joining, you only want to keep the yield rate column from each maturity (data.table).
	    # Reshaping for Nelson.Siegel:

	    # The Nelson.Siegel function expects a matrix or an xts object, with rows as observations over time and columns as different maturities. After joining, you'll have a data.table with one column for each maturity. You can convert this to a matrix or an xts object.
	    # Here's a rough example in R code to illustrate these steps:

	    library(data.table)

	    # Assuming you have a list of data.tables: list_dt = [dt_1m, dt_3m, dt_6m, ...]
	    # And each data.table has columns: 'Date' and 'Yield'

	    # Step 1 & 2: Combine data.tables
	    combined_dt <- Reduce(function(x, y) merge(x, y, by = "Date"), list_dt)

	    # Step 3: Optionally, you can select only the yield rate columns
	    # combined_dt <- combined_dt[, .(Date, Yield1, Yield2, ...)]

	    # Step 4: Convert to matrix or xts
	    yield_matrix <- as.matrix(combined_dt[,-1])  # Excluding the Date column
	    # or as xts
	    yield_xts <- xts(yield_matrix, order.by = as.Date(combined_dt$Date))

	    # Now yield_matrix or yield_xts is in the format suitable for Nelson.Siegel function



	    # Replace dt_1m, dt_3m, dt_6m, etc., with your actual data.table objects, and Yield1, Yield2, etc., with the column names for yield rates in your data.tables. Remember to check that dates align correctly and handle any missing data appropriately.



	###############################################
	###### My real data ###########################
	###############################################

	    data <- dataFutures_H1

	    ls()
	    str(data)
	    head(data)
	    head(data$TU, n = 20)
	    head(data$FV, n = 20)
	    head(data$TU$Hour == data$FV$Hour, n = 20)
	    summary(data$TU$Hour == data$FV$Hour, n = 20)


	    ###################
	    # Merge data.tables
	    # Need to combine the maturities into columns of one variable, but make sure the Hours match
	    # For two tables it easy:
	    data_TUFV_sorted <- merge.data.table(data[[1]], data[[2]], by = "Hour", all = TRUE, suffixes = c("_TU", "_FV"))
	    str(data_TUFV_sorted)
	    head(data_TUFV_sorted, n = 20)

	    # For multiple data tables merging use this function
	    # https://stackoverflow.com/questions/13273833/merging-multiple-data-tables
	    # dt_list actually wants it in the form of list(df1, df2, df3...) so np, since our "data" variable is already such a list, so just pass that
	    mergeDTs <- function(dt_list, by = NULL, all = TRUE, sort = TRUE) {
	        Reduce(
	            function(...) {
	                merge(..., by = by, all = all, sort = sort)
	            }, dt_list)
	    }

	    args(mergeDTs)

	    # The false sorting puts all the NAs at the end of the object, which is useful for checking more NAs out visually at first
	    data_all_unsorted <- mergeDTs(dt_list = data, by = "Hour", all = TRUE, sort = FALSE)
	    str(data_all_unsorted)
	    head(data_all_unsorted, n = 20)
	    tail(data_all_unsorted, n = 20)

	    # But this sorted dataset is what we want
	    data_all_sorted <- mergeDTs(dt_list = data, by = "Hour", all = TRUE, sort = TRUE)
	    str(data_all_sorted)
	    head(data_all_sorted, n = 20)
	    tail(data_all_sorted, n = 20)

	    # Rename columns
	    names(data)
	    paste0("Close_", names(data))
	    names(data_all_unsorted) <- c("Hour", paste0("Close_", names(data)))
	    names(data_all_sorted) <- c("Hour", paste0("Close_", names(data)))
	    names(data_all_sorted)
	    head(data_all_sorted, n = 10)








	    ##################
	    # Generating an hour sequence to determine missing NA data
	    # TBD
	    # First we add a new fictitious list to our data. This list will span the full date range from 2006-01-02 18:00:00 to 2019-09-19 15:00:00.
	    # This is to make sure that once we merge the lists into columns, we get NAs for the hours where the data is missing.
	    # This can be different hours/rows for different maturities. The same as when we merge them without first creating a full date list.
	    # But inspect the new NAs with this method and make sure they are still just arbitrary and not full days eg.
	    # As can be seen from the str(data), each maturity/list has different number of observations, from 79616 to 79971. Lets first count
	    # the number of real hours in our new full_dates variable and compare that number to those observations.

	    # TBD generate a vector of POSIXct hours from 2006-01-02 18:00:00 to 2019-09-19 15:00:00.
	    vector_dates <- as.POSIXct(strptime("2019-12-30 05:45:00", format = "%Y-%m-%d %H:%M:%S"))
	    vector_dates

	    dates_format <- "%Y-%m-%d" #  %H:%M:%S
	    start_date <- "2006-01-02"
	    end_date <- "2006-03-05"
	    # end_date <- "2019-09-19"
	    # sequence "by =" explained, can use either "day" or "days" plural, also can use "2 days"
	    # https://stat.ethz.ch/R-manual/R-devel/library/base/html/seq.Date.html
	    sequence <- seq(from = as.Date(start_date, format = dates_format), to = as.Date(end_date, format = dates_format), by = "day")
	    class(sequence)
	    sequence

	    # Package timeDate can generate holidays etc well.
	    # https://stackoverflow.com/questions/36014742/business-day-dates-sequence-in-r
	    library(timeDate)
	    holidays <- holidayNYSE(year = c(2006:2019))
	    days_seq <- as.timeDate(seq(from = as.Date(start_date), to = as.Date(end_date), by = "day")) # https://stat.ethz.ch/R-manual/R-devel/library/base/html/seq.Date.html
	    head(days_seq, n = 20)
	    head(holidays)
	    indices_business_days <- isBizday(days_seq, holidays = holidays, wday = 1:5)
	    head(indices_business_days, n = 20)
	    head(days_seq[indices_business_days], n = 20)
	    # now this is a list of business days

	    indices_2006_01_16 <- grepl(pattern = "2006-01-16.*", data_all_sorted$Hour) # regex for only the day 2006-01-16 which is a holiday, wanna explore it
	    indices_2006_01_15_to_17 <- grepl(pattern = "2006-01-1[5-8].*", data_all_sorted$Hour) # regex for only the days 2006-01-15 to 19th, around the 16th holiday
	    str(indices_2006_01_16)
	    head(data_all_sorted[indices_2006_01_16, ])
	    str(data_all_sorted[indices_2006_01_16, ])
	    data_all_sorted[indices_2006_01_16, ]
	    data_all_sorted[indices_2006_01_15_to_17, ]
	    # Hours 16pm and 17pm are missing on 2006-01-17 and 18th, maybe between end of day trading hours and start of after hour trading?
	    # Check other days also. TBD
	    # In this case it'd mean that it trades from 18pm to 15pm. On holidays like 2006-01-16, it starts already at 18pm even tho earlier no prices.

	    # TBC
	    head(data_all_sorted[indices_business_days, ]) # TBD
	    # we need a vector of hours first
	    indices_hours <- c(0:23)
	    indices_hours

	    # join the date and the hour
	    day <- c()
	    for (i in head(indices_business_days, n = 5)) {
	        print(i)
	    }

	    # or just generate the hours from the get go? TBC
	    dates_format <- "%Y-%m-%d %H:%M:%S"
	    start_date <- c("2006", "01", "02", "00")
	    end_date <- c("2006", "01", "04", "23")
	    hours_seq <- as.timeDate(seq.POSIXt(from = ISOdate(start_date[1], start_date[2], start_date[3], start_date[4]),
	                                        to = ISOdate(end_date[1], end_date[2], end_date[3], end_date[4]), by = "hour")) # https://stat.ethz.ch/R-manual/R-devel/library/base/html/seq.POSIXt.html
	    str(hours_seq)
	    head(hours_seq, n = 20)
	    tail(hours_seq)
	    class(hours_seq)

	    days_only_seq <- as.Date(hours_seq)
	    str(days_only_seq)
	    head(days_only_seq)
	    tail(days_only_seq)
	    class(days_only_seq)

	    hours_and_days_seq <- strsplit(as.character(hours_seq), " ")
	    str(hours_and_days_seq)
	    class(hours_and_days_seq)
	    head(hours_and_days_seq)
	    hours_and_days_seq[[2]]

	    # data.table approach, splits it into two columns
	    # https://stackoverflow.com/questions/1676990/split-a-string-vector-at-whitespace
	    words_vector <- c("separate", "merge", "fuse")
	    hours_vector <- c("18:00:00", "19:00:00", "20:00:00")
	    hour_and_day_seq <- data.table(a = paste(sample(1:3, size=10, replace=TRUE), sample(hours_vector, size = 10, replace = TRUE)))
	    hour_and_day_seq
	    class(hour_and_day_seq)
	    hour_and_day_seq[, number := unlist(strsplit(x = as.character(a), split = " "))[[1]], by = a]
	    hour_and_day_seq
	    hour_and_day_seq[, word := unlist(strsplit(x = as.character(a), split = " "))[[2]], by = a]
	    hour_and_day_seq

	    # now for our real data create new columns of day and hour
	    # TBCC
	    data_excerpt_sorted <- data_all_sorted[27:32, 1]#[1:10]
	    class(data_excerpt_sorted)
	    str(data_excerpt_sorted)
	    head(data_excerpt_sorted, n = 30)
	    tail(data_excerpt_sorted)
	    class(data_excerpt_sorted[1, 1])
	    data_excerpt_sorted[, number := unlist(strsplit(x = as.character(Hour), split = " "))[[1]], by = Hour]
	    data_excerpt_sorted[, word := unlist(strsplit(x = as.character(Hour), split = " "))[[2]], by = Hour]

	    data_excerpt_sorted[, number := unlist(strsplit(x = as.character(Hour), split = "00"))[[1]], by = Hour]
	    data_excerpt_sorted[, word := unlist(strsplit(x = as.character(Hour), split = "00"))[[3]], by = Hour]
	    # There's an issue with the midnight zero hour "2006-01-03 00:00:00", as it thinks there's nothing after the space? And so the second
	    # subscript doesn't exist, and it throws error since that row. So actually the code seems to work ok, just the data is in a weirsd format
	    # for it. Delve what it is, POSIXct, mb transfer it to sth else first before doing as.character? TBD Maybe just go back to preprocessing
	    # and have the colums split for this step, so I don't even have to generate it.

	    data_excerpt_sorted$Hour
	    strsplit(as.character(data_excerpt_sorted$Hour), split = " ")
	    class(unlist(strsplit(as.character(data_excerpt_sorted$Hour), split = " "))[6])

	    as.character(data_excerpt_sorted$Hour)[3]



	    # TBCC
	    data_excerpt_sorted
	    as.character(data_excerpt_sorted$Hour)
	    unlist(strsplit(x = as.character(data_excerpt_sorted$Hour), split = " "))[[2]]


	    data_excerpt_sorted[, word := unlist(strsplit(x = as.character(data_excerpt_sorted$Hour), split = " "))[[2]], by = Hour]

	    str(unlist(strsplit(x = as.character(data_excerpt_sorted$Hour), split = " "))[[2]])



	    full_dates <- list(Dates = data$TU)
	    full_dates$Dates$Close <- c(0)
	    str(full_dates)

	    data_full_dates <- append(data, full_dates)
	    str(data_full_dates)










	    ##################
	    # Dealing with NAs
	    sum(is.na(data_all_sorted)) # all NAs
	    na_count <- sapply(data_all_sorted, function(y) length(which(is.na(y)))) # NA counts for each column
	    na_count
	    tail(data_all_unsorted, n = 100)
	    data_all_sorted[is.na(data_all_sorted)[, 5], ] # 4 NAs, 2 of which are also for TY
	    data_all_sorted[is.na(data_all_sorted)[, 4], ] # 2 NAs
	    data_all_sorted[is.na(data_all_sorted)[, 3], ][1:50, ] # 111 NAs
	    data_all_sorted[is.na(data_all_sorted)[, 2], ][1:50, ] # 357 NAs

	    # We don't see any pattern in the NAs, there's like at most 2-4 hours consecutive in one day of NAs, not like there's a whole day of NAs
	    # So we fill the NAs with the previous price (previous hour for H1), i.e. no price change.
	    (NA_rows_TU <- which(is.na(data_all_sorted)[, 2]))
	    (NA_rows_FV <- which(is.na(data_all_sorted)[, 3]))
	    (NA_rows_TY <- which(is.na(data_all_sorted)[, 4]))
	    (NA_rows_US <- which(is.na(data_all_sorted)[, 5]))
	    # TBD simplify it into one command? sth like sapply(data_all_sorted, function(y) which(is.na(y)))


	        head(which(is.na(data_all_sorted)[, 2]), n = 10)
	        head(is.na(data_all_sorted)[, 2], n = 10)
	        data_all_sorted[is.na(data_all_sorted)[, 5], 5]


	    # data.table's function "nafill" for vectors is useful here, or by reference setnafill for columns
	    # head(nafill(data_all_sorted, type = "locf"), n = 10)
	    head(data_all_sorted, n = 10)
	    setnafill(data_all_sorted, type = "locf")
	    head(data_all_sorted, n = 10)

	    na_left <- sapply(data_all_sorted, function(y) length(which(is.na(y)))) # NA counts for each column
	    na_left

	    # Manually cheacking a few of the occasions, all filled out well with the previous value
	    data_all_sorted[NA_rows_TY[1], ]
	    data_all_sorted[NA_rows_TY[1] - 1, ]
	    data_all_sorted[NA_rows_US[4], ]
	    data_all_sorted[NA_rows_US[4] - 1, ]
	    data_all_sorted[NA_rows_FV[100], ]
	    data_all_sorted[NA_rows_FV[100] - 1, ]
	    head(data_all_sorted, n = 10)
	    data_all_sorted[NA_rows_TU[100], ]
	    data_all_sorted[NA_rows_TU[100] - 1, ]

	    str(data_all_sorted)

	    dataFutures_H1_merged <- data_all_sorted

	    # Delete the variables we won't be needing
	    rm(list = setdiff(ls(), c("dataFutures", "dataFutures_M5", "dataFutures_H1", "dataFutures_H1_merged", "dataFutures_H4", "dim", "futurenames",
	                                "size_objects", "time_start", "time_end")))


	    # Save the workspace of the maturity
	    print("Saving the workspace")
	    # save.image(file = "Workspaces/Data_05_merged.RData")


	    ####### CONT MY DATA now in columns and NAs solved, dataFutures_H1_merged
	    maturity_bonds <- c(2, 5, 10, 30) # in years
	    # maturity_bonds <- c(24, 60, 120, 360) # in months, TBD not sure what the package wants, in doc they say months, in example they use years
	    # maturity_bonds <- c(17280, 43200, 86400, 259200) # TBD or should the maturity be expressed in the frequency of the data? so hours here?
	    str(dataFutures_H1_merged) # 4 maturities in columns 2:5, Hour in column 1
	    head(dataFutures_H1_merged)

	    # TBD just try to convert to XTS as is, the see if any errors, or if it possible w/o conversion already w the data.table
	    # data_xts <- as.xts(dataFutures_H1_merged) / 10 # TBD correct this, it just looks better for now
	    data_xts <- as.xts(dataFutures_H1_merged)
	    # for some reason the graph is much better for the data divided by 10, and not for the original prices
	    # prolly need to convert to yields first - TBD how??
	    str(data_xts)
	    head(data_xts)

	    # NS
	    NS_parameters <- Nelson.Siegel(rate = first(data_xts, '24 hour'), maturity = maturity_bonds)
	    str(NS_parameters)
	    head(NS_parameters, n = 10)
	    tail(NS_parameters, n = 10)
	    # TBD for some reason lambda is 2 for all rows

	    obs <- 1
	    obs <- 24
	    y <- NSrates(Coeff = NS_parameters[obs,], maturity = maturity_bonds)
	    y

	    # Plot
	    plot(maturity_bonds, first(data_xts, '24 hour')[obs,], main = "Fitting Nelson-Siegel \"price\" curve", xlab = c("Pillars in years of maturity"), type = "o") # original observed data
	    lines(maturity_bonds, y, col = 2, type = "o") # add NS fitted rates
	    legend("topleft", legend = c("observed price curve", "fitted price curve"), col=c(1,2), lty=1)
	    grid()

	    # Plot if the predicted are way off the true
	    plot(maturity_bonds, y, main = "Fitting Nelson-Siegel \"price\" curve", xlab = c("Pillars in years of maturity"), type = "o") # original observed data
	    lines(maturity_bonds, first(data_xts, '24 hour')[obs,], col = 2, type = "o") # add NS fitted rates
	    legend("topleft", legend = c("observed price curve", "fitted price curve"), col=c(1,2), lty=1)
	    grid()


	    data_to_predict <- NS_parameters




	    str(yield_curve) # 4 maturities in columns, 372 months in rows as observations
	    head(yield_curve)

	    # Notes
	    # Work with: dataFutures_train, probably window the data after the NS, as is the case currently in Model_fit.R



	##### OLDER CODE FOR JUST ONE DATASET #####

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

        # Time varying lambda
        # !!! WARNING LONG !!! Takes about 13min for the full hourly dataset of 80k obs.
        start <- time_start()
        NSParameters_lambda_varying <- Nelson.Siegel_custom_lambda_parallel(rate = data, maturity = maturities)
        time_end(start)
        beep(3) # make a sound once it finishes

        str(NSParameters_lambda_varying)
        head(NSParameters_lambda_varying)
        tail(NSParameters_lambda_varying)
        # rm(NSParameters_lambda_varying) # TBD don't need to remove once function is final

        # Fixed lambda
        # Takes about 1min for the full hourly dataset of 80k obs.
        start <- time_start()
        # 2024-01-16: ran the function call with "lambda = 7"
        NSParameters_lambda_fixed <- Nelson.Siegel_custom_lambda_parallel(rate = data, maturity = maturities, lambda = 7)
        time_end(start)
        beep(3) # make a sound once it finishes

        str(NSParameters_lambda_fixed)
        head(NSParameters_lambda_fixed)
        tail(NSParameters_lambda_fixed)
        # rm(NSParameters_lambda_fixed) # TBD don't need to remove once function is final

        # Clear the memory TBD
        gc()

        # Stop the cluster
        stopCluster(cl)


	##### OLDER CODE FOR JUST UNIVARIATE PLOTS #####

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



###########################################################
### File: 04-Model_fit.R ############
###########################################################

	#############################
	#### Factor models ##########
	#############################
		# TBD move this info - To run more scripts:
		# p_load(tidyverse) # to pipe (%>%) and map across each file
		# List files and source each - TBD y?
		# list.files("./dl-git-repo/deep-learning-for-term-structure-forecasting/Data Analysis", full.names = TRUE)# %>% map(source)


	#############################
	### Data Exploration - WIP ##
	#############################

		# IES revive tinker

		# For the prices, we explore the following.
		# Print unique values in our dataset, how many of them and the first few

		# getOption("digits") # global number of rounding digits, default is 7
		# options("digits" = 16)

		# There is only 118 unique price values in our small_US dataset of a month
		str(unique(dataFutures_train_orig), digits = 16) # original prices data
		plot(unique(dataFutures_train_orig))
		# plot(dataFutures_train_orig) # can be contrasted to the price graph, altho it takes a while to plot

		# There is only 12 unique differenced values in our small_US dataset of a month
		str(unique(dataFutures_train), digits = 16) # differenced data
		plot(unique(dataFutures_train))

		summary(dataFutures_train != 0)

		timpy <- dataFutures_train[dataFutures_train != 0]
		str(timpy)
		summary(timpy)

		hist(timpy)

		min(timpy)
		max(timpy)
		hist(timpy, breaks = 20000, xlim = c(min(timpy), max(timpy))) # !!! a great graph use TBD

		p_load(HistogramTools)

		myhist <- HistogramTools:::.BuildHistogram(timpy) # doesn't work
		plot(myhist)

		plot(timpy)
		summary(timpy == 0.03125)
		summary(timpy == -0.03125)
		summary(!(timpy == -0.03125 | timpy == 0.03125))
		timpyy <- timpy[!(timpy == -0.03125 | timpy == 0.03125)]
		summary(timpyy)
		str(timpyy)
		plot(timpyy)

		timpyyy <- timpyy[!(timpyy == -0.0625 | timpyy == 0.0625)]
		summary(timpyyy)
		str(timpyyy)
		plot(timpyyy)

		# For the yields, we explore the following.
		# TBD

		#############################
		###### Graph datasets #######
		#############################

			# USEFUL!!!
			# TBD change the labels from prices to yields. Or do we care about the prices at all? Maybe have both?

			# Graph of the split original dataset
				data_df <- data_frame(time = 1:(length(dataFutures_train_orig) + length(dataFutures_val_orig) + length(dataFutures_test_orig)),
				                 	  train = c(dataFutures_train_orig, rep(NA, length(dataFutures_val_orig) + length(dataFutures_test_orig))),
				                 	  val = c(rep(NA, length(dataFutures_train_orig)), dataFutures_val_orig, rep(NA, length(dataFutures_test_orig))),
				                 	  test = c(rep(NA, length(dataFutures_train_orig) + length(dataFutures_val_orig)), dataFutures_test_orig))
				data_df <- data_df %>% gather(key = 'train_val_test', value = 'value', -time) # -time

				custom_scale <- 1.5 # for the graph to look nice, sizes of graph items and fonts
				dataset_graph <- ggplot(data_df, aes(x = time, y = value, color = train_val_test)) +
					geom_line() +
					labs(x = "obs", y = "price") +
					# scale_x_continuous(breaks = c(1, round(seq(20000, end - 3, by = 20000), 1), split_train - 1, split_val - 2, end - 3)) +
					# scale_x_continuous(breaks = pretty_breaks(5)) +
					scale_colour_discrete(name = "Dataset", breaks = c("train", "val", "test"), labels = c("train", "validate", "test")) +
					guides(colour = guide_legend(override.aes = list(size = 4/3 * custom_scale))) +
					theme_bw(base_family = "LM Roman 10", base_size = 16 * custom_scale) +
					theme(legend.key.height = unit(1 * custom_scale, "lines"))
					# theme(legend.key.height = unit(1 * custom_scale, "lines"), axis.text.x = element_text(angle = 35, vjust = 0.8))
				dataset_graph

			# Graph of the split differenced dataset
				data_df <- data_frame(time = 1:(length(dataFutures_train) + length(dataFutures_val) + length(dataFutures_test)),
				                 	  train = c(dataFutures_train, rep(NA, length(dataFutures_val) + length(dataFutures_test))),
				                 	  val = c(rep(NA, length(dataFutures_train)), dataFutures_val, rep(NA, length(dataFutures_test))),
				                 	  test = c(rep(NA, length(dataFutures_train) + length(dataFutures_val)), dataFutures_test))

				data_df <- data_df %>% gather(key = 'train_val_test', value = 'value', -time) # -time

				custom_scale <- 1.5 # for the graph to look nice, sizes of graph items and fonts
				split_graph <- ggplot(data_df, aes(x = time, y = value, color = train_val_test)) +
					geom_line() +
					labs(x = "obs", y = "price difference") +
					# scale_x_continuous(breaks = c(1, round(seq(20000, end - 3, by = 20000), 1), split_train - 1, split_val - 2, end - 3)) +
					# scale_x_continuous(breaks = pretty_breaks(5)) +
					scale_colour_discrete(name = "Dataset", breaks = c("train", "val", "test"), labels = c("train", "validate", "test")) +
					guides(colour = guide_legend(override.aes = list(size = 4/3 * custom_scale))) +
					theme_bw(base_family = "LM Roman 10", base_size = 16 * custom_scale) +
					theme(legend.key.height = unit(1 * custom_scale, "lines"))
					# theme(legend.key.height = unit(1 * custom_scale, "lines"), axis.text.x = element_text(angle = 35, vjust = 0.8))
				split_graph

			# Save the graphs to file
				GoldenRatio <- (1 + sqrt(5)) / 2
				# plots_width <- 5.55226 # width in inches of thesis template textwidth
				plots_width <- 10 * custom_scale # 10 inches plus have nicely smallish graph elements, adjust custom scale for each graph type what looks nice
				plots_height <- plots_width / GoldenRatio

			# A day
				# ggsave(dataset_graph, filename = paste0("Graphs/Model_fit/dataset_", futurenames[future], "_aday.pdf"), device = cairo_pdf,
				# 	width = plots_width, height = plots_height, units = "in")
				# ggsave(split_graph, filename = paste0("Graphs/Model_fit/differenced_", futurenames[future], "_aday.pdf"), device = cairo_pdf,
				# 	width = plots_width, height = plots_height, units = "in")

			# A month
				ggsave(dataset_graph, filename = paste0("Graphs/Model_fit/dataset_", futurenames[future], "_amonth.pdf"), device = cairo_pdf,
					width = plots_width, height = plots_height, units = "in")
				ggsave(split_graph, filename = paste0("Graphs/Model_fit/differenced_", futurenames[future], "_amonth.pdf"), device = cairo_pdf,
					width = plots_width, height = plots_height, units = "in")

			# Save workspace
			# save.image(file = paste0("Workspaces/Data_04_split-small_", futurenames[future], "_tick_05-2019.RData")) # 1min 21MB
			
			#######################################################################################
			########### Workspaces/Data_04_split-small_US_tick_05-2019.RData ######################
			#######################################################################################


			#############################
			########## ANNs #############
			#############################

				#############################
				####### Windowing ###########
				#############################
				
				# K <- backend() # don't need this approach anymore, from Keras 2.1.2 can use k_expand_dims and k_eval
				# data_train <- K$eval(K$expand_dims(data_train, axis = 2L)) # 2L because of this https://github.com/rstudio/tensorflow/issues/190