# Master Thesis
# Benchmarking various versions of code and functions, offloading here
# v1.0 - paste previous stuff from normal code, so that code is less cluttered



################################
### Workspace setup ############
################################

	rm(list = ls())



################################
### 01-Data_conversion.R #######
################################

	################################
	### 01 - Load up data ##########
	################################

		# After loading the data.
		# Benchmarking the speed of joining two columns
			# data <- dataFutures[[1]]

			# p_load(microbenchmark)
			# p_load(stringi)
			# p_load(stringr)
			# b <- microbenchmark(
			# 	stringrS = stringr::str_c(data$Date, data$Time, sep = " "),
			# 	paste = paste(data$Date, data$Time),
			# 	paste0 = paste0(data$Date, " ", data$Time),
			# 	# stringi = stringi::stri_c(data$Date, " ", data$Time),
			# 	# stringiS = stringi::stri_c(data$Date, data$Time, sep = " "),
			# 	# stringr = stringr::str_c(data$Date, " ", data$Time),
			# 	unite = data %>% unite("DateTime", c("Date", "Time")),
			# 	times = 10
			# )
			# b
				 # Unit: seconds
				 #     expr       min        lq      mean    median       uq      max neval
				 # stringrS  7.181248  7.305488  8.816205  7.404083  7.48882 14.95292    10
				 #    paste 10.318972 10.366211 11.114709 10.430571 10.76827 16.64542    10
				 #   paste0 12.285284 12.385603 14.121135 12.461027 13.16945 20.57462    10
				 #    unite 10.309580 10.462405 15.038606 14.380144 18.89820 22.98747    10


		# Using fread() from data.table package speeds things up a lot, at least half the load times vs read.csv (see Benchmarking.R file)
		# Can also easily pick which columns to load up. And the data has the first row containing column names, hence "header = TRUE".
		# data.table = TRUE is to return a data.table instead of a dataframe, should be faster for future operations also.


	################################
	### 02 - Clean data ############ (now 02 - Data stats)
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



################################
### 03-Model_factor.R ##########
################################

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
    

	###############################################
	###### TBD CLEAN UP - SLOW NONPARALLEL ########
	###############################################

    # Passing the full dataset length
    # Takes 31.8min for the full dataset, with seq(by=1) instead of by=0.5 in the lambda maturities inside the function
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