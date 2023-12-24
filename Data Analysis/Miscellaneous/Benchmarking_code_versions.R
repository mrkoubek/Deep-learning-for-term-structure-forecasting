# Master Thesis
# Benchmarking various versions of code and functions, offloading here
# v1.0 - paste previous stuff from normal code, so that code is less cluttered



################################
### Workspace setup ############
################################

	rm(list = ls())



################################
### 01-Data_conversion #########
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