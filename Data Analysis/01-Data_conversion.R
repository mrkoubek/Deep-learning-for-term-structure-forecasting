# Master Thesis
# Data conversion follows
# v1.5 - WIP "04 - Aggregated data" cleaned up testing chunks and polished w lapply()?



##################################
### Workspace setup ##############
##################################

	rm(list = ls())

	# Packages
	if (!require("pacman")) install.packages("pacman") # installs pacman package if not installed yet
	pacman::p_load(pacman, ggplot2, zoo, xtable, tibble, data.table, stringr, lubridate, plyr) # load packages TBD e.g. these, edit when u determine which needed/used..

	# Workspace
	setwd("E:/Google_Drive/Diploma_Thesis/Code")
	# Loading basic environment functions
	load(file = "Workspaces/00_Environment_functions.RData")
	# Loading data
	# load(file = "Workspaces/Data_01_loaded_cleaned_TUFVTYUS.RData") # takes 1min
	# load(file = "Workspaces/Data_02_stats.RData")
	load(file = "Workspaces/Data_03_trimmed-small.RData")
	print("Workspace ready set go!")



##################################
### 00 - Environment functions ###
##################################

	# Check the size of the variables
	# https://stackoverflow.com/questions/1395270/determining-memory-usage-of-objects
	size_objects <- function(objects = ls(.GlobalEnv)) {
		# TBD the sort doesn't work very well, since the format function makes it characters.
		print(sort(sapply(objects, function(x) format(object.size(get(x)), unit = "auto"))))
	}

	# The starting time, for timing how long stuff takes
	time_start <- function() {
		startTime <- Sys.time()
		print(paste0("Started loading at: ", startTime))
		return(startTime)
	}

	# The ending time and duration, for timing how long stuff takes
	time_end <- function(startTime) {
		endTime <- Sys.time()
		print(paste0("Finished loading at: ", endTime, "."))
		print(paste0("(Started loading at: ", startTime, ".)"))
		print(paste0("Time taken: ", format(endTime - startTime, units = "auto")))
	}

	# Save the workspace of basic environment functions
	# ifelse(!dir.exists("Workspaces"), dir.create("Workspaces"), "Directory already exists.") # creates a "Workspaces" directory if it doesn't exist yet
	# save.image(file = "Workspaces/00_Environment_functions.RData")

	# for size_objects delve
	# TBD the sort doesn't work very well, since the format function makes it characters.
	format(object.size(get(x)), unit = "auto"))
	object <- object.size(data)
	class(object)
	objects_list <- vector(mode = "list", length = 0)
	objects_list <- append(objects_list, object)
	objects_list
	c(object)
	class(c(object))

	objects <- c(object.size(data), object.size(dim))
	sorted <- sort(objects)
	class(sorted[1])
	format(1000, unit = "auto")
	class(format(object.size(data), unit = "auto"))
	as.numeric(format(object.size(data), unit = "auto"))



##################################
### 01 - Load up and clean data ##
##################################

	# Create path variables
	pathConversion <- file.path("E:/Diploma_Thesis_Data/Data/TickWriteOutput/Conversion_Data")
	foldernames <- list.dirs(path = pathConversion, recursive = FALSE)[5:8] # foldernames ~ paths, just the US bonds, the first 4 are the EU bonds
	foldernames <- foldernames[c(2, 1, 3, 4)] # reshuffle manually so it's according to maturity ("TU" "FV" "TY" "US"), not alphabetically
	futurenames <- list.dirs(path = pathConversion, full.names = FALSE, recursive = FALSE)[5:8] # only names of our futures w/o path
	futurenames <- futurenames[c(2, 1, 3, 4)] # reshuffle manually so it's according to maturity ("TU" "FV" "TY" "US"), not alphabetically
	dim <- length(foldernames) # number of futures

	# Create empty variable arrays
	filePaths <- vector(mode = "list", length = dim) # an empty list that has a dimension of the number of futures, the values are added later
	names(filePaths) <- futurenames # name the filePaths lists
	dataFutures <- array(list(NULL), dim = dim, dimnames = list(futurenames)) # an array in which we'll save all our data

	# Construct paths of files (for each maturity)
	for (i in 1:dim) {
		filePaths[[i]] <- paste0(foldernames[i], "/", futurenames[i], ".csv")
	}
	filePaths

	# Load the data from Data location
	# Select which columns we need
	columns_to_select <- c("Date", "Time", "Open", "Close")

	# Set the RAM limit higher, but make sure the virtual RAM in the OS is also allowed. And might be better to set it manually higher vs auto.
	# In Windows have it swap to HDD +30GB (altho +20GB would do). I set mine to min10GB-max50GB.
	# https://www.programmingr.com/r-error-messages/cannot-allocate-vector-of-size/
	# How To Manage Virtual Memory (Pagefile) In Windows 10:
	# https://www.tomshardware.com/news/how-to-manage-virtual-memory-pagefile-windows-10,36929.html
	# In elevated Windows CMD, "wmic pagefile list /format:list" shows info about the page file.
	# To see the file in C:/ root, uncheck the option in file explorer options that says "Hide protected operating system files (Recommended)."
	# Deprecated from R Version 4.2.0 (2022-04-22), no longer needed to specify it in R:
		# memory.limit()
		# memory.limit(size = 60000)

	# MAIN CLEANING LOOP
	# Cleans the data so it takes less space, combine two columns
	clean_data <- function(data) {
		# Set to show milliseconds
		op_backup <- options("digits.secs") # save original options
		options(digits.secs = 3) # set seconds digits to have milliseconds, by default digits.secs = 0

		data$Time <- stringr::str_c(data$Date, data$Time, sep = " ") # combine Date and Time columns, stringr is the fastes from our testing (see Benchmarking.R file), since we're joining characters
		data[, c("Date")] <- NULL # getting rid of redundant columns


		# data.table does not support POSIXlt (which is result of only the strptime()) data types for performance reason, use POSIXct or ITime
		# https://stackoverflow.com/questions/21487614/error-creating-r-data-table-with-date-time-posixlt
		# data$Date <- as.POSIXct(strptime(data$Date, format = "%m/%d/%Y %H:%M:%OS", tz = "GMT")) # 1min
		# Improving upon strptime, which is the main bottleneck here, we use the lubridate's fast_strptime, about 3x faster!
		data$Time <- as.POSIXct(fast_strptime(data$Time, format = "%m/%d/%Y %H:%M:%OS", tz = "GMT")) # 1min

		# Have a look at the data
		print(str(data))
		print(head(data))
		print(tail(data))
		# summary(data$Close)
		options(op_backup) # reset options, only after the above as.character is performed, else it writes the data_head w/o milliseconds

		return(data)
	}

	# Using fread() from data.table package speeds things up a lot, at least half the load times vs read.csv (see Benchmarking.R file)
	# Can also easily pick which columns to load up. And the data has the first row containing column names, hence "header = TRUE".
	# data.table = TRUE is to return a data.table instead of a dataframe, should be faster for future operations also.

	# MAIN LOADING LOOP
	# Load up the datasets, with cleaning one by one
	# TU 4min, FV 13min, TY 25min, US 13min
	for (i in 1:dim) {
		startTime <- time_start() # time the execution
		print(paste("Started loading", filePaths[[i]]))

		dataFutures[[i]] <- fread(filePaths[[i]], select = columns_to_select, header = TRUE, data.table = TRUE) # main load, takes a few mins
		
			print("Size before cleaning")		
			size_objects("dataFutures")

			print("Cleaning the dataset")
			dataFutures[[i]] <- clean_data(dataFutures[[i]])

			print("Size after cleaning")
			size_objects("dataFutures") # TU 2.3 -> 1.3, FV 6.2 -> 3.5, TY 9.8 -> 5.9, US 6.3 -> 3.6

		# Save the workspace of the maturity
		print("Saving the workspace")
		save.image(file = paste0("Workspaces/Data_01_loaded_cleaned_", futurenames[i], ".RData"))

		# reinstate the empty variable, so it fast for each maturity
		dataFutures <- array(list(NULL), dim = dim, dimnames = list(futurenames)) # an array in which we'll save all our data
		print(gc()) # garbage collection to free up memory before the next round of the loop, TBD not necessary on my rig

		time_end(startTime)
	}

	# Close R so all the memory is freed up. Probably optional, maybe the above gc() is enough. Also close Sublime since can't restart REPL well.
	q("no")

	# Merge the workspaces
	maturities <- c("TU", "FV", "TY", "US")
	data <- array(list(NULL), dim = dim, dimnames = list(futurenames)) # an array in which we'll merge our data

	# MAIN MATURITIES MERGING LOOP
	# We load up each maturity's workspace one by one, and append that dataset into a new list of one variable "data".
	# Takes 1min.
	for (i in 1:dim) {
		print(paste("Loading workspace for", maturities[i]))
		load(file = paste0("Workspaces/Data_01_loaded_cleaned_", maturities[i], ".RData")) # TBD this loads up the startTime variable, so can't time this for loop
		data[[i]] <- dataFutures[[i]]
		size_objects("data")
		rm(dataFutures) # remove to make space
		print(gc()) # garbage collection to free up memory before the next round of the loop, TBD not necessary on my rig
		if(i == dim) print("Done merging all maturities.")
	}

	str(data)
	size_objects("data") # the vector of all cleaned maturities is 15Gb (before writing the clean_data function it was 25Gb)

	# Save the workspace of all maturities in one variable
	# print("Saving the workspace")
	# startTime <- time_start()
	# save.image(file = paste0("Workspaces/Data_01_loaded_cleaned_TUFVTYUS.RData")) # takes 3min, 800MB
	# time_end(startTime)



##################################
### 02 - Data stats ##############
##################################

	options("digits.secs") # see what it's set at
	op_backup <- options("digits.secs") # save original options
	options(digits.secs = 3) # set seconds digits to have milliseconds, by default digits.secs = 0 or NULL

	# Statistics - TBD CLEAN UP
	str(data)
	str(data$US)
	nobs <- lapply(data, lengths) # number of observations in each maturity
	nobs_sum <- sum(sapply(data, function(x) sum(lengths(x)))) # sum of all the observations
	lapply(nobs, function(x) paste0(format(round(x / 1e6, 1), trim = TRUE), "m"))
	paste0("All maturities, number of observations: ", format(round(nobs_sum / 1e9, 1), trim = TRUE), "bn")

	# US (30Y maturity) stats
	(data_US_str <- capture.output(str(data$US)))
	(data_US_head <- head(data$US))
	(data_US_tail <- tail(data$US))
	(data_US_summary <- summary(data$US))

	data_US_head$Time <- as.character(data_US_head$Time) # in order for xtable to work on the Time, it doesn't work with date classes

	print(xtable(data_US_summary, caption = "US 30-Year bond summary statistics"),
		type = "latex", file = paste0("Results/Tables/data_US_summary.tex"), include.rownames = FALSE)

	print(xtable(data_US_head, caption = "US 30-Year bond data glimpse"),
		type = "latex", file = paste0("Results/Tables/data_US_head.tex"), include.rownames = FALSE)

	print(xtable(data_US_tail, caption = "US 30-Year bond data glimpse"),
		type = "latex", file = paste0("Results/Tables/data_US_tail.tex"), include.rownames = FALSE)

	rm(data_US_str, data_US_summary, data_US_head, data_US_tail)

	# All maturities stats
	(data_str <- str(data))
	(glimpse(data))
	(data_head <- capture.output(head(data)))

	options(op_backup) # reset options, only after the above is performed, else it writes the data_head etc w/o milliseconds

	print(xtable(data_summary, caption = "US 30-Year bond summary statistics"),
		type = "latex", file = paste0("Results/Tables/data_summary.tex"), include.rownames = FALSE)

	print(xtable(data_head, caption = "US 30-Year bond data glimpse"),
		type = "latex", file = paste0("Results/Tables/data_head.tex"), include.rownames = FALSE)

	rm(data_summary, data_str, data_head)

	# Save the data_summary after cleaning
	print(xtable(data_summary, caption = "US 30-Year bond summary statistics, tick data frequency"),
		type = "latex", file = paste0("Results/Tables/data_summary_US_tick.tex"), include.rownames = FALSE)

	# Save the env
	# The following workspace image takes 5?min to save and is 580MB
	# save.image(file = "Workspaces/Data_02_stats.RData") # save the workspace



##################################
### 03 - Trim data ###############
##################################

	# Load all the maturities workspace
	# load(file = "Workspaces/Data_01_loaded_cleaned_TUFVTYUS.RData")

	options("digits.secs") # see what it's set at
	op_backup <- options("digits.secs") # save original options
	options(digits.secs = 3) # set seconds digits to have milliseconds, by default digits.secs = 0 or NULL

	str(data) # Number of observations (for each variable and maturity): TU 59mm, FV 158mm, TY 264mm, US 161mm
	head(data) # 2006-01-02 18:00
	tail(data) # 2019-09-19 15:59

	# We leave the full dataset for the main analysis, despite the end being weirdly mid September, that's just when the database was acquired.

	# TBD Winsorize outliers if any, treat missing data?

	# Small data samples follow
	# One maturity only
	colnames(data$TU)

	# Take an excerpt from the data to easily work on/visualise, e.g. only the day 20th May 2019 or the month May 2019:
	data_TU_small_aday <- subset(data$TU, Time >= as.POSIXct("2019-05-20", tz = "GMT") & Time < as.POSIXct("2019-05-21", tz = "GMT"))
	data_TU_small_amonth <- subset(data$TU, Time >= as.POSIXct("2019-05-01", tz = "GMT") & Time < as.POSIXct("2019-06-01", tz = "GMT"))

	# A day
		str(data_TU_small_aday) # 11k observations
		head(data_TU_small_aday)

		# Exploratory analysis, polish graph TBD
		plot.ts(x = data_TU_small_aday$Time, y = data_TU_small_aday$Close, type = "l")
		summary(data_TU_small_aday)

	# A month
		str(data_TU_small_amonth) # 379k observations
		head(data_TU_small_amonth)

		# Exploratory analysis, polish graph TBD
		plot.ts(x = data_TU_small_amonth$Time, y = data_TU_small_amonth$Close, type = "l")
		summary(data_TU_small_amonth)

	# Small data sample of all maturities
	# A day
		data_small_aday <- array(list(NULL), dim = dim, dimnames = list(futurenames)) # an array in which we subset the small sample
		# Loop through all four maturities
		data_small_aday <- lapply(data, function(x) subset(x, Time >= as.POSIXct("2019-05-20", tz = "GMT") & Time < as.POSIXct("2019-05-21", tz = "GMT")))
		str(data_small_aday)
		head(data_small_aday)

	# A month
		data_small_amonth <- array(list(NULL), dim = dim, dimnames = list(futurenames)) # an array in which we subset the small sample
		# Loop through all four maturities
		data_small_amonth <- lapply(data, function(x) subset(x, Time >= as.POSIXct("2019-05-01", tz = "GMT") & Time < as.POSIXct("2019-06-01", tz = "GMT")))
		str(data_small_amonth)
		head(data_small_amonth)

	# Exploratory analysis, polish graph TBD
	m <- 4 # select which maturity to plot
	plot.ts(x = data_small_amonth[[m]]$Time, y = data_small_amonth[[m]]$Close, type = "l")
	summary(data_small_amonth[[m]])

	rm("data") # for the Data_03_trimmed-small RDatas to be smaller

	options(op_backup) # reset options, only after the above is performed, else it writes the data_head etc w/o milliseconds

	# Save the workspace
	# The following takes 0min to save and is 6MB
	# save.image(file = "Workspaces/Data_03_trimmed-small.RData")



##################################
### 04 - Aggregate data ##########
##################################

	# Load all the maturities
	load(file = "Workspaces/Data_03_trimmed-small.RData") # only the small data excerpts
	# load(file = "Workspaces/Data_03_trimmed_TUFVTYUS.RData") # the full dataset

	# First aggregating the small trimmed data of one day/month, then scale up
	# First try it on one maturity, then scale up to all maturities
	ls()
	data_small <- data_TU_small_aday
	# data_small <- data_TU_small_amonth
	str(data_small)
	head(data_small, n = 50)

	# TBD this "cut" sets the first (zeroth) hour as 2019-05-20, without the hour at all, instead of 2019-05-20 00:00:00
	# It creates 24 factor levels, the first one being w/o the hour.
	data_small$Hour <- cut(data_small$Time, breaks = "1 hour")
	str(data_small)
	head(data_small, n = 50)
	tail(data_small, n = 30)
	summary(data_small$Hour == "2019-05-20 08:00:00")

		# TBD clean up testing
		# https://stackoverflow.com/questions/13649019/split-time-series-data-into-time-intervals-say-an-hour-and-then-plot-the-count
		set.seed(1)
		MyDates <- ISOdatetime(2012, 1, 1, 0, 0, 0, tz = "GMT") + sample(1:27000, 500)
		MyDates <- ISOdatetime(2012, 1, 1, 0, 0, 0, tz = "GMT") + sample(1:86400*2, 500)
		head(MyDates)
		str(MyDates)

		# Define start and end times for the breaks
		start_time <- as.POSIXct("2012-01-01 00:00:00", tz="GMT")
		end_time <- as.POSIXct("2012-01-03 00:00:00", tz="GMT")

		# Create hourly breaks
		breaks <- seq(from=start_time, to=end_time, by="hour")

		# Format breaks as strings
		labels <- format(breaks, "%Y-%m-%d %H:%M:%S")
		labels <- labels[-length(labels)] # Remove the last label as it is not needed

		# Use cut with these breaks and labels
		MyDatesCut <- cut(MyDates, breaks = breaks,  include.lowest = FALSE, right = TRUE)

		# Check the result
		str(MyDatesCut)
		levels(MyDatesCut)
		head(MyDatesCut)
		MyDatesCut <- as.POSIXct(fast_strptime(as.character(MyDatesCut), format = "%Y-%m-%d %H:%M:%OS", tz = "GMT"))

		# Summarize the hour counts to a table
		MyDatesTable <- table(MyDatesCut)
		data.frame(MyDatesTable)

		plot(MyDatesTable, type="l", xlab="Time", ylab="Freq")

		# For 30 minute breaks sth like this
		data.frame(table(cut(MyDates, breaks = "30 mins")))



	# then use ddply (from plyr package), splitting the data frame by that cut variable
	data_small_hourly <- ddply(data_small, .(Hour), summarize, Close = tail(Close, n = 1))
	# data.table equivalent to ddply: https://stackoverflow.com/questions/50854040/r-aggregate-second-data-to-minutes-more-efficient
	data_small_hourly <- data_small[ , .(Close = tail(Close, n = 1)), by = Hour]

	str(data_small_hourly)
	head(data_small_hourly)
	tail(data_small_hourly)


	# MISSING DATA - weird that nothing in the 16th hour, TBD need to solve that, missing data
	head(data_small_hourly)
	str(data_small)
	data_small[10452, ] # hour 15:59:58
	data_small[10453, ] # hour 17:00:00, skips trading from 16-17h??? TBD, can be seen even in the one day tick graph in chapter 03


	# WIP
	dim <- 1
	par(mfrow = c(dim, 1))
	plot(data_small$Time, data_small$Close, type = "l", main = paste("TU", "2019-05-20"), xlab = "Time", ylab = "Price")
	plot(data_small_hourly$Hour, data_small_hourly$Close, type = "l", main = paste("TU", "2019-05-20"), xlab = "Time", ylab = "Price")


		# Test TBD CLEAN UP
		# Start and end for the manual breaks
		breaks_start_time <- as.POSIXct("2019-05-01 00:00:00", tz="GMT")
		breaks_end_time <- as.POSIXct("2019-05-31 15:59:59", tz="GMT")

		# (start_time <- data_small_amonth[[1]]$Time[1])
		# (rounded_start_hour <- trunc(start_time, "hours"))
		# class(rounded_start_hour)
		# rounded_start_hour <- as.POSIXct(fast_strptime(as.character(rounded_start_hour), format = "%Y-%m-%d", tz = "GMT"))
		# rounded_start_hour

		# Create hourly breaks
		breaks_hourly <- seq(from = breaks_start_time, to = breaks_end_time, by = "hour")

		# Format breaks as strings
		labels_breaks <- format(breaks_hourly, "%Y-%m-%d %H:%M:%S")
		# labels_breaks <- labels_breaks[-length(labels_breaks)] # removes the last label when it's outside of our data

		dataFutures_small_H1[[1]]$Hour <- cut(data_small_amonth[[1]]$Time, breaks = "1 hour", labels = labels_breaks) # replace data w/ aggregated hourly data


	# On the full Futures list, first the small one month
	dataFutures_small_Tick <- data_small_amonth # save the original tick data into its appropriate name

	dim <- 4
	dataFutures_small_H1 <- data_small_amonth

	# Start and end for the manual breaks in cut()
	breaks_start_time <- as.POSIXct("2019-05-01 00:00:00", tz="GMT")
	breaks_end_time <- as.POSIXct("2019-05-31 15:59:59", tz="GMT")

	# Create hourly breaks
	breaks_hourly <- seq(from = breaks_start_time, to = breaks_end_time, by = "hour")
	# Format breaks
	labels_breaks <- format(breaks_hourly, "%Y-%m-%d %H:%M:%S")
	str(labels_breaks)

	for (i in 1:dim) {
		print(paste("Aggregating", names(data_small_amonth[i]), "to be hourly data"))
		# Replace data w/ aggregated hourly data
		dataFutures_small_H1[[i]]$Hour <- cut(data_small_amonth[[i]]$Time, breaks = "1 hour", labels = labels_breaks)
		# Then use ddply (from plyr package), splitting the data frame by that cut variable:
		# dataFutures_small_H1[[i]] <- as.data.table(ddply(dataFutures_small_H1[[i]], .(Hour), summarize, Close = tail(Close, n = 1)))
		# Or the data.table equivalent to ddply: https://stackoverflow.com/questions/50854040/r-aggregate-second-data-to-minutes-more-efficient
		dataFutures_small_H1[[i]] <- dataFutures_small_H1[[i]][ , .(Close = tail(Close, n = 1)), by = Hour]
		dataFutures_small_H1[[i]]$Hour <- as.POSIXct(fast_strptime(as.character(dataFutures_small_H1[[i]]$Hour), format = "%Y-%m-%d %H:%M:%OS", tz = "GMT"))
	}

	str(dataFutures_small_H1)
	head(dataFutures_small_H1)
	head(data_small_amonth) # comparing to this manually, it seems to be without a flaw



	# Load all the maturities workspace
	# load(file = "Workspaces/Data_01_loaded_cleaned_TUFVTYUS.RData")	

	# On the full Futures list, full length
	dataFutures_Tick <- data # save the original tick data into its appropriate name

	# Start and end for the manual breaks in cut()
	head(data) # 2006-01-02 18:00
	tail(data) # 2019-09-19 15:59
	breaks_start_time <- as.POSIXct("2006-01-02 18:00:00", tz="GMT")
	breaks_end_time <- as.POSIXct("2019-09-19 15:59:59", tz="GMT")

	# Create 5 minute breaks
	breaks_5min <- seq(from = breaks_start_time, to = breaks_end_time, by = "5 min")
	head(breaks_5min)
	tail(breaks_5min)
	# Format 5 minute breaks
	labels_breaks_5min <- format(breaks_5min, "%Y-%m-%d %H:%M:%S")
	str(labels_breaks_5min)
	head(labels_breaks_5min)
	tail(labels_breaks_5min)

	# Create hourly breaks
	breaks_hour <- seq(from = breaks_start_time, to = breaks_end_time, by = "hour")
	head(breaks_hour)
	tail(breaks_hour)
	# Format hourly breaks
	labels_breaks_hour <- format(breaks_hour, "%Y-%m-%d %H:%M:%S")
	str(labels_breaks_hour)
	head(labels_breaks_hour)
	tail(labels_breaks_hour)

	# Create 4H breaks
	breaks_4H <- seq(from = breaks_start_time, to = breaks_end_time, by = "4 hour")
	head(breaks_4H)
	tail(breaks_4H)
	# Format 4H breaks
	labels_breaks_4H <- format(breaks_4H, "%Y-%m-%d %H:%M:%S")
	str(labels_breaks_4H)
	head(labels_breaks_4H)
	tail(labels_breaks_4H)

	# Create 1D breaks
	# Add 2 hours to the last one, to include the last day, since it ends at 4pm.
	breaks_1D <- seq(from = breaks_start_time, to = breaks_end_time + (2*3600 + 1), by = "1 day")
	head(breaks_1D)
	tail(breaks_1D)
	# Format 1D breaks
	labels_breaks_1D <- format(breaks_1D, "%Y-%m-%d")
	str(labels_breaks_1D)
	head(labels_breaks_1D)
	tail(labels_breaks_1D)

	# Might be faster to do a M5 first, then H1 from it, but it fast and small enough like this anyway (a few minutes), no need to optimise

	dataFutures_M5 <- data
	for (i in 1:dim) { # 4min, 54Mb
		print(paste("Aggregating", names(data[i]), "to be 5 minute data"))
		dataFutures_M5[[i]]$Minute <- cut(data[[i]]$Time, breaks = "5 min", labels = labels_breaks_5min) # replace data w/ aggregated 5min data
		# dataFutures_M5[[i]] <- as.data.table(ddply(dataFutures_M5[[i]], .(Hour), summarize, Close = tail(Close, n = 1)))
		# data.table equivalent to ddply:
		dataFutures_M5[[i]] <- dataFutures_M5[[i]][ , .(Close = tail(Close, n = 1)), by = Minute]
		dataFutures_M5[[i]]$Minute <- as.POSIXct(fast_strptime(as.character(dataFutures_M5[[i]]$Minute), format = "%Y-%m-%d %H:%M:%OS", tz = "GMT"))
	}

	str(dataFutures_M5)
	head(dataFutures_M5)
	# TU M5 it starts with 18:01 instead of 18:00, np for 5min data, for 1min it'd be an issue

	dataFutures_H1 <- data
	for (i in 1:dim) { # 2min, 5Mb
		print(paste("Aggregating", names(data[i]), "to be hourly data"))
		dataFutures_H1[[i]]$Hour <- cut(data[[i]]$Time, breaks = "1 hour", labels = labels_breaks_hour) # replace data w/ aggregated hourly data
		# dataFutures_H1[[i]] <- as.data.table(ddply(dataFutures_H1[[i]], .(Hour), summarize, Close = tail(Close, n = 1)))
		# data.table equivalent to ddply:
		dataFutures_H1[[i]] <- dataFutures_H1[[i]][ , .(Close = tail(Close, n = 1)), by = Hour]
		dataFutures_H1[[i]]$Hour <- as.POSIXct(fast_strptime(as.character(dataFutures_H1[[i]]$Hour), format = "%Y-%m-%d %H:%M:%OS", tz = "GMT"))
	}

	str(dataFutures_H1)
	head(dataFutures_H1)

	dataFutures_H4 <- data
	for (i in 1:dim) { # 4min, 1Mb
		print(paste("Aggregating", names(data[i]), "to be 4 hour data"))
		dataFutures_H4[[i]]$Hour <- cut(data[[i]]$Time, breaks = "4 hour", labels = labels_breaks_4H) # replace data w/ aggregated hourly data
		# dataFutures_H4[[i]] <- as.data.table(ddply(dataFutures_H4[[i]], .(Hour), summarize, Close = tail(Close, n = 1)))
		# data.table equivalent to ddply:
		dataFutures_H4[[i]] <- dataFutures_H4[[i]][ , .(Close = tail(Close, n = 1)), by = Hour]
		dataFutures_H4[[i]]$Hour <- as.POSIXct(fast_strptime(as.character(dataFutures_H4[[i]]$Hour), format = "%Y-%m-%d %H:%M:%OS", tz = "GMT"))
	}

	str(dataFutures_H4)
	head(dataFutures_H4)

	dataFutures_D1 <- data
	for (i in 1:dim) { # 4min, 1Mb
		print(paste("Aggregating", names(data[i]), "to be daily data"))
		dataFutures_D1[[i]]$Day <- cut(data[[i]]$Time, breaks = "1 day", labels = labels_breaks_1D) # replace data w/ aggregated hourly data
		# dataFutures_D1[[i]] <- as.data.table(ddply(dataFutures_D1[[i]], .(Day), summarize, Close = tail(Close, n = 1)))
		# data.table equivalent to ddply:
		dataFutures_D1[[i]] <- dataFutures_D1[[i]][ , .(Close = tail(Close, n = 1)), by = Day]
		dataFutures_D1[[i]]$Day <- as.POSIXct(fast_strptime(as.character(dataFutures_D1[[i]]$Day), format = "%Y-%m-%d", tz = "GMT"))
	}

	str(dataFutures_D1)
	head(dataFutures_D1)

	size_objects()

	# Delete the variables we won't be needing
	rm(list = setdiff(ls(), c("dataFutures", "dataFutures_M5", "dataFutures_H1", "dataFutures_H4", "dataFutures_D1", "dim", "futurenames",
								"size_objects", "time_start", "time_end")))


	# Save the workspace
	# The following takes 0min to save and is 6MB
	# save.image(file = "Workspaces/Data_04_aggregated-small_amonth_TU.RData")
	save.image(file = "Workspaces/Data_04_aggregated_TUFVTYUS.RData")



	# TBD plot all maturities
	# Plotting the data again
	dim <- 4
	par(mfrow = c(dim, 1))
	for (i in 1:dim) {
		plot(dataFutures_D1[[i]]$Day, dataFutures_D1[[i]]$Close, type = "l", main = paste(futurenames[i], "D1 2006-2019"), xlab = "Time", ylab = "Price")
	}



##################################
### END RESULT ###################
##################################

	# TBD CONT
	load(file = "Workspaces/Data_04_aggregated_TUFVTYUS.RData")



##################################
### Procedure:####################
##################################
	# rows observations, say aggregate from tick to 5min data, save each workspace or together if small enough
	# gradually load workspaces and merge columns maturities 2, 5, 10, 30Y, delete unused variables
	# cut data for say 1 month or 1Y of data depending on size, so it is manageable for tests
	# try Nelson.Siegel()
	# rewrite Nelson.Siegel() and literature into my own code DNS fitting
	# redo NNs input structure so the output from DNS can be fit and predicted by NNs
	# try NNs onto the multivariable time series of DNS coefficeints
	# transform back the coefficients into prices predicted
	# evaluate



##################################
### WIP Play #####################
##################################
	dates <- c("02/27/92", "02/27/92", "01/14/92", "02/28/92", "02/01/92")
	times <- c("23:03:20", "22:29:56", "01:03:30", "18:21:03", "16:56:26")
	x <- paste(dates, times)
	strptime(x, "%m/%d/%y %H:%M:%S")



	# R is smart so reassigning a variable doesn't create a copy, https://stackoverflow.com/questions/22951811/how-to-rename-a-variable-in-r-without-copying-the-object
	tracemem(dataFutures[[4]])
	tracemem(data[[4]])


	# Profiling
	p_load(profr)
	x <- profr(example(glm))
	plot(x)

	y <- profr(
		clean_data(dataFutures[[1]])
		)
	plot(y)

	Rprof(tmp <- tempfile())
	example(glm)
	Rprof()
	summaryRprof(tmp)

	Rprof(tmp <- tempfile())
		clean_data(dataFutures[[1]])
	Rprof()
	summaryRprof(tmp)



##################################
### TO DO ########################
##################################

	# Revive:
		# clean up code formatting - mark, then search all the "TBD CLEANUP" and deal with it
		# put away snippets of code that are not being used, set up a new file "unused functions" or something to offload there
		# run thru the code to make sure it is all MWE working
		# continue w details and polish a MWE

	# Old ideas:
		# MWE graph
		# polish an exploratory graph axes
		# convert the data to xts?


