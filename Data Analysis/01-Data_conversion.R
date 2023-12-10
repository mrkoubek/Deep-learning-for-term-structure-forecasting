# Master Thesis
# Data conversion follows
# v1.3 - revive, clean up code formatting, and start prep for taking out unnecessary snippets



################################
### Workspace setup ############
################################

	rm(list = ls())

	# Packages
	if (!require("pacman")) install.packages("pacman") # installs pacman package if not installed yet
	pacman::p_load(pacman, ggplot2, zoo, xtable, tibble, data.table, stringr, lubridate, plyr) # load packages TBD e.g. these, edit when u determine which needed/used..

	# Workspace
	setwd("E:/Google_Drive/Diploma_Thesis/Code")
	# Loading basic environment functions
	load(file = "Workspaces/00_Environment_functions.RData")
	# Loading data
	# load(file = "Workspaces/Data_01_loaded_TU.RData") # takes 8min!! we load the latest version of a prepared workspace

	# load(file = "Workspaces/tmp_Data_01_loaded_test.RData") # takes 8min!! we load the latest version of a prepared workspace
	# load(file = "Workspaces/Data_01_loaded_TUFVTYUS.RData") # takes 8min!! we load the latest version of a prepared workspace
	load(file = "Workspaces/Data_01_loaded_cleaned_TUFVTYUS.RData") # takes ?min!! we load the latest version of a prepared workspace

	# load(file = "Workspaces/Data_02_cleaned_US.RData") # we load the latest version of a prepared workspace
	# load(file = "Workspaces/Data_03_trimmed-small_US.RData") # we load the latest version of a prepared workspace
	print("Workspace ready set go!")



################################
### 00 - Environment functions #
################################

	# Check the size of the variables
	# https://stackoverflow.com/questions/1395270/determining-memory-usage-of-objects
	size_objects <- function(objects = ls(.GlobalEnv)) {
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
	# save.image(file = "Workspaces/00_Environment_functions.RData")



################################
### 01 - Load up data ##########
################################

	# New, finish:
	# Preparation of variables
	pathConversion <- file.path("E:/Diploma_Thesis_Data/Data/TickWriteOutput/Conversion_Data")
	foldernames <- list.dirs(path = pathConversion, recursive = FALSE)[5:8] # foldernames ~ paths, just the US bonds, the first 4 are the EU bonds
	foldernames <- foldernames[c(2, 1, 3, 4)] # reshuffle manually so it's according to maturity ("TU" "FV" "TY" "US"), not alphabetically
	futurenames <- list.dirs(path = pathConversion, full.names = FALSE, recursive = FALSE)[5:8] # only names of our futures w/o path
	futurenames <- futurenames[c(2, 1, 3, 4)] # reshuffle manually so it's according to maturity ("TU" "FV" "TY" "US"), not alphabetically
	dim <- length(foldernames) # number of futures

	# Empty variables
	# filenames <- vector(mode = "list", length = dim) # create an empty list that has a dimension of the number of futures, the values are added later
	# names(filenames) <- futurenames # name the filenames lists, prob useless
	filePaths <- vector(mode = "list", length = dim) # TBD optimise to one variable instead of 3 foldernames, filenames, filePaths put in one list or sth
	names(filePaths) <- futurenames
	# bindedFile <- vector(mode = "list", length = dim) # TBD
	dataFutures <- array(list(NULL), dim = dim, dimnames = list(futurenames)) # an array in which we'll save all our data

	# Construct paths of files (for each maturity)
	for (i in 1:dim) {
		filePaths[[i]] <- paste0(foldernames[i], "/", futurenames[i], ".csv")
	}
	filePaths

	# Load the data from Data location
	# Select which columns we need
	columns_to_select <- c("Date", "Time", "Open", "Close")

	# Set the RAM limit higher, but make sure the virtual RAM in the OS is also allowed
	# In Windows have it swap to HDD +30GB (altho +20GB would do) and set this in R, https://www.programmingr.com/r-error-messages/cannot-allocate-vector-of-size/
	memory.limit()
	memory.limit(size = 60000)

	# Using fread() from data.table package speeds things up a lot, at least half the load times vs read.csv
	# Can also easily pick which columns to load up. And the data has the first row containing column names, hence "header = TRUE".
	# data.table = TRUE is to return a data.table instead of a dataframe, should be faster for future operations also.

	# startTime <- time_start()
	# dataFutures[[3]] <- fread(filePaths[[3]], select = columns_to_select, header = TRUE, data.table = TRUE) # takes Xmin
	# time_end(startTime)


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

	# Cleans the data so it takes less space
	clean_data <- function(data) {
		# Set to show milliseconds
		op_backup <- options("digits.secs") # set seconds digits to have milliseconds, by default digits.secs = 0, save orig options, TBD
		options(digits.secs = 3) # set seconds digits to have milliseconds, by default digits.secs = 0, save orig options, TBD

		data$Time <- stringr::str_c(data$Date, data$Time, sep = " ") # combine Date and Time columns, stringr is the fastes from our testing above, since we're joining characters
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


	# Load up the datasets
	for (i in 1:1) {
		startTime <- time_start() # time the execution
		print(paste("Started loading", filePaths[[i]]))

		dataFutures[[i]] <- fread(filePaths[[i]], select = columns_to_select, header = TRUE, data.table = TRUE) # main load, takes a few mins
		
		time_end(startTime)
		size_objects("dataFutures")
	}


	# Load up the datasets - NEW function w cleaning
	# TU 2min, FV 7min, TY 21min, US 7min
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

		time_end(startTime)
	}


	# Merge the workspaces
	maturities <- c("TU", "FV", "TY", "US")
	data <- array(list(NULL), dim = dim, dimnames = list(futurenames)) # an array in which we'll merge our data

	for (i in 1:dim) {
		print(paste("Loading workspace for", maturities[i]))
		load(file = paste0("Workspaces/Data_01_loaded_cleaned_", maturities[i], ".RData")) # we load the basic functions first, then in addition latest ones below:
		data[[i]] <- dataFutures[[i]]
		size_objects("data")
		rm(dataFutures)
		if(i == dim) print("Done merging all maturities.")
	}

	str(data)
	size_objects("data") # the vector of all cleaned maturities is 15Gb (before cleaning it was 25Gb)

	# Save the workspace of all maturities in one variable
	# print("Saving the workspace")
	# save.image(file = paste0("Workspaces/Data_01_loaded_cleaned_TUFVTYUS.RData"))







	# Save the env
	ifelse(!dir.exists("Workspaces"), dir.create("Workspaces"), "Directory already exists.") # creates a "Workspaces" directory if it doesn't exist yet
	startTime <- time_start()
	# save.image(file = "Workspaces/Data_01_loaded_TUFVTYUS.RData") # save the workspace, takes 8min
	time_end(startTime)





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

	# Statistics
	str(dataFutures$US)
	(data_US_str <- capture.output(str(dataFutures$US)))
	(data_US_head <- head(dataFutures$US))
	tail(dataFutures$US)
	dataFutures$US[1000, ]
	(data_US_summary <- summary(dataFutures$US))

	print(xtable(data_US_summary, caption = "US 30-Year bond summary statistics"),
		type = "latex", file = paste0("Results/Tables/data_US_summary.tex"), include.rownames = FALSE)

	print(xtable(data_US_head, caption = "US 30-Year bond data glimpse"),
		type = "latex", file = paste0("Results/Tables/data_US_head.tex"), include.rownames = FALSE)

	rm(data_US_str, data_US_summary, data_US_head)



	# Cleaning

	# TMP testing
		data <- dataFutures$TU[, c("Date", "Time", "Close")]
		data$Date <- paste(data$Date, data$Time) # combine Date and Time columns
		test <- data[1:13, ]
		rm(dataFutures)
		# The following workspace image takes 1min to save and is 150MB
		# save.image(file = "Workspaces/tmp_Data_01_loaded_test.RData") # save the workspace

		test[, c("Time")] <- NULL
		class(test$Date)
		strip <- strptime(test$Date, format = "%m/%d/%Y %H:%M:%OS", tz = "GMT")
		class(strip)
		str(strip)
		typeof(strip)
		typeof(strip[[1]])
		strip[[1]]

		strip_POSIXct <- as.POSIXct(strip)
		class(strip_POSIXct)
		str(strip_POSIXct)
		typeof(strip_POSIXct)

		test$Date <- strip
		test$new <- rep(8, 8)
		test$Date <- strip_POSIXct
		test

		df <- data.frame(test)
		df
		str(df)
		class(df)
		typeof(df)
		df$Date <- strip
		df
	# TMP end testing

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



	size_objects("data") # 3Gb
	# data.table does not support POSIXlt (which is result of only the strptime()) data types for performance reason, use POSIXct or ITime
	# https://stackoverflow.com/questions/21487614/error-creating-r-data-table-with-date-time-posixlt
	data$Date <- as.POSIXct(strptime(data$Date, format = "%m/%d/%Y %H:%M:%OS", tz = "GMT")) # 1min
	size_objects("data") # 2Gb
	op_backup <- options(digits.secs = 3) # set seconds digits, by default digits.secs = 0, save orig options, TBD
	tail(data) # prints with milliseconds
	data[, c("Time")] <- NULL # getting rid of redundant columns
	size_objects("data") # 1Gb
	str(data)
	head(data)
	summary(data$Close)
	(data_str <- str(data))
	(data_summary <- summary(data))
	data_str <- glimpse(data)
	data_head <- head(data)
	data_head$Date <- as.character(data_head$Date) # in order for xtable to work, it doesn't work with date classes
	options(op_backup) # reset options, only after the above as.character is performed, else it writes the data_head w/o milliseconds

	print(xtable(data_summary, caption = "US 30-Year bond summary statistics"),
		type = "latex", file = paste0("Results/Tables/data_summary.tex"), include.rownames = FALSE)

	print(xtable(data_head, caption = "US 30-Year bond data glimpse"),
		type = "latex", file = paste0("Results/Tables/data_head.tex"), include.rownames = FALSE)

	rm(data_summary, data_str, data_head)

	# Save the env
	# The following workspace image takes 5?min to save and is 580MB
	# save.image(file = "Workspaces/Data_02_cleaned_US.RData") # save the workspace




	#### 2023 edits start #####


	# Test Dodo's run cell thingy on the following

	#%%
	str(data$US)
	(data_summary <- summary(data$US))
	# %%

	print(xtable(data_summary, caption = "US 30-Year bond summary statistics, tick data frequency"),
		type = "latex", file = paste0("Results/Tables/data_summary_US_tick.tex"), include.rownames = FALSE)




	#### 2023 edits end #####



################################
### 03 - Trim data #############
################################

	# Load all the maturities
	load(file = "Workspaces/Data_01_loaded_cleaned_TUFVTYUS.RData")

	str(data) # Number of observations: TU 59mm, FV 158mm, TY 264mm, US 161mm
	head(data) # 2006-01-02 18:00
	tail(data) # 2019-09-19 15:59

	# We leave the full dataset for the main analysis, despite the end being weirdly mid September, that's just when the database was acquired.

	# TBD Winsorize outliers if any, treat missing data?

	# Small data samples follow
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
	data_small_amonth <- data # TBD copying the data, mb create an empty variable instead!
	# Loop through all four maturities
	for (i in 1:dim) {
		print(paste("Trimming", names(data[i]), "to a smaller one month span."))
		data_small_amonth[[i]] <- subset(data[[i]], Time >= as.POSIXct("2019-05-01", tz = "GMT") & Time < as.POSIXct("2019-06-01", tz = "GMT"))
	}
	str(data_small_amonth)
	head(data_small_amonth)

	# Exploratory analysis, polish graph TBD
	m <- 4 # select which maturity to plot
	plot.ts(x = data_small_amonth[[m]]$Time, y = data_small_amonth[[m]]$Close, type = "l")
	summary(data_small_amonth[[m]])

	rm("data") # for the Data_03_trimmed-small RDatas


	# Save the workspace
	# The following takes 0min to save and is 6MB
	# save.image(file = "Workspaces/Data_03_trimmed-small_US.RData")
	# save.image(file = "Workspaces/Data_03_trimmed-small_aday_TU.RData")
	# save.image(file = "Workspaces/Data_03_trimmed-small_amonth_TU.RData")
	# save.image(file = "Workspaces/Data_03_trimmed-small_amonth_TUFVTYUS.RData")
	# save.image(file = "Workspaces/Data_03_trimmed_TUFVTYUS.RData")



################################
### 04 - Aggregate data ########
################################

	# Load all the maturities, now it contains the small datasets as well
	# load(file = "Workspaces/Data_03_trimmed-small_amonth_TUFVTYUS.RData") # only the small dataset
	load(file = "Workspaces/Data_03_trimmed_TUFVTYUS.RData") # the full dataset

	# First aggregating the small trimmed data of one day/month, then scale up
	ls()
	data_small <- data_TU_small_aday
	# data_small <- data_TU_small_amonth
	str(data_small)

	data_small$Hour <- cut(data_small$Time, breaks = "1 hour")
	str(data_small)
	head(data_small)
	tail(data_small, n = 30)
	summary(data_small$Hour == "2019-05-20 08:00:00")

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
	plot(data_small_hourly$Hour, data_small_hourly$price_close, type = "l", main = paste("TU", "2019-05-20"), xlab = "Time", ylab = "Price")


	# On the full Futures list, first the small one month
	dataFutures_small_Tick <- data_small_amonth # save the original tick data into its appropriate name

	dataFutures_small_H1 <- data_small_amonth
	for (i in 1:dim) {
		print(paste("Aggregating", names(data_small_amonth[i]), "to be hourly data"))
		dataFutures_small_H1[[i]]$Hour <- cut(data_small_amonth[[i]]$Time, breaks = "1 hour") # replace data w/ aggregated hourly data
		# dataFutures_small_H1[[i]] <- as.data.table(ddply(dataFutures_small_H1[[i]], .(Hour), summarize, Close = tail(Close, n = 1)))
		# data.table equivalent to ddply:
		dataFutures_small_H1[[i]] <- dataFutures_small_H1[[i]][ , .(Close = tail(Close, n = 1)), by = Hour]
		dataFutures_small_H1[[i]]$Hour <- as.POSIXct(fast_strptime(as.character(dataFutures_small_H1[[i]]$Hour), format = "%Y-%m-%d %H:%M:%OS", tz = "GMT"))
	}

	str(dataFutures_small_H1)
	head(dataFutures_small_H1)


	# On the full Futures list, full length
	dataFutures_Tick <- data # save the original tick data into its appropriate name

	# Might be faster to do a M5 first, then H1 from it, but it fast and small enough like this anyway (a few minutes), no need to optimise

	dataFutures_M5 <- data
	for (i in 1:dim) { # 4min, 54Mb
		print(paste("Aggregating", names(data[i]), "to be 5 minute data"))
		dataFutures_M5[[i]]$Minute <- cut(data[[i]]$Time, breaks = "5 min") # replace data w/ aggregated hourly data
		# dataFutures_M5[[i]] <- as.data.table(ddply(dataFutures_M5[[i]], .(Hour), summarize, Close = tail(Close, n = 1)))
		# data.table equivalent to ddply:
		dataFutures_M5[[i]] <- dataFutures_M5[[i]][ , .(Close = tail(Close, n = 1)), by = Minute]
		dataFutures_M5[[i]]$Minute <- as.POSIXct(fast_strptime(as.character(dataFutures_M5[[i]]$Minute), format = "%Y-%m-%d %H:%M:%OS", tz = "GMT"))
	}

	str(dataFutures_M5)
	head(dataFutures_M5)
	# TBD TU M5 it starts with 18:01 instead of 18:00, and then continues thru all the timespan years, should fix that

	dataFutures_H1 <- data
	for (i in 1:dim) { # 2min, 5Mb
		print(paste("Aggregating", names(data[i]), "to be hourly data"))
		dataFutures_H1[[i]]$Hour <- cut(data[[i]]$Time, breaks = "1 hour") # replace data w/ aggregated hourly data
		# dataFutures_H1[[i]] <- as.data.table(ddply(dataFutures_H1[[i]], .(Hour), summarize, Close = tail(Close, n = 1)))
		# data.table equivalent to ddply:
		dataFutures_H1[[i]] <- dataFutures_H1[[i]][ , .(Close = tail(Close, n = 1)), by = Hour]
		dataFutures_H1[[i]]$Hour <- as.POSIXct(fast_strptime(as.character(dataFutures_H1[[i]]$Hour), format = "%Y-%m-%d %H:%M:%OS", tz = "GMT"))
	}

	str(dataFutures_H1)
	head(dataFutures_H1)

	dataFutures_H4 <- data
	for (i in 1:dim) { # 2min, 1Mb
		print(paste("Aggregating", names(data[i]), "to be 4 hour data"))
		dataFutures_H4[[i]]$Hour <- cut(data[[i]]$Time, breaks = "4 hour") # replace data w/ aggregated hourly data
		# dataFutures_H4[[i]] <- as.data.table(ddply(dataFutures_H4[[i]], .(Hour), summarize, Close = tail(Close, n = 1)))
		# data.table equivalent to ddply:
		dataFutures_H4[[i]] <- dataFutures_H4[[i]][ , .(Close = tail(Close, n = 1)), by = Hour]
		dataFutures_H4[[i]]$Hour <- as.POSIXct(fast_strptime(as.character(dataFutures_H4[[i]]$Hour), format = "%Y-%m-%d %H:%M:%OS", tz = "GMT"))
	}

	str(dataFutures_H4)
	head(dataFutures_H4)

	size_objects()

	# Delete the variables we won't be needing
	rm(list = setdiff(ls(), c("dataFutures", "dataFutures_M5", "dataFutures_H1", "dataFutures_H4", "dim", "futurenames",
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
		plot(data_small[[i]]$Time, data_small[[i]]$Close, type = "l", main = paste(futurenames[i], "H1 2017"), xlab = "Time", ylab = "Price")
	}



################################
### 05 - Bind maturities #######
################################

	# WIP
	str(data_small)
	head(data_small)
	tail(data_small)

	for (i in maturities) {
		# Bind maturities together
		progress <- paste0(i, "/", length(maturities)) # fraction to see progress
		print(paste(progress, "Binding", futurenames[i], "...")) # print run

	TBD

		if (i == dim) {
			print(paste("Done binding and saving all", progress))
		}
	}


	# Save the workspace
	# The following takes ?min to save and is ?MB
	save.image(file = "Workspaces/Data_conversion_04_trimmed-small_US_2-30Ymaturities.RData")



################################
### END RESULT #################
################################

	# TBD CONT
	load(file = "Workspaces/Data_04_aggregated_TUFVTYUS.RData")



################################
### Procedure:##################
################################
	# rows observations, say aggregate from tick to 5min data, save each workspace or together if small enough
	# gradually load workspaces and merge columns maturities 2, 5, 10, 30Y, delete unused variables
	# cut data for say 1 month or 1Y of data depending on size, so it is manageable for tests
	# try Nelson.Siegel()
	# rewrite Nelson.Siegel() and literature into my own code DNS fitting
	# redo NNs input structure so the output from DNS can be fit and predicted by NNs
	# try NNs onto the multivariable time series of DNS coefficeints
	# transform back the coefficients into prices predicted
	# evaluate



################################
### WIP Play ###################
################################
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



################################
### TO DO ######################
################################

	# Revive:
		# clean up code formatting
		# put away snippets of code that are not being used, set up a new file "unused functions" or something to offload there
		# run thru the code to make sure it is all MWE working
		# continue w details and polish a MWE

	# Old ideas:
		# MWE graph
		# polish an exploratory graph axes
		# convert the data to xts?


