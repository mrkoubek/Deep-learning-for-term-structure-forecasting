# Master Thesis
# Data conversion follows
# v1.2 - trimming the data

rm(list = ls())

# Packages
if (!require("pacman")) install.packages("pacman") # installs pacman package if not installed yet
pacman::p_load(pacman, ggplot2, zoo, xtable, tibble, data.table, stringr, lubridate) # load packages TBD e.g. these, edit when u determine which needed/used..

# Workspace
setwd("E:/Google_Drive/Diploma_Thesis/Code")
# Loading basic functions
load(file = "Workspaces/Basics_01_functions.RData") # we load the basic functions first, then in addition latest ones below:
# Loading data
# load(file = "Workspaces/tmp_Data_01_loaded_test.RData") # takes 8min!! we load the latest version of a prepared workspace
# load(file = "Workspaces/Data_01_loaded_TUFVTYUS.RData") # takes 8min!! we load the latest version of a prepared workspace

# load(file = "Workspaces/Data_02_cleaned_US.RData") # we load the latest version of a prepared workspace
# load(file = "Workspaces/Data_03_trimmed-small_US.RData") # we load the latest version of a prepared workspace
print("Workspace rdy set go!")



##################################
#### 00 - Basics - functions #####
##################################

# Check the size of the variables
# https://stackoverflow.com/questions/1395270/determining-memory-usage-of-objects
size_objects <- function(objects = ls(.GlobalEnv)) {
	print(sort(sapply(objects, function(x) format(object.size(get(x)), unit = 'auto'))))
}

# The starting time
time_start <- function() {
	startTime <- Sys.time()
	print(paste0("Started loading at: ", startTime))
	startTime
}

# The ending time and duration
time_end <- function(startTime) {
	endTime <- Sys.time()
	print(paste0("Finished loading at: ", endTime, ". Time taken: ", endTime - startTime, " units."))
}

# save.image(file = "Workspaces/Basics_01_functions.RData") # save the workspace of basic functions



#########################
### 01 - Load up data ###
#########################

# New, finish:
# Preparation of variables
pathConversion <- file.path("E:/Diploma_Thesis_Data/Data/TickWriteOutput/Conversion_Data")
foldernames <- list.dirs(path = pathConversion, recursive = FALSE)[5:8] # foldernames ~ paths, just the US bonds, the first 4 are the EU bonds
foldernames <- foldernames[c(2, 1, 3, 4)] # reshuffle manually so it's according to maturity ("TU" "FV" "TY" "US"), not alphabet
futurenames <- list.dirs(path = pathConversion, full.names = FALSE, recursive = FALSE)[5:8] # only names of our futures w/o path
futurenames <- futurenames[c(2, 1, 3, 4)] # reshuffle manually so it's according to maturity ("TU" "FV" "TY" "US"), not alphabet
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



#########################
### 02 - Clean data #####
#########################

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



#########################
### 03 - Trim data ######
#########################

# TBD CONT
load(file = "Workspaces/Data_01_loaded_cleaned_TUFVTYUS.RData") # we load all the maturities

colnames(data$TU)

# Take an excerpt from the data to easily work on/visualise, e.g. only the day 20th May 2019 or the month May 2019:
data_TU_small_aday <- subset(data$TU, Time >= as.POSIXct("2019-05-20", tz = "GMT") & Time < as.POSIXct("2019-05-21", tz = "GMT"))
data_TU_small_amonth <- subset(data$TU, Time >= as.POSIXct("2019-05-01", tz = "GMT") & Time < as.POSIXct("2019-06-01", tz = "GMT"))

data_small <- data_TU_small_aday
data_small <- data_TU_small_amonth
str(data_small)
head(data_small)
tail(data_small)

rm(data, data_TU_small_aday, data_TU_small_amonth)

# Exploratory analysis
plot.ts(x = data_small$Time, y = data_small$Close, type = "l")
summary(data_small)

# Polish graph TBD

# Save the workspace
# The following takes 0min to save and is 6MB
# save.image(file = "Workspaces/Data_03_trimmed-small_US.RData")
# save.image(file = "Workspaces/Data_03_trimmed-small_aday_TU.RData")
# save.image(file = "Workspaces/Data_03_trimmed-small_amonth_TU.RData")


#############################
### 04 - Aggregate data #####
#############################

# First aggregating the small trimmed data of one day/month, then scale up
load(file = "Workspaces/Data_03_trimmed-small_amonth_TU.RData") # we load all the maturities

ls()
str(data_small)

data_small$Hour <- cut(data_small$Time, breaks = "1 hour")
str(data_small)
head(data_small)
tail(data_small)
summary(data_small$Hour == "2019-05-20 08:00:00")

# then use ddply (from plyr package), splitting the data frame by that cut variable
p_load(plyr)
data_small_hourly <- ddply(data_small, .(Hour), summarize, price_close = tail(Close, n = 1))
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


# Save the workspace
# The following takes 0min to save and is 6MB
# save.image(file = "Workspaces/Data_04_aggregated-small_amonth_TU.RData")



# TBD plot all maturities
# Plotting the data again
dim <- 4
par(mfrow = c(dim, 1))
for (i in 1:dim) {
	plot(data_small[[i]]$Time, data_small[[i]]$Close, type = "l", main = paste(futurenames[i], "H1 2017"), xlab = "Time", ylab = "Price")
}








#############################
######### END RESULT ########
#############################

# TBD CONT
# First aggregating the small trimmed data of one day/month, then scale up
load(file = "Workspaces/Data_04_aggregated-small_amonth_TU.RData") # we load all the maturities








#############
# Procedure:#
#############
# rows observations, say aggregate from tick to 5min data, save each workspace or together if small enough
# gradually load workspaces and merge columns maturities 2, 5, 10, 30Y, delete unused variables
# cut data for say 1 month or 1Y of data depending on size, so it is manageable for tests
# try Nelson.Siegel()
# rewrite Nelson.Siegel() and literature into my own code DNS fitting
# redo NNs input structure so the output from DNS can be fit and predicted by NNs
# try NNs onto the multivariable time series of DNS coefficeints
# transform back the coefficients into prices predicted
# evaluate


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





##############
## WIP Play ##
##############
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






#############
### TO DO ###
#############
	# MWE graph
	# polish an exploratory graph axes
	# convert the data to xts?
	# 

