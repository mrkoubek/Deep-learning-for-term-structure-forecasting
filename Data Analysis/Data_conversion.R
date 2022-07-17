# Master Thesis
# Data conversion follows
# v1.1 - merging maturities

rm(list = ls())

# Packages
if (!require("pacman")) install.packages("pacman") # installs pacman package if not installed yet
pacman::p_load(pacman, ggplot2, zoo, xtable, tibble, data.table) # load packages TBD e.g. these, edit when u determine which needed/used..

# Workspace
setwd("E:/Google_Drive/Diploma_Thesis/Code")
# Loading basic functions
load(file = "Workspaces/Basics_01_functions.RData") # we load the basic functions first, then in addition latest ones below:
# Loading data
load(file = "Workspaces/Data_01_loaded_TUFVTYUS.RData") # takes 8min!! we load the latest version of a prepared workspace
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

# Load up the datasets
for (i in 1:dim) {
	startTime <- time_start() # time the execution
	print(paste("Started loading", filePaths[[i]]))

	dataFutures[[i]] <- fread(filePaths[[i]], select = columns_to_select, header = TRUE, data.table = TRUE) # main load, takes a few mins
	
	time_end(startTime)
	size_objects("dataFutures")
}

size_objects() # check how large is the dataFutures vector, 25Gb w all 4 maturities

# Save the env
ifelse(!dir.exists("Workspaces"), dir.create("Workspaces"), "Directory already exists.") # creates a "Workspaces" directory if it doesn't exist yet
startTime <- time_start()
# save.image(file = "Workspaces/Data_01_loaded_TUFVTYUS.RData") # save the workspace, takes 8min
time_end(startTime)



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

	dataFutures <- array(list(NULL), dim = dim, dimnames = list(futurenames)) # an array in which we'll save all our data
	for (i in 1:3) {
		print(paste("Loading up dataset", i, "/", dim, futurenames[i]))
		dataFutures[i] <- load_RData_dataFutures(paste0("Workspaces/Data_01_loaded_", futurenames[i], ".RData"))[i]
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
str(data_US)
(data_US_str <- capture.output(str(data_US)))
head(data_US)
tail(data_US)
data_US[1000, ]
(data_US_summary <- summary(data_US))
data_US_head <- head(data_US)[, -ncol(data_US)]

print(xtable(data_US_summary, caption = "US 30-Year bond summary statistics"),
	type = "latex", file = paste0("Results/Tables/data_US_summary.tex"), include.rownames = FALSE)

print(xtable(data_US_head, caption = "US 30-Year bond data glimpse"),
	type = "latex", file = paste0("Results/Tables/data_US_head.tex"), include.rownames = FALSE)

rm(data_US_str, data_US_summary, data_US_head)

# Cleaning
data <- data_US[, c("Date", "Time", "Close")]
data$Date <- paste(data$Date, data$Time) # combine Date and Time columns
data$Date <- strptime(data$Date, format = "%m/%d/%Y %H:%M:%OS", tz = "GMT")
op_backup <- options(digits.secs = 3) # set seconds digits, by default digits.secs = 0, save orig options, TBD
tail(data) # prints with milliseconds
data[, c("Time")] <- NULL # getting rid of redundant columns
str(data)
head(data)
summary(data)
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

# Take an excerpt from the data to easily work on/visualise, e.g. only the day 20th May 2019 or the month May 2019:
data_small_aday <- subset(data, Date >= as.POSIXlt("2019-05-20", tz = "GMT") & Date < as.POSIXlt("2019-05-21", tz = "GMT"))
data_small_amonth <- subset(data, Date >= as.POSIXlt("2019-05-01", tz = "GMT") & Date < as.POSIXlt("2019-06-01", tz = "GMT"))

data_small <- data_small_aday
data_small <- data_small_amonth
str(data_small)
head(data_small)
tail(data_small)

rm(data, data_US, dim)

# Exploratory analysis
plot.ts(x = data_small$Date, y = data_small$Close, type = "l")
summary(data_small)

# Polish graph TBD

# Save the workspace
# The following takes 0min to save and is 6MB
save.image(file = "Workspaces/Data_03_trimmed-small_US.RData")








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




#############
### TO DO ###
#############
	# MWE graph
	# polish an exploratory graph axes
	# convert the data to xts?
	# 

