# Master Thesis
# Data conversion follows
# v1.0 - basic preprocessing

rm(list = ls())

# Packages
if (!require("pacman")) install.packages("pacman") # installs pacman package if not installed yet
pacman::p_load(pacman, ggplot2, zoo, xtable, tibble) # load packages TBD e.g. these, edit when u determine which needed/used..

# Workspace
setwd("E:/Google_Drive/Diploma_Thesis/Code")
# loading data workspace takes 1min
load(file = "Workspaces/Data_02_cleaned_US.RData") # we load the latest version of a prepared workspace
print("Workspace rdy set go!")



#########################
### 01 - Load up data ###
#########################

# New, finish:
# Preparation of variables
pathConversion <- file.path("E:/Diploma_Thesis/Data/Conversion_Data")
foldernames <- list.dirs(path = pathConversion, recursive = FALSE) # foldernames ~ paths
futurenames <- list.dirs(path = pathConversion, full.names = FALSE, recursive = FALSE) # only names of our futures w/o path
dim <- length(foldernames) # number of futures

# Empty variables
filenames <- vector(mode = "list", length = dim) # create an empty list that has a dimension of the number of futures, the values are added later
# names(filenames) <- futurenames # name the filenames lists, prob useless
filePaths <- vector(mode = "list", length = dim) # TBD optimise to one variable instead of 3 foldernames, filenames, filePaths put in one list or sth
bindedFile <- vector(mode = "list", length = dim) # TBD
dataForex <- array(list(NULL), dim = dim, dimnames = list(futurenames)) # an array in which we'll save all our data
# End new

dim <- 1 # how many datasets are we using
futurenames <- "US"
dataFutures <- array(list(NULL), dim = dim, dimnames = list(futurenames)) # an array in which we'll save all our data
# the data has the first row containing column names, hence "header = TRUE"
dataFutures[[1]] <- read.csv("../Data/TickWriteOutput/US/US.csv", header = TRUE) # load the data from Data location, takes 13min for US
data_US <- dataFutures[[1]]
rm(dataFutures)
ls()
str(data_US)
head(data_US)

# Save the env
ifelse(!dir.exists("Workspaces"), dir.create("Workspaces"), "Directory already exists.") # creates a "Workspaces" directory if it doesn't exist yet
# The following workspace image takes 5?min to save and is 380MB
# save.image(file = "Workspaces/Data_01_loaded_US.RData") # save the workspace



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



#############################
### 04 - Merge maturities ###
#############################

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

