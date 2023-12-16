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