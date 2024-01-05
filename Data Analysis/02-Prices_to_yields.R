# Master Thesis
# Prices to yields conversion
# v1.0 - real data calculation of yields and a basic NAs treatment, for amonth and ayear of data



################################
### Workspace setup ############
################################

	rm(list = ls())

	# Packages
	if (!require("pacman")) install.packages("pacman") # installs pacman package if not installed yet
	pacman::p_load(pacman, ggplot2, zoo, xtable, tibble, data.table, stringr, lubridate, plyr, readxl) # load packages TBD e.g. these, edit when u determine which needed/used..

    # Workspace
    setwd("E:/Google_Drive/Diploma_Thesis/Code")
    # Loading basic environment functions
    load(file = "Workspaces/00_Environment_functions.RData")
    # Loading data
    load(file = "Workspaces/Data_04_aggregated_TUFVTYUS.RData") # 0min




##################################
### 05 - Prices to yields ########
##################################
	# TBD restructure the numbering of sections, renew in this file



##################################
### 05.01 - Merge maturities #####
##################################

    # Our datasets
    maturities <- c(2, 5, 10, 30) # in years

    # A function for preparing the data into a better format for the Nelson-Siegel
    # Merges the maturities from separate lists into one data.table
    merge_maturities <- function (data, frequency) {
	    # Set "Hour" as the key for each data.table. Or "Day" for daily data.
	    data <- lapply(data, function(x) setkeyv(x, frequency))

	    # Merge all data.tables using Reduce, suppress a warning about duplicate column names, we're renaming right after this.
	    result <- suppressWarnings(Reduce(function(x, y) merge(x, y, by = frequency, all = TRUE), data))

	    # Rename the "Close" columns w maturity suffixes.
	    setnames(result, c(frequency, paste0("Close_", names(data))))

	    return(result)
    }

    result_hourly <- merge_maturities(data = dataFutures_H1, frequency = "Hour")
    result_daily <- merge_maturities(data = dataFutures_D1, frequency = "Day")

    str(result_hourly)
    head(result_hourly)
    tail(result_hourly)

    str(result_daily)
    	# 4249 obs.
 


##################################
### 05.02 - Subset time series ###
##################################

    # Subset the hourly data to contain just a month for further testing purposes
    data_amonth <- result_hourly[Hour >= as.POSIXct("2006-01-01", tz = "GMT") & Hour < as.POSIXct("2006-02-01", tz = "GMT")]
    data_ayear <- result_hourly[Hour >= as.POSIXct("2006-01-01", tz = "GMT") & Hour < as.POSIXct("2007-01-01", tz = "GMT")]
    data_hourly_all <- result_hourly[Hour >= as.POSIXct("2006-01-01", tz = "GMT") & Hour < as.POSIXct("2019-09-20", tz = "GMT")]
    data_daily_Fedcompare <- result_daily[Day >= as.POSIXct("2006-01-01", tz = "GMT") & Day < as.POSIXct("2012-11-30", tz = "GMT")]
    data_daily_all <- result_daily

    head(data_amonth)
    str(data_amonth)
        # 444 obs.
    str(data_ayear)
    	# 5451 obs.
    str(data_hourly_all)
    	# 79973 obs.
    str(data_daily_Fedcompare)
    	# 2135 obs.



##################################
### 05.03 - Missing values #######
##################################

    # Quick NAs check and filling the missing values with interpolation
    NA_remove <- function(data) {
    	print(paste0("Processing NAs in ", deparse(substitute(data))))
	    print(paste("Number of NAs before:", sum(is.na(data))))
	        # 14 for a month
	        # 167 for a year
	        # 474 for all hourly
	        # 0 for Fedcompare
	        # 0 for all daily

	    # Extract just the rows with contain any NA
		NA_rows <- data[rowSums(is.na(data)) > 0, ]
	    print(str(NA_rows))
	    print(head(NA_rows))

	    # We use linear interpolation for the few NAs (missing values, mostly in the TU maturity)
	    # From the zoo package, na.approx() function:
	    data[, (2:ncol(data)) := lapply(.SD, na.approx, na.rm = FALSE), .SDcols = 2:ncol(data)]
	    print(paste("Number of NAs after interpolation:", sum(is.na(data))))
	        # 0
    }

    NA_remove(data = data_amonth)
    NA_remove(data = data_ayear)
    NA_remove(data = data_hourly_all)
    NA_remove(data = data_daily_Fedcompare)
    NA_remove(data = data_daily_all)



#######################################
### 05.04 - Excel conversion factor ###
#######################################

	# Load the excel file with the conversion factor lookup tables

	# The path to the lookup table excel file
	lookuptable_path <- "./dl-git-repo/deep-learning-for-term-structure-forecasting/Data Analysis/Miscellaneous/Info_Futures-Yields/treasury-futures-conversion-factor-look-up-tables.xls"

	# Read the sheets we need, skip the first few rows
	conversion_factors <- array(list(NULL), dim = dim, dimnames = list(futurenames)) # an empty array

	sheets <- c("2-Year Note Table", "5-Year Note Table", "10-Year Note Table", "Classic Bond & Ultra Bond Table")
	conversion_factors <- lapply(sheets, function(x) read_excel(lookuptable_path, sheet = x, skip = 4))
	names(conversion_factors) <- futurenames

	# str(conversion_factors)

	# Conversion factors, zero-coupon bonds
	maturities_longdash <- c("2—0", "5—0", "10—0", "30—0") # need to use the long dash, not the regular "2-0" which doesn't work for this `2â€”0`
	conversion_factors_zerocoupon <- lapply(seq_along(conversion_factors), function(i) {
		maturity <- maturities_longdash[i]
	    tibble <- conversion_factors[[i]]
		if (maturity %in% names(tibble)) {
			as.numeric(tibble[tibble$Coupon == 0, maturity]) # pick out the first row (zero coupon), and column corresponding to our maturity
		} else { # when the specified maturity column isn't found
			NULL
		}
	})
	names(conversion_factors_zerocoupon) <- futurenames
	conversion_factors_zerocoupon <- unlist(conversion_factors_zerocoupon)

	conversion_factors_zerocoupon
		#     TU     FV     TY     US 
		#	0.8885 0.7441 0.5537 0.1697 



#######################################
### 05.05 - Yield calculation #########
#######################################

	# For the (2, 5, 10, 30)Y US Treasure Bond Futures, in USD:
	# https://www.cmegroup.com/trading/interest-rates/basics-of-us-treasury-futures.html
	conversion_factors_zerocoupon
	contract_multipliers <- c(2000, 1000, 1000, 1000)
	face_value <- c(200000, 100000, 100000, 100000)
	maturities <- c(2, 5, 10, 30)

	# Create a table
	bonds_specs <- data.frame(
	    Name = names(conversion_factors_zerocoupon),
	    Maturity = maturities,
	    Conversion_Factor = conversion_factors_zerocoupon,
	    Contract_Multiplier = contract_multipliers,
	    Face_Value = face_value
	)

	bonds_specs

	# Put all the various data excerpts and frequencies in one list variable
    data_list <- list(data_amonth = data_amonth,
				      data_ayear = data_ayear,
				      data_hourly_all = data_hourly_all,
				      data_daily_Fedcompare = data_daily_Fedcompare,
				      data_daily_all = data_daily_all)
	lapply(data_list, print(names))

	# Calculation of Treasury bond yields from prices, using prespecified bond specs
	calculate_yield <- function(specs, data) { 
	    adjusted_prices <- data[[paste0("Close_", specs$Name)]] * specs$Conversion_Factor # multiply each price observation by the corresponding conversion factor (depends on maturity)
	    total_prices <- adjusted_prices * specs$Contract_Multiplier
	    yields <- (specs$Face_Value / total_prices)^(1/specs$Maturity) - 1 # the yield-to-maturity calculation
	    yields <- 100 * yields
	    return(yields)
	}

	# Create a list to store all the yields
	yields <- lapply(data_list, function(d) {
		yield_set <- lapply(1:nrow(bonds_specs), function(i) calculate_yield(specs = bonds_specs[i, ], data = d))

		# Format the lists as data.table and name the columns
		yield_set <- as.data.table(yield_set)
		setnames(yield_set, paste0("Yield_", bonds_specs$Name))

		# Add the Hour or Day column
		first_col_name <- NULL # initialise
		if ("Hour" %in% names(d)) {
			yield_set[, Hour := d$Hour]
			first_col_name <- "Hour"
		} else if ("Day" %in% names(d)) {
			yield_set[, Day := d$Day]
			first_col_name <- "Day"
		}

		# Rearrange columns so the Hour or Day is the first column
		if (!is.null(first_col_name)) {
        	setcolorder(yield_set, c(first_col_name, setdiff(names(yield_set), first_col_name)))
    	}

    	# Clone the sorted attribute from the original data.table
	    if (!is.null(attributes(d)$sorted)) {
	        setattr(yield_set, "sorted", attributes(d)$sorted)
	    }

		return(yield_set)
		}
	)

	str(yields)
	head(yields$data_hourly_all)



#######################################
### 05.06 - Save the variables ########
#######################################

	# Delete the variables we won't be needing
	rm(list = setdiff(ls(), c("yields", "data_list",
								"dataFutures", "dataFutures_M5", "dataFutures_H1", "dataFutures_H4", "dataFutures_D1",
								"maturities", "dim", "futurenames", "size_objects", "time_start", "time_end")))

	# Save the workspace
	# The following takes 0min to save and is 14MB
	save.image(file = "Workspaces/Data_05_pricestoyields_TUFVTYUS.RData")



#######################################
### 05.07 - Visualisation #############
#######################################

	# TBC

	# TBD plot all maturities, or move this to next file?

	# Plotting the hourly data
	dim <- 4
	par(mfrow = c(dim, 1))
	for (i in 1:dim) {
		plot(yields$Hour, yields[[i+1]], type = "l", main = paste(futurenames[i], "H1 2006-2019 yields"), xlab = "Time", ylab = "Yield")
	}



#############
### TO DO ###
#############
	
	# Port the basic plots into ggplot format, but first check the file 03-Model_factor_YieldCurve-package.R, there's already
	# a yields_graph ggplot, maybe what we want?


