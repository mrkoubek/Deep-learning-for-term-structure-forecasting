# Master Thesis
# Factor models
# v1.1 - restructure, take out YieldCurve example chunk of code to another file



###############################################
###### Workspace setup ########################
###############################################

    rm(list = ls())

    # Packages
    if (!require("pacman")) install.packages("pacman") # installs pacman package if not installed yet
    pacman::p_load(pacman, YieldCurve, dplyr, tidyr, ggplot2, scales, Cairo, zoo, xtable, tibble, forecast, naturalsort, data.table) # load packages TBD e.g. these, edit when u determine which needed/used..
    # p_load(reticulate, tensorflow, keras)

    # Workspace
    setwd("E:/Google_Drive/Diploma_Thesis/Code")
    load(file = "Workspaces/Data_04_aggregated_TUFVTYUS.RData") # 0min
    load(file = "Workspaces/Data_05_pricestoyields_TUFVTYUS-small.RData") # 0min
    print("Workspace rdy set go!")



###############################################
###### Factor models ##########################
###############################################

    # We run the preprocessed data through the factor models (DNS) before training the NNs.
    # Returns the estimated coefficients of the Nelson-Siegel's model.
    # https://www.rdocumentation.org/packages/YieldCurve/versions/4.1/topics/Nelson.Siegel
    # Nelson.Siegel(rate, maturity)



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



###############################################
####### Notes #################################
###############################################

    # Work with: dataFutures_train, probably window the data after the NS, as is the case currently in Model_fit.R

    # We need to change our prices data into a different shape, with columns representing maturities.
    # What are our maturities? Do we load up the 2, 5, 10, 30Y maturity prices as columns?
    # For this we need to go back to Data_conversion.R and load up more datasets.
    # Do we then add maturity interpolations?


    # TBD:
    	# go thru the Nelson.Siegel function source code
    	# factor models documentation, find packages inspiration and test out

