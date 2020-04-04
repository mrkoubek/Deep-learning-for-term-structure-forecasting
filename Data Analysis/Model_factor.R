# Master Thesis
# Factor models
# v1.0 - basic layout, test of YieldCurve package

rm(list = ls())

# Packages
if (!require("pacman")) install.packages("pacman") # installs pacman package if not installed yet
pacman::p_load(pacman, dplyr, tidyr, ggplot2, scales, Cairo, zoo, xtable, tibble, forecast, naturalsort) # load packages TBD e.g. these, edit when u determine which needed/used..
# p_load(reticulate, tensorflow, keras)

# Workspace
setwd("E:/Google_Drive/Diploma_Thesis/Code")
# load(file = "Workspaces/Data_03_trimmed-small_US.RData") # 0min
load(file = "Workspaces/Data_04_split-small_US_tick_05-2019.RData") # 0min
print("Workspace rdy set go!")


#############################
###### Factor models ########
#############################

# We run the preprocessed data through the factor models (DNS) before training the NNs.
p_load(YieldCurve)

# https://www.rdocumentation.org/packages/YieldCurve/versions/4.1/topics/Nelson.Siegel
# Nelson.Siegel(rate, maturity)


###############################################
###### Example from package YieldCurve ########
###############################################

data(FedYieldCurve) # Fed data set
str(FedYieldCurve) # 8 maturities in columnt, 372 months in rows as observations
head(FedYieldCurve)
maturity.Fed <- c(3/12, 0.5, 1,2,3,5,7,10)

# first() from xts package can have number of periods in character string, n = 'n period.type', where period.type can be: secs, seconds, mins, minutes, hours, days, weeks, months, quarters, and years
# Nelson.Siegel(rate, maturity) explained:
# - rate is a matrix with i.r.
# - maturity is a vector of maturities of rate (in months??)
(NSParameters <- Nelson.Siegel(rate=first(FedYieldCurve,'10 month'), maturity=maturity.Fed))
# NSrates(Coeff, maturity) explained:
# - https://www.rdocumentation.org/packages/YieldCurve/versions/4.1/topics/NSrates
(y <- NSrates(NSParameters[5,], maturity.Fed))

# Plot
plot(maturity.Fed,FedYieldCurve[5,],main="Fitting Nelson-Siegel yield curve",
  xlab=c("Pillars in months"), type="o")
lines(maturity.Fed,y, col=2)
legend("topleft",legend=c("observed yield curve","fitted yield curve"),
col=c(1,2),lty=1)
grid()




#####################
####### Notes #######
#####################


# Work with: dataFutures_train, probably window the data after the NS, as is the case currently in Model_fit.R

# We need to change our prices data into a different shape, with columns representing maturities.
# What are our maturities? Do we load up the 2, 5, 10, 30Y maturity prices as columns?
# For this we need to go back to Data_conversion.R and load up more datasets.
# Do we then add maturity interpolations?


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



# TBD:
	# go thru the Nelson.Siegel function source code
	# factor models documentation, find packages inspiration and test out

