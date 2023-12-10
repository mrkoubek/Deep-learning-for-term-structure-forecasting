# Master Thesis
# Prices to yields conversion
# v0.1 - new file



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




list.dirs(path = pathConversion, full.names = TRUE, recursive = FALSE)



#############
### TO DO ###
#############
	# 
