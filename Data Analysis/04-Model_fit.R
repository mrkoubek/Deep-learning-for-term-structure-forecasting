# Master Thesis
# Model fitting follows
# v1.3 - revive this file, tries with the new yields data instead of just prices



################################
### Workspace setup ############
################################

    rm(list = ls())

	# Bind R to a Python environment that has TensorFlow with GPU support
	# Do this first before loading any further packages, otherwise the R session can be binded with a default CPU environment and you will have to restart R to rebind
	# reticulate::use_condaenv("r-tensorflow-gpu") # Specify the name of a conda environment.
	# reticulate::use_condaenv("r-tensorflow-gpu", required = TRUE)
	# play:
	# reticulate::repl_python()
	# import tensorflow as tf

	# Packages
	if (!require("pacman")) install.packages("pacman") # installs pacman package if not installed yet
	pacman::p_load(pacman, dplyr, tidyr, ggplot2, scales, Cairo, zoo, xtable, tibble, forecast, naturalsort) # load packages TBD e.g. these, edit when u determine which needed/used..
	p_load(reticulate, tensorflow, keras)
	# py_config()
	# tf_config()
	# devtools::session_info()

	# Workspace
	setwd("E:/Google_Drive/Diploma_Thesis/Code")
	# load(file = "Workspaces/Data_03_trimmed-small_US.RData") # 0min
	# load(file = "Workspaces/Data_04_split-small_US_tick_05-2019.RData") # 0min
	# load(file = "Workspaces/Data_04_aggregated_TUFVTYUS.RData") # 0min
	load(file = "Workspaces/Data_06_yields-to-NSparameters_TUFVTYUS.RData") # 0min
	print("Workspace rdy set go!")



#############################
#### Factor models ##########
#############################

	# We run the data through the factor models (DNS) before training the NNs.
	# In a previous separate file 03-Model_factor.R.
	# The output is the DNS parameters, "NS_parameters", which contains the processed datasets for a fixed lambda and a time-varying one.

	ls()

	# We have the yields available, calculated from the prices, in the file 02-Prices_to_yields.R.
	str(yields)
	str(yields$data_amonth)
	head(yields$data_amonth)

	# Then we have the main DNS parameters, calculated from the yields, in the file 03-Model_factor.R
	str(NS_parameters)
	head(NS_parameters$lambda_fixed$data_hourly_all)

	# Pick which dataset to analyse
	dataFutures_tmp <- NS_parameters$lambda_fixed$data_amonth
	dataFutures_tmp <- NS_parameters$lambda_fixed$data_daily_all
	dataFutures_tmp <- NS_parameters$lambda_fixed$data_hourly_all
	head(dataFutures_tmp)



#############################
###### Normalisation? #######
#############################

	# TBD Do we normalise or standardise the data, so the LSTM can work with similarly scaled inputs? Or do we skip this step?



#############################
###### Split datasets #######
#############################

	# We split the dataset into train-validation-test sets, and apply possible differencing

	# TBD edit the code so the variables are named more intuitively (yields/prices etc?)
	# TBD edit the code to be multivariate?
	# Split the dataset into 60% training, 20% validation, 20% testing sets
	# An xts object needs a nrow() instead of length() to get the number of rows (observations)
	(end <- nrow(dataFutures_tmp))
	(split_train <- round(3/5 * end))
	(split_val <- round(4/5 * end))

	# Edit: the data is multivariate, with 4 columns (plus SSR as 5th which we don't need). So we add a dimension to our data here
	dataFutures_train <- dataFutures_tmp[1:split_train, ]
	dataFutures_val <- dataFutures_tmp[(split_train + 1):split_val, ]
	dataFutures_test <- dataFutures_tmp[(split_val + 1):end, ]

	# backup of original data, for "undifferencing" at the end
	dataFutures_train_orig <- dataFutures_train
	dataFutures_val_orig <- dataFutures_val
	dataFutures_test_orig <- dataFutures_test

	# Difference the data
	# TBD we don't apply differencing to yields, that just for prices. We establish this by:
		# 1. Visualise the data
		# 2. Test for stationarity
		# 3. Consider the implications and decide what is appropriate

		# 1. Visualise the data
			# We plot the data.
			# We need to inspect each of the columns of the dataset (beta_0, beta_1, beta_2, lambda, SSR).
			str(dataFutures_tmp)
			head(dataFutures_tmp)

			# Plot each series in the xts object
			plot(dataFutures_tmp$beta_0, main = "Beta_0 Over Time")
			plot(dataFutures_tmp$beta_1, main = "Beta_1 Over Time")
			plot(dataFutures_tmp$beta_2, main = "Beta_2 Over Time")
			plot(dataFutures_tmp$lambda, main = "Lambda Over Time")
			plot(dataFutures_tmp$SSR, main = "SSR Over Time")

		# 2. Test for stationarity
			p_load(tseries)

			# Perform ADF test on each series
			adf.test(dataFutures_tmp$beta_0, alternative = "stationary")
			adf.test(dataFutures_tmp$beta_1, alternative = "stationary")
			adf.test(dataFutures_tmp$beta_2, alternative = "stationary")
			adf.test(dataFutures_tmp$lambda, alternative = "stationary")
			adf.test(dataFutures_tmp$SSR, alternative = "stationary")

		# 3. Consider the implications and decide what is appropriate
			# The NS_parameters series show signs of non-stationarity, the ADF test p-values are too high to reject the null
			# hypothesis of unit root. The series should be differenced to make them stationary. This is essential for
			# further time series modelling, some of which assume stationarity.

			# Differencing the series
			dataFutures_tmp_diff <- dataFutures_tmp
			dataFutures_tmp_diff$beta_0 <- diff(dataFutures_tmp$beta_0, differences = 1)
			dataFutures_tmp_diff$beta_1 <- diff(dataFutures_tmp$beta_1, differences = 1)
			dataFutures_tmp_diff$beta_2 <- diff(dataFutures_tmp$beta_2, differences = 1)
			dataFutures_tmp_diff$SSR <- diff(dataFutures_tmp$SSR, differences = 1)

			str(dataFutures_tmp_diff)
			head(dataFutures_tmp_diff)

			# Remove first NA observation after differencing.
			dataFutures_tmp_diff <- dataFutures_tmp_diff[-1,]

			str(dataFutures_tmp_diff)
			head(dataFutures_tmp_diff)



	# dataFutures_train <- diff(dataFutures_train)
	# dataFutures_val <- diff(dataFutures_val)
	# dataFutures_test <- diff(dataFutures_test)
	summary(dataFutures_train)
	str(dataFutures_train)
	head(dataFutures_train)



# OLD CODE FROM HERE, not refactored



#############################
########## ANNs #############
#############################

	#############################
	####### Windowing ###########
	#############################

	# For windowing the data, we need to set how wide the window is.
	# This is also the number of time steps, that the model (LSTM) sees into the past at each point.
	# TBD do we loop over this, to automate this as part of the hyperparameter tuning?
	# lstm_num_timesteps <- 2
	lstm_num_timesteps <- 4
	# lstm_num_timesteps <- 12

	# Window the input data for multivariate series
	window_data_xts <- function(data, lstm_num_timesteps) {
		# Initialise a list for storing the windows
		# TBD don't grow a list, predefine its dimensions
		windows <- list()

		# Number of windows
		num_windows <- nrow(data) - lstm_num_timesteps + 1

		for (i in 1:num_windows) {
			# Extract the window
			window <- data[(i):(i + lstm_num_timesteps - 1), ]
			windows[[i]] <- window
		}

		# sapply version?
		# windows <- t(sapply(1:num_windows, function(i) data[i:(i + lstm_num_timesteps - 1), ]))
		
		return(windows)
		# Old single variable code:
		# t(sapply(1:(length(data) - lstm_num_timesteps), function(x) data[x:(x + lstm_num_timesteps - 1)]))
	}

	# TBC this variable has several dimensions, what do we pass? Explore.
	data_train_windowed <- window_data_xts(dataFutures_train, lstm_num_timesteps)
	data_val_windowed <- window_data_xts(dataFutures_val, lstm_num_timesteps)
	data_test_windowed <- window_data_xts(dataFutures_test, lstm_num_timesteps)
	str(data_train_windowed[[1]])
	data_train_windowed[[1]]
	data_train_windowed[[2]]
	data_train_windowed[[length(data_train_windowed) - 1]]
	data_train_windowed[[length(data_train_windowed)]]

	# Window the labels data
	window_labels_xts <- function(data) {
		sapply((lstm_num_timesteps + 1):(length(data)), function(x) data[x])
	}

	labels_train_windowed <- window_labels_xts(dataFutures_train, lstm_num_timesteps)
	labels_val_windowed <- window_labels_xts(dataFutures_val, lstm_num_timesteps)
	labels_test_windowed <- window_labels_xts(dataFutures_test, lstm_num_timesteps)
	labels_train_windowed[[1]]


	# Keras LSTMs expect the input array to be shaped as (no. samples, no. time steps, no. features),
	# yet we have just the two axes, need to add an axis "no.features" at the end
	# which will just say that we have 1 feature (we have just one column/price to feed).
	dim(data_train) # two dimensions, (no. samples, no. time steps)
	class(data_train) # a matrix
	dim(data_val)
	dim(data_test)

	# From Keras 2.1.2 we can use k_expand_dims() and k_eval() functions, instead of "subvariables".
	data_train <- k_eval(k_expand_dims(data_train, axis = -1)) # -1 for the last axis to expand at
	data_val <- k_eval(k_expand_dims(data_val, axis = -1))
	data_test <- k_eval(k_expand_dims(data_test, axis = -1))
	dim(data_train) # three dimensions (no. samples, no. time steps, no. features)
	str(data_train)
	class(data_train) # array
	data_train[1:5, 1:4, ]


	# LSTM input shape: (samples, time steps, features)
	num_samples <- dim(data_train)[1]
	num_steps <- dim(data_train)[2]
	num_features <- dim(data_train)[3]
	c(num_samples, num_steps, num_features)



	#############################
	####### Parameters ##########
	#############################

	# TensorBoard initialisation/stopping
	# dir.create("TensorBoard_logs")
	tensorboard(log_dir = "TensorBoard_logs/tmp2", host = "127.0.0.1", port = "1001", launch_browser = utils::browseURL("http://127.0.0.1:1001"), reload_interval = 5) # Launch TensorBoard and wait for output in specified directory
	# tensorboard(log_dir = "TensorBoard_logs", host = "127.0.0.1", port = "1001", reload_interval = 5) # Launch TensorBoard manually at http://127.0.0.1:1001 and wait for output in specified directory
	tensorboard(log_dir = "TensorBoard_logs/tmp2", action = "stop") # stops the TensorBoard
	# browseURL("http://127.0.0.1:1001") # launches the TensorBoard url

	##### TBD Tune #####
	# Define manually parameters here that will be used in the model
	### MAE ###
		n_epochs <- 5 # for now 50 takes 10min for smallest network, later obviously need to train longer to optimise until it starts overfitting
		batch <- 128

		parameters_architecture <- list(nlayers = 1, units = c(8, 0, 0), dropout = 0.2, recurrent_dropout = 0.2) # 11s per epoch on a month of data
		parameters_compile <- list(loss = "mae", optimizer = "rmsprop", metrics = "mse") ### for MAE ###
		# parameters_compile <- list(loss = "mse", optimizer = "rmsprop", metrics = "mae") ### for MSE trials ###
		parameters_fit <- list(epochs = n_epochs, batch_size = batch)
		parameters_1 <- c(parameters_architecture, parameters_compile, parameters_fit)

		parameters_architecture <- list(nlayers = 1, units = c(32, 0, 0), dropout = 0.2, recurrent_dropout = 0.2)
		# parameters_fit <- list(epochs = n_epochs, batch_size = batch) # if we want to change sth
		parameters_2 <- c(parameters_architecture, parameters_compile, parameters_fit)

		parameters_architecture <- list(nlayers = 2, units = c(8, 16, 0), dropout = 0.1, recurrent_dropout = 0.5)
		parameters_3 <- c(parameters_architecture, parameters_compile, parameters_fit)

		parameters_architecture <- list(nlayers = 2, units = c(32, 64, 0), dropout = 0.1, recurrent_dropout = 0.5)
		parameters_4 <- c(parameters_architecture, parameters_compile, parameters_fit)

		parameters_architecture <- list(nlayers = 3, units = c(32, 64, 128), dropout = 0.1, recurrent_dropout = 0.5) # 43s per epoch
		parameters_5 <- c(parameters_architecture, parameters_compile, parameters_fit)

		parameters <- list(NN_1L_6N = parameters_1, NN_1L_32N = parameters_2,
						   NN_2L_6N12N = parameters_3, NN_2L_32N64N = parameters_4,
						   NN_3L_32N32N64N = parameters_5)
		str(parameters)

	# Keras LSTM model
	# Looping the layers to automate it
	build_model_n <- function(nlayers = 1, units = rep(6, nlayers), dropout = 0, recurrent_dropout = 0,
							  loss = "mae", optimizer = "rmsprop", metrics = "mse", ...) {
		k_clear_session() # clearing the model/session first with backend to avoid TensorBoard errors
		
		model <- keras_model_sequential()
		for (i in 1:nlayers) { # hidden layers
			# model %>% layer_cudnn_lstm(units = units[i], return_sequences = (i != nlayers), input_shape = c(num_steps, num_features))
			# AttributeError: module 'tensorflow_core.keras.layers' has no attribute 'CuDNNLSTM'
			model %>% layer_lstm(units = units[i], return_sequences = (i != nlayers), dropout = dropout, recurrent_dropout = recurrent_dropout, input_shape = c(num_steps, num_features))
		}
		model %>% layer_dense(units = 1) # output layer

		model %>% compile(
			loss = loss,
			optimizer = optimizer,
			metrics = metrics
		)
	}

	save <- function(what, name) {
		save_model_hdf5(what, paste0("Models/tmp2/Model_", name, ".h5"))
		save.image(file = paste0("Workspaces/tmp2/Image_", name, "_trained.RData"))
	}



	#############################
	######## Fitting ############
	#############################

	# model_names <- c()
	len <- 1
	# for (i in 1:len) {
		i <- 1 # number of the run
		print(paste("Iteration", i, "/", len))
		for (hyp_tune in 1:1) { # 1:5 or 1:(length(parameters)
			hyp_tune <- 1 # in case u want to skip the for loop and manually select which one to train
			print(paste("Hyperparameter tuning", hyp_tune, "/", length(parameters)))

			model <- do.call(what = build_model_n, args = parameters[[hyp_tune]])
			print(model)

			layers_name <- c()
			for (j in 1:parameters[[hyp_tune]]$nlayers) {layers_name <- paste0(layers_name, model$layers[[j]]$units, "N-")}
			model_name <- paste0(futurenames[future], "-1H-2003-2017-LSTM-k", lstm_num_timesteps, "-", length(model$layers) - 1, "L-", layers_name,
								  100 * model$layers[[1]]$dropout, "pDr-", 100 * model$layers[[1]]$recurrent_dropout, "pRDr-",
								  toupper(model$loss),"-", parameters_compile$optimizer, "-", toupper(model$metrics_names[2]),"-", parameters[[hyp_tune]]$epochs, "E_run_", i)
			print(paste("Training model", model_name))

			# dir.create("TensorBoard_logs", showWarnings = FALSE)
			log_run_dir <- paste0("TensorBoard_logs/tmp2/", model_name)
			model_checkpoint_dir <- paste0("Models/checkpoints/tmp2/", model_name)
			dir.create(model_checkpoint_dir, showWarnings = FALSE, recursive = TRUE)
			model_checkpoint_name <- "/{epoch:02d}E-{val_loss:.5f}vloss.hdf5"

			callbacks_list = list(
				callback_tensorboard(
					log_dir = log_run_dir,
					histogram_freq = 1 # Records activation histograms every 1 epoch
					# embeddings_freq = 1 # Records embedding data every 1 epoch
				),
				# callback_early_stopping(
				# 	monitor = "val_loss",
				# 	patience = 100
				# ),
				callback_model_checkpoint(
					filepath = paste0(model_checkpoint_dir, "/", model_checkpoint_name),
					monitor = "val_loss"
					# save_best_only = TRUE # uncomment if u want to checkpoint only the best models, but when it's commented out, all models will be written (provided the model_checkpoint_dir name for each model varies w/ next epoch..)
				)
			)

			history <- model %>% fit(
				data_train, labels_train,
				epochs = parameters[[hyp_tune]]$epochs, batch_size = parameters[[hyp_tune]]$batch_size, # batch_size = 128, epochs = 500 for real run; epochs = i*ep for increasing number of epochs w/ iterations
				validation_data = list(data_val, labels_val), # computes loss and accuracy for validation data after every epoch
				# validation_split = 0.2,
				callbacks = callbacks_list, # for TensorBoard logging
				# initial_epoch = 100, # TBD try, integer, Epoch at which to start training (useful for resuming a previous training run).
				verbose = 2
			)

			# dir.create("Workspaces/runs", showWarnings = FALSE, recursive = TRUE)
			save(what = model, name = model_name)
		}
	# }

	plot(history)



#####################
####### Notes #######
#####################

# TBD:
	# revive
		# add yields data instead of prices

	# extensive hyperpar tuning once the code is scalable

