# Master Thesis
# Model evaluation follows
# v1.0 - basic manual evaluation

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
py_config()
tf_config()
# devtools::session_info()

# Workspace
setwd("E:/Google_Drive/Diploma_Thesis/Code")
# load(file = "Workspaces/Data_03_trimmed-small_US.RData") # 0min
load(file = "Workspaces/Data_04_split-small_US_tick_05-2019.RData") # 0min
print("Workspace rdy set go!")


#############################
###### Evaluation #######
#############################

### Manually ###
	eval_add <- function(to_update_scores, data, labels) {
		results <- model %>% evaluate(data, labels, verbose = 0)
		for (i in 1:length(results)) {
			to_update_scores[[i]] <- c(to_update_scores[[i]], results[[i]])
		}
		return(to_update_scores)
	}

	train_all_scores <- list(loss = NULL, metric = NULL)
	val_all_scores <- list(loss = NULL, metric = NULL)
	test_all_scores <- list(loss = NULL, metric = NULL)

	model_names <- list.files(path = paste0("Models/checkpoints/"))
	model_names <- naturalsort(model_names)
	model_names


	for (n in model_names) {
		n <- model_names[1]
		print(paste("Merging scores for model", n))
		
		load(file = paste0("Workspaces/tmp/Image_", n, "_trained.RData"))
		model <- load_model_hdf5(paste0("Models/tmp/Model_", n, ".h5"))

		train_all_scores <- eval_add(to_update_scores = train_all_scores, data = data_train, labels = labels_train)
		val_all_scores <- eval_add(to_update_scores = val_all_scores, data = data_val, labels = labels_val)
		test_all_scores <- eval_add(to_update_scores = test_all_scores, data = data_test, labels = labels_test)
	}

	train_all_scores <- as.data.frame(train_all_scores, row.names = c(paste0("train_model_", 1:(length(model_names)))), col.names = c(model$loss, model$metrics_names[2]))
	val_all_scores <- as.data.frame(val_all_scores, row.names = c(paste0("val_model_", 1:(length(model_names)))), col.names = c(model$loss, model$metrics_names[2]))
	test_all_scores <- as.data.frame(test_all_scores, row.names = c(paste0("test_model_", 1:(length(model_names)))), col.names = c(model$loss, model$metrics_names[2]))

	train_all_scores
	val_all_scores
	test_all_scores


	(eval_train <- model %>% evaluate(data_train, labels_train, verbose = 0))
	(eval_val <- model %>% evaluate(data_val, labels_val, verbose = 0))
	(eval_test <- model %>% evaluate(data_test, labels_test, verbose = 0))

	pred_train <- model %>% predict(data_train, batch_size = 128)
	pred_val <- model %>% predict(data_val, batch_size = 128)
	pred_test <- model %>% predict(data_test, batch_size = 128)




#####################
####### Notes #######
#####################

# TBD:
	# add an automatic evaluation

