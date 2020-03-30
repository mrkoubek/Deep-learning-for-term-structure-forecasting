# Master Thesis
# Model evaluation follows
# v1.1 - added automatic evaluation (compariton of models)

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
	n_of_models <- length(model_names)


	for (n in model_names) {
		# n <- model_names[1]
		print(paste("Merging scores for model", n))
		
		load(file = paste0("Workspaces/tmp/Image_", n, "_trained.RData"))
		model <- load_model_hdf5(paste0("Models/tmp/Model_", n, ".h5"))

		train_all_scores <- eval_add(to_update_scores = train_all_scores, data = data_train, labels = labels_train)
		val_all_scores <- eval_add(to_update_scores = val_all_scores, data = data_val, labels = labels_val)
		test_all_scores <- eval_add(to_update_scores = test_all_scores, data = data_test, labels = labels_test)
	}

	train_all_scores <- as.data.frame(train_all_scores, row.names = c(paste0("train_model_", 1:n_of_models)), col.names = c(model$loss, model$metrics_names[2]))
	val_all_scores <- as.data.frame(val_all_scores, row.names = c(paste0("val_model_", 1:n_of_models)), col.names = c(model$loss, model$metrics_names[2]))
	test_all_scores <- as.data.frame(test_all_scores, row.names = c(paste0("test_model_", 1:n_of_models)), col.names = c(model$loss, model$metrics_names[2]))

	train_all_scores
	val_all_scores
	test_all_scores


	# TBD
	(eval_train <- model %>% evaluate(data_train, labels_train, verbose = 0))
	(eval_val <- model %>% evaluate(data_val, labels_val, verbose = 0))
	(eval_test <- model %>% evaluate(data_test, labels_test, verbose = 0))

	pred_train <- model %>% predict(data_train, batch_size = 128)
	pred_val <- model %>% predict(data_val, batch_size = 128)
	pred_test <- model %>% predict(data_test, batch_size = 128)








#################################################################
#################################################################
#################################################################

# Load up the list of models for a future pair
futurenames
future <- 1
futurenames[future]
model_names <- list.files(path = paste0("Models/checkpoints/"))
model_names <- naturalsort(model_names)
model_names
n_of_models <- length(model_names)
max_epoch <- 500

### Hyperparameter picking best according to validation set ###
model_row_names_tables <- paste0(futurenames[future], ".model.", 1:n_of_models)

# Evaluating the best number of epochs and plotting the training/validation
best_epoch_all <- data.frame(NULL)
history_train_plots <- vector("list", n_of_models)
history_val_plots <- vector("list", n_of_models)
history_train_all <- data.frame(epoch = 1:max_epoch)
history_val_all <- data.frame(epoch = 1:max_epoch)
names(history_train_plots) <- model_names
names(history_val_plots) <- model_names

iterator <- 1

for (n in model_names) {
	print(paste("Evaluating optimal number of epochs for model", n))
	
	load(file = paste0("Workspaces/tmp/Image_", n, "_trained.RData"))
	model <- load_model_hdf5(paste0("Models/tmp/Model_", n, ".h5"))

	history_df <- as.data.frame(history)
	history_val <- history_df[(history_df$metric == "loss") & (history_df$data == "validation"), ]
	# history_val <- history_df[(history_df$metric == "mean_squared_error") & (history_df$data == "validation"), ] ### for MSE trials ###
	history_val <- droplevels(history_val)
	print(history_val[which.min(history_val$value), ])

	best_epoch_all <- rbind(best_epoch_all, history_val[which.min(history_val$value), ])

	# Advanced plotting
	# Reproducing the plot(history) keras function with ggplot to look the same and then have more control over custom labels etc:
	# Separate training and validation data graphs
	history_train <- history_df[(history_df$metric == "loss") & (history_df$data == "training"), ]
	history_train <- droplevels(history_train)
	history_train_plots[[iterator]] <- ggplot(history_train, aes(x = epoch, y = value)) +
		geom_point(aes(fill = data), shape = 21, colour = "black") +
		geom_smooth(method = "loess", span = 0.3, aes(colour = data)) +
		xlab("epoch") +
		ylab(paste0("loss (", toupper(model$loss), ")")) +
		# ggtitle(paste("Training history of LSTM")) +
		# scale_x_continuous(breaks = seq(0, 500, 50)) + # TBD look at these numbers and try rewrite to variables?
		scale_y_continuous(breaks = pretty_breaks(5)) +
		theme_bw(base_family = "LM Roman 10", base_size = 16)

	history_train_all <- cbind(history_train_all, history_train$value)

	history_val_plots[[iterator]] <- ggplot(history_val, aes(x = epoch, y = value)) +
		geom_point(aes(fill = data), shape = 21, colour = "black") +
		geom_smooth(method = "loess", span = 0.3, aes(colour = data)) +
		# geom_smooth(method = "loess", method.args = list(family = "symmetric"), span = 0.3, aes(colour = data)) + # for geom to be resistant to outliers
		xlab("epoch") +
		ylab(paste0("loss (", toupper(model$loss), ")")) +
		# ggtitle("Training and validation history of LSTM") + 
		theme_bw(base_family = "LM Roman 10", base_size = 16) +
		# geom_line(aes(x = 199, y = value)) +
		geom_point(data=history_val[history_val[which.min(history_val$value), "epoch"], ],
					aes(x = epoch, y = value), colour = "blue", size = 3)

	history_val_all <- cbind(history_val_all, history_val$value)

	iterator <- iterator + 1
}

best_epoch_all <- data.frame(model_row_names_tables, best_epoch_all)
rownames(best_epoch_all) <- NULL

best_epoch_all

# best_epoch_all_MAE <- best_epoch_all
# best_epoch_all_MSE <- best_epoch_all ### for MSE trials ###

best_epoch_all_sorted <- best_epoch_all[order(best_epoch_all$value), ]

best_epoch_all_sorted

history_train_plots[[n_of_models]]
history_val_plots[[1]]

colnames(history_train_all) <- c("epoch", model_row_names_tables)
colnames(history_val_all) <- c("epoch", model_row_names_tables)

custom_scale <- 1.5 # for the graph to look nice, sizes of graph items and fonts
tr <- history_train_all %>%
	gather(model, value, -epoch)
history_all_train_plot_500 <- ggplot(tr, aes(x = epoch, y = value, colour = model)) + 
	geom_point(aes(fill = model), shape = 21, colour = "black", alpha = 0.2) +
	geom_smooth(method = "loess", span = 0.3, aes(colour = model), se = FALSE) +
	xlab("epoch") +
	ylab(paste0("loss (", toupper(model$loss), ")")) +
	scale_y_continuous(breaks = pretty_breaks(5)) +
	scale_colour_discrete(breaks = model_row_names_tables, labels = 1:n_of_models) +
	scale_fill_discrete(breaks = model_row_names_tables, labels = 1:n_of_models) +
	guides(colour = guide_legend(override.aes = list(size = 4/3 * custom_scale, alpha = 1))) +
	theme_bw(base_family = "LM Roman 10", base_size = 16 * custom_scale) +
	theme(legend.key.height = unit(1 * custom_scale, "lines"))
	history_all_train_plot_500

zoom_in_epochs <- 50
small_tr <- tr[tr$epoch %in% 1:zoom_in_epochs, ]
history_all_train_plot_50 <- ggplot(small_tr, aes(x = epoch, y = value, colour = model)) + 
	geom_smooth(method = "loess", span = 0.3, aes(colour = model), se = FALSE) +
	xlab("epoch") +
	ylab(paste0("loss (", toupper(model$loss), ")")) +
	scale_y_continuous(breaks = pretty_breaks(5)) +
	scale_colour_discrete(breaks = model_row_names_tables, labels = 1:n_of_models) +
	guides(colour = guide_legend(override.aes = list(size = 4/3 * custom_scale))) +
	theme_bw(base_family = "LM Roman 10", base_size = 16 * custom_scale) +
	theme(legend.key.height = unit(1 * custom_scale, "lines"))

pointz <- c()
for (i in 1:n_of_models) {pointz <- c(pointz, best_epoch_all$epoch[i] + ((i-1) * max_epoch))}
vl <- history_val_all %>%
	gather(model, value, -epoch)
history_all_val_plot_500 <- ggplot(vl, aes(x = epoch, y = value, colour = model)) + 
	geom_point(aes(fill = model), shape = 21, colour = "black", alpha = 0.1) +
	geom_smooth(method = "loess", span = 0.3, aes(colour = model), se = FALSE) +
	# geom_point(data = vl[pointz, ], aes(x = epoch, y = value), colour = "blue", size = 2) +
	xlab("epoch") +
	ylab(paste0("loss (", toupper(model$loss), ")")) +
	scale_y_continuous(breaks = pretty_breaks(5)) +
	scale_colour_discrete(breaks = model_row_names_tables, labels = 1:n_of_models) +
	scale_fill_discrete(breaks = model_row_names_tables, labels = 1:n_of_models) +
	guides(colour = guide_legend(override.aes = list(size = 4/3 * custom_scale, alpha = 1))) +
	theme_bw(base_family = "LM Roman 10", base_size = 16 * custom_scale) +
	theme(legend.key.height = unit(1 * custom_scale, "lines"))

small_vl <- vl[vl$epoch %in% 1:zoom_in_epochs, ]
history_all_val_plot_50 <- ggplot(small_vl, aes(x = epoch, y = value, colour = model)) +
	geom_smooth(method = "loess", span = 0.3, aes(colour = model), se = FALSE) +
	xlab("epoch") +
	ylab(paste0("loss (", toupper(model$loss), ")")) +
	scale_y_continuous(breaks = pretty_breaks(5)) +
	scale_colour_discrete(breaks = model_row_names_tables, labels = 1:n_of_models) +
	guides(colour = guide_legend(override.aes = list(size = 4/3 * custom_scale))) +
	theme_bw(base_family = "LM Roman 10", base_size = 16 * custom_scale) +
	theme(legend.key.height = unit(1 * custom_scale, "lines"))

GoldenRatio <- (1 + sqrt(5)) / 2
# plots_width <- 5.55226 # width in inches of thesis template textwidth
plots_width <- 10 * custom_scale # 10 inches plus have nicely smallish graph elements, adjust custom scale for each graph type what looks nice
plots_height <- plots_width / GoldenRatio
ggsave(history_all_train_plot_500, filename = paste0("Graphs/history_all_train_plot_500_", futurenames[future], ".pdf"),
		device = cairo_pdf, width = plots_width, height = plots_height, units = "in")
ggsave(history_all_train_plot_50, filename = paste0("Graphs/history_all_train_plot_50_", futurenames[future], ".pdf"),
		device = cairo_pdf, width = plots_width, height = plots_height, units = "in")
ggsave(history_all_val_plot_500, filename = paste0("Graphs/history_all_val_plot_500_", futurenames[future], ".pdf"),
		device = cairo_pdf, width = plots_width, height = plots_height, units = "in")
ggsave(history_all_val_plot_50, filename = paste0("Graphs/history_all_val_plot_50_", futurenames[future], ".pdf"),
		device = cairo_pdf, width = plots_width, height = plots_height, units = "in")















### COMPARISON OF THE MODELS, automatic evaluation ###
stats <- function(price, pred, error_Model, error_RW, RW = FALSE) {
	MAE <- mean(abs(error_Model), na.rm = TRUE)
	RMSE <- sqrt(mean(error_Model^2, na.rm = TRUE))
	MAPE <- 100 * mean(abs(error_Model / price), na.rm = TRUE)
	MBE <- mean(error_Model, na.rm = TRUE)

	if (RW == TRUE) { # For first RW row to which the other models are compared by DM
		DM <- NA
		p1p <- NA

		DM2 <- NA
		p2p <- NA
	} else {
		# Diebold-Mariano test for predictive accuracy
		# https://www.rdocumentation.org/packages/forecast/versions/8.1/topics/dm.test
		# https://stats.stackexchange.com/questions/139462/diebold-mariano-test-for-predictive-accuracy
		# if the p-value is above your desired significance level, the results indicate that the researcher cannot reject the hypothesis of equal expected forecast error, or forecast accuracy
		# Or one can compare the DM value to the ±1.96 criteria, because it is normally distributed
		# power = 2 (default) - the loss function is quadratic (~ for RMSE/MSE like comparison prolly)
		# power = 1 - the loss function is linear (~ for MAE like comparison)
		diebold <- dm.test(error_RW, error_Model, alternative = "two.sided", h = 1, power = 1) # alternative = "less" / "two.sided" / "greater". For alternative = "greater", the alternative hypothesis is that method 2 is more accurate than method 1.
		DM <- diebold$statistic
		p1p <- diebold$p.value

		diebold2 <- dm.test(error_RW, error_Model, alternative = "two.sided", h = 1, power = 2) # alternative = "less" / "two.sided" / "greater". For alternative = "greater", the alternative hypothesis is that method 2 is more accurate than method 1.
		DM2 <- diebold2$statistic
		p2p <- diebold2$p.value
	}

	c(MAE = MAE, RMSE = RMSE, MAPE = MAPE, MBE = MBE,
		DM.test = DM, pvalue = p1p, DM2.quad = DM2, DM2.pval = p2p) # return a vector of resulting stats
}

eval_add <- function(to_update_scores, data_eval, iter) {
	eval <- data_eval

	eval <- eval %>% mutate(pred_RW = c(NA, price[1:(length(price) - 1)]),
						  error_NN = pred_NN - price)

	eval <- eval %>% mutate(error_RW = pred_RW - price)

	eval <- eval[!is.na(eval$pred_NN) & !is.na(eval$pred_RW), ] # this is needed for fair comparison of stats below, the NN has 1000 steps lookback, and we need to compare the stats only to the same set of rows for the RW and NN, so we remove any rows in each model that have NAs

	RW_stats <- stats(price = eval$price, pred = eval$pred_RW, error_Model = eval$error_RW, error_RW = eval$error_RW, RW = TRUE)
	LSTM_stats <- stats(price = eval$price, pred = eval$pred_NN, error_Model = eval$error_NN, error_RW = eval$error_RW, RW = FALSE)

	for (i in 1:length(LSTM_stats)) {
		if (iter == 1) {
			to_update_scores[[i]] <- c(to_update_scores[[i]], RW_stats[i], use.names = FALSE)
		}

		to_update_scores[[i]] <- c(to_update_scores[[i]], LSTM_stats[i], use.names = FALSE)
	}

	return(to_update_scores)
}

# Aggregating the evaluation scores
train_all_scores <- list(MAE = NULL, RMSE = NULL, MAPE = NULL, MBE = NULL,
						DM.test = NULL, pvalue = NULL, DM2.quad = NULL, DM2.pval = NULL)
val_all_scores <- list(MAE = NULL, RMSE = NULL, MAPE = NULL, MBE = NULL,
						DM.test = NULL, pvalue = NULL, DM2.quad = NULL, DM2.pval = NULL)
test_all_scores <- list(MAE = NULL, RMSE = NULL, MAPE = NULL, MBE = NULL,
						DM.test = NULL, pvalue = NULL, DM2.quad = NULL, DM2.pval = NULL)

lstm_num_timesteps_max <- 4 # maximum number of timesteps among our models we are comparing (for our case we have k=4 and k=24),
# so for the comparisons to be fair, we need to compare them on the same data sets (without the first 24+1 observations (due to differencing the models don't have predictions for k+1 first steps)),
# otherwise the models would be compared on differently large sets, k=4 models would have only 5 first samples NA, k=24 has 25 NAs, so we crop it according to the largest stepback among our model (RW has 1 NA)

iterator <- 1
for (n in model_names) {
	print(paste("Merging scores for model", n))

	# Load the workspace and model
	load(file = paste0("Workspaces/tmp/Image_", n, "_trained.RData"))
	# TBD solve below that if best epoch is under number 10, this doesn't work and I needed to put "^0" there, rewrite so robust for any epoch
	model_path <- list.files(path = paste("Models/checkpoints", n, sep = "/"), pattern = paste0("^0", best_epoch_all$epoch[iterator], "E"), full.names = TRUE)
	model <- load_model_hdf5(model_path)

	# Predictions
	pred_train <- model %>% predict(data_train, batch_size = 128)
	pred_val <- model %>% predict(data_val, batch_size = 128)
	pred_test <- model %>% predict(data_test, batch_size = 128)

	# Undifferencing
	pred_train_undiff <- pred_train + dataFutures_train_orig[(lstm_num_timesteps + 1):(length(dataFutures_train_orig) - 1)]
	pred_val_undiff <- pred_val + dataFutures_val_orig[(lstm_num_timesteps + 1):(length(dataFutures_val_orig) - 1)]
	pred_test_undiff <- pred_test + dataFutures_test_orig[(lstm_num_timesteps + 1):(length(dataFutures_test_orig) - 1)]

	# Train
	eval_train <- data.frame(time = 1:length(dataFutures_train_orig),
		   			  price = dataFutures_train_orig,
		   			  pred_NN = c(rep(NA, lstm_num_timesteps + 1), pred_train_undiff))

	# Val
	eval_val <- data.frame(time = 1:length(dataFutures_val_orig),
					price = dataFutures_val_orig,
					pred_NN = c(rep(NA, lstm_num_timesteps + 1), pred_val_undiff))

	# Test
	eval_test <- data.frame(time = 1:length(dataFutures_test_orig),
		   			  price = dataFutures_test_orig,
		   			  pred_NN = c(rep(NA, lstm_num_timesteps + 1), pred_test_undiff))

	train_all_scores <- eval_add(to_update_scores = train_all_scores, data_eval = eval_train[-c(1:lstm_num_timesteps_max), ], iter = iterator)
	val_all_scores <- eval_add(to_update_scores = val_all_scores, data_eval = eval_val[-c(1:lstm_num_timesteps_max), ], iter = iterator)
	test_all_scores <- eval_add(to_update_scores = test_all_scores, data_eval = eval_test[-c(1:lstm_num_timesteps_max), ], iter = iterator)

	iterator <- iterator + 1
}

train_all_scores <- data.frame(Model = c("RW", paste0("NN.", 1:n_of_models)), as.data.frame(train_all_scores))
val_all_scores <- data.frame(Model = c("RW", paste0("NN.", 1:n_of_models)), as.data.frame(val_all_scores))
test_all_scores <- data.frame(Model = c("RW", paste0("NN.", 1:n_of_models)), as.data.frame(test_all_scores))

rownames(train_all_scores) <- c("RW", paste0("NN.", 1:n_of_models))
rownames(val_all_scores) <- c("RW", paste0("NN.", 1:n_of_models))
rownames(test_all_scores) <- c("RW", paste0("NN.", 1:n_of_models))

format_columns <- function(df, digits) {
	for (i in 2:length(df)) {
		if (i == 7 | i == 9) {
			df[, i] <- format.pval(round(df[, i], digits = digits[i]), eps = 0.001, na.form = "-")
		} else if (i == 6 | i == 8) {
			df[, i] <- gsub( "NA" , "-" , format(round(df[, i], digits = digits[i]), digits = digits[i], nsmall = digits[i]))
		} else {
			df[, i] <- gsub( "NA" , "-" , format(df[, i], digits = digits[i]))
		}
	}
	return(df)
}

digits_columns <- c(0, 5, 5, 5, 2, 2, 3, 2, 3)
train_all_scores <- format_columns(train_all_scores, digits_columns)
val_all_scores <- format_columns(val_all_scores, digits_columns)
test_all_scores <- format_columns(test_all_scores, digits_columns)

# train_all_scores
# val_all_scores
# test_all_scores

train_all_scores_ordered_by_MAE <- train_all_scores[order(train_all_scores$MAE), ]
val_all_scores_ordered_by_MAE <- val_all_scores[order(val_all_scores$MAE), ]
test_all_scores_ordered_by_MAE <- test_all_scores[order(test_all_scores$MAE), ]

# train_all_scores_ordered_by_MAE
# val_all_scores_ordered_by_MAE
# test_all_scores_ordered_by_MAE

train_all_scores_ordered_by_RMSE <- train_all_scores[order(train_all_scores$RMSE), ]
val_all_scores_ordered_by_RMSE <- val_all_scores[order(val_all_scores$RMSE), ]
test_all_scores_ordered_by_RMSE <- test_all_scores[order(test_all_scores$RMSE), ]

train_all_scores_ordered_by_RMSE
val_all_scores_ordered_by_RMSE
test_all_scores_ordered_by_RMSE


# Save
# save.image(file = paste0("Results/", futurenames[future], "/Tables/Image_", futurenames[future], "_evaluated.RData"))
load(file = paste0("Results/", futurenames[future], "/Tables/Image_", futurenames[future], "_evaluated.RData"))












#####################
####### Notes #######
#####################

# TBD:
	# repair TBDs
	# look at the Results folder, write code to auto create it and populate with best training?
	# split the code into separate scripts? quite long and confusing already

