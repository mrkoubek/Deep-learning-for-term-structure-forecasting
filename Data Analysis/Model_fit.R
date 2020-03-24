# Master Thesis
# Model fitting follows
# v1.0 - first preprocessing split and graph for small data

rm(list = ls())

# Packages
if (!require("pacman")) install.packages("pacman") # installs pacman package if not installed yet
pacman::p_load(pacman, dplyr, tidyr, ggplot2, scales, Cairo, zoo, xtable, tibble) # load packages TBD e.g. these, edit when u determine which needed/used..

# Workspace
setwd("E:/Google_Drive/Diploma_Thesis/Code")
# load(file = "Workspaces/Data_03_trimmed-small_US.RData") # 0min
load(file = "Workspaces/Data_04_split-small_US_tick_05-2019.RData") # 0min
print("Workspace rdy set go!")

# Picks the future
futurenames
future <- 1
futurenames[future]

# Train and test split
# dataFutures_tmp <- data_small_aday$Close # one day of data
dataFutures_tmp <- data_small_amonth$Close # one month of data
(end <- length(dataFutures_tmp))
(split_train <- round(3/5 * end))
(split_val <- round(4/5 * end))

dataFutures_train <- dataFutures_tmp[1:split_train]
dataFutures_val <- dataFutures_tmp[(split_train + 1):split_val]
dataFutures_test <- dataFutures_tmp[(split_val + 1):end]

# Differencing the data - optional, but better results
dataFutures_train_orig <- dataFutures_train # backup of original data for the end "undiff"
dataFutures_val_orig <- dataFutures_val
dataFutures_test_orig <- dataFutures_test

dataFutures_train <- diff(dataFutures_train)
dataFutures_val <- diff(dataFutures_val)
dataFutures_test <- diff(dataFutures_test)
summary(dataFutures_train)

# Graph of the split original dataset
	data_df <- data_frame(time = 1:(length(dataFutures_train_orig) + length(dataFutures_val_orig) + length(dataFutures_test_orig)),
	                 	  train = c(dataFutures_train_orig, rep(NA, length(dataFutures_val_orig) + length(dataFutures_test_orig))),
	                 	  val = c(rep(NA, length(dataFutures_train_orig)), dataFutures_val_orig, rep(NA, length(dataFutures_test_orig))),
	                 	  test = c(rep(NA, length(dataFutures_train_orig) + length(dataFutures_val_orig)), dataFutures_test_orig))
	data_df <- data_df %>% gather(key = 'train_val_test', value = 'value', -time) # -time

	custom_scale <- 1.5 # for the graph to look nice, sizes of graph items and fonts
	dataset_graph <- ggplot(data_df, aes(x = time, y = value, color = train_val_test)) +
		geom_line() +
		labs(x = "obs", y = "price") +
		# scale_x_continuous(breaks = c(1, round(seq(20000, end - 3, by = 20000), 1), split_train - 1, split_val - 2, end - 3)) +
		# scale_x_continuous(breaks = pretty_breaks(5)) +
		scale_colour_discrete(name = "Dataset", breaks = c("train", "val", "test"), labels = c("train", "validate", "test")) +
		guides(colour = guide_legend(override.aes = list(size = 4/3 * custom_scale))) +
		theme_bw(base_family = "LM Roman 10", base_size = 16 * custom_scale) +
		theme(legend.key.height = unit(1 * custom_scale, "lines"))
		# theme(legend.key.height = unit(1 * custom_scale, "lines"), axis.text.x = element_text(angle = 35, vjust = 0.8))
	dataset_graph

# Graph of the split differenced dataset
	data_df <- data_frame(time = 1:(length(dataFutures_train) + length(dataFutures_val) + length(dataFutures_test)),
	                 	  train = c(dataFutures_train, rep(NA, length(dataFutures_val) + length(dataFutures_test))),
	                 	  val = c(rep(NA, length(dataFutures_train)), dataFutures_val, rep(NA, length(dataFutures_test))),
	                 	  test = c(rep(NA, length(dataFutures_train) + length(dataFutures_val)), dataFutures_test))

	data_df <- data_df %>% gather(key = 'train_val_test', value = 'value', -time) # -time

	custom_scale <- 1.5 # for the graph to look nice, sizes of graph items and fonts
	split_graph <- ggplot(data_df, aes(x = time, y = value, color = train_val_test)) +
		geom_line() +
		labs(x = "obs", y = "price difference") +
		# scale_x_continuous(breaks = c(1, round(seq(20000, end - 3, by = 20000), 1), split_train - 1, split_val - 2, end - 3)) +
		# scale_x_continuous(breaks = pretty_breaks(5)) +
		scale_colour_discrete(name = "Dataset", breaks = c("train", "val", "test"), labels = c("train", "validate", "test")) +
		guides(colour = guide_legend(override.aes = list(size = 4/3 * custom_scale))) +
		theme_bw(base_family = "LM Roman 10", base_size = 16 * custom_scale) +
		theme(legend.key.height = unit(1 * custom_scale, "lines"))
		# theme(legend.key.height = unit(1 * custom_scale, "lines"), axis.text.x = element_text(angle = 35, vjust = 0.8))
	split_graph

# Save the graphs to file
	GoldenRatio <- (1 + sqrt(5)) / 2
	# plots_width <- 5.55226 # width in inches of thesis template textwidth
	plots_width <- 10 * custom_scale # 10 inches plus have nicely smallish graph elements, adjust custom scale for each graph type what looks nice
	plots_height <- plots_width / GoldenRatio

# A day
	# ggsave(dataset_graph, filename = paste0("Graphs/dataset_", futurenames[future], "_aday.pdf"), device = cairo_pdf,
	# 	width = plots_width, height = plots_height, units = "in")
	# ggsave(split_graph, filename = paste0("Graphs/differenced_", futurenames[future], "_aday.pdf"), device = cairo_pdf,
	# 	width = plots_width, height = plots_height, units = "in")

# A month
	ggsave(dataset_graph, filename = paste0("Graphs/dataset_", futurenames[future], "_amonth.pdf"), device = cairo_pdf,
		width = plots_width, height = plots_height, units = "in")
	ggsave(split_graph, filename = paste0("Graphs/differenced_", futurenames[future], "_amonth.pdf"), device = cairo_pdf,
		width = plots_width, height = plots_height, units = "in")


# Save workspace
# save.image(file = paste0("Workspaces/Data_04_split-small_", futurenames[future], "_tick_05-2019.RData")) # 1min 21MB




########################## TRAINING ##########################
ls()

