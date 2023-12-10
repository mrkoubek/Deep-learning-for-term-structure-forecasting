# Master Thesis
# 03.00-Model_factor_loadings.R
# Graph of loadings on the DNS factor model
# v1.0 - mostly finished everything



################################
### Workspace setup ############
################################

    rm(list = ls())

    # Packages
    if (!require("pacman")) install.packages("pacman") # installs pacman package if not installed yet
    pacman::p_load(pacman, ggplot2, tibble, tidyr, latex2exp) # load packages

    # Workspace
    setwd("E:/Google_Drive/Diploma_Thesis/Code")
    print("Workspace rdy set go!")



#############################
###### Generate data ########
#############################

    # First, we generate the x and y axes of the graph. The goal is to graph the level, slope, and curvature factor loadings vs maturity.
    # x axis has "Maturity (in years)"
    # y axis has "Factor loading"
    # Annotation on the side says "Level", "Slope", "Curvature"

    # The maturities, denoted tau, range from 0 to say mmax = 50.
        mmax <- 100 # maximum maturity in graph, in years
        tau <- seq(from = 0.1, to = mmax, by = 0.1) # we exclude zero, so the following calculations are not dividing by 0, and we add it after
        # tau <- c(1:mmax)
        # tau

    # The level factor has a static loading of 1, irrespective of maturity.
        loading_level <- rep(1, times = length(tau) + 1) # +1 for zero maturity
        # loading_level

    # Now the slope and curvature need to abide by the DNS equation
        lambdas <- c(Malinska = 0.1379, # from Malinska (2018), for maturity expressed in years, optimised lambda by SSE, peaks curvature at maturity about 7.5 years 
                     Diebold = 0.7173) # from Diebold & Li (2006) 0.0609 for maturity 30 months, which is 0.7173 for maturity expressed in years as Malinska (2018) p.5 says

    # A function to generate the loadings
        generate_loadings <- function(lambda) {
            # The slope loading:
            #  \frac{1 - e^{-\lambda_t \tau}}{\lambda_t \tau}
            loading_slope <- (1 - exp(-lambda * tau)) / (lambda * tau)
            loading_slope <- c(1, loading_slope) # at zero maturity it in the limit approaches 1

            # The curvature loading:
            # \frac{1 - e^{-\lambda_t \tau}}{\lambda_t \tau} - e^{-\lambda_t \tau}
            loading_curvature <- (1 - exp(-lambda * tau)) / (lambda * tau) - exp(-lambda * tau)
            loading_curvature <- c(0, loading_curvature) # at zero maturity it in the limit approaches 0

            # Add zero maturity to tau maturity vector, after the above calculations are done - they can't compute in this point, division by 0
            tau <- c(0, tau) 

            # Rearrange the data to one variable
            data_tibble <- tibble(maturity = tau,
                               level = loading_level,
                               slope = loading_slope,
                               curvature = loading_curvature)

            return(data_tibble)
        }

    # Call the generate_loadings() function and apply it to all lambda values
        data_tibble_list <- lapply(lambdas, generate_loadings)

    # Gather the tibble for a ggplot
        data_tibble_list_gathered <- lapply(data_tibble_list, function(x) gather(x, key = 'level_slope_curvature', value = 'value', -maturity))


    # Maximum of the curvature, TBD add to graph, or just text
        max(data_tibble_list[[1]]$curvature) # maximum value
        which(data_tibble_list[[1]]$curvature == max(data_tibble_list[[1]]$curvature)) # the index of the maximum value
        data_tibble_list[[1]][131, ] # 13 years maturity

        max(data_tibble_list[[2]]$curvature) # maximum value
        which(data_tibble_list[[2]]$curvature == max(data_tibble_list[[2]]$curvature)) # the index of the maximum value
        data_tibble_list[[2]][26, ] # 2.5 years maturity



#############################
###### Plot data ############
#############################

    # Basic plots
        plot(x = data_tibble_list$Malinska$maturity, y = data_tibble_list$Malinska$level)
        plot(x = data_tibble_list$Malinska$maturity, y = data_tibble_list$Malinska$slope)
        plot(x = data_tibble_list$Malinska$maturity, y = data_tibble_list$Malinska$curvature)

    # ggplot graph loadings    
        # A function for the main graphing
        graph_loadings <- function(data, custom_scale = 1.5) {
            graph_loadings <- ggplot(data, aes(x = maturity, y = value, color = level_slope_curvature)) + 
                # geom_point(aes(fill = level_slope_curvature), shape = 21, colour = "black") +
                geom_line(aes(color = level_slope_curvature, linetype = level_slope_curvature)) + 
                scale_x_continuous(breaks = pretty(data$maturity, n = mmax/10)) +
                scale_y_continuous(breaks = pretty(data$value, n = mmax/10)) +
                labs(x = "Maturity (in years)", y = "Factor loadings") + 
                # labs(x = TeX("Maturity $\\tau$ (in years)"), y = "Factor loadings") + # doesn't load the right fonts, tau is uptau so doesn't match the rest of thesis
                # ggtitle("Dynamic Nelson-Siegel factor loadings,\nfor a fixed optimised lambda = 0.1379") + # without title for thesis
                scale_colour_discrete(name = "Factor loadings", breaks = c("level", "slope", "curvature"), labels = c("level", "slope", "curvature")) +
                scale_linetype_manual(name = "Factor loadings", breaks = c("level", "slope", "curvature"), labels = c("level", "slope", "curvature"),
                                      values = c("solid", "dashed", "22")) + # changed scale_linetype_discrete to manual for now (need for values), seems to work
                guides(colour = guide_legend(override.aes = list(size = 4/3 * custom_scale))) + # doesn't do anything
                theme_bw(base_family = "LM Roman 10", base_size = 16 * custom_scale) +
                theme(legend.key.height = unit(1 * custom_scale, "lines"))

            return(graph_loadings)
        }

        custom_scale <- 1.1 # for the graph to look nice, sizes of graph items and fonts
        graphs_list <- lapply(data_tibble_list_gathered, function(x) graph_loadings(x, custom_scale = custom_scale))

        # Don't call str(graphs_list) else it freezes.
        graphs_list[[1]]
        graphs_list[[2]]

    # Save the graphs to file
        GoldenRatio <- (1 + sqrt(5)) / 2
        # plots_width <- 5.55226 # width in inches of thesis template textwidth
        plots_width <- 10 * custom_scale # 10 inches plus have nicely smallish graph elements, adjust custom scale for each graph type what looks nice
        plots_height <- plots_width / GoldenRatio

        for(i in 1:length(lambdas)) {
            ggsave(graphs_list[[i]], filename = paste0("Graphs/Model_factor/factor_loadings_theoretical_lambda-", names(graphs_list)[i], ".pdf"),
                                     device = cairo_pdf, width = plots_width, height = plots_height, units = "in")
        }



#############################
###### To Do ################
#############################

    # mark the maximum? At least calculate it

    # Optional:
    # The Diebold graph may be better in 50Y maturity instead of 100Y on x-axis, try it.



#############################
###### Notes ################
#############################

    # The Diebold graph may be better in 50Y maturity instead of 100Y on x-axis, but it's ok, at least same scale as Malinska

    # Other alternatives design-wise:
        # to force 0,0 origin w/o padding: https://stackoverflow.com/questions/13701347/force-the-origin-to-start-at-0
            # scale_x_continuous(breaks = pretty(data$maturity, n = mmax/10), expand = c(0, 0), limits = c(0, 105)) +
            # scale_y_continuous(breaks = pretty(data$value, n = mmax/10), expand = c(0, 0), limits = c(0, 1.05)) +
            # expand = expand_scale(mult=c(0,0.1))
            # expand = c(0, 0), limits = c(0,5)
            # expand = c(0, 0), limits = c(0, NA)
            # coord_cartesian(expand = FALSE)

        # Using LaTeX in label texts, but so far it loads wrong font for tau, it uptau while in rest of thesis we have noncurly tau
            # https://stackoverflow.com/questions/73378207/how-to-evaluate-latex-and-a-variable-in-a-ggplot-title

