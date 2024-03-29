# Master Thesis
## Deep Learning for Term Structure Forecasting
### Author: Bc. David Koubek
#### Supervisor: doc. PhDr. Jozef Baruník, Ph.D.
#### Academic Year: 2023/2024
#### Version: v1.2, December 2023 (WIP)

### Foreword
In v1.0 we focused on setting up the basic backbone in code for our neural network (NN) models and an excerpt of our datasets.
In v1.1 we focused on adding the Nelson-Siegel model before the NN training takes place.
In v1.2 we focus on polishing up some preprocessing issues with the data, treating missing observations.

### Introduction
We analyse high frequency datasets on US and EU bond futures using a combination of factor and neural network models. We use the Dynamic Nelson Siegel factor models for preparing the datasets into a time series of factors, and then train various deep learning models for forecasting the factors. An essential part of this work is the extensive hyperparameter tuning of the deep learning models in order to arrive at a well performing model for the task at hand. A lot of attention is devoted to designing the hyperparameter tuning scheme and to regularisation techniques for fighting the overfitting of our complex models.

### Thesis structure
#### Methodology
 - The factor models used will be described in the [Methodology/Factor_models.ipynb](https://github.com/mrkoubek/deep-learning-for-term-structure-forecasting/blob/master/Methodology/Factor_models.ipynb) - TBD, not written yet.
 
#### Data Analysis
 - The basic analysis will be summarised in a Jupyter notebook file in [Data Analysis/Data_analysis.ipynb](https://github.com/mrkoubek/deep-learning-for-term-structure-forecasting/blob/master/Data%20Analysis/Data_analysis.ipynb). In this file we perform and present the data preprocessing (loading, merging, cleaning), data visualisation, and summary statistics. - TBD, for now only noninteractive source code is accessible in [Data Analysis/Data_conversion.R](https://github.com/mrkoubek/deep-learning-for-term-structure-forecasting/blob/master/Data%20Analysis/Data_conversion.R) and [Data Analysis/Model_fit.R](https://github.com/mrkoubek/deep-learning-for-term-structure-forecasting/blob/master/Data%20Analysis/Model_fit.R). The models' evaluation is done in [Data Analysis/Model_evaluation.R](https://github.com/mrkoubek/deep-learning-for-term-structure-forecasting/blob/master/Data%20Analysis/Model_evaluation.R).

### Hyperparameter Tuning
Remember your ABCs.
 - Always
 - Be
 - Comparing
 
### Contribution
Extensive hyperparameter tuning making sure the results are robust. We use rather unique high frequency datasets and the combination of neural network and factor models is also scarce in literature. Our contribution makes previous results in the field more (or less) trustworthy and provides a usable template for future research using deep learning in R and Python.
