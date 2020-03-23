# Master Thesis
## Deep Learning for Term Structure Forecasting
### Author: Bc. David Koubek
#### Supervisor: doc. PhDr. Jozef Baruník, Ph.D.
#### Academic Year: 2019/2020
31 July 2020

### Introduction
We analyse high frequency datasets on US and EU bond futures using a combination of factor and neural network models. We use the Dynamic Nelson Siegel factor models for preparing the datasets into a time series of factors, and then train various deep learning models for forecasting the factors. An essential part of this work is the extensive hyperparameter tuning of the deep learning models in order to arrive at a well performing model for the task at hand. A lot of attention is devoted to designing the hyperparameter tuning scheme and to regularisation techniques for fighting the overfitting of our complex models.

### Thesis structure
#### Methodology
 - The factor models used are described in the [Methodology/Factor_models.ipynb](https://github.com/mrkoubek/deep-learning-for-term-structure-forecasting/blob/master/Methodology/Factor_models.ipynb).
 
#### Data Analysis
 - The basic analysis is summarised in a Jupyter notebook file in [Data Analysis/Data_analysis.ipynb](https://github.com/mrkoubek/deep-learning-for-term-structure-forecasting/blob/master/Data%20Analysis/Data_analysis.ipynb). In this file we perform and present the data preprocessing (loading, merging, cleaning), data visualisation, and summary statistics.

### Hyperparameter Tuning
Remember your ABCs.
 - Always
 - Be
 - Comparing
 
### Contribution
Extensive hyperparameter tuning making sure the results are robust. We use rather unique high frequency datasets and the combination of neural network and factor models is also scarce in literature. Our contribution makes previous results in the field more (or less) trustworthy and provides a usable template for future research using deep learning in R and Python.