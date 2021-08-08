# Biomass-Demand-Analysis-model(BDA model)


BDA model is a machine learning estimation to recover the massive biomass consumption in China. Estimates revealed large uncounted biomass consumption in China between 1992-2015. These estimates were produed within a Bayesian framework using a spatially and temporally explicit generalized linear regression model. The dataset include household survey data (such as family income, fiamliy size, building area) and provincial statistics data (such as farmland, forest area, rural road length).  All data are available from the corresponding author Chu wei (xiaochu@ruc.edu.cn) upon reasonable request.


# The codes include:
1) process_data.R is the function to prepare the data
2) biomass_main.R is the main script of code for the paper
3) inla_compute.R is functions to run Bayesian spatial-temproal model
4) choice_variable.R is functions to make variable selection
5) model_validation.R is functions to make cross-validation
6) biomass_plot.R is functions to plot pictures
7) model_selection.R is functions to select the best model

