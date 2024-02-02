# simulateR

Simple and modular Monte Carlo simulations for common statistical analyses.

[TOC]



## Usage

Install the package directly from GitHub, then view the vignettes in RStudio.

1. Run the following code:

```R
devtools::install_github("ianhussey/simulateR")
library(simulateR)
?simulateR
```

2. Click "Index" and then "User guides, package vignettes and other documentation." to view a list of all vignettes.

# TODO

- documentation of each function needs improving
- Vignette
- revise data generation functions, simplify the single one to not use any latent variables, check that alpha value is recovered when effect size is zero
- add estimates of dispersion to summarize_mean_estimates
- standardize language. currently data preprocessing creates data processed, which seems contradictory.
- add str_remove_all(pop_model, " ") to generate_data so that it can handle models with and without spaces
- vignette("simulateR") not working
- data generation is done using lavaan::simulateData. however note this may cause problems for correlations/simulating standardized data: https://cran.r-project.org/web/packages/simstandard/vignettes/simstandard_tutorial.html
  - The is commented out code for using simstandard::sim_standardized() instead, but this breaks when using beta values over 1 (ie non standardized data). Maybe create a new standardized argument that toggles which is to be used.

- elaborate the decision variables especially for mediation, where there are multiple p values produced. not sure if i've created a single global decision making variable.
- How to vary the population SD between variables? Right now it defaults to 1.
- swap out the examples in individual files for simpler ones
- rework data generation for factorial designs
- change data_raw and data_processed to data_item_level and data_variable_level
- compare my likert function to faux's
- add score latent function and check if item level indicators works again, for continuous and experimental
- rename generate data cohens d to experimental?
