# ebmeddev v0.1.0

**Author:** Jess Hsing, Dingjing Shi

**Email:** jesscc.hsing@gmail.com

**Bayesian Mediation Analysis for Multiple Predictors and Mediators in R**

---

## Overview

`ebmeddev` is an R package for running **Empirical Bayes Mediation (EBMed)** models. It allows you to:

- Model multiple **predictors** (independent variables) at once
- Model multiple **mediators**
- Fit Bayesian mediation models using **JAGS**
- Easily prepare data and run models with a single wrapper function

---

## Installation

You can install the development version from GitHub:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install ebmeddev from GitHub
devtools::install_github("olfactorybulb/ebmeddev")

library(ebmeddev)

# Example dataset
data(College)

# Prepare data for EBMed model
bdata <- prepare_ebmed_data(
  data = College,
  x = "Accept",            # predictor
  y = "Enroll",            # outcome
  mediators = c("Outstate", "Room.Board", "Books") # mediators
)

# Fit model
fit <- fit_med(
  data = bdata,
  n.iter = 1000,           # number of MCMC iterations
  n.chains = 2
)

# Summarize results
summary(fit)

## Citation
If you use `ebmeddev` in your research, please cite:
Dingjing Shi, Dexin Shi & Amanda J. Fairchild (2023) Variable Selection for Mediators under a Bayesian Mediation Model, Structural Equation Modeling: A Multidisciplinary Journal, 30:6, 887-900, DOI: 10.1080/10705511.2022.2164285
