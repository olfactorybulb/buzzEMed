# buzzMed v0.1.2

**Exploratory Bayesian Mediation Analysis with Variable Selection**

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

## Overview

A collection of quantitative tools for selecting mediating effects within exploratory Bayesian mediation models. The package accommodates both continuous and dichotomous outcomes, including the dependent variables and the mediators for identifying and analyzing mediation pathways.

- **Multiple predictors and mediators**
- **Continuous or binary (0/1) mediators**
- **Continuous or binary (0/1) outcomes**
- **Automated model dispatch via formula syntax**

---

## Requirements

This package requires **JAGS (Just Another Gibbs Sampler)** to be installed on your system.
Download from: [https://mcmc-jags.sourceforge.io/](https://mcmc-jags.sourceforge.io/)

---

## Installation

You can install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("olfactorybulb/buzzMed")
library(buzzMed)
```

---

## Main Functions
The package provides a primary automated interface and four specialized model-fitting functions based on variable types:
- buzzEBMedAuto(): Automatically dispatches to the appropriate model based on mediator and outcome types detected in the dataset.
- buzzEBMcontYcont(): Continuous mediators, continuous outcome.
- buzzEBMcontYcat(): Continuous mediators, binary outcome.
- buzzEBMcatYcont(): Binary mediators, continuous outcome.
- buzzEBMcatYcat(): Binary mediators, binary outcome.

---

## Example Usage
1. Automatic Model Selection
The most efficient way to run a model is using the lavaan-style formula syntax.

```r
library(buzzMed)

# Specify your mediation model using lavaan-style syntax
# M1 and M2 are mediators; X is the predictor; Y is the outcome
model_string <- "
  M1 + M2 ~ X
  Y ~ M1 + M2 + X
"

# Run the automated model
fit <- buzzEBMedAuto(model = model_string, dataset = my_data)

# Summary of posterior samples
summary(fit)
```

---

## Citation
If you use buzzMed in your research, please cite:

> Shi, D., Dexin Shi, & Amanda J. Fairchild (2023). Variable Selection for Mediators under a Bayesian Mediation Model. Structural Equation Modeling: A Multidisciplinary Journal, 30(6), 887-900. DOI: 10.1080/10705511.2022.2164285

---

## License
This project is licensed under the **GNU General Public License v3.0.**
See the `LICENSE` file for details.
