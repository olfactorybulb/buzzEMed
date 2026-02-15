# ebmeddev v0.1.0

**Bayesian Understanding of Mediation Selection in Exploratory Mediation Analysis**

**Authors:** Jess Hsing, Dingjing Shi

**Contact:** jesscc.hsing@gmail.com

---

## Overview

The `buzzMed` package offers a tool for selecting mediating effects within **exploratory Bayesian mediation models**. It accommodates continuous and binary mediators and outcomes, allowing identification and analysis of complex mediation pathways.

- Multiple predictors
- Multiple mediators
- Continuous or binary mediators
- Continuous or binary outcomes

## Requirements
This package requires JAGS (Just Another Gibbs Sampler) to be installed on your system.
Download from: https://mcmc-jags.sourceforge.io/

---

## Model framework

The package implements Bayesian mediation models following the framework described in Shi et al. (2023), with shrinkage-based variable selection for mediators.

Currently supported variable types:

- **Continuous variables**
- **Categorical variables**, restricted to **binary (0/1)** inputs

Multi-category outcomes or mediators are not supported at this time.

---

## Main functions

There are five primary model-fitting functions:

- `buzzEBMedAuto()`
  Automatically dispatches to the appropriate model based on mediator and outcome types.

- `buzzMYcont()`
  Continuous mediators, continuous outcome.

- `buzzMcat()`
  Binary mediators, continuous outcome.

- `buzzYcat()`
  Continuous mediators, binary outcome.

- `buzzMYcat()`
  Binary mediators, binary outcome.

Each function fits a Bayesian mediation model using JAGS and returns posterior summaries for direct, indirect, and total effects.

---

## Installation

You can install the development version from GitHub:

```r
install.packages("remotes")
remotes::install_github("olfactorybulb/buzzMed")

library(buzzMed)
```

---

# Example Usage
## Example: Binary Mediators and Binary Outcomes

```r
# Install and load package
remotes::install_github("olfactorybulb/buzzMed")
library(buzzMed)

# Load example dataset
College <- read.csv(
  "https://raw.githubusercontent.com/selva86/datasets/master/College.csv"
  )
# Create binary outcome
College$Enroll_high <- ifelse(
  College$Enroll > median(College$Enroll, na.rm = TRUE),
  1, 0)
# Create binary mediators
College$Outstate_high <- ifelse(
  College$Outstate > median(College$Outstate, na.rm = TRUE),
  1, 0)
College$Room.Board_high <- ifelse(
  College$Room.Board > median(College$Room.Board, na.rm = TRUE),
  1, 0)
College$Expend_high <- ifelse(
  College$Expend > median(College$Expend, na.rm = TRUE),
  1, 0)

# Define variables
X <- "Accept"
Y_cont <- "Enroll"
Y_cat <- "Enroll_high"
M_cont <- c("Outstate", "Room.Board", "Expend")
M_cat <- c("Outstate_high", "Room.Board_high", "Expend_high")
dataset <- College[, c(X, Y_cont, Y_cat, M_cont, M_cat)]

# Running the buzzMed model
output_mcont_ycont <- buzzMYcont(dataset, X, M_cont, Y_cont)
output_mcont_ycat <- buzzYcat(dataset, X, M_cont, Y_cat)
output_mcat_ycont <- buzzMcat(dataset, X, M_cat, Y_cont)
output_mcat_ycat <- buzzMYcat(dataset, X, M_cat, Y_cat)

```

## Example: Automatic model selection
The same model can be fit using automatic dispatch based on variable types:

```
output_mcont_ycont_auto <- buzzEBMedAuto(dataset, X, M_cont, Y_cont)
output_mcont_ycat_auto <- buzzEBMedAuto(dataset, X, M_cont, Y_cat)
output_mcat_ycont_auto <- buzzEBMedAuto(dataset, X, M_cat, Y_cont)
output_mcat_ycat_auto <- buzzEBMedAuto(dataset, X, M_cat, Y_cat)
```

# Example: Set your own parameters
You can also set up your own parameters:

```
# Conduct buzzMed analysis with your own parameters
output <- buzzMYcont(
  dataset,
  X,
  M_cont,
  Y_cont,
  shape_m = 2,
  rate_m  = 0.002,
  shape_y = 2,
  rate_y  = 0.01,
  shape_a = 2,
  rate_a  = 0.05,
  shape_b = 2,
  rate_b  = 0.05,
  alpha_ind = 2,
  beta_ind  = 5,
  tau_cprime = 0.001,
  prec.m   = 0.001,
  prec.y   = 1.2,
  c.prime  = 0.1,
  taua     = 1.5,
  taub     = 1.5,
  ind.p    = 0.3,
  n_burnin = 2000,
  n_iter   = 15000,
  thin     = 5
)
```

------

## Package Status
buzzMed is under active development and intended for research and methodological use. The API may change, and users are encouraged to inspect model code and assumptions before applying the package to substantive analyses.

## Citation
If you use `buzzMed` in your research, please cite:
Dingjing Shi, Dexin Shi & Amanda J. Fairchild (2023) Variable Selection for Mediators under a Bayesian Mediation Model, Structural Equation Modeling: A Multidisciplinary Journal, 30:6, 887-900, DOI: 10.1080/10705511.2022.2164285

## License
This project is licensed under the GNU General Public License v3.0.
See the LICENSE file for details.
