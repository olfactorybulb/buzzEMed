# ebmeddev v0.1.0

**Bayesian Mediation Analysis for Multiple Predictors and Mediators in R**

**Authors:** Jess Hsing, Dingjing Shi
**Contact:** jesscc.hsing@gmail.com

---

## Overview

`ebmeddev` is an R package for fitting **Empirical Bayes mediation (EBMed)** models using Bayesian estimation via **JAGS**. The package is designed to support mediation analysis with:

- Multiple predictors
- Multiple mediators
- Continuous or binary mediators
- Continuous or binary outcomes

Rather than relying on a single generic model, `ebmeddev` provides **separate model-fitting functions** for each mediator–outcome type combination. This makes model assumptions explicit and avoids ambiguity in likelihood specification.

---

## Model framework

The package implements Bayesian mediation models following the EBMed framework described in Shi et al. (2023), with shrinkage-based variable selection for mediators.

Currently supported variable types:

- **Continuous variables**
- **Categorical variables**, restricted to **binary (0/1)** inputs

Multi-category outcomes or mediators are not supported at this time.

---

## Main functions

There are five primary model-fitting functions:

- `fit_ebmed_auto()`
  Automatically dispatches to the appropriate model based on mediator and outcome types.

- `fit_ebmed_mcont_ycont()`
  Continuous mediators, continuous outcome.

- `fit_ebmed_mcat_ycont()`
  Binary mediators, continuous outcome.

- `fit_ebmed_mcont_ycat()`
  Continuous mediators, binary outcome.

- `fit_ebmed_mcat_ycat()`
  Binary mediators, binary outcome.

Each function fits a Bayesian mediation model using JAGS and returns posterior summaries for direct, indirect, and total effects.

---

## Installation

You can install the development version from GitHub:

```r
install.packages("devtools")
devtools::install_github("olfactorybulb/ebmeddev")

library(ebmeddev)
```

---

# Example Usage
## Example: Binary Mediators and Binary Outcomes

```r
# Install and load package
remotes::install_github("olfactorybulb/ebmeddev")
library(ebmeddev)

# Load dataset
College <- read.csv(
  "https://raw.githubusercontent.com/selva86/datasets/master/College.csv"
)

# Create binary outcome
College$Enroll_high <- ifelse(
  College$Enroll > median(College$Enroll, na.rm = TRUE),
  1, 0
)

# Create binary mediators
College$Outstate_high <- ifelse(
  College$Outstate > median(College$Outstate, na.rm = TRUE),
  1, 0
)

College$Room.Board_high <- ifelse(
  College$Room.Board > median(College$Room.Board, na.rm = TRUE),
  1, 0
)

College$Expend_high <- ifelse(
  College$Expend > median(College$Expend, na.rm = TRUE),
  1, 0
)

# Define variables
X <- "Accept"
Y <- "Enroll_high"
M <- c("Outstate_high", "Room.Board_high", "Expend_high")

dataset <- College[, c(X, Y, M)]

# Fit EBMed model: binary mediators, binary outcome
fit <- fit_ebmed_mcat_ycat(
  data = dataset,
  X = X,
  M = M,
  Y = Y
)

summary(fit)

```

## Example: Automatic model selection
The same model can be fit using automatic dispatch based on variable types:

```
fit_auto <- fit_ebmed_auto(
  data = dataset,
  X = X,
  M = M,
  Y = Y
)

summary(fit_auto)
```

------

## Package Status
ebmeddev is under active development and intended for research and methodological use. The API may change, and users are encouraged to inspect model code and assumptions before applying the package to substantive analyses.

## Citation
If you use `ebmeddev` in your research, please cite:
Dingjing Shi, Dexin Shi & Amanda J. Fairchild (2023) Variable Selection for Mediators under a Bayesian Mediation Model, Structural Equation Modeling: A Multidisciplinary Journal, 30:6, 887-900, DOI: 10.1080/10705511.2022.2164285

## License
This project is licensed under the GNU General Public License v3.0.
See the LICENSE file for details.
