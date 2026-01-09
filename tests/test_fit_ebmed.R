# Run this line to pull the package if you haven't already
#devtools::install_github("olfactorybulb/ebmeddev")

library(ebmeddev)
#### 1. Load dataset ----
College <- read.csv("https://raw.githubusercontent.com/selva86/datasets/master/College.csv")
# select only public schools
College1 <- College[College$Private == "No", ]
# variables of interest
vars <- c(3,4,9:12,15,17)

#### Example 1: Raw (unscaled) data ----
cdata_raw <- College1[ , vars]
X <- "Accept"
y <- "Enroll"
mediators <- c("Outstate","Room.Board","Books")
output_1 <- fit_ebmed(
  data      = cdata_raw,
  X         = X,
  y         = y,
  mediators = mediators
)
print(extract_ebmed_results(output_1, X, mediators))

#### Example 2: Pre-scaled data ----
cdata_scaled <- data.frame(scale(College1[ , vars]))
output_2 <- fit_ebmed(
  data      = cdata_scaled,
  X         = X,
  y         = y,
  mediators = mediators,
  scale     = FALSE  # user already scaled
)
print(extract_ebmed_results(output_2, X, mediators))

#### Example 3: Multiple X ----
X_multi <- c("Accept", "Personal")
output_3 <- fit_ebmed(
  data      = cdata_scaled,
  X         = X_multi,
  y         = y,
  mediators = mediators,
  scale     = FALSE
)
print(extract_ebmed_results(output_3, X_multi, mediators))

#### Example 4a: Single mediator ----
mediators_1 <- "Outstate"
output_4a <- fit_ebmed(
  data      = cdata_scaled,
  X         = X,
  y         = y,
  mediators = mediators_1,
  scale     = FALSE
)

print(extract_ebmed_results(output_4a, X, mediators_1))

#### Example 4b: Larger mediator set ----
mediators_6 <- c("Outstate","Room.Board","Books","Personal","S.F.Ratio","Expend")

output_4b <- fit_ebmed(
  data      = cdata_scaled,
  X         = X,
  y         = y,
  mediators = mediators_6,
  scale     = FALSE
)
print(extract_ebmed_results(output_4b, X, mediators_6))

#### Example 5: Tuning model parameters ----
output_5 <- fit_ebmed(
  data      = cdata_scaled,
  X         = X,
  y         = y,
  mediators = mediators,
  scale     = FALSE,

  # MCMC tuning
  n_iter    = 3000,
  n_burnin  = 1000,
  thin      = 2,

  # Prior precision tuning (example names)
  taua     = 0.5,
  taub     = 0.5
)

print(extract_ebmed_results(output_5, X, mediators))
