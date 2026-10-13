# Check current working directory
#getwd()

# Set a new working directory.
setwd("C:/Users/Aspuru Pello/Desktop/2025_AE_PA")

# Verify the change
#getwd()

# List files in the working directory
#dir()

# Clean working space
rm(list = ls())

# Install and call libraries:

library(readr)
library(stargazer)
library(jtools)
library(ggplot2)

# Auto-generated data generating process for hprices.csv
# Reproduces joint structure (via MVN on transformed covariates) and outcome model for log(price)
# Dependencies: MASS

simulate_hprices <- function(N, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (!requireNamespace("MASS", quietly = TRUE)) stop("Please install MASS: install.packages('MASS')")

  # Parameters estimated from the provided dataset (N = 506)
  mu <- c(-0.779893503464944, 5.54978258949023, 6.28405138108099, 1.18823315651569, 3.62881953809339, 18.459288772387, 2.37287247631559)
  Sigma <- matrix(c(4.67649697538792, 1.97504167118216, -0.466475338913936, -0.86843963257299, 0.694157047672133, 1.81058116665565, 0.764997872503085, 1.97504167118216, 1.34187934865165, -0.246466041101719, -0.520490554313452, 0.302673995534365, 0.468817071562232, 0.402785219510739, -0.466475338913936, -0.246466041101719, 0.493638005432295, 0.0972601833234494, -0.0833418891303204, -0.53869027247638, -0.28128606730186, -0.86843963257299, -0.520490554313452, 0.0972601833234494, 0.291061346854264, -0.128218240650861, -0.274363000623539, -0.170790205970063, 0.694157047672133, 0.302673995534365, -0.0833418891303204, -0.128218240650861, 0.157106422553356, 0.362468156369281, 0.120984397393931, 1.81058116665565, 0.468817071562232, -0.53869027247638, -0.274363000623539, 0.362468156369281, 4.69077605101896, 0.5339909107492, 0.764997872503085, 0.402785219510739, -0.28128606730186, -0.170790205970063, 0.120984397393931, 0.5339909107492, 0.364232748981612), nrow=7, ncol=7, byrow=TRUE)
  col_names <- c("log_crime", "nox", "rooms", "log_dist", "log_proptax", "stratio", "log_lowstat")

  radial_levels <- c(1, 2, 3, 4, 5, 6, 7, 8, 24)
  radial_probs <- c(0.0395256916996047, 0.0474308300395257, 0.075098814229249, 0.217391304347826, 0.227272727272727, 0.0513833992094862, 0.033596837944664, 0.0474308300395257, 0.260869565217391)

  # Draw transformed covariates Z ~ MVN(mu, Sigma)
  Z <- MASS::mvrnorm(n=N, mu=mu, Sigma=Sigma)
  Z <- as.data.frame(Z)
  names(Z) <- col_names

  # Back-transform to original scales
  crime   <- exp(Z$log_crime)
  nox     <- Z$nox
  rooms   <- Z$rooms
  dist    <- exp(Z$log_dist)
  proptax <- exp(Z$log_proptax)
  stratio <- Z$stratio
  lowstat <- exp(Z$log_lowstat)

  # Sample radial as a categorical with empirical frequencies
  radial  <- sample(radial_levels, size=N, replace=TRUE, prob=radial_probs)

  # Build design matrix to mimic the fitted model
  design <- data.frame(
    log_crime = log(crime),
    nox = nox,
    rooms = rooms,
    log_dist = log(dist),
    log_proptax = log(proptax),
    stratio = stratio,
    log_lowstat = log(lowstat),
    radial = factor(radial, levels = radial_levels)
  )

  X <- model.matrix(~ log_crime + nox + rooms + log_dist + log_proptax + stratio + log_lowstat + factor(radial), data=design)

  # Coefficients for log(price) and residual SD
  beta <- setNames(c(11.9605788490683, -0.033837720765607, -0.0455554206146332, 0.0735104626548852, -0.190375013975601, -0.194007348041023, -0.0315477422678444, -0.395827499154783, 0.109044447269735, 0.173782298833583, 0.166231210714541, 0.170964471631535, 0.180480565483913, 0.23323444699239, 0.252854177242763, 0.285482922122663), c("(Intercept)", "log_crime", "nox", "rooms", "log_dist", "log_proptax", "stratio", "log_lowstat", "factor(radial)2", "factor(radial)3", "factor(radial)4", "factor(radial)5", "factor(radial)6", "factor(radial)7", "factor(radial)8", "factor(radial)24"))
  sigma_eps <- 0.207184296274063

  # Generate outcome
  log_price_hat <- as.numeric(X %*% beta[colnames(X)])
  log_price <- log_price_hat + rnorm(N, mean=0, sd=sigma_eps)
  price <- as.integer(round(exp(log_price)))

  out <- data.frame(
    price = price,
    crime = as.numeric(crime),
    nox = as.numeric(nox),
    rooms = as.numeric(rooms),
    dist = as.numeric(dist),
    radial = as.integer(radial),
    proptax = as.numeric(proptax),
    stratio = as.numeric(stratio),
    lowstat = as.numeric(lowstat)
  )
  return(out)
}

# Example:
df_pop_housing <- simulate_hprices(165000, seed=123)

df_pop_housing$rooms <- trunc(df_pop_housing$rooms)

# Save as df_pop_house_prices.csv
write.csv(df_pop_housing, "./AE_week3_MLR2/df_pop_house_prices.csv")
