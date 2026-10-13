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

# Open population of N = 165,000 houses in vienna: 
population <- read.csv("./AE_week3_MLR2/df_pop_house_prices.csv")

# Obtain population parameters
mlr_params = lm(price ~  nox + rooms, data = population)
# Summarize regression results: 
summary(mlr_params)

# Save beta_1 population parameter
beta_1 <- mlr_params$coefficients["nox"]

# Get a dataset of n observations in vienna:
n <- 100
df <- population[sample(nrow(population), n), ]

# Obtain OLS estimates, for our dataset
ols_est = lm(price ~ nox + rooms, data = df)
summary(ols_est)

# Multicolinearity
multicolinearity = lm(nox ~ rooms, data = df)
summary(multicolinearity)

# Perfect multicolinearity
df$bedrooms = df$rooms - 1
ols_est_perf_mc = lm(price ~ nox + rooms + bedrooms, data = df)
summary(ols_est_perf_mc)

# OLS estimator unbiasedness
n <- 100
reps <- 1000
beta_hat_vec <- numeric(reps)

for (i in 1:reps) {
  df <- population[sample(nrow(population), n), ]
  fit <- lm(price ~ nox + rooms, data = df)
  beta_hat_vec[i] <- coef(fit)["nox"]}

# Plot all beta_1 saved estimates, also plot a vertical line with beta_1
sims <- data.frame(beta_hat = beta_hat_vec)
ggplot(sims, aes(x = beta_hat)) +
  geom_histogram(bins = 40, alpha = 0.8) +
  geom_vline(xintercept = beta_1, linetype = "dashed", linewidth = 1)

# OLS estimator consistency

n <- 10000
reps <- 1000
gaps_beta_hat_beta <- numeric(reps)

for (i in 1:reps) {
  df <- population[sample(nrow(population), n), ]
  fit <- lm(price ~ nox + rooms, data = df)
  gap <- abs(coef(fit)["nox"]-beta_1)
  gaps_beta_hat_beta[i] <- gap}

# Plot all beta_1 saved estimates, also plot a vertical line with beta_1
sims_gaps <- data.frame(gaps = gaps_beta_hat_beta)
ggplot(sims_gaps, aes(x = gaps)) +
  geom_histogram(bins = 40, alpha = 0.8) +
  coord_cartesian(xlim = c(0, 2000))

# Polynomials of a regressor
df$nox_sq <- (df$nox)^2
ols_est = lm(price ~ nox + nox_sq + rooms, data = df)
summary(ols_est)

# Logarithmic transformations
df$log_price <- log(df$price)
df$log_nox <- log(df$nox)

# Level log model
ols_lev_log = lm(price ~ log_nox + rooms, data = df)

# log level model
ols_log_lev = lm(log_price ~ nox + rooms, data = df)

# log log model
ols_log_log = lm(log_price ~ log_nox + rooms, data = df)

# Generate indicator variable
df$is_big <- 1*(df$rooms>5)

ols_est_indic = lm(price ~ nox + rooms + is_big, data = df)
summary(ols_est_indic)

# Interaction term
ols_est_inter = lm(price ~ nox + rooms*is_big, data = df)
summary(ols_est_inter)

