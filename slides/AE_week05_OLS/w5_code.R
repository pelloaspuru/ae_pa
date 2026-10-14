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

library(sandwich)  # vcovHC()
library(lmtest)    # coeftest()

# Open population of N = 165,000 houses in vienna: 
population <- read.csv("./AE_week3_MLR2/df_pop_house_prices.csv")
population$constant = 1

# Obtain population parameters
mlr_params = lm(price ~  nox + rooms, data = population)
# Summarize regression results: 
summary(mlr_params)

# Get a dataset of n observations in vienna:
n <- 100
df <- population[sample(nrow(population), n), ]

# Estimate MLR on sample
mlr_param_est = lm(price ~  nox + rooms, data = df)
# Summarize regression results: 
summary(mlr_param_est)

# Dataset in matrix notation
X <- as.matrix(df[,names(df) %in% c("constant","nox", "rooms")])
Y <- as.matrix(df[,names(df) %in% c("price")])

# vector of residuals
beta_hat = solve(t(X) %*% X) %*% t(X) %*% Y
u_hat_sq = (as.vector(Y - X %*% beta_hat))^2

# Approximation of omega:
Omega_hat <- as.matrix(diag(u_hat_sq))

# Estimation of the robust variance of beta_hat
var_beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Omega_hat %*% X %*% solve(t(X) %*% X)

# Comparing parameter standard errors
SE_hat <- sqrt(diag(var_beta_hat))

# How do our hand-made standard errors compare to those reported by R?
ols_est = lm(price ~ nox + rooms, data = df)
summary(ols_est)

# Homoskedastic variance
sigma2_hat <- (sum(u_hat_sq))/(n-3)
Omega_hat_hom <- sigma2_hat
var_beta_hat_hom <- solve(t(X) %*% X) * sigma2_hat
SE_hat_homo <-sqrt(diag(var_beta_hat_hom))

# Heteroskedastic variance
robust_se <- sqrt(diag(vcovHC(ols_est, type = "HC0")))
robust_se
coeftest(ols_est, vcov. = vcovHC(ols_est, type = "HC0"))
SE_hat # Calculated previously.

# New model with two highly correlated covariates:

ols_est_2 = lm(price ~  nox + rooms + proptax, data = df)

R_j_test = lm(nox ~  rooms + proptax, data = df)
summary(R_j_test)

coeftest(ols_est, vcov. = vcovHC(ols_est, type = "HC0"))
coeftest(ols_est_2, vcov. = vcovHC(ols_est_2, type = "HC0"))

