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

# Open dataset 

df <- read.csv("./AE_week2_MLR/testscoresMI.csv")

# First glance at the data:

summary(df)

# Create expenditures per student variable:
df$expend_ps = df$expend/df$enroll
df$expend_ps_thous = (df$expend/df$enroll)/1000
df$constant = 1

# Linear regression for math performance, store results as slr_math

# Dataset in matrix notation
X <- as.matrix(df[,names(df) %in% c("constant","expend_ps_thous")])
Y <- as.matrix(df[,names(df) %in% c("math4")])

# Single linear regression using OLS formula:
beta_hat_m1 = solve(t(X) %*% X) %*% t(X) %*% Y

# Single linear regression using R function
slr_math = lm(math4 ~ expend_ps_thous, data = df)
# Summarize regression results: 
summary(slr_math)

# Linear regression with additional control:

# Dataset in matrix notation
X <- as.matrix(df[,names(df) %in% c("constant", "expend_ps_thous", "lunch")])
Y <- as.matrix(df[,names(df) %in% c("math4")])
beta_hat_m2 = solve(t(X) %*% X) %*% t(X) %*% Y
beta_hat_m2

mlr_math = lm( math4 ~ expend_ps_thous + lunch , data = df )
summary(mlr_math)

# Comparing the results in table format
stargazer(slr_math, mlr_math, type = "text")

# Comparign the results: Plot of coefficients
coefs_plot <- plot_coefs(slr_math, mlr_math, 
           coefs = c("Expenditures per student (in $1000)" = 
                       "expend_ps_thous"))

ggsave("./AE_week2_MLR/coefs_plot.pdf", plot = coefs_plot, width = 8, height = 6)