# Applied Economics Course
# Winter 2026/2027
# Lecturer: Pello Aspuru

# R script for the Econometrics Review Session (session 05)
# Code gathered sequentially from the session slides.
# The slides show the feols() syntax with a generic dataset df; the small
# simulated panel below makes the templates runnable.

# Set a new working directory (adjust the path to your own folder)
# setwd("C:/Users/YourName/Desktop/AE_2026")

# Verify the change
getwd()

# Clean working space
rm(list = ls())

# Call libraries
# install.packages("fixest")  # once per computer
library(fixest)

# Example data to run the syntax templates on: a small simulated panel
# of 50 units observed over 10 years
set.seed(1)
df <- data.frame(unit = rep(1:50, each = 10),
                 year = rep(2001:2010, times = 50))
df$x <- rnorm(nrow(df))
df$y <- 1 + 2 * df$x + rnorm(nrow(df))

# ---- Homoskedastic Standard Errors ---- ----

feols(y ~ x, data = df, vcov = "iid")

# ---- Heteroskedasticity-Robust Standard Errors ---- ----

feols(y ~ x, data = df, vcov = "hetero")

# ---- Clustered Standard Errors ---- ----

library(fixest)
feols(y ~ x | unit + year, data = df, cluster = ~unit)
