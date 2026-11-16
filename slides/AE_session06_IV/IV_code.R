# Set a new working directory.
setwd("C:/Users/Aspuru Pello/Desktop/2025_AE_PA")

# Verify the change
#getwd()

# List files in the working directory
#dir()

# Clean working space
rm(list = ls())

## Call Libraries:

library(sandwich)
library(lmtest)
library(ggplot2)
library(wooldridge)

# Open dataset labsup from wooldrige package:
library(wooldridge)
data("labsup")

# SLR model
slr_ols <- lm(hours ~ kids, data = labsup)
summary(slr_ols)

# First stage.
first_stage <- lm(kids ~ samesex, labsup)
summary(first_stage)

# Second stage
labsup$fitted_kids <- first_stage$fitted.values # get fitted values from first stage
second_stage <- lm(hours ~ fitted_kids, data = labsup)
coeftest(second_stage, vcov. = vcovHC(second_stage, "HC0"))  

# Using iv_robust function
#install.packages("estimatr")
library(estimatr)
slr_iv <- iv_robust(hours ~ kids | samesex, 
                    data = labsup, se_type = "HC0", diagnostics = TRUE)
summary(slr_iv)

# ---------- MLR Case ------------

# MLR model
mlr_labsup <- lm(hours ~ kids + age + educ, data = labsup)
summary(mlr_labsup)

# First stage.
mlr_first_stage <- lm( kids ~ samesex + age + educ, labsup)
summary(mlr_first_stage)

# Second stage
labsup$fitted_kids <- mlr_first_stage$fitted.values # get fitted values from first stage
mlr_second_stage <- lm(hours ~ fitted_kids + age + educ, data = labsup)
coeftest(mlr_second_stage, vcov. = vcovHC(mlr_second_stage, "HC0"))  

# Using iv_robust function
library(estimatr)
mlr_iv <- iv_robust(hours ~ kids + age + educ | samesex + age + educ, 
                  data = labsup, se_type = "HC0", diagnostics = TRUE)
summary(mlr_iv)
