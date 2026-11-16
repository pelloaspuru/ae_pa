# Applied Economics Course
# Winter 2026/2027
# Lecturer: Pello Aspuru

# R script for the Instrumental Variables (IV) Session (session 06)
# Code gathered sequentially from the session slides.

# Set a new working directory (adjust the path to your own folder)
# setwd("C:/Users/YourName/Desktop/AE_2026")

# Verify the change
getwd()

# Clean working space
rm(list = ls())

# Call libraries
# install.packages(c("wooldridge", "sandwich", "lmtest", "car", "estimatr"))  # once per computer
library(sandwich)
library(lmtest)

# ---- Motivation: Labor Supply and Number of Kids ---- ----

library(wooldridge)
data("labsup")
mlr_labsup <- lm(hours ~ kids, data = labsup)
summary(mlr_labsup)

# ---- Two-Stage Least Squares (2SLS) ---- ----

# First stage: regress the endogenous regressor on the instrument
first_stage <- lm(kids ~ samesex, labsup)
summary(first_stage)

# Get fitted values from the first stage
labsup$fitted_kids <- first_stage$fitted.values

# Second stage: regress the outcome on the fitted values
second_stage <- lm(hours ~ fitted_kids, data = labsup)

# ---- IV Estimation with iv_robust() ---- ----

library(estimatr)
slr_iv <- iv_robust(hours ~ kids | samesex,
                    data = labsup, se_type = "HC0", diagnostics = TRUE)

# ---- Variance of the IV Estimator ---- ----

coeftest(second_stage, vcov. = vcovHC(second_stage, "HC0"))
summary(slr_iv)

# ---- Example in R: Causal Effect of Education on Wages ---- ----

library(wooldridge)
library(sandwich)
library(lmtest)
library(car)
data("mroz")  # data on education and wages of married women in the US

# Keep the women with an observed wage
mroz <- mroz[!is.na(mroz$lwage), ]

# OLS benchmark
ols_est <- lm(lwage ~ educ, data = mroz)
coeftest(ols_est, vcov. = vcovHC(ols_est, "HC1"))

# ---- Using Maternal Education as an IV ---- ----

# First stage
first_st <- lm(educ ~ motheduc, data = mroz)
coeftest(first_st, vcov. = vcovHC(first_st, "HC1"))

# Second stage
mroz$fitted_educ <- first_st$fitted.values # get fitted values from first stage
second_st <- lm(lwage ~ fitted_educ, data = mroz)
coeftest(second_st, vcov. = vcovHC(second_st, "HC1"))

# 2SLS in one step with iv_robust()
# install.packages("estimatr")
library(estimatr)
tsls <- iv_robust(lwage ~ educ | motheduc, data = mroz,
                  se_type = "HC1", diagnostics = T)
summary(tsls)

# ---- General Case: MLR with an Endogenous Regressor ---- ----

# First stage and relevance
mlr_first_stage <- lm( kids ~ samesex + age + educ, labsup)
summary(mlr_first_stage)

labsup$fitted_kids <- mlr_first_stage$fitted.values

# Second stage
mlr_second_stage <- lm(hours ~ fitted_kids + age + educ, labsup)
coeftest(mlr_second_stage, vcov. = vcovHC(mlr_second_stage, "HC0"))

# IV estimator in one step
mlr_iv <- iv_robust(hours ~ kids + age + educ | samesex + age + educ,
                    data = labsup, se_type = "HC0", diagnostics = TRUE)
summary(mlr_iv)

# ---- Example in R: Labor Supply ---- ----

data("labsup")   # load dataset on mothers, incl. number of kids and hours worked
table(labsup$kids) # sample includes women with two or more kids
ols_labsup <- lm(hours ~ kids + age + agesq + educ +
    black + hispan, data = labsup) # baseline model

coeftest(ols_labsup, vcov. = vcovHC(ols_labsup, "HC1"))
