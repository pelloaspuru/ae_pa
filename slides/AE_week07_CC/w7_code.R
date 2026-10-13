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
#library(wooldridge)

# Open dataset as df_wages:
df_wages <- read.csv("./AE_week7_CC/wage2.csv")

# Initial model

slr <- lm(log(wage) ~ educ, data = df_wages)
summary(slr)

mlr <- lm(log(wage) ~ educ +  IQ, data = df_wages)
summary(mlr)

test_1 <- lm(log(wage) ~ IQ, data = df_wages)
summary(test)

test_2 <- lm(educ ~ IQ, data = df_wages)
summary(test)


