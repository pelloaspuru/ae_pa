# Applied Economics Course
# 2025 Winter
# Lecturer: Pello Aspuru

# R script for the Panel Data Session 

# Set a new working directory.
setwd("C:/Users/Aspuru Pello/Desktop/2025_AE_PA")

# Verify the change
getwd()

# Clean working space
rm(list = ls())

# Call Libraries:

library(sandwich)
library(lmtest)
library(ggplot2)
library(wooldridge)
library(dplyr)
library(fixest)

# Open dataset seatbelts.csv:

df <- read.csv("./AE_week9_panel/seatbelts.csv")
head(df)
table(df$year)

# --------- dplyr package example --------- ----

df_average <- df %>% group_by(state) %>% summarise(avg_usage = mean(seatbelt_usage))

# -------- Pooled OLS ----------- ----

# Single linear regression:
pooled_slr <- lm(fatality_rate ~ seatbelt_usage, data = df) 
coeftest(pooled_slr, vcov. = vcovHC(pooled_slr, "HC0"))

# scatter_1: First look at data:

scatter_1 <-
ggplot(df, aes(x = seatbelt_usage, y = fatality_rate, color = state)) +
  geom_point(size = 3) +                         # scatterplot points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # fitted linear line
  labs(x = "Seatbelt Usage", y = "Fatality Rate") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("./AE_week9_panel/scatterplot_1.pdf", plot = scatter_1, width = 7, height = 5)

# Adding some controls:
pooled_mlr <- lm(fatality_rate ~ seatbelt_usage + income + age, data = df) 
coeftest(pooled_mlr, vcov. = vcovHC(pooled_mlr, "HC0"))



# -------- Panel methods: Dummy variable estimator -------- ----

# Dummy variable estimator
panel_dummy <- lm(fatality_rate ~ 0 + factor(state) + seatbelt_usage, data = df)
coeftest(panel_dummy, vcov. = vcovHC(panel_dummy, "HC0"))

# scatter_2: A more careful look at data, state by state

# Intermediate step: Generate dataset pred_df
pred_df <- df %>%
  group_by(state) %>%
  summarize(
    seatbelt_usage = seq(min(seatbelt_usage), max(seatbelt_usage), length.out = 100),
    .groups = "drop"
  )
pred_df$pred <- predict(panel_dummy, newdata = pred_df)

scatter_2 <-
ggplot(df, aes(x = seatbelt_usage, y = fatality_rate, color = state)) +
  geom_point(size = 3) +
  geom_line(data = pred_df, aes(y = pred), linewidth = 1) +
  #geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Seatbelt Usage", y = "Fatality Rate") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("./AE_week9_panel/scatterplot_2.pdf", plot = scatter_2, width = 7, height = 5)

# -------- Panel methods: Within Transformation estimator -------- ----

# Demean df dataset:

df_demeaned <- df %>%
  group_by(state) %>%
  mutate(
    fatality_rate_dm   = fatality_rate - mean(fatality_rate, na.rm = TRUE),
    seatbelt_usage_dm  = seatbelt_usage - mean(seatbelt_usage, na.rm = TRUE)
  ) %>%
  ungroup()

# "Manual" estimation using df_demeaned
panel_within_manual <- lm(fatality_rate_dm ~ 0 + seatbelt_usage_dm, data = df_demeaned)
coeftest(panel_within_manual, vcov. = vcovHC(panel_within_manual, "HC0"))

# Estimating Within tranformation with feols() function.  
panel_within_fixest <- feols(fatality_rate ~ seatbelt_usage | state, data = df)
summary(panel_within_fixest, vcov = "hetero")


  
