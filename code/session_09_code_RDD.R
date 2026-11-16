# Applied Economics Course
# 2025 Winter
# Lecturer: Pello Aspuru

# R script for the Regression Discontinuity Design (RDD) Session 

# Set a new working directory.
setwd("C:/Users/Aspuru Pello/Desktop/2025_AE_PA")

# Verify the change
getwd()

# Clean working space
rm(list = ls())

# Call libraries
library(dplyr)
library(sandwich)
library(lmtest)
#library(fixest)
#library(plm)
library(ggplot2)

# Open dataset:

df <- read.csv("./AE_week11_RDD/dataRDD.csv")

# ---------------------- Start with a scatterplot: ----------------------------- ----

rdd_plot<-
ggplot(df, aes(x = age_cell, y = death_rate_per_100T,
               color = as.factor(treat))) +
  geom_point(size = 2, alpha = 0.7) +
  geom_vline(xintercept = 21, linetype = "dashed") +
  labs(x = "Age",
       y = "Death rate") +
  coord_cartesian(xlim = c(18, 24), ylim = c(87, 103)) +
  theme_minimal()+theme(legend.position = "none")

ggsave("./AE_week11_RDD/RDD_graph_1.pdf", plot = rdd_plot, width = 7, height = 4)

# ---------------------- Run RDD as a linear model manually:-------------------- ----

df$age_centered <- df$age_cell - 21

rdd_lm <- lm(death_rate_per_100T ~ age_centered + treat, data=df)
coeftest(rdd_lm, vcov. = vcovHC(rdd_lm, "HC0"))

rdd_lm_0 <- lm(death_rate_per_100T ~ age_cell + treat, data=df)
coeftest(rdd_lm_0, vcov. = vcovHC(rdd_lm_0, "HC0"))

# --------------------- Plot the RDD regression -------------------------------- ----

df$fit_common_slope <- predict(rdd_lm)

rdd_plot_2 <-
  ggplot(df, aes(x = age_cell, y = death_rate_per_100T)) +
  geom_point(aes(color = as.factor(treat)), size = 2, alpha = 0.7) +
  geom_vline(xintercept = 21, linetype = "dashed") +
  labs(x = "Age", y = "Death rate") +
  geom_line(
    data = subset(df, age_cell < 21),
    aes(y = fit_common_slope, color = as.factor(treat)),
    linewidth = 1
  ) +
  geom_line(
    data = subset(df, age_cell >= 21),
    aes(y = fit_common_slope, color = as.factor(treat)),
    linewidth = 1
  ) +
  coord_cartesian(xlim = c(18, 24), ylim = c(87, 103)) +
  theme_minimal() +
  theme(legend.position = "none")


ggsave("./AE_week11_RDD/RDD_graph_2.pdf", plot = rdd_plot_2, width = 7, height = 4)

# --------------------- RDD with varying slopes -------------------------------- ----

rdd_2slopes <- lm(death_rate_per_100T ~ age_centered*treat, data=df)
coeftest(rdd_2slopes, vcov. = vcovHC(rdd_2slopes, "HC0"))

df$fit_2slopes <- predict(rdd_2slopes)

rdd_plot_3 <-
  ggplot(df, aes(x = age_cell, y = death_rate_per_100T)) +
  geom_point(aes(color = as.factor(treat)), size = 2, alpha = 0.7) +
  geom_vline(xintercept = 21, linetype = "dashed") +
  labs(x = "Age", y = "Death rate") +
  geom_line(
    data = subset(df, age_cell < 21),
    aes(y = fit_2slopes, color = as.factor(treat)),
    linewidth = 1
  ) +
  geom_line(
    data = subset(df, age_cell >= 21),
    aes(y = fit_2slopes, color = as.factor(treat)),
    linewidth = 1
  ) +
  coord_cartesian(xlim = c(18, 24), ylim = c(87, 103)) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("./AE_week11_RDD/RDD_graph_3.pdf", plot = rdd_plot_3, width = 7, height = 4)

# --------------------- RDD with quadratic polynomials ------------------------- ----


df$age_centered_sq <- (df$age_centered)^2 # new variable

rdd_quadratic <- lm(death_rate_per_100T ~ age_centered*treat + age_centered_sq*treat, data=df)
coeftest(rdd_quadratic, vcov. = vcovHC(rdd_quadratic, "HC0"))

df$fit_quadratic <- predict(rdd_quadratic)

rdd_plot_4 <-
  ggplot(df, aes(x = age_cell, y = death_rate_per_100T)) +
  geom_point(aes(color = as.factor(treat)), size = 2, alpha = 0.7) +
  geom_vline(xintercept = 21, linetype = "dashed") +
  labs(x = "Age", y = "Death rate") +
  geom_line(
    data = subset(df, age_cell < 21),
    aes(y = fit_quadratic, color = as.factor(treat)),
    linewidth = 1
  ) +
  geom_line(
    data = subset(df, age_cell >= 21),
    aes(y = fit_quadratic, color = as.factor(treat)),
    linewidth = 1
  ) +
  coord_cartesian(xlim = c(18, 24), ylim = c(87, 103)) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("./AE_week11_RDD/RDD_graph_4.pdf", plot = rdd_plot_4, width = 7, height = 4)

# --------- RDD with quadratic polynomials - narrow bandwidth ------------------ ----

df_narrow <- df %>% filter((age_cell>19) & (age_cell<23))

rdd_quadratic_narrow <- lm(death_rate_per_100T ~ age_centered*treat + age_centered_sq*treat, 
                           data=df_narrow)
coeftest(rdd_quadratic_narrow, vcov. = vcovHC(rdd_quadratic_narrow, "HC0"))

df_narrow$fit_quad_narrow <- predict(rdd_quadratic_narrow)

rdd_plot_5 <-
  ggplot(df_narrow, aes(x = age_cell, y = death_rate_per_100T)) +
  geom_point(aes(color = as.factor(treat)), size = 2, alpha = 0.7) +
  geom_vline(xintercept = 21, linetype = "dashed") +
  labs(x = "Age", y = "Death rate") +
  geom_line(
    data = subset(df_narrow, age_cell < 21),
    aes(y = fit_quad_narrow, color = as.factor(treat)),
    linewidth = 1
  ) +
  geom_line(
    data = subset(df_narrow, age_cell >= 21),
    aes(y = fit_quad_narrow, color = as.factor(treat)),
    linewidth = 1
  ) +
  coord_cartesian(xlim = c(18, 24), ylim = c(87, 103)) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("./AE_week11_RDD/RDD_graph_5.pdf", plot = rdd_plot_5, width = 7, height = 4)