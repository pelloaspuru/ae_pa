# Applied Economics Course
# 2025 Winter
# Lecturer: Pello Aspuru

# R script for the Differences in Differences (DD) Session 

# Acknowledgement: https://rpubs.com/phle/r_tutorial_difference_in_differences

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
library(fixest)
library(plm)
library(ggplot2)

# Open dataset:

df <- read.csv("./AE_week10_DD/df_min_wage.csv")

# Generate auxiliary variables
df <- df %>%
  mutate(
    time = ifelse(observation == "November 1992", 1, 0),
    treated = ifelse(state == "New Jersey", 1, 0))

# ---------------------- Start with a Panel Model: ----------------------------- ----

# define treatment
df$is_min_wage_up <- df$time*df$treated

panel_0 <- feols(emptot ~ is_min_wage_up | sheet, data = df)
summary(panel_0, vcov = "hetero")

# --------------------- Visualizing DiD: Minimum graph ------------------------- ----

# df_plot: Aggregate data: calculate mean and SD for each state-time combination
df_plot <- df %>%
  group_by(state, observation) %>%
  summarise(
    mean_emptot = mean(emptot, na.rm = TRUE),
    sd_emptot = sd(emptot, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate PA slope
pa_data <- df_plot %>% filter(state == "Pennsylvania")
pa_slope <- diff(pa_data$mean_emptot)

# Get endpoint values for state labels
nj_feb <- df_plot %>% filter(state == "New Jersey", observation == "February 1992")
pa_feb <- df_plot %>% filter(state == "Pennsylvania", observation == "February 1992")

# Create counterfactual New Jersey line data
NJ_counterfactual <- data.frame(
  observation = c("February 1992", "November 1992"),
  mean_emptot = c(nj_feb$mean_emptot, nj_feb$mean_emptot + pa_slope),
  state = "Counterfactual")

ggplot(df_plot, aes(x = observation, y = mean_emptot, group = state, color = state)) +
  geom_line(linewidth = 0.5) +
  geom_line(data = NJ_counterfactual, aes(x = observation, y = mean_emptot, group = state),
            linetype = "dashed", linewidth = 0.5, color = "gray40") +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(16, 24)) +
  theme_minimal()

# ------------------- Visualizing DiD ; fancy graph ---------------------------- ----

# Define custom colors
state_colors <- c("New Jersey" = "#8DA0CB", "Pennsylvania" = "#FC8D62")

# NJ_counterfactual_2, NJ_counterfactual_3: Generate violations of the paralell trends assumption
NJ_counterfactual_2 <- data.frame(
  observation = c("February 1992", "November 1992"),
  mean_emptot = c(nj_feb$mean_emptot, nj_feb$mean_emptot + pa_slope-1.5),
  state = "Counterfactual")
NJ_counterfactual_3 <- data.frame(
  observation = c("February 1992", "November 1992"),
  mean_emptot = c(nj_feb$mean_emptot, nj_feb$mean_emptot + pa_slope+1.5),
  state = "Counterfactual")

# Create the differences-in-differences plot
dd_plot <-
ggplot(df_plot, aes(x = observation, y = mean_emptot, group = state, color = state)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 2) +
  geom_line(data = NJ_counterfactual, aes(x = observation, y = mean_emptot, group = state),
            linetype = "dashed", linewidth = 0.5, color = "gray40") +
  geom_line(data = NJ_counterfactual_2, aes(x = observation, y = mean_emptot, group = state),
            linetype = "dashed", linewidth = 1.5, color = "#E78AC3") +
  geom_line(data = NJ_counterfactual_3, aes(x = observation, y = mean_emptot, group = state),
            linetype = "dashed", linewidth = 1.5, color = "#E78AC3") +
  annotate("text", x = "November 1992", y = NJ_counterfactual$mean_emptot[2], 
           label = "Counterfactual New Jersey", hjust = -0.05, vjust = 0.5, 
           color = "gray40", size = 3.5) +
  annotate("text", x = "February 1992", y = nj_feb$mean_emptot, 
           label = "New Jersey", hjust = -2.0, vjust = 0.5, 
           color = "#8DA0CB", size = 3.5, fontface = "bold") +
  annotate("text", x = "February 1992", y = pa_feb$mean_emptot, 
           label = "Pennsylvania", hjust = -1.0, vjust = 0.5, 
           color = "#FC8D62", size = 3.5, fontface = "bold") +
  scale_color_manual(values = state_colors) +
  scale_y_continuous(limits = c(16, 24)) +
  labs(
    title = "",
    x = "",
    y = "Total Employment",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 12),
    legend.position = "none"
  )

ggsave("./AE_week10_DD/DD_graph_4.pdf", plot = dd_plot, width = 7, height = 4)

# ---------------------- Run diff in diff manually:----------------------------- ----

did_model <- lm(emptot ~ time + treated + time:treated, data = df)
summary(did_model)

# --------------- Diff in diff, with additional controls:----------------------- ----

panel_1 <- feols(emptot ~ time + treated + is_min_wage_up | sheet, data = df)
summary(panel_1, vcov = "hetero")


