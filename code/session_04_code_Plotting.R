# Applied Economics Course
# Winter 2026/2027
# Lecturer: Pello Aspuru

# R script for the Plotting Data Session (session 04)
# Code gathered sequentially from the session slides.
# Requires data/df_flights_enriched.csv, produced in session 03.

# Set a new working directory (adjust the path to your own folder)
# setwd("C:/Users/YourName/Desktop/AE_2026")

# Verify the change
getwd()

# Clean working space
rm(list = ls())

# Call libraries
# install.packages(c("ggplot2", "dplyr", "sf", "rnaturalearth"))  # once per computer
library(ggplot2)
library(dplyr)

# ---- Types of Variables in a Dataset ---- ----

df_flights <- read.csv("data/df_flights_enriched.csv")

# ---- Manually Coding Graphs ---- ----

# Histogram of arrival delays
ggplot(df_flights, aes(x = arr_delay)) +
  geom_histogram(bins = 30) +
  labs(x = "Arrival delay (minutes)",
       y = "Frequency")

# ---- Histogram ---- ----

ggplot(df_flights, aes(x = arr_delay)) +
  geom_histogram(binwidth = 10,
                 fill = "steelblue",
                 color = "white",
                 na.rm = TRUE) +
  labs(title = "Distribution of Arrival Delays",
       x = "Arrival delay (minutes)",
       y = "Number of flights") +
  theme_minimal()

# Refining the histogram: limit the horizontal axis
n_above <- sum(df_flights$arr_delay > 300,
               na.rm = TRUE)

ggplot(df_flights, aes(x = arr_delay)) +
  geom_histogram(binwidth = 10,
                 fill = "steelblue",
                 color = "white",
                 na.rm = TRUE) +
  coord_cartesian(xlim = c(NA, 300)) +
  labs(title = "Distribution of Arrival Delays",
       x = "Arrival delay (minutes)",
       y = "Number of flights",
       caption = paste0(
         "Note: horizontal axis limited to 300 ",
         "minutes. ", n_above, " flights with ",
         "longer delays are not shown.")) +
  theme_minimal()

# ---- Bar Chart ---- ----

ggplot(df_flights,
       aes(x = factor(month,
                      levels = 1:12,
                      labels = month.abb))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Number of Flights per Month",
       x = "Month",
       y = "Number of flights") +
  theme_minimal()

# Bar chart: stacked colours
ggplot(df_flights,
       aes(x = factor(month,
                      levels = 1:12,
                      labels = month.abb),
           fill = origin)) +
  geom_bar() +  # stacked by default
  labs(title = "Flights per Month by Origin",
       x = "Month",
       y = "Number of flights",
       fill = "Origin") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Bar chart: side-by-side colours
ggplot(df_flights,
       aes(x = factor(month,
                      levels = 1:12,
                      labels = month.abb),
           fill = origin)) +
  geom_bar(position = "dodge") +  # side by side
  labs(title = "Flights per Month by Origin",
       x = "Month",
       y = "Number of flights",
       fill = "Origin") +
  theme_minimal() +
  theme(legend.position = "bottom")

# ---- Box Plot ---- ----

ggplot(df_flights,
       aes(x = origin, y = arr_delay)) +
  geom_boxplot(na.rm = TRUE) +
  labs(title = "Arrival Delays by Origin",
       x = "Airport of origin",
       y = "Arrival delay (minutes)") +
  theme_minimal()

# Box plot without outliers
ggplot(df_flights,
       aes(x = origin, y = arr_delay)) +
  geom_boxplot(outliers = FALSE,
               na.rm = TRUE) +
  labs(title = "Arrival Delays by Origin",
       x = "Airport of origin",
       y = "Arrival delay (minutes)") +
  theme_minimal()

# Box plot: adding categories with colours
ggplot(df_flights,
       aes(x = origin, y = arr_delay,
           fill = factor(month,
                         levels = 1:12,
                         labels = month.abb))) +
  geom_boxplot(outliers = FALSE,
               na.rm = TRUE) +
  labs(title = "Arrival Delays by Origin and Month",
       x = "Airport of origin",
       y = "Arrival delay (minutes)",
       fill = "Month") +
  theme_minimal() +
  theme(legend.position = "bottom")

# ---- Scatter Plot ---- ----

ggplot(df_flights,
       aes(x = distance,
           y = arr_delay)) +
  geom_point(alpha = 0.2,  # many points
             color = "steelblue",
             na.rm = TRUE) +
  labs(title = "Arrival Delay vs. Distance",
       x = "Distance (miles)",
       y = "Arrival delay (minutes)") +
  theme_minimal()

# ---- Binscatter Plot ---- ----

# 20 bins of distance, equal number of flights
df_bins <- df_flights |>
  filter(!is.na(arr_delay)) |>
  mutate(bin = ntile(distance, 20)) |>
  group_by(bin) |>
  summarise(distance  = mean(distance),
            arr_delay = mean(arr_delay))

ggplot(df_bins, aes(x = distance,
                    y = arr_delay)) +
  geom_point(size = 2) +
  labs(x = "Distance (miles)",
       y = "Mean arrival delay (minutes)",
       caption = paste(
         "Note: 20 distance bins with equal",
         "numbers of flights. Points are bin",
         "means. Cancelled flights excluded.",
         sep = "\n")) +
  theme_minimal()

# Binscatter improvements: adding categories with colours
# Same 20 bins, means by bin and airport
df_bins <- df_flights |>
  filter(!is.na(arr_delay)) |>
  mutate(bin = ntile(distance, 20)) |>
  group_by(bin, origin) |>
  summarise(distance  = mean(distance),
            arr_delay = mean(arr_delay))

ggplot(df_bins, aes(x = distance,
                    y = arr_delay,
                    colour = origin)) +
  geom_point(size = 2) +
  labs(x = "Distance (miles)",
       y = "Mean arrival delay (minutes)",
       colour = "Origin",
       caption = paste(
         "Note: same 20 distance bins for all airports.",
         "Points are bin means for each airport.",
         "Cancelled flights (no delay) excluded.",
         sep = "\n")) +
  theme_minimal()

# ---- Line Graph ---- ----

# Count the flights of each day
daily <- df_flights |>
  mutate(date = as.Date(
    paste(year, month, day, sep = "-"))) |>
  count(date)

ggplot(daily, aes(x = date, y = n)) +
  geom_line(color = "steelblue") +
  labs(title = "Number of Flights per Day",
       x = "Date",
       y = "Number of flights") +
  theme_minimal()

# Line graph: aggregating to months
# Count the flights of each month
monthly <- df_flights |>
  mutate(ym = as.Date(
    paste(year, month, 1, sep = "-"))) |>
  count(ym)

ggplot(monthly, aes(x = ym, y = n)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  labs(title = "Number of Flights per Month",
       x = "Month",
       y = "Number of flights") +
  theme_minimal()

# Line graph: adjusting the vertical axis
# Same monthly data as before
ggplot(monthly, aes(x = ym, y = n)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") +
  expand_limits(y = 0) +  # axis from zero
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  labs(title = "Number of Flights per Month",
       x = "Month",
       y = "Number of flights") +
  theme_minimal()

# Line graph: adding categories with colours
# Count the flights of each month and airport
monthly_origin <- df_flights |>
  mutate(ym = as.Date(
    paste(year, month, 1, sep = "-"))) |>
  count(ym, origin)

ggplot(monthly_origin, aes(x = ym, y = n,
                           colour = origin)) +
  geom_line() +
  geom_point() +
  expand_limits(y = 0) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  labs(title = "Flights per Month by Origin",
       x = "Month", y = "Number of flights",
       colour = "Origin") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Line graph: comparisons in percentages
weekly <- df_flights |>
  # week = Monday that starts each week
  mutate(date = as.Date(paste(year, month, day,
                              sep = "-")),
         week = as.Date(cut(date, "week"))) |>
  group_by(origin, week) |>
  summarise(n = n(), days = n_distinct(date),
            .groups = "drop") |>
  # drop incomplete weeks (start/end of year)
  filter(days == 7) |>
  group_by(origin) |>
  mutate(index = 100 * n / first(n),
         pct_change = index - 100) |>
  ungroup()

ggplot(weekly, aes(x = week, y = pct_change,
                   colour = origin)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line() + geom_point(size = 1) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  labs(x = "Week", y = "Change in flights (%)",
       colour = "Origin",
       caption = paste("Note: first full week of 2013 = 100.",
                       "Incomplete weeks excluded.")) +
  theme_minimal()

# ---- Maps ---- ----

library(sf)
library(rnaturalearth)
world <- ne_countries(returnclass = "sf")

# Map of the world
ggplot(world) +
  geom_sf()

# Countries coloured by a variable
ggplot(world, aes(fill = pop_est)) +
  geom_sf() +
  theme(legend.position = "bottom")
