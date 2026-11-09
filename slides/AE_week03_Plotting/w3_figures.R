# Figures for the "Plotting Data with ggplot2" slides (AE_w02_plotting.tex).
# Run from the project root:  Rscript slides/AE_week03_Plotting/w3_figures.R
library(ggplot2)
library(dplyr)
Sys.setlocale("LC_TIME", "C")  # English month names on date axes

df_flights <- read.csv("data/df_flights_enriched.csv")
out_dir <- "slides/AE_week03_Plotting"

# Histogram of arrival delays
p_hist <- ggplot(df_flights, aes(x = arr_delay)) +
  geom_histogram(bins = 30) +
  labs(x = "Arrival delay (minutes)", y = "Frequency")
ggsave(file.path(out_dir, "w3_hist_arr_delay.pdf"), p_hist, width = 5, height = 3.5)

# "Vibecoding" first attempt: the kind of script an AI assistant returns for the histogram prompt
p_vibe1 <- ggplot(df_flights, aes(x = arr_delay)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white", na.rm = TRUE) +
  labs(title = "Distribution of Arrival Delays",
       x = "Arrival delay (minutes)", y = "Number of flights") +
  theme_minimal()
ggsave(file.path(out_dir, "w3_vibe_hist_1.pdf"), p_vibe1, width = 5, height = 3.5)

# "Vibecoding" second attempt: axis limited to 300 minutes, with a footnote
n_above <- sum(df_flights$arr_delay > 300, na.rm = TRUE)
p_vibe2 <- ggplot(df_flights, aes(x = arr_delay)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white", na.rm = TRUE) +
  coord_cartesian(xlim = c(NA, 300)) +
  labs(title = "Distribution of Arrival Delays",
       x = "Arrival delay (minutes)", y = "Number of flights",
       caption = paste0("Note: horizontal axis limited to 300 minutes. ",
                        n_above, " flights with longer delays are not shown.")) +
  theme_minimal()
ggsave(file.path(out_dir, "w3_vibe_hist_2.pdf"), p_vibe2, width = 5, height = 3.5)

# Scatterplot of air time against distance
p_scatter <- ggplot(df_flights, aes(x = distance, y = air_time)) +
  geom_point(size = 2) +
  labs(x = "Distance (miles)", y = "Air time (minutes)")
ggsave(file.path(out_dir, "w3_scatter_distance_airtime.png"), p_scatter,
       width = 5, height = 3.5, dpi = 200)

# Scatter plot: arrival delay against distance (PNG: too many points for a PDF)
p_sc_delay <- ggplot(df_flights, aes(x = distance, y = arr_delay)) +
  geom_point(alpha = 0.2) +
  labs(x = "Distance (miles)", y = "Arrival delay (minutes)")
ggsave(file.path(out_dir, "w3_scatter_distance_delay.png"), p_sc_delay,
       width = 5, height = 3.5, dpi = 200)

# Scatter plot: arrival delay against distance, by airport of origin
p_sc_delay_origin <- ggplot(df_flights, aes(x = distance, y = arr_delay, colour = origin)) +
  geom_point(alpha = 0.2) +
  labs(x = "Distance (miles)", y = "Arrival delay (minutes)", colour = "Origin") +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme(legend.position = "bottom")
ggsave(file.path(out_dir, "w3_scatter_distance_delay_origin.png"), p_sc_delay_origin,
       width = 5, height = 3.5, dpi = 200)

# Binscatter: mean arrival delay within 20 bins of distance (equal number of flights per bin)
dist_breaks <- unique(quantile(df_flights$distance, probs = seq(0, 1, length.out = 21)))
p_binscatter <- ggplot(df_flights, aes(x = distance, y = arr_delay)) +
  stat_summary_bin(fun = "mean", breaks = dist_breaks, geom = "point", size = 2) +
  labs(x = "Distance (miles)", y = "Mean arrival delay (minutes)")
ggsave(file.path(out_dir, "w3_binscatter_distance_delay.pdf"), p_binscatter, width = 5, height = 3.5)

# "Vibecoding" binscatter: the kind of script an AI assistant returns for the binscatter prompt
d <- subset(df_flights, !is.na(arr_delay))
brk <- quantile(d$distance, probs = seq(0, 1, 0.05))
d$bin <- cut(d$distance, unique(brk), include.lowest = TRUE)
bins <- aggregate(cbind(distance, arr_delay) ~ bin,
                  data = d, FUN = mean)
p_vibe_bin <- ggplot(bins, aes(x = distance, y = arr_delay)) +
  geom_point(size = 2) +
  labs(x = "Distance (miles)", y = "Mean arrival delay (minutes)",
       caption = paste("Note: 20 distance bins with roughly equal",
                       "numbers of flights. Points are bin means.",
                       "Cancelled flights (no delay) excluded.", sep = "\n"))
ggsave(file.path(out_dir, "w3_vibe_binscatter.pdf"), p_vibe_bin, width = 5, height = 3.5)

# "Vibecoding" binscatter by airport of origin: same bins, means by bin and airport
bins_origin <- aggregate(cbind(distance, arr_delay) ~ bin + origin,
                         data = d, FUN = mean)
p_vibe_bin_origin <- ggplot(bins_origin, aes(x = distance, y = arr_delay,
                                             colour = origin)) +
  geom_point(size = 2) +
  labs(x = "Distance (miles)", y = "Mean arrival delay (minutes)",
       colour = "Origin",
       caption = paste("Note: same 20 distance bins for all airports.",
                       "Points are bin means for each airport.",
                       "Cancelled flights (no delay) excluded.", sep = "\n"))
ggsave(file.path(out_dir, "w3_vibe_binscatter_origin.pdf"), p_vibe_bin_origin,
       width = 5, height = 3.5)

# Line graph: number of flights per day
df_flights <- df_flights |>
  mutate(date = as.Date(paste(year, month, day, sep = "-")),
         ym   = as.Date(paste(year, month, 1, sep = "-")))
daily <- df_flights |>
  group_by(date) |>
  summarise(n = n())
p_line <- ggplot(daily, aes(x = date, y = n)) +
  geom_line() +
  labs(x = "Date", y = "Number of flights")
ggsave(file.path(out_dir, "w3_line_daily.pdf"), p_line, width = 5, height = 3.5)

# Line graph: number of flights per month (month x year)
monthly <- df_flights |>
  group_by(ym) |>
  summarise(n = n())
p_line_month <- ggplot(monthly, aes(x = ym, y = n)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = "Month", y = "Number of flights")
ggsave(file.path(out_dir, "w3_line_monthly.pdf"), p_line_month, width = 5, height = 3.5)

# Same monthly line graph, vertical axis from zero
p_line_month_zero <- p_line_month + expand_limits(y = 0)
ggsave(file.path(out_dir, "w3_line_monthly_zero.pdf"), p_line_month_zero, width = 5, height = 3.5)

# Monthly line graph by airport of origin, vertical axis from zero
monthly_origin <- df_flights |>
  group_by(ym, origin) |>
  summarise(n = n(), .groups = "drop")
p_line_month_origin <- ggplot(monthly_origin, aes(x = ym, y = n, colour = origin)) +
  geom_line() +
  geom_point() +
  expand_limits(y = 0) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = "Month", y = "Number of flights", colour = "Origin") +
  theme(legend.position = "bottom")
ggsave(file.path(out_dir, "w3_line_monthly_origin.pdf"), p_line_month_origin, width = 5, height = 3.5)

# "Vibecoding" line graph: % change in weekly flights, first full week = 100, by airport
# (weeks start on Monday; incomplete weeks at the start and end of the year are dropped)
weekly <- df_flights |>
  mutate(week = as.Date(cut(date, "week"))) |>
  group_by(origin, week) |>
  summarise(n = n(), days = n_distinct(date), .groups = "drop") |>
  filter(days == 7) |>
  group_by(origin) |>
  mutate(index = 100 * n / first(n),
         pct_change = index - 100) |>
  ungroup()
p_vibe_line <- ggplot(weekly, aes(x = week, y = pct_change, colour = origin)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line() + geom_point(size = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = "Week", y = "Change in flights (%)", colour = "Origin",
       caption = "Note: first full week of 2013 = 100. Incomplete weeks excluded.")
ggsave(file.path(out_dir, "w3_vibe_line_weekly.pdf"), p_vibe_line, width = 5, height = 3.5)

# Bar chart: number of flights per month
p_bar <- ggplot(df_flights, aes(x = factor(month))) +
  geom_bar() +
  labs(x = "Month", y = "Number of flights")
ggsave(file.path(out_dir, "w3_bar_month.pdf"), p_bar, width = 5, height = 3.5)

# Bar chart: number of flights per month, by airport of origin
p_bar_origin <- ggplot(df_flights, aes(x = factor(month), fill = origin)) +
  geom_bar(position = "dodge") +
  labs(x = "Month", y = "Number of flights", fill = "Origin") +
  theme(legend.position = "bottom")
ggsave(file.path(out_dir, "w3_bar_month_origin.pdf"), p_bar_origin, width = 5, height = 3.5)

# Bar chart: number of flights per month, by airport of origin, stacked (default position)
p_bar_origin_stack <- ggplot(df_flights, aes(x = factor(month), fill = origin)) +
  geom_bar() +
  labs(x = "Month", y = "Number of flights", fill = "Origin") +
  theme(legend.position = "bottom")
ggsave(file.path(out_dir, "w3_bar_month_origin_stack.pdf"), p_bar_origin_stack, width = 5, height = 3.5)

# Box plot: arrival delay by airport of origin
p_box <- ggplot(df_flights, aes(x = origin, y = arr_delay)) +
  geom_boxplot() +
  labs(x = "Airport of origin", y = "Arrival delay (minutes)")
ggsave(file.path(out_dir, "w3_box_origin.pdf"), p_box, width = 5, height = 3.5)

# Same box plot without outliers (outliers = FALSE also drops them from the axis range;
# median, box and whiskers are still computed with all observations)
p_box_noout <- ggplot(df_flights, aes(x = origin, y = arr_delay)) +
  geom_boxplot(outliers = FALSE) +
  labs(x = "Airport of origin", y = "Arrival delay (minutes)")
ggsave(file.path(out_dir, "w3_box_origin_nooutliers.pdf"), p_box_noout, width = 5, height = 3.5)

# Box plot: arrival delay by airport of origin and time of day
df_flights$time_of_day <- ifelse(df_flights$hour < 12, "Morning", "Afternoon/evening")
p_box_tod <- ggplot(df_flights, aes(x = origin, y = arr_delay, fill = time_of_day)) +
  geom_boxplot(outliers = FALSE) +
  labs(x = "Airport of origin", y = "Arrival delay (minutes)", fill = "Departure") +
  theme(legend.position = "bottom")
ggsave(file.path(out_dir, "w3_box_origin_tod.pdf"), p_box_tod, width = 5, height = 3.5)

# Box plot: arrival delay by airport of origin and month (deliberately cluttered: 36 boxes)
p_box_month <- ggplot(df_flights, aes(x = origin, y = arr_delay, fill = factor(month))) +
  geom_boxplot(outliers = FALSE) +
  labs(x = "Airport of origin", y = "Arrival delay (minutes)", fill = "Month") +
  theme(legend.position = "bottom")
ggsave(file.path(out_dir, "w3_box_origin_month.pdf"), p_box_month, width = 5, height = 3.5)
