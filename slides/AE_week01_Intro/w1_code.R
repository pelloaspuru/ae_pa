# Check current working directory
#getwd()

# Set a new working directory.
#setwd("C:/Users/YourName/Desktop/AE_2025/AE_w1_Intro")

# Verify the change
#getwd()

# List files in the working directory
#dir()

# Clean working space
rm(list = ls())

# set seed for reproducibility
set.seed(123)

# generate objects
n <- 100 # Scalar object
id <- 1:n # Vector object
age <- sample(18:65, n, replace = TRUE) # Another vector

# Visualize 
age # Print object in console
str(age) # Object type, size
class(age) # Object type 
ls() # List all objects

# Vectors: three vectors with one value per month (AirPassengers, 1949-1960)
passengers <- as.numeric(AirPassengers)
year       <- as.numeric(floor(time(AirPassengers)))
month      <- as.numeric(cycle(AirPassengers))

length(passengers)     # 144

# Access elements of a vector
passengers[1]          # first element: 112
passengers[1:3]        # elements 1 to 3: 112 118 132
passengers[c(1, 144)]  # first and last: 112 432

# Create a dataset from the three vectors
df_air <- data.frame(year, month, passengers)
dim(df_air)            # 144 rows, 3 columns

head(df_air)

# More objects!
gender   <- sample(c("Male", "Female"), n, replace = TRUE)
education<- sample(c("High School", "Bachelor", "Master", "PhD"), n, replace = TRUE,
                   prob = c(0.4, 0.35, 0.2, 0.05))
income   <- round(rnorm(n, mean = 40000 + 1500*age/10, sd = 8000), 0)
employed <- ifelse(income > 35000, 1, 0)

# put into a data frame
df <- data.frame(id, age, gender, education, income, employed)

# inspect first rows
head(df)
summary(df)

# Built-in datasets
# longley: US macroeconomic data, one row per year (1947-1962)
data(longley)
class(longley)        # "data.frame"
head(longley)

# AirPassengers: monthly airline passengers (1949-1960)
data(AirPassengers)
class(AirPassengers)  # "ts" (time series), not a data frame

# put its vectors into a data frame:
df_air <- data.frame(year       = floor(time(AirPassengers)),
                     month      = cycle(AirPassengers),
                     passengers = as.numeric(AirPassengers))

head(AirPassengers)


df$age # Print the age column of df
df[[2]]

# Accessing data through row/column indexes
df[1:3,] # Rows 1 to 3, all columns.
df[,c(1,4)] # All rows, Columns 1 and 4.
df[c(1,25),c(1,3)] # Rows 1 and 25, Columns 1 and 3.

df_first_fifty_rows <- df[1:50,]
dim(df_first_fifty_rows)

# Logical condition vector
index_old_males <- (df$gender == "Male")&(df$age>50)
table(index_old_males)

# Data subset
df_old_males <- df[index_old_males,]
df_old_males <- df[(df$gender == "Male")&(df$age>50),]

# New variables generation
df$log_age <- log(df$age)

df$is_higly_educated <- df$education %in% c("Master", "PhD")
table(df$is_higly_educated)

#install.packages("ggplot2")

library(ggplot2)

# Histogram of income
ggplot(df, aes(x = income)) +
  geom_histogram(bins = 30) +
  labs(x = "Income", y = "Frequency")

# Scatterplot of income against age
ggplot(df, aes(x = income, y = age)) +
  geom_point(size  = 2) +
  labs(x = "Income", y = "Age")

# ------------------------------------------------------------
# Data
# ------------------------------------------------------------

# Types of variables
class(df$income)    # numerical
class(df$education) # categorical
table(df$employed)  # binary (dummy) variable

# Collapse: from one row per individual to one row per education level
df_collapsed <- aggregate(income ~ education, data = df, FUN = mean)
df_collapsed

# Merge: add the variables of a second dataset, matching on a key variable
df_school <- data.frame(education = c("High School", "Bachelor", "Master", "PhD"),
                        years_school = c(12, 15, 17, 21))
df_merged <- merge(df, df_school, by = "education")
head(df_merged)

# Append: stack the observations of two datasets with the same variables
df_a <- df[1:50,]   # first 50 individuals
df_b <- df[51:100,] # last 50 individuals
df_appended <- rbind(df_a, df_b)
dim(df_appended)

# ------------------------------------------------------------
# Graphs
# ------------------------------------------------------------

# Order the education categories (otherwise R sorts them alphabetically)
df$education <- factor(df$education,
                       levels = c("High School", "Bachelor", "Master", "PhD"))

# Histogram: one continuous variable
ggplot(df, aes(x = income)) +
  geom_histogram(bins = 20)

# ... one panel per category
ggplot(df, aes(x = income)) +
  geom_histogram(bins = 20) +
  facet_wrap(~ gender, ncol = 1)

# Bar chart: one categorical variable
ggplot(df, aes(x = education)) +
  geom_bar()

# ... one bar per category
ggplot(df, aes(x = education, fill = gender)) +
  geom_bar(position = "dodge") +
  theme(legend.position = "bottom")

# Box plot: one continuous and one categorical variable
ggplot(df, aes(x = education, y = income)) +
  geom_boxplot()

# ... split by a second categorical variable
ggplot(df, aes(x = education, y = income,
               fill = gender)) +
  geom_boxplot() +
  theme(legend.position = "bottom")

# Scatter plot: two continuous variables
ggplot(df, aes(x = age, y = income)) +
  geom_point()

# ... one colour (and one fitted line) per category
ggplot(df, aes(x = age, y = income,
               colour = gender)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Line graph: one continuous variable over time
# (simulated unemployment rates of two countries, 2000-2024)
year    <- rep(2000:2024, times = 2)
country <- rep(c("A", "B"), each = 25)
unemp   <- c(6 + cumsum(rnorm(25, mean = 0, sd = 0.5)),
             9 + cumsum(rnorm(25, mean = 0, sd = 0.5)))
df_ts   <- data.frame(year, country, unemp)

ggplot(df_ts[df_ts$country == "A", ],
       aes(x = year, y = unemp)) +
  geom_line()

# ... one line per category
ggplot(df_ts, aes(x = year, y = unemp,
                  colour = country)) +
  geom_line()
