# Applied Economics Course
# Winter 2026/2027
# Lecturer: Pello Aspuru

# R script for the R Basics Session (session 02)
# Code gathered sequentially from the session slides.

# ---- Basic R Programming Concepts ---- ----

# This is a comment

# Create object "n"
n <- 100

# Define the function "square()"
square <- function(x) { return(x^2) }

# Run function on object, store as "result"
result <- square(n)

# Print the result in the console
print(result)

# ---- Script Set Up ---- ----

# Check current working directory
getwd()

# Set a new working directory (adjust the path to your own folder)
# setwd("C:/Users/YourName/Desktop/AE_2026")

# Verify the change
getwd()

# List files in the working directory
dir()

# ---- Clean the Environment ---- ----

# List all objects in the environment
ls()

# Remove all objects from the environment
rm(list = ls())

# Verify the change
ls()

# ---- R Objects ---- ----

# Create object "n"
n <- 100

# Print the object in the console:
n

# generate vector "age":
age <- sample(18:65, n, replace = TRUE)

# Visualize
age # Print object in console
str(age) # Object type, size
class(age) # Object type
dim(age)
ls() # List all objects

# ---- Vectors ---- ----

# Three vectors with one value per month
passengers <- as.numeric(AirPassengers)
year       <- as.numeric(floor(time(AirPassengers)))
month      <- as.numeric(cycle(AirPassengers))

length(passengers)     # 144

# Access elements of a vector
passengers[1]          # first element: 112
passengers[1:3]        # elements 1 to 3: 112 118 132
passengers[c(1, 144)]  # first and last: 112 432

# ---- Data Frames ---- ----

# Create a dataset from the three vectors
df_air <- data.frame(year, month, passengers)
dim(df_air)            # 144 rows, 3 columns

head(df_air)

# ---- Functions ---- ----

ls()

# Define the function "square()"
square <- function(x) { return(x^2) }

# Run function on object, store as "result"
result <- square(n)

# Print the result in the console
print(result)

ls()

# ---- R Packages ---- ----

# Install once per computer (downloads from CRAN)
# install.packages("dplyr")

# Load every time you start R
library(dplyr)

# ---- Inspecting Dataframes: Size and Structure ---- ----

dim(df_air)
names(df_air)
str(df_air)

tail(df_air)

# ---- Summary Statistics and Frequency Tables ---- ----

summary(df_air$passengers)

summary(df_air)

mean(df_air$passengers)
sd(df_air$passengers)

table(df_air$year)

# ---- Selecting Observations and Variables ---- ----

# Accessing data through row/column indexes
df_air[1:3,] # Rows 1 to 3, all columns.
df_air[c(2,5),c(1,3)] # Rows 2 and 5, Columns 1 and 3.
head(df_air[,c(1,3)]) # All rows, Columns 1 and 3 (first six rows shown).

# Access the 'passengers' variable.
df_air$passengers

# Store a subset under a new name, which keeps the original data
df_air_first_9_rows <- df_air[1:9,]
ls()

# ---- Creating Variables: From Existing Variables ---- ----

df_air$passengers_millions <- df_air$passengers / 1000
head(df_air)

# ---- Logical Conditions ---- ----

# Logical condition vector
index_fifties <- (df_air$year >= 1950) & (df_air$year <= 1959)
table(index_fifties)

# ---- Variables Based on a Condition ---- ----

df_air$season  <- ifelse(df_air$month %in% 6:8, "Summer", "Rest of year")
df_air$traffic <- ifelse(df_air$passengers > median(df_air$passengers), "High", "Low")

table(df_air$season)

# ---- Two-way Tables and Selecting with a Condition ---- ----

table(df_air$season, df_air$traffic)

prop.table(table(df_air$season, df_air$traffic), margin = 1)

# A logical vector selects the rows where it is TRUE
df_air_fifties <- df_air[index_fifties, ]
dim(df_air_fifties)

# ---- Working with Several Datasets: longley ---- ----

# longley: US macroeconomic data, one row per year (1947-1962)
data(longley)
class(longley)

dim(longley)
head(longley)

# ---- Creating Variables with dplyr ---- ----

# The old-fashioned way:
longley$employment_share <- longley$Employed/longley$Population

# The dplyr way: mutate() creates several variables at once
longley <- longley |>
  mutate(employment_share   = Employed / Population,
         unemployment_share = (Unemployed / 100) / Population,
         population_growth  = (Population - lag(Population)) / lag(Population))

head(longley)

# ---- Aggregating Data with dplyr ---- ----

# Aggregate the monthly data to one row per year
df_air_annual <- df_air |>
  group_by(year) |>
  summarise(annual_passengers       = sum(passengers),
            mean_monthly_passengers = mean(passengers),
            min_monthly_passengers  = min(passengers),
            max_monthly_passengers  = max(passengers))

head(df_air_annual)

# ---- Merging Datasets ---- ----

# The key variables: imperfect overlap (1949-1960 vs 1947-1962)
df_air_annual$year
longley$Year

# inner_join() keeps only the years that appear in both datasets
df_inner <- longley |>
  inner_join(df_air_annual, by = c("Year" = "year"))

# left_join() keeps all 16 rows of longley; years without a match get NA
df_left <- longley |>
  left_join(df_air_annual, by = c("Year" = "year"))

# right_join() keeps all rows of the right dataset
df_right <- longley |>
  right_join(df_air_annual, by = c("Year" = "year"))

# ---- Missing Values ---- ----

sum(is.na(df_left$annual_passengers))
colSums(is.na(df_left))

# Many functions return NA if the variable contains missing values;
# na.rm = TRUE tells them to ignore the missing values
mean(df_left$annual_passengers)
mean(df_left$annual_passengers, na.rm = TRUE)
