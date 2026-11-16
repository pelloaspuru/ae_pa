# Applied Economics Course
# Winter 2026/2027
# Lecturer: Pello Aspuru

# R script for the Scraping Data Session (session 03)
# Code gathered sequentially from the session slides.
# Note: this script scrapes Wikipedia (103 airport pages, twice) and takes
# several minutes to run. It needs an internet connection.

# Set a new working directory (adjust the path to your own folder)
# setwd("C:/Users/YourName/Desktop/AE_2026")

# Verify the change
getwd()

# Clean working space
rm(list = ls())

# ---- Programming Languages for Web Scraping ---- ----

# Install once per computer:
# install.packages(c("rvest", "httr", "stringr"))

library(rvest)
library(httr)
library(stringr)

# ---- Base Dataset: flights ---- ----

library(nycflights13)
data("flights")
dim(flights)
head(flights)
names(flights)

# Origin and destination airports
table(flights$origin, flights$dest)

# ---- Complementary Dataset: airports ---- ----

data("airports")
dim(airports)   # 1458 rows, 8 columns
head(airports)
names(airports)

# ---- Setting Up for Vibecoding ---- ----

# Keep only the airports that appear in flights, as origin or destination
airports <- airports[airports$faa %in% c(flights$origin, flights$dest), ]
nrow(airports)
head(airports$faa)

# ---- The First Vibecoded Scraper ---- ----

library(rvest)

# Empty dataframe to store the results, one row per airport
df_airports_wiki <- data.frame()

for (code in airports$faa) {

  # Read the Wikipedia page of the airport code; skip the code if the page does not exist
  page <- tryCatch(read_html(paste0("https://en.wikipedia.org/wiki/", code)),
                   error = function(e) NULL)
  if (is.null(page)) next

  # Rows of the runway table, inside the infobox at the top right of the page
  rows <- html_text2(html_elements(page, "table.infobox table tr"))

  # Runway rows start with a runway direction, e.g. "4L/22R  3,353  11,000  Asphalt"
  runways <- strsplit(rows[grepl("^[0-9]+[LCR]?/[0-9]+", rows)], "\t")

  # Lengths in metres: 2nd column, or 3rd if the table lists feet first
  m_col     <- if ("ft\tm" %in% rows) 3 else 2
  lengths_m <- sapply(runways, function(r) r[m_col])

  df_airports_wiki <- rbind(df_airports_wiki, data.frame(
    airport_code    = code,
    n_runways       = length(runways),
    runway_length_m = paste(lengths_m, collapse = "; ")
  ))

  Sys.sleep(0.5) # wait half a second between pages, to be polite to Wikipedia
}

# ---- Checking df_airports_wiki ---- ----

table(is.na(df_airports_wiki$n_runways))

table(df_airports_wiki$n_runways)

head(df_airports_wiki[df_airports_wiki$n_runways != 0, ])

# Sanity check: Dulles International Airport (IAD)
print(df_airports_wiki[df_airports_wiki$airport_code == "IAD", ])

# ---- Refining the Prompt ---- ----

# Airport codes whose page was not found (no runways scraped)
df_airports_not_found <- df_airports_wiki$airport_code[df_airports_wiki$n_runways == 0]
print(df_airports_not_found)

# ---- The Second Vibecoded Scraper ---- ----

df_airports_wiki2 <- data.frame()

for (code in airports$faa) {

  # New: instead of guessing the page address, ask Wikipedia's search engine
  # for "<code> airport" and take the title of the first result
  search_url <- paste0("https://en.wikipedia.org/w/api.php?action=query&list=search",
                       "&format=json&srsearch=", code, "%20airport")
  title <- tryCatch(content(GET(search_url))$query$search[[1]]$title,
                    error = function(e) NULL)

  # Read the Wikipedia page of that title (NULL if the search or the page fails)
  page <- if (is.null(title)) NULL else
    tryCatch(read_html(paste0("https://en.wikipedia.org/wiki/", URLencode(title))),
             error = function(e) NULL)

  # New: if the page is not found, keep the airport in the dataset with NA values
  if (is.null(page)) {
    df_airports_wiki2 <- rbind(df_airports_wiki2, data.frame(
      airport_code = code, airport_name = NA, n_runways = NA, runway_length_m = NA))
    next
  }

  # Rows of the runway table, inside the infobox at the top right of the page
  rows <- html_text2(html_elements(page, "table.infobox table tr"))

  # Runway rows start with a runway direction, e.g. "4L/22R  3,353  11,000  Asphalt"
  runways <- strsplit(rows[grepl("^[0-9]+[LCR]?/[0-9]+", rows)], "\t")

  # Lengths in metres: 2nd column, or 3rd if the table lists feet first
  m_col     <- if ("ft\tm" %in% rows) 3 else 2
  lengths_m <- sapply(runways, function(r) r[m_col])

  # New: NA instead of 0 if no runways are found, and then also NA for their lengths
  n_runways       <- if (length(runways) == 0) NA else length(runways)
  runway_length_m <- if (is.na(n_runways)) NA else paste(lengths_m, collapse = "; ")

  df_airports_wiki2 <- rbind(df_airports_wiki2, data.frame(
    airport_code    = code,
    n_runways       = n_runways,
    runway_length_m = runway_length_m
  ))

  Sys.sleep(1) # wait one second between pages, to be polite to Wikipedia
}

# ---- Results of the Second Attempt ---- ----

head(df_airports_wiki2)

table(is.na(df_airports_wiki2$n_runways))
table(df_airports_wiki2$n_runways)

print(df_airports_wiki2[df_airports_wiki2$airport_code == "IAD",
                        c("airport_code", "n_runways", "runway_length_m")])

# ---- Merging Twice: Airport of Origin ---- ----

library(dplyr)

# Airport information, referring to the airport of origin
df_origin <- df_airports_wiki2 |>
  select(origin                 = airport_code,
         origin_n_runways       = n_runways,
         origin_runway_length_m = runway_length_m)

df_flights <- flights |>
  left_join(df_origin, by = "origin")

table(df_flights$origin, df_flights$origin_n_runways)

# ---- Merging Twice: Airport of Destination ---- ----

# Airport information, referring to the airport of destination
df_dest <- df_airports_wiki2 |>
  select(dest                 = airport_code,
         dest_n_runways       = n_runways,
         dest_runway_length_m = runway_length_m)

df_flights <- df_flights |>
  left_join(df_dest, by = "dest")

dim(df_flights)   # 336776 rows, 23 columns

table(is.na(df_flights$dest_n_runways), is.na(df_flights$origin_n_runways))

# ---- Saving the Dataset ---- ----

# Print the working directory
getwd()

# Save dataset df_flights
write.csv(df_flights, "data/df_flights_enriched.csv", row.names = FALSE)
