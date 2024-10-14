#### Preamble ####
# Purpose: Clean Raw US Polling Data
# Author: Michelle Ji, Robert Ford
# Date: 6 October 2024
# Contact: michelle.ji@mail.utoronto.ca, robert.ford@mail.utoronto.ca
# License: MIT
# Prerequisites: access and run 02-download_data

#### Workplace setup ####
library(tidyverse)
library(lubridate)

#### Clean data ####
# read file first
raw_poll_data <- read_csv("data/raw_poll_data.csv")

# filter out polls from before Biden dropped out of race
raw_poll_data <- raw_poll_data |>
  mutate(end_date = mdy(end_date)) 

raw_poll_data <- raw_poll_data |>
  filter(end_date >= as.Date("2024-07-22"))

# select only rows of interest
cleaned_poll_data <- raw_poll_data |>
  select(pollster, numeric_grade, pollscore, end_date, transparency_score, 
         question_id, methodology, state, sample_size, answer, pct)


# rename column names for clarity
cleaned_poll_data <- cleaned_poll_data |>
  rename("method" = "methodology",
         "candidate" = "answer")

cleaned_poll_data <- cleaned_poll_data %>%
  filter(candidate == "Harris")

cleaned_poll_data <- na.omit(cleaned_poll_data)

# Adding how many days before the election that the poll was conducted

election_date <- 11-5-24
cleaned_poll_data <- cleaned_poll_data %>%
  mutate(
    election_date = as.Date("11-5-24", format = "%m-%d-%y")
  )

# Calculate the days to election and add as a new column
cleaned_poll_data <- cleaned_poll_data %>%
  mutate(days_to_election = as.numeric(election_date - end_date))

# Adding weights
cleaned_poll_data <- cleaned_poll_data %>%
  mutate(
    initial_weight = ifelse(
      is.na(numeric_grade),
      1,
      sqrt(numeric_grade / 3)
    )
  )

# Normalize weights
average_weight <- mean(cleaned_poll_data$initial_weight, na.rm = TRUE)
cleaned_poll_data <- cleaned_poll_data %>%
  mutate(weight = initial_weight / average_weight)

View(cleaned_poll_data)
str(cleaned_poll_data)

# write to csv
write_csv(cleaned_poll_data, "data/cleaned_poll_data.csv")

