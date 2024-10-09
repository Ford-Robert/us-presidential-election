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
  select(display_name, methodology, state, sample_size, 
         population, candidate_name)

# rename column names for clarity
cleaned_poll_data <- cleaned_poll_data |>
  rename("Pollster Name" = "display_name",
         "Methodology" = "methodology",
         "State" = "state",
         "Type of Voter" = "population",
         "Candidate Favored" = "candidate_name")

# write to csv
write_csv(cleaned_poll_data, "data/cleaned_poll_data.csv")

