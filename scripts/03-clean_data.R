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

#!CHECKING STATES!#
all_states <- state.name
all_states <- c(all_states, "District of Columbia")
states <- unique(raw_poll_data$state)

missing_states <- setdiff(all_states, states)
print(missing_states)
#!CHECKING STATES!#

raw_poll_data <- raw_poll_data |>
  filter(end_date >= as.Date("2024-07-22"))

# select only rows of interest
cleaned_poll_data <- raw_poll_data |>
  select(pollster, numeric_grade, pollscore, end_date, transparency_score, 
         question_id, methodology, state, sample_size, answer, pct)


# rename column names for clarity
cleaned_poll_data <- cleaned_poll_data |>
  rename("method" = "methodology",
         "candidate" = "answer",
         "support" = "pct")

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

# Convert method to a factor
cleaned_poll_data <- cleaned_poll_data %>%
  mutate(
    method = as.factor(method)
  )

#TODO set states that are NA to "National", figure out how to add that in

#Group states by the 538 state groups to improve model accuracy
#found here: https://abcnews.go.com/538/538s-2024-presidential-election-forecast-works/story?id=113068753
cleaned_poll_data$state[cleaned_poll_data$state == "Maine CD-1"] <- "Maine"
cleaned_poll_data$state[cleaned_poll_data$state == "Maine CD-2"] <- "Maine"
cleaned_poll_data$state[cleaned_poll_data$state == "Nebraska CD-1"] <- "Nebraska"
cleaned_poll_data$state[cleaned_poll_data$state == "Nebraska CD-2"] <- "Nebraska"


# Define the state to region mapping
region_mapping <- c(
  # Pacific Region
  "Washington" = "Pacific",
  "Oregon" = "Pacific",
  "California" = "Pacific",
  "Hawaii" = "Pacific",
  
  # Mountain Region
  "Alaska" = "Mountain",
  "Idaho" = "Mountain",
  "Montana" = "Mountain",
  "Wyoming" = "Mountain",
  "Utah" = "Mountain",
  
  # Southwest
  "Nevada" = "Southwest",
  "Colorado" = "Southwest",
  "Arizona" = "Southwest",
  "New Mexico" = "Southwest",
  
  # Plains
  "North Dakota" = "Plains",
  "South Dakota" = "Plains",
  "Nebraska" = "Plains",
  "Kansas" = "Plains",
  
  # Rust Belt
  "Minnesota" = "Rust",
  "Iowa" = "Rust",
  "Wisconsin" = "Rust",
  "Michigan" = "Rust",
  "Illinois" = "Rust",
  "Indiana" = "Rust",
  "Ohio" = "Rust",
  "Pennsylvania" = "Rust",
  
  # Texish
  "Oklahoma" = "Texish",
  "Texas" = "Texish",
  "Louisiana" = "Texish",
  
  # South
  "Missouri" = "South",
  "Arkansas" = "South",
  "Kentucky" = "South",
  "Tennessee" = "South",
  "Mississippi" = "South",
  "Alabama" = "South",
  "West Virginia" = "South",
  "South Carolina" = "South",
  
  # Southeast
  "Virginia" = "Southeast",
  "North Carolina" = "Southeast",
  "Georgia" = "Southeast",
  "Florida" = "Southeast",
  
  # Northeast
  "New York" = "Northeast",
  "New Jersey" = "Northeast",
  "Rhode Island" = "Northeast",
  "Connecticut" = "Northeast",
  "Maryland" = "Northeast",
  "Delaware" = "Northeast",
  "District of Columbia" = "Northeast",
  
  # New England
  "Maine" = "NEngland",
  "New Hampshire" = "NEngland",
  "Vermont" = "NEngland",
  "Massachusetts" = "NEngland"
)

cleaned_poll_data <- cleaned_poll_data %>%
  mutate(pol_region = region_mapping[state])

#!CHECKING STATES!#
all_states <- state.name
all_states <- c(all_states, "District of Columbia")
states <- unique(cleaned_poll_data$state)

missing_states <- setdiff(all_states, states)
print(missing_states)
#!CHECKING STATES!#


View(cleaned_poll_data)
str(cleaned_poll_data)
# write to csv
write_csv(cleaned_poll_data, "data/cleaned_poll_data.csv")

