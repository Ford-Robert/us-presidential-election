#### Preamble ####
# Purpose: Simulate possible observations with the US polling dataset
# Author: Michelle Ji, Robert Ford, Cher Ning
# Date: 6 October 2024
# Contact: michelle.ji@mail.utoronto.ca, robert.ford@mail.utoronto.ca, cher.ning@mail.utoronto.ca
# License: MIT
# Prerequisites: access to cleaned_poll_data

#### Workplace setup ####
library(tidyverse)

#### Simulate data #### 


# set up vectors where we will sample from
states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
              "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", 
              "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
              "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
              "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", 
              "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", 
              "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", 
              "Rhode Island", "South Carolina", "South Dakota", "Tennessee", 
              "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", 
              "Wisconsin", "Wyoming")
pollsters <- c(unique(cleaned_poll_data$pollster))
collection_methods <- c(unique(cleaned_poll_data$method))
candidate_favored <- c(unique(cleaned_poll_data$candidate))
start_date <- as.Date("2024-07-22")
end_date <- as.Date("2024-10-08")
date_sequence <- seq.Date(from = start_date, to = end_date, by = "day")

# simulate
set.seed(3)
simulated_poll_data <-
  tibble(
    "Pollster Name" = sample(pollsters, size = 100, replace = TRUE),
    "Methodology" = sample(collection_methods, size = 100, replace = TRUE),
    "Numeric Grade" = sample(0.7:3.0, size = 100, replace = TRUE),
    "Poll Score" = sample(-1.5:1.7, size = 100, replace = TRUE),
    "Date Ended" = sample(date_sequence, size = 100, replace = TRUE),
    "Transparency Score" = sample(1:10, size = 100, replace = TRUE),
    "Question ID" = sample(204124:212159, size = 100, replace = TRUE),
    "Support" = sample(0:70, size = 100, replace = TRUE),
    "Election Date" = "2024-11-05",
    "Days to Election" = sample(28:105, size = 100, replace = TRUE),
    "State" = sample(states, size = 100, replace = TRUE),
    "Type of Voter" = sample(c("lv", "rv", "a", "v"), size = 100, replace = TRUE),
    "Candidate Favored" = sample(candidate_favored, size = 100, replace = TRUE),
    "Percentage" = sample(1:100, size = 100, replace = TRUE)
  )

write_csv(simulated_poll_data, "data/00-simulate_data.csv")
