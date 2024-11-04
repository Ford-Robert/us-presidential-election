#### Preamble ####
# Purpose: Download US Polling Data
# Author: Michelle Ji, Robert Ford, Cher Ning
# Date: 6 October 2024
# Contact: michelle.ji@mail.utoronto.ca, robert.ford@mail.utoronto.ca, cher.ning@mail.utoronto.ca
# License: MIT
# Prerequisites: none

#LAST RUN 31 October

#### Workplace Setup ####
# install.packages("tidyverse")
library(tidyverse)
# Install devtools if you haven't already
library(devtools)
library(dplyr)

# Install electionData package from GitHub
devtools::install_github("jaytimm/PresElectionResults")


#### Download data ####
historical_state_data <- PresElectionResults::pres_by_state


raw_poll_data <- 
  read_csv(
    file = 
      "https://projects.fivethirtyeight.com/polls/data/president_polls.csv"
    ,
    show_col_types = FALSE
  )


electoral_votes <- data.frame(
  state = c(
    "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
    "Connecticut", "Delaware", "Florida", "Georgia",
    "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
    "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
    "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
    "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
    "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
    "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
    "Washington", "West Virginia", "Wisconsin", "Wyoming", "District of Columbia"
  ),
  ev = c(
    9, 3, 11, 6, 54, 10,
    7, 3, 30, 16,
    4, 4, 19, 11, 6, 6, 8,
    8, 4, 10, 11, 15, 10,
    6, 10, 4, 5, 6, 4,
    14, 5, 28, 16, 3,
    17, 7, 8, 19, 4, 9,
    3, 11, 40, 6, 3, 13,
    12, 4, 10, 3, 3
  )
)



write_csv(electoral_votes, "data/electoral_votes.csv")

#View(raw_poll_data)
#View(historical_state_data)
# save to csv
write_csv(raw_poll_data, "data/raw_poll_data.csv")
write_csv(historical_state_data, "data/historical_state_data.csv")
