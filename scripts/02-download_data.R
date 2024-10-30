#### Preamble ####
# Purpose: Download US Polling Data
# Author: Michelle Ji, Robert Ford, Cher Ning
# Date: 6 October 2024
# Contact: michelle.ji@mail.utoronto.ca, robert.ford@mail.utoronto.ca, cher.ning@mail.utoronto.ca
# License: MIT
# Prerequisites: none

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


#View(raw_poll_data)
#View(historical_state_data)
# save to csv
write_csv(raw_poll_data, "data/raw_poll_data.csv")
write_csv(historical_state_data, "data/historical_state_data.csv")
