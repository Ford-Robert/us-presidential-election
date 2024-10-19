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

#### Download data ####
raw_poll_data <- 
  read_csv(
    file = 
      "https://projects.fivethirtyeight.com/polls/data/president_polls.csv"
    ,
    show_col_types = FALSE
  )


View(raw_poll_data)
# save to csv
write_csv(raw_poll_data, "data/raw_poll_data.csv")
