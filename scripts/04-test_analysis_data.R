#### Preamble ####
# Purpose: Test analysis data
# Author: Michelle Ji, Robert Ford
# Date: 6 October 2024
# Contact: michelle.ji@mail.utoronto.ca, robert.ford@mail.utoronto.ca
# License: MIT
# Prerequisites: 
# - access to cleaned datasets: access to and run cleaned_poll_data.csv
# - `tidyverse` library must be installed and loaded

#### Workplace setup ####
library(tidyverse)

#### Test analysis data ####
clean_data <- read_csv("data/cleaned_poll_data.csv")

# test classes of each column
clean_data$`Pollster Name` |> class() == "character"
clean_data$Methodology |> class() == "character"
clean_data$State |> class() == "character"
clean_data$`Type of Voter` |> class() == "character"
clean_data$`Candidate Favored` |> class() == "character"

# test to see if only 4 types of voters that exist in type of voter
voter_types <- unique(clean_data$`Type of Voter`)

if (all(voter_types %in% c("lv", "rv", "a", "v"))) {
  print("Only lv, rv, a, v exist in 'type of voter' column")
} else {
  print(paste("Other types of voters found:", paste(voter_types, collapse = ", ")))
}

