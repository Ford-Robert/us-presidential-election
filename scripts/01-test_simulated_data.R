#### Preamble ####
# Purpose: Test simulated data
# Author: Michelle Ji, Robert Ford
# Date: 6 October 2024
# Contact: michelle.ji@mail.utoronto.ca, robert.ford@mail.utoronto.ca
# License: MIT
# Prerequisites: 
  # - access to cleaned datasets: access to and run 00-simulate_data.csv
  # - `tidyverse` library must be installed and loaded


#### Workplace setup ####
simulated_polling_data <- read_csv("data/00-simulate_data.csv")

#### Test simulated data ####

# test classes of each column
simulated_polling_data$`Pollster Name` |> class() == "character"
simulated_polling_data$Methodology |> class() == "character"
simulated_polling_data$State |> class() == "character"
simulated_polling_data$`Type of Voter` |> class() == "character"
simulated_polling_data$`Candidate Favored` |> class() == "character"

# test to see if only 4 types of voters that exist in type of voter
voter_types <- unique(simulated_polling_data$`Type of Voter`)

if (all(voter_types %in% c("lv", "rv", "a", "v"))) {
  print("Only lv, rv, a, v exist in 'type of voter' column")
} else {
  print(paste("Other types of voters found:", paste(voter_types, collapse = ", ")))
}


