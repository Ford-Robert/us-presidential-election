#### Preamble ####
# Purpose: Test simulated data
# Author: Michelle Ji, Robert Ford, Cher Ning
# Date: 6 October 2024
# Contact: michelle.ji@mail.utoronto.ca, robert.ford@mail.utoronto.ca, cher.ning@mail.utoronto.ca
# License: MIT
# Prerequisites: 
  # - access to cleaned datasets: access to and run 00-simulate_data.csv
  # - `tidyverse` library must be installed and loaded

#TODO: Set up a more comprehensive set of tests. See Chapter 9 for all the tools that we need to use

#### Workplace setup ####
simulated_polling_data <- read_csv("data/00-simulate_data.csv")

#### Test simulated data ####

# test classes of each column
simulated_polling_data$`Pollster Name` |> class() == "character"
simulated_polling_data$Methodology |> class() == "character"
simulated_polling_data$State |> class() == "character"
simulated_polling_data$numeric_grade |> class() == "numeric"
simulated_polling_data$`Candidate Favored` |> class() == "character"

# can test min and max values
max(simulated_polling_data$transparency_score) == 10
min(simulated_polling_data$transparency_score) == 1


