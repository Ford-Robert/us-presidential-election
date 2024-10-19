#### Preamble ####
# Purpose: Test analysis data
# Author: Michelle Ji, Robert Ford, Cher Ning
# Date: 6 October 2024
# Contact: michelle.ji@mail.utoronto.ca, robert.ford@mail.utoronto.ca, cher.ning@mail.utoronto.ca
# License: MIT
# Prerequisites: 
# - access to cleaned datasets: access to and run cleaned_poll_data.csv
# - `tidyverse` library must be installed and loaded

#### Workplace setup ####
library(tidyverse)

#### Test analysis data ####
clean_data <- read_csv("data/cleaned_poll_data.csv")

# test classes of each column
cleaned_poll_data$pollster |> class() == "character"
cleaned_poll_data$method |> class() == "character"
cleaned_poll_data$state |> class() == "character"
cleaned_poll_data$numeric_grade |> class() == "numeric"
clean_data$`Candidate Favored` |> class() == "character"

# can test min and max values
max(cleaned_poll_data$transparency_score) == 10
min(cleaned_poll_data$transparency_score) == 1

