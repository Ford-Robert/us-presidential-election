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


# Test historical wins

historical_wins <- tibble(
  State = c("California", "Massachusetts", "Texas", "West Virigina"),
  Party_Lean = c("Democrat", "Democrat", "Republican", "Republican")
)



test_that("State wins align with historical trends", {
  # Filter Kamala Harris results
  harris_results <- cleaned_poll_data |>
    filter(cleaned_poll_data$candidate == "Harris")
  
  
  historical_dem_states <- historical_wins %>%
    filter(Party_Lean == "Democrat") %>%
    pull(State)
  
  for (state in historical_dem_states) {
    avg_support <- harris_results %>%
      filter(state == state) %>%
      summarize(avg_support = mean(support, na.rm = TRUE)) %>%
      pull(avg_support)
    
    
    
    expect_true(avg_support > 50, info = paste("Kamala Harris is winning", state))
  }
  
  # Check that she does not win in historically Republican states
  historical_rep_states <- historical_wins %>%
    filter(Party_Lean == "Republican") %>%
    pull(State)
  
  for (state in historical_rep_states) {
    avg_support <- harris_results %>%
      filter(state == state) %>%
      summarize(avg_support = mean(support, na.rm = TRUE)) %>%
      pull(avg_support)
    
    # Test if average support in these states is less than 50%
    expect_true(avg_support <= 50, info = paste("Kamala Harris is losing", state))
  }
})
