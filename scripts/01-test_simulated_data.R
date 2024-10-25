#### Preamble ####
# Purpose: Test simulated data
# Author: Michelle Ji, Robert Ford, Cher Ning
# Date: 6 October 2024
# Contact: michelle.ji@mail.utoronto.ca, robert.ford@mail.utoronto.ca, cher.ning@mail.utoronto.ca
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
simulated_polling_data$numeric_grade |> class() == "numeric"
simulated_polling_data$`Candidate Favored` |> class() == "character"

# can test min and max values
max(simulated_polling_data$transparency_score) == 10
min(simulated_polling_data$transparency_score) == 1


#

historical_wins <- tibble(
  State = c("California", "New York", "Texas", "Florida"),
  Party_Lean = c("Democrat", "Democrat", "Republican", "Republican")
)


# Function to check if simulated outcomes align with historical expectations
test_that("State wins align with historical trends", {
  # Filter Kamala Harris results
  harris_results <- simulated_poll_data |>
    filter(simulated_poll_data$`Candidate Favored` == "Harris")
  
  # Check if Harris is winning in historically Democrat-leaning states
  historical_dem_states <- historical_wins %>%
    filter(Party_Lean == "Democrat") %>%
    pull(State)
  
  for (state in historical_dem_states) {
    avg_support <- harris_results %>%
      filter(State == state) %>%
      summarize(avg_support = mean(Percentage, na.rm = TRUE)) %>%
      pull(avg_support)
    
    # Test if average support in these states is reasonable for a Democrat

        expect_true(avg_support > 50, info = paste("Kamala Harris is winning", state))
  }
  
  # Check that she does not win in historically Republican states
  historical_rep_states <- historical_wins %>%
    filter(Party_Lean == "Republican") %>%
    pull(State)
  
  for (state in historical_rep_states) {
    avg_support <- harris_results %>%
      filter(State == state) %>%
      summarize(avg_support = mean(Percentage, na.rm = TRUE)) %>%
      pull(avg_support)
    
    # Test if average support in these states is less than 50%
    expect_true(avg_support <= 50, info = paste("Kamala Harris is losing", state))
  }
})
