#### Preamble ####
# Purpose: Models the regional support, then calculates the EV each canditate is
#          expected to get
# Author:  Robert Ford, Michelle Ji
# Date: 14 October 2024
# Contact: robert.ford@mail.utoronto.ca, michelle.ji@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(tidyr)

#### Read data ####
region_mapping <- c(
  # Pacific Region
  "Washington" = "Pacific",
  "Oregon" = "Pacific",
  "California" = "Pacific",
  "Hawaii" = "Pacific",
  
  # Mountain Region
  "Alaska" = "Mountain",
  "Idaho" = "Mountain",
  "Montana" = "Mountain",
  "Wyoming" = "Mountain",
  "Utah" = "Mountain",
  
  # Southwest
  "Nevada" = "Southwest",
  "Colorado" = "Southwest",
  "Arizona" = "Southwest",
  "New Mexico" = "Southwest",
  
  # Plains
  "North Dakota" = "Plains",
  "South Dakota" = "Plains",
  "Nebraska" = "Plains",
  "Kansas" = "Plains",
  
  # Rust Belt
  "Minnesota" = "Rust",
  "Iowa" = "Rust",
  "Wisconsin" = "Rust",
  "Michigan" = "Rust",
  "Illinois" = "Rust",
  "Indiana" = "Rust",
  "Ohio" = "Rust",
  "Pennsylvania" = "Rust",
  
  # Texish
  "Oklahoma" = "Texish",
  "Texas" = "Texish",
  "Louisiana" = "Texish",
  
  # South
  "Missouri" = "South",
  "Arkansas" = "South",
  "Kentucky" = "South",
  "Tennessee" = "South",
  "Mississippi" = "South",
  "Alabama" = "South",
  "West Virginia" = "South",
  "South Carolina" = "South",
  
  # Southeast
  "Virginia" = "Southeast",
  "North Carolina" = "Southeast",
  "Georgia" = "Southeast",
  "Florida" = "Southeast",
  
  # Northeast
  "New York" = "Northeast",
  "New Jersey" = "Northeast",
  "Rhode Island" = "Northeast",
  "Connecticut" = "Northeast",
  "Maryland" = "Northeast",
  "Delaware" = "Northeast",
  "District of Columbia" = "Northeast",
  
  # New England
  "Maine" = "NEngland",
  "New Hampshire" = "NEngland",
  "Vermont" = "NEngland",
  "Massachusetts" = "NEngland"
)
poll_data <- read_csv("data/cleaned_poll_data.csv")

View(poll_data)

#TODO add lm model, response: pct, predictors: everything else

#Then put the polls through the lm to recover the adjusted average pct for each poll

#Do this for each state and each candidate

#Use this to calculate the number of electoral college votes both candidates
#will get

# TODO Create a baysian model
# TODO Make a training/testing split, ideally using CV

### Model data ####
harris_models <- list()
trump_models <- list()
region_results <- data.frame()

pol_regions <- unique(poll_data$pol_region)

for (region in pol_regions) {
  region_data <- poll_data %>%
    filter(pol_region == region)
  
  harris_region_data <- region_data %>%
    filter(candidate == "Harris")
  
  harris_model <- lm(
    support ~ sample_size + days_to_election + transparency_score + pollscore,
    data = harris_region_data,
    weights = weight
  )
  # Save the model
  harris_models[[as.character(region)]] <- harris_model
  
  
  trump_region_data <- region_data  %>%
    filter(candidate == "Trump")
  
  trump_model <- lm(
    support ~ sample_size + days_to_election + transparency_score + pollscore,
    data = trump_region_data,
    weights = weight
  )
  # Save the model
  trump_models[[as.character(region)]] <- trump_model
}

states <- unique(poll_data$state)

# Initialize a data frame to store state-level results
state_results <- data.frame()


for (state_name in states) {
  
  # Get the region of the state from the mapping
  state_region <- region_mapping[as.character(state_name)]
  
  region <- state_region

  # Filter data for the current state
  state_data <- poll_data %>%
    filter(state == state_name)
  
  # Initialize variables to store pooled support
  harris_support <- data.frame()
  trump_support <- data.frame()
  
  harris_model <- harris_models[[as.character(region)]]
  
  harris_state_data <- state_data %>%
    filter(candidate == "Harris")
  
  harris_state_data$adjusted_support <- predict(harris_model, newdata = harris_state_data)
  
  # Pool the adjusted support
  harris_support <- harris_state_data %>%
    summarize(
      candidate = "Harris",
      pooled_support = weighted.mean(adjusted_support, w = sample_size, na.rm = TRUE)
    ) %>%
    mutate(state = state_name)
  
  trump_model <- trump_models[[as.character(region)]]
  
  trump_state_data <- state_data %>%
    filter(candidate == "Trump")
  
  # Predict adjusted support
  trump_state_data$adjusted_support <- predict(trump_model, newdata = trump_state_data)
  
  # Pool the adjusted support
  trump_support <- trump_state_data %>%
    summarize(
      candidate = "Trump",
      pooled_support = weighted.mean(adjusted_support, w = sample_size, na.rm = TRUE)
    ) %>%
    mutate(state = state_name)
  
  # Combine results if both candidates have data
  if (nrow(harris_support) > 0 && nrow(trump_support) > 0) {
    state_support <- rbind(harris_support, trump_support)
    state_results <- rbind(state_results, state_support)
  } else if (nrow(harris_support) > 0) {
    state_results <- rbind(state_results, harris_support)
  } else if (nrow(trump_support) > 0) {
    state_results <- rbind(state_results, trump_support)
  }
  
}


#TODO Missing states for some reason, need to check why
View(state_results)


electoral_votes <- data.frame(
  state = c(
    "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
    "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia",
    "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
    "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
    "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
    "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
    "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
    "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
    "Washington", "West Virginia", "Wisconsin", "Wyoming"
  ),
  ev = c(
    9, 3, 11, 6, 54, 10,
    7, 3, 3, 30, 16,
    4, 4, 19, 11, 6, 6, 8,
    8, 4, 10, 11, 15, 10,
    6, 10, 4, 5, 6, 4,
    14, 5, 28, 16, 3,
    17, 7, 8, 19, 4, 9,
    3, 11, 40, 6, 3, 13,
    12, 4, 10, 3
  )
)

# Merge state_results with electoral votes
state_results_ev <- merge(state_results, electoral_votes, by = "state")

# Reshape the data for easier analysis
state_results_wide <- state_results_ev %>%
  select(state, candidate, pooled_support, ev) %>%
  pivot_wider(names_from = candidate, values_from = pooled_support)

# Determine the winner in each state
state_results_wide <- state_results_wide %>%
  mutate(
    winner = ifelse(`Harris` > `Trump`, "Harris", "Trump")
  )

# Calculate total electoral votes for Harris
harris_ev <- state_results_wide %>%
  filter(winner == "Harris") %>%
  summarize(total_ev = sum(ev, na.rm = TRUE))

# Calculate total electoral votes for Trump
trump_ev <- state_results_wide %>%
  filter(winner == "Trump") %>%
  summarize(total_ev = sum(ev, na.rm = TRUE))

# Display the results
print(harris_ev)
print(trump_ev)





#### Save model ####
saveRDS(
  first_model,
  file = "models/first_model.rds"
)


