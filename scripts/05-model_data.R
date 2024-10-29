#### Preamble ####
# Purpose: Models the regional support, then calculates the EV each canditate is
#          expected to get
# Author:  Robert Ford, Michelle Ji, Cher Ning
# Date: 14 October 2024
# Contact: robert.ford@mail.utoronto.ca, michelle.ji@mail.utoronto.ca, cher.ning@mail.utoronto.ca
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
View(states)


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
harris_ev_lm <- state_results_wide %>%
  filter(winner == "Harris") %>%
  summarize(total_ev = sum(ev, na.rm = TRUE))

# Calculate total electoral votes for Trump
trump_ev_lm <- state_results_wide %>%
  filter(winner == "Trump") %>%
  summarize(total_ev = sum(ev, na.rm = TRUE))

# Display the results
print(harris_ev_lm)
print(trump_ev_lm)







# Bayes Model
#### Define State Priors ####
# Define state categories based on political leaning
state_priors <- data.frame(
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
  category = c(
    "Deep Red", "Deep Red", "Swing", "Deep Red", "Deep Blue", "Light Blue",
    "Deep Blue", "Deep Blue", "Deep Blue", "Swing", "Light Red",
    "Deep Blue", "Deep Red", "Deep Blue", "Light Red", "Swing", "Deep Red", "Deep Red",
    "Deep Red", "Light Blue", "Deep Blue", "Deep Blue", "Swing", "Light Blue",
    "Deep Red", "Light Red", "Light Red", "Deep Red", "Light Blue", "Light Blue",
    "Deep Blue", "Light Blue", "Deep Blue", "Swing", "Deep Red",
    "Swing", "Deep Red", "Light Blue", "Swing", "Deep Blue", "Deep Red",
    "Deep Red", "Deep Red", "Light Red", "Deep Red", "Deep Blue", "Light Blue",
    "Light Blue", "Deep Red", "Swing", "Deep Red"
  )
)

# Assign prior means and standard deviations based on categories
state_priors <- state_priors %>%
  mutate(
    prior_mean = case_when(
      category == "Deep Blue" ~ 70,
      category == "Light Blue" ~ 60,
      category == "Swing" ~ 50,
      category == "Light Red" ~ 40,
      category == "Deep Red" ~ 30
    ),
    prior_sd = case_when(
      category == "Deep Blue" ~ 5,
      category == "Light Blue" ~ 10,
      category == "Swing" ~ 15,
      category == "Light Red" ~ 10,
      category == "Deep Red" ~ 5
    )
  )

#### Merge Priors with Poll Data ####
# Merge state priors into poll data
poll_data <- poll_data %>%
  left_join(state_priors, by = "state")

# Create candidate-specific prior mean
poll_data <- poll_data %>%
  mutate(
    candidate_prior_mean = ifelse(candidate == "Harris", prior_mean, 100 - prior_mean)
  )

#### Prepare Data for Modeling ####
# Ensure that all necessary variables are appropriately formatted
poll_data <- poll_data %>%
  mutate(
    state = factor(state),
    candidate = factor(candidate),
    pollster = factor(pollster)
  )


#### Fit Bayesian Models for Each Candidate ####
# Filter data for Harris and Trump
poll_data_harris <- poll_data %>% filter(candidate == "Harris")
poll_data_trump <- poll_data %>% filter(candidate == "Trump")

# Fit the model for Harris
model_harris <- stan_glmer(
  formula = support ~ sample_size + days_to_election + transparency_score + pollscore + candidate_prior_mean + (1 | state),
  data = poll_data_harris,
  family = gaussian(),
  prior = normal(0, 5, autoscale = FALSE),
  prior_intercept = normal(50, 10, autoscale = FALSE),
  prior_aux = exponential(1, autoscale = FALSE),
  seed = 123,
  chains = 4,
  iter = 2000,
  adapt_delta = 0.95
)

# Fit the model for Trump
model_trump <- stan_glmer(
  formula = support ~ sample_size + days_to_election + transparency_score + pollscore + candidate_prior_mean + (1 | state),
  data = poll_data_trump,
  family = gaussian(),
  prior = normal(0, 5, autoscale = FALSE),
  prior_intercept = normal(50, 10, autoscale = FALSE),
  prior_aux = exponential(1, autoscale = FALSE),
  seed = 123,
  chains = 4,
  iter = 2000,
  adapt_delta = 0.95
)

#### Generate State-Level Predictions ####
# Create prediction data for Harris
state_list <- levels(poll_data$state)
prediction_data_harris <- data.frame(
  state = state_list,
  sample_size = mean(poll_data_harris$sample_size, na.rm = TRUE),
  days_to_election = mean(poll_data_harris$days_to_election, na.rm = TRUE),
  transparency_score = mean(poll_data_harris$transparency_score, na.rm = TRUE),
  pollscore = mean(poll_data_harris$pollscore, na.rm = TRUE),
  candidate_prior_mean = state_priors$prior_mean[match(state_list, state_priors$state)]
)

# Ensure 'state' is a factor with levels matching the model
prediction_data_harris$state <- factor(prediction_data_harris$state, levels = levels(poll_data_harris$state))

# Generate predictions for Harris
harris_predictions <- posterior_epred(
  model_harris,
  newdata = prediction_data_harris,
  re.form = NULL
)

# Calculate mean predicted support for Harris
harris_mean_support <- data.frame(
  state = prediction_data_harris$state,
  Harris = apply(harris_predictions, 2, mean)
)

# Repeat the same steps for Trump
prediction_data_trump <- data.frame(
  state = state_list,
  sample_size = mean(poll_data_trump$sample_size, na.rm = TRUE),
  days_to_election = mean(poll_data_trump$days_to_election, na.rm = TRUE),
  transparency_score = mean(poll_data_trump$transparency_score, na.rm = TRUE),
  pollscore = mean(poll_data_trump$pollscore, na.rm = TRUE),
  candidate_prior_mean = 100 - state_priors$prior_mean[match(state_list, state_priors$state)]
)

prediction_data_trump$state <- factor(prediction_data_trump$state, levels = levels(poll_data_trump$state))

# Generate predictions for Trump
trump_predictions <- posterior_epred(
  model_trump,
  newdata = prediction_data_trump,
  re.form = NULL
)

# Calculate mean predicted support for Trump
trump_mean_support <- data.frame(
  state = prediction_data_trump$state,
  Trump = apply(trump_predictions, 2, mean)
)

#### Combine Predictions and Determine Winners ####
# Combine predictions
state_results <- merge(harris_mean_support, trump_mean_support, by = "state")

# Determine the winner
state_results <- state_results %>%
  mutate(
    winner = ifelse(Harris > Trump, "Harris", "Trump")
  )

#### Allocate Electoral Votes ####
# Ensure 'state' in electoral_votes is a factor and matches levels
electoral_votes$state <- factor(electoral_votes$state, levels = levels(state_results$state))

# Merge with electoral votes
state_results_ev <- merge(state_results, electoral_votes, by = "state")

# Calculate total electoral votes for Harris
harris_ev <- state_results_ev %>%
  filter(winner == "Harris") %>%
  summarize(total_ev = sum(ev, na.rm = TRUE))

# Calculate total electoral votes for Trump
trump_ev <- state_results_ev %>%
  filter(winner == "Trump") %>%
  summarize(total_ev = sum(ev, na.rm = TRUE))

# Display the results
print("Total Electoral Votes for Harris:")
print(harris_ev)

print("Total Electoral Votes for Trump:")
print(trump_ev)

#### Optional: Visualize the Results ####
# Prepare data for plotting
state_results_long <- state_results_ev %>%
  select(state, Harris, Trump) %>%
  pivot_longer(cols = c("Harris", "Trump"), names_to = "candidate", values_to = "predicted_support")

# Plot predicted support
ggplot(state_results_long, aes(x = reorder(state, -predicted_support), y = predicted_support, fill = candidate)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Predicted Support by State", x = "State", y = "Predicted Support (%)") +
  theme_minimal()

#### Save the Models (Optional) ####
# Save the models
saveRDS(model_harris, file = "models/model_harris_stan_glmer.rds")
saveRDS(model_trump, file = "models/model_trump_stan_glmer.rds")
