#### Preamble ####
# Purpose: Models the regional support, then calculates the EV each candidate is
#          expected to get, using available data and priors where data is missing.
# Author:  Robert Ford, Michelle Ji, Cher Ning
# Date: 14 October 2024
# Contact: robert.ford@mail.utoronto.ca, michelle.ji@mail.utoronto.ca, cher.ning@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(tidyverse)
library(rstanarm)

#### Read data ####
poll_data <- read_csv("data/cleaned_poll_data.csv")

### Bayes Model ###

# Define State Priors #
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

#### Generate Predictions Using Available Data ####
# Get predicted support for Harris using available data
harris_predictions <- posterior_epred(model_harris)
harris_pred_means <- apply(harris_predictions, 2, mean)
poll_data_harris$predicted_support <- harris_pred_means

# Calculate mean predicted support for Harris by state
harris_state_support <- poll_data_harris %>%
  group_by(state) %>%
  summarize(Harris = mean(predicted_support))

# Get predicted support for Trump using available data
trump_predictions <- posterior_epred(model_trump)
trump_pred_means <- apply(trump_predictions, 2, mean)
poll_data_trump$predicted_support <- trump_pred_means

# Calculate mean predicted support for Trump by state
trump_state_support <- poll_data_trump %>%
  group_by(state) %>%
  summarize(Trump = mean(predicted_support))

#### Combine Predictions and Handle Missing States ####
# Create a list of all states
state_list <- state_priors$state

# Start with state priors
state_results <- state_priors %>%
  select(state, prior_mean) %>%
  mutate(prior_mean_trump = 100 - prior_mean)

# Merge predicted supports for Harris
state_results <- state_results %>%
  left_join(harris_state_support, by = "state") %>%
  mutate(
    Harris = ifelse(is.na(Harris), prior_mean, Harris)
  )

# Merge predicted supports for Trump
state_results <- state_results %>%
  left_join(trump_state_support, by = "state") %>%
  mutate(
    Trump = ifelse(is.na(Trump), prior_mean_trump, Trump)
  )

# Determine the winner in each state
state_results <- state_results %>%
  mutate(
    winner = ifelse(Harris > Trump, "Harris", "Trump")
  )

#### Allocate Electoral Votes ####
electoral_votes <- read_csv("data/electoral_votes.csv") 

# Merge electoral votes with state results
state_results_ev <- state_results %>%
  left_join(electoral_votes, by = "state")

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
