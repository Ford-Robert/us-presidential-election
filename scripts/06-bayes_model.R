#### Preamble ####
# Purpose: Models regional support and calculates the Electoral Votes (EV) each candidate is expected to get,
#          using available polling data and historical averages where data is missing.
# Author:  Robert Ford, Michelle Ji, Cher Ning
# Modified by: [Your Name]
# Date: [Current Date]
# Contact: [Your Contact Information]
# License: MIT

# -------------------------------------------------------------------------

#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(brms)
library(usmap)      # For US maps
library(ggplot2)


#### Read data ####
poll_data <- read_csv("data/cleaned_poll_data.csv")
historical_state_data <- read_csv("data/cleaned_historical_data.csv")
ev_votes <- read_csv("data/electoral_votes.csv")

# Clean poll_data by removing rows with NAs in specified predictors
poll_data <- poll_data %>%
  drop_na(days_to_election, sample_size, transparency_score, pollscore, state)

# Filter historical data from 2000 onwards
historical_recent <- historical_state_data %>%
  filter(year >= 2000)

# Calculate average support for each party in each state
state_stats <- historical_recent %>%
  group_by(state) %>%
  summarize(
    avg_democrat = mean(democrat, na.rm = TRUE),
    avg_republican = mean(republican, na.rm = TRUE)
  )

# Separate polling data for Trump and Harris
poll_data_trump <- poll_data %>%
  filter(candidate == "Trump") %>%
  mutate(state = as.character(state))  # Ensure 'state' is character

poll_data_harris <- poll_data %>%
  filter(candidate == "Harris") %>%
  mutate(state = as.character(state))  # Ensure 'state' is character

#### Define State Lists ####

# All US states (assuming `ev_votes` contains all states)
all_states <- unique(ev_votes$state)

# States with polling data for either Trump or Harris
states_with_poll_trump <- unique(poll_data_trump$state)
states_with_poll_harris <- unique(poll_data_harris$state)
states_with_poll <- union(states_with_poll_trump, states_with_poll_harris)

# States with no polling data
states_without_poll <- setdiff(all_states, states_with_poll)
print(states_without_poll)

#### Merge state_stats with Electoral Votes ####

# Merge state_stats with ev_votes using left_join to retain all states
state_evs <- ev_votes %>%
  left_join(state_stats, by = "state")

# Verify the merged dataframe
print(head(state_evs))

#### Define Model Formula ####
fixed_formula <- support ~ days_to_election + sample_size + transparency_score + pollscore + state

#### Build Bayesian Models for Trump and Harris ####

# Function to build a general model for a candidate using a subset of data
build_general_model <- function(candidate_name) {
  # Select appropriate polling data
  if (candidate_name == "Trump") {
    data_candidate <- poll_data_trump
  } else if (candidate_name == "Harris") {
    data_candidate <- poll_data_harris
  } else {
    stop("Candidate not recognized")
  }
  
  # Check if there's sufficient data to build a model
  if (nrow(data_candidate) < 10) {  # Assuming 10 data points as minimum
    message(paste("Insufficient polling data for", candidate_name, ". Using historical average."))
    return(NULL)
  }
  
  # Convert 'state' to factor with levels present in data
  data_candidate$state <- factor(data_candidate$state)
  
  # Fit the Bayesian model
  model <- stan_glm(
    fixed_formula,
    data = data_candidate,
    family = gaussian(),
    prior = normal(0, 10),
    prior_intercept = normal(0, 10),  # Weak prior for intercept
    chains = 4,
    iter = 2000,
    seed = 123
  )
  
  return(model)
}

# Build models for Trump and Harris
model_trump <- build_general_model("Trump")
model_harris <- build_general_model("Harris")

# Check if models are built
if (is.null(model_trump) & is.null(model_harris)) {
  stop("Neither model could be built. Check your polling data.")
} else if (is.null(model_trump)) {
  stop("Trump model could not be built. Check your polling data.")
} else if (is.null(model_harris)) {
  stop("Harris model could not be built. Check your polling data.")
}

#### Generating Predictions ####

# Plot posterior predictive checks
pp_check(model_trump)
pp_check(model_harris)

# Summarize the models
summary(model_trump)
summary(model_harris)

#### Step 1: Group Polling Data and Calculate Means ####

# Function to calculate mean predictors for each candidate and state
calculate_mean_predictors <- function(poll_data_candidate) {
  mean_predictors <- poll_data_candidate %>%
    group_by(state) %>%
    summarize(
      mean_days_to_election = mean(days_to_election, na.rm = TRUE),
      mean_sample_size = mean(sample_size, na.rm = TRUE),
      mean_transparency_score = mean(transparency_score, na.rm = TRUE),
      mean_pollscore = mean(pollscore, na.rm = TRUE)
    )
  return(mean_predictors)
}

# Calculate mean predictors for Trump and Harris
mean_predictors_trump <- calculate_mean_predictors(poll_data_trump)
mean_predictors_harris <- calculate_mean_predictors(poll_data_harris)

#### Step 2: Generate Newdata for Posterior Predictions ####

# Function to generate newdata with one row per state
generate_newdata_one_row <- function(mean_predictors) {
  newdata <- mean_predictors %>%
    mutate(
      days_to_election = mean_days_to_election,
      sample_size = mean_sample_size,
      transparency_score = mean_transparency_score,
      pollscore = mean_pollscore
    ) %>%
    select(days_to_election, sample_size, transparency_score, pollscore, state)
  
  return(newdata)
}

# Generate newdata for Trump and Harris with one row per state
newdata_trump <- generate_newdata_one_row(mean_predictors_trump)
newdata_harris <- generate_newdata_one_row(mean_predictors_harris)

#### Step 3: Draw Posterior Predictions ####

# Generate posterior predictions for Trump
posterior_trump <- posterior_predict(model_trump, newdata = newdata_trump, draws = 1000)

# Generate posterior predictions for Harris
posterior_harris <- posterior_predict(model_harris, newdata = newdata_harris, draws = 1000)

# Assign state names to columns
colnames(posterior_trump) <- newdata_trump$state
colnames(posterior_harris) <- newdata_harris$state

# Verify dimensions and column names
dim(posterior_trump)  # Should be 1000 simulations x number of states with polling data
dim(posterior_harris) # Should be 1000 simulations x number of states with polling data
print(colnames(posterior_trump))
print(colnames(posterior_harris))

#### Step 4: Prepare Predicted Support for All States ####

# Initialize matrices to store predicted support
num_simulations <- 1000
trump_support_matrix <- matrix(NA_real_, nrow = num_simulations, ncol = length(all_states))
harris_support_matrix <- matrix(NA_real_, nrow = num_simulations, ncol = length(all_states))
colnames(trump_support_matrix) <- all_states
colnames(harris_support_matrix) <- all_states

# Assign predictions for states with polling data
# Loop through each state with polling data and assign predictions
for (state in states_with_poll) {
  # For Trump
  if (state %in% colnames(posterior_trump)) {
    trump_support_matrix[, state] <- posterior_trump[, state]
  }
  
  # For Harris
  if (state %in% colnames(posterior_harris)) {
    harris_support_matrix[, state] <- posterior_harris[, state]
  }
}

# Assign historical averages for states without polling data
for (state in states_without_poll) {
  # Get historical averages
  avg_democrat <- state_evs$avg_democrat[state_evs$state == state]
  avg_republican <- state_evs$avg_republican[state_evs$state == state]
  
  # Assign the historical average support to all simulations
  trump_support_matrix[, state] <- avg_republican
  harris_support_matrix[, state] <- avg_democrat
}

#### Step 5: Simulate 1000 Elections ####

# Initialize vectors to store EV counts for each simulation
trump_ev_counts <- rep(0, num_simulations)
harris_ev_counts <- rep(0, num_simulations)

# Loop through each state to allocate EVs based on support
for (state in all_states) {
  ev <- state_evs$ev[state_evs$state == state]
  
  # Retrieve support vectors for the state across all simulations
  trump_support <- trump_support_matrix[, state]
  harris_support <- harris_support_matrix[, state]
  
  # Allocate EVs based on which candidate has higher support
  trump_wins_state <- trump_support > harris_support
  harris_wins_state <- harris_support > trump_support
  # In case of a tie, you could randomly assign the EV or split it
  # Here, we ignore ties or could implement a specific rule
  # For simplicity, ties result in no EV allocated
  
  # Update EV counts
  trump_ev_counts <- trump_ev_counts + ev * trump_wins_state
  harris_ev_counts <- harris_ev_counts + ev * harris_wins_state
}

#### Step 6: Summarize Simulation Results ####

# Calculate the number of wins for each candidate
trump_wins <- sum(trump_ev_counts > harris_ev_counts)
harris_wins <- sum(harris_ev_counts > trump_ev_counts)
tie <- sum(trump_ev_counts == harris_ev_counts)

# Calculate the percentage chance of victory
trump_win_percent <- (trump_wins / num_simulations) * 100
harris_win_percent <- (harris_wins / num_simulations) * 100
tie_percent <- (tie / num_simulations) * 100

# Print results
cat("Simulation Results out of", num_simulations, "Elections:\n")
cat("Trump wins in", trump_wins, "simulations (", round(trump_win_percent, 2), "%).\n", sep = " ")
cat("Harris wins in", harris_wins, "simulations (", round(harris_win_percent, 2), "%).\n", sep = " ")
cat("Tied elections in", tie, "simulations (", round(tie_percent, 2), "%).\n", sep = " ")

# Optionally, calculate the average Electoral Votes for each candidate
avg_trump_ev <- mean(trump_ev_counts)
avg_harris_ev <- mean(harris_ev_counts)

cat("Average Electoral Votes for Trump:", round(avg_trump_ev, 2), "\n")
cat("Average Electoral Votes for Harris:", round(avg_harris_ev, 2), "\n")

#### Save the Models ####
saveRDS(model_harris, file = "models/bayes_model_harris.rds")
saveRDS(model_trump, file = "models/bayes_model_trump.rds")

### Model Diagnostics ###

# Model Convergence

# Check convergence for Trump model
print(rhat(model_trump))  # Should be ~1

# Check convergence for Harris model
print(rhat(model_harris))  # Should be ~1

# Plot trace plots for Trump
plot(model_trump, plotfun = "trace")

# Plot trace plots for Harris
plot(model_harris, plotfun = "trace")

# PPC Checks

# Enhanced PPC with different types
library(bayesplot)

# Trump PPC
ppc_dens_overlay(y = poll_data_trump$support, 
                 yrep = posterior_predict(model_trump)) +
  ggtitle("Posterior Predictive Density Overlay - Trump")

# Harris PPC
ppc_dens_overlay(y = poll_data_harris$support, 
                 yrep = posterior_predict(model_harris)) +
  ggtitle("Posterior Predictive Density Overlay - Harris")

# PPC for residuals
pp_check(model_trump, type = "residuals")
pp_check(model_harris, type = "residuals")

# Residual Analysis

# Function to plot residuals
plot_residuals <- function(model, candidate_name) {
  residuals <- residuals(model)
  fitted <- fitted(model)
  
  ggplot(data = data.frame(fitted, residuals), aes(x = fitted, y = residuals)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = paste("Residuals vs Fitted -", candidate_name),
         x = "Fitted Values",
         y = "Residuals") +
    theme_minimal()
}

# Plot residuals for Trump
plot_residuals(model_trump, "Trump")

# Plot residuals for Harris
plot_residuals(model_harris, "Harris")



# Graphing

# Initialize vectors to store probabilities
prob_trump_victory <- numeric(length(days_range))
prob_harris_victory <- numeric(length(days_range))



#### Graph 2: State-wise Victory Map ####

# Determine the winner in each simulation for each state
# We'll use the support matrices generated earlier

# Initialize a data frame to store the number of wins per state
state_win_counts <- tibble(state = all_states, 
                           trump_wins = 0,
                           harris_wins = 0)

# Loop through each state and count wins
for (state in all_states) {
  ev <- state_evs$ev[state_evs$state == state]
  
  # Retrieve support vectors
  trump_support <- trump_support_matrix[, state]
  harris_support <- harris_support_matrix[, state]
  
  # Count wins
  trump_win_count <- sum(trump_support > harris_support)
  harris_win_count <- sum(harris_support > trump_support)
  
  # Update counts
  state_win_counts <- state_win_counts %>%
    mutate(
      trump_wins = if_else(state == !!state, trump_win_count, trump_wins),
      harris_wins = if_else(state == !!state, harris_win_count, harris_wins)
    )
}

# Determine the winning candidate per state
state_win_counts <- state_win_counts %>%
  mutate(
    winner = case_when(
      trump_wins > harris_wins ~ "Trump",
      harris_wins > trump_wins ~ "Harris",
      TRUE ~ "Tie"
    )
  )

# Prepare data for mapping
# Ensure state names match the mapping package's expectations
state_win_counts <- state_win_counts %>%
  mutate(state = tolower(state))  # usmap uses lowercase state names

#Put this in an appendix
View(state_win_counts)
# Get US map data
state_win_counts <- state_win_counts %>%
  mutate(state = str_to_title(state))

us_map <- us_map(regions = "states")

#View(us_map)
#View(state_win_counts)
# Merge map data with state win counts
map_data_combined <- us_map %>%
  left_join(state_win_counts, by = c("full" = "state"))

map_data_combined <- map_data_combined |>
  rename("state" = "full")

# Define colors for candidates
candidate_colors <- c("Trump" = "#FF0000",   # Red
                      "Harris" = "#0000FF",  # Blue
                      "Tie" = "#808080")      # Gray

# Plot the map
election_map <- plot_usmap(data = map_data_combined, regions = "states", values = "winner") +
  scale_fill_manual(
    values = candidate_colors,
    name = "Winning Candidate",
    na.value = "white"
  ) +
  labs(
    title = "State Election Predictions",
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

election_map

ggsave(
  filename = "election_map.png",        # File name
  plot = election_map,                  # Plot object
  path = "other/plots",               # Directory to save (create if it doesn't exist)
  width = 10,                            # Width in inches
  height = 6,                            # Height in inches
  dpi = 300                              # Resolution
)

# Chance of victory over time

# Define the maximum number of days before the election
max_days <- 105

# Define the interval size
interval_size <- 10

# Create break points for cumulative groups
break_points <- seq(interval_size, max_days, by = interval_size)

# Ensure the last group includes all remaining days
if (max_days %% interval_size != 0) {
  break_points <- c(break_points, max_days)
}

# Assign each poll to a cumulative group based on days_to_election
poll_data <- poll_data %>%
  mutate(
    group = cut(
      days_to_election,
      breaks = c(-Inf, break_points),
      labels = break_points,
      right = FALSE
    )
  ) %>%
  filter(!is.na(group)) %>%  # Remove any polls beyond the defined groups
  mutate(
    group = as.numeric(as.character(group))
  )


# Function to calculate mean predictors for a candidate within a group
calculate_group_means <- function(poll_data_candidate) {
  poll_data_candidate %>%
    group_by(group) %>%
    summarize(
      mean_days_to_election = mean(days_to_election, na.rm = TRUE),
      mean_sample_size = mean(sample_size, na.rm = TRUE),
      mean_transparency_score = mean(transparency_score, na.rm = TRUE),
      mean_pollscore = mean(pollscore, na.rm = TRUE)
    )
}

# Separate polling data for Trump and Harris if not already separated
poll_data_trump <- poll_data %>%
  filter(candidate == "Trump")

poll_data_harris <- poll_data %>%
  filter(candidate == "Harris")

# Calculate mean predictors for each group
group_means_trump <- calculate_group_means(poll_data_trump)
group_means_harris <- calculate_group_means(poll_data_harris)


# Define a function to generate predictions for a candidate given group means
generate_posterior_predictions <- function(model, group_means) {
  if (is.null(model)) {
    # If the model is not available, use historical averages
    if (model == model_trump) {
      return(rep(state_evs$avg_republican, each = 1000))
    } else if (model == model_harris) {
      return(rep(state_evs$avg_democrat, each = 1000))
    }
  } else {
    # Generate posterior predictions
    posterior_predict(model, newdata = group_means, draws = 1000)
  }
}

# Generate predictions for Trump
posterior_trump_groups <- generate_posterior_predictions(model_trump, group_means_trump)

# Generate predictions for Harris
posterior_harris_groups <- generate_posterior_predictions(model_harris, group_means_harris)


#### Function to Simulate Elections for a Group ####

simulate_elections <- function(posterior_trump, posterior_harris, state_evs, all_states, num_simulations = 1000) {
  
  # Initialize EV counts
  trump_ev_counts <- rep(0, num_simulations)
  harris_ev_counts <- rep(0, num_simulations)
  
  # Loop through each state to allocate EVs
  for (state in all_states) {
    ev <- state_evs$ev[state_evs$state == state]
    
    # Retrieve support vectors
    trump_support <- posterior_trump[, state]
    harris_support <- posterior_harris[, state]
    
    # Determine which candidate wins the state in each simulation
    trump_wins_state <- trump_support > harris_support
    harris_wins_state <- harris_support > trump_support
    
    # Update EV counts
    trump_ev_counts <- trump_ev_counts + ev * trump_wins_state
    harris_ev_counts <- harris_ev_counts + ev * harris_wins_state
  }
  
  # Determine overall election winners for each simulation
  trump_wins_election <- trump_ev_counts > harris_ev_counts
  harris_wins_election <- harris_ev_counts > trump_ev_counts
  ties <- trump_ev_counts == harris_ev_counts
  
  # Calculate probabilities
  prob_trump_victory <- mean(trump_wins_election) * 100  # Percentage
  prob_harris_victory <- mean(harris_wins_election) * 100
  prob_tie <- mean(ties) * 100
  
  return(list(
    trump = prob_trump_victory,
    harris = prob_harris_victory,
    tie = prob_tie
  ))
}

#### Apply Simulation to Each Group ####

# Initialize a dataframe to store probabilities for each group
prob_victory_over_time <- tibble(
  group = unique(group_means_trump$group)
) %>%
  arrange(group) %>%
  mutate(
    trump_win_percent = NA_real_,
    harris_win_percent = NA_real_,
    tie_percent = NA_real_
  )

# Loop through each group and simulate elections
for (i in seq_along(prob_victory_over_time$group)) {
  current_group <- prob_victory_over_time$group[i]
  
  # Extract posterior predictions for the current group
  # Assuming that `posterior_trump_groups` and `posterior_harris_groups` are matrices
  # with columns named after states
  # Here, we need to ensure that `posterior_trump_groups` and `posterior_harris_groups` 
  # are lists or similar structures where each element corresponds to a group
  
  # Extract the predictions for the current group
  # Assuming that `posterior_trump_groups` and `posterior_harris_groups` are lists
  # where each list element is a matrix of 1000 x number_of_states
  current_posterior_trump <- posterior_trump_groups[[i]]
  current_posterior_harris <- posterior_harris_groups[[i]]
  
  # Simulate elections for the current group
  simulation_result <- simulate_elections(
    posterior_trump = current_posterior_trump,
    posterior_harris = current_posterior_harris,
    state_evs = state_evs,
    all_states = all_states,
    num_simulations = 1000
  )
  
  # Store the results
  prob_victory_over_time$trump_win_percent[i] <- simulation_result$trump
  prob_victory_over_time$harris_win_percent[i] <- simulation_result$harris
  prob_victory_over_time$tie_percent[i] <- simulation_result$tie
}


#### Prepare Data for Plotting ####

# Create a date label based on the group's days_before_election
# Since groups are cumulative, the oldest in the group defines the label
prob_victory_over_time <- prob_victory_over_time %>%
  mutate(
    days_before_election = group  # The 'group' represents the days_before_election of the oldest poll in the group
  ) %>%
  select(days_before_election, trump_win_percent, harris_win_percent, tie_percent) %>%
  pivot_longer(
    cols = c("trump_win_percent", "harris_win_percent"),
    names_to = "Candidate",
    values_to = "Probability"
  ) %>%
  mutate(
    Candidate = recode(Candidate,
                       "trump_win_percent" = "Trump",
                       "harris_win_percent" = "Harris")
  )

#### Plotting ####

ggplot(prob_victory_over_time, aes(x = days_before_election, y = Probability, color = Candidate)) +
  geom_line(size = 1.2) +
  scale_x_reverse() +  # So that days_before_election decreases to the right
  scale_y_continuous(limits = c(0, 100), labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Probability of Victory Over Time",
    subtitle = "As Polls Are Aggregated Up to 105 Days Before Election",
    x = "Days Before Election",
    y = "Probability of Victory (%)",
    color = "Candidate"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )





