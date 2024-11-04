#### Preamble ####
# Purpose: Models regional support and calculates the Electoral Votes (EV) each candidate is expected to get,
#          using available polling data and historical averages where data is missing.
# Author:  Robert Ford, Michelle Ji, Cher Ning
# Date: 
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

set.seed(304)

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

# Replace NA in the 'state' column with "District of Columbia"
state_stats$state[is.na(state_stats$state)] <- "District of Columbia"

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

#### Merge state_stats with Electoral Votes ####

# Merge state_stats with ev_votes using left_join to retain all states
state_evs <- ev_votes %>%
  left_join(state_stats, by = "state")

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


# Define the seven swing states
swing_states <- c("Arizona", "Georgia", "Michigan", "Nevada", "North Carolina", "Pennsylvania", "Wisconsin")

# Initialize a data frame to store the probabilities
swing_probabilities <- data.frame(
  state = swing_states,
  Trump = NA_real_,
  Harris = NA_real_,
  stringsAsFactors = FALSE
)

# Loop through each swing state to calculate win probabilities
for (state in swing_states) {
  if (state %in% all_states) {
    # Retrieve the support vectors for the current state across all simulations
    trump_support <- trump_support_matrix[, state]
    harris_support <- harris_support_matrix[, state]
    
    # Calculate the number of simulations where Trump wins the state
    trump_wins <- sum(trump_support > harris_support, na.rm = TRUE)
    
    # Calculate the number of simulations where Harris wins the state
    harris_wins <- sum(harris_support > trump_support, na.rm = TRUE)
    
    # Calculate the probabilities as percentages
    trump_prob <- (trump_wins / num_simulations) * 100
    harris_prob <- (harris_wins / num_simulations) * 100
    
    # Assign the probabilities to the data frame
    swing_probabilities[swing_probabilities$state == state, "Trump"] <- trump_prob
    swing_probabilities[swing_probabilities$state == state, "Harris"] <- harris_prob
  } else {
    # If the state is not present in the data, assign NA
    swing_probabilities[swing_probabilities$state == state, "Trump"] <- NA
    swing_probabilities[swing_probabilities$state == state, "Harris"] <- NA
    warning(paste("State", state, "is not present in the dataset."))
  }
}

# Optionally, format the probabilities to two decimal places
swing_probabilities <- swing_probabilities %>%
  mutate(
    Trump = round(Trump, 2),
    Harris = round(Harris, 2)
  )

# Print the swing states probability table
print(swing_probabilities)

# Optionally, save the table to a CSV file for further analysis or reporting
write_csv(swing_probabilities, "other/plots/swing_states_probabilities.csv")

#### Save the Models ####
saveRDS(model_harris, file = "models/bayes_model_harris.rds")
saveRDS(model_trump, file = "models/bayes_model_trump.rds")

### Model Diagnostics ###


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
trump_ppc <- pp_check(model_trump, type = "residuals")
harris_ppc <- pp_check(model_harris, type = "residuals")


ggsave(
  filename = "trump_ppc.pdf",        # File name
  plot = trump_ppc,                  # Plot object
  path = "other/plots",               # Directory to save (create if it doesn't exist)
  width = 10,                            # Width in inches
  height = 6,                            # Height in inches
  dpi = 300                              # Resolution
)

ggsave(
  filename = "harris_ppc.pdf",        # File name
  plot = harris_ppc,                  # Plot object
  path = "other/plots",               # Directory to save (create if it doesn't exist)
  width = 10,                            # Width in inches
  height = 6,                            # Height in inches
  dpi = 300                              # Resolution
)


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
trump_residuals <- plot_residuals(model_trump, "Trump")

# Plot residuals for Harris
harris_residuals <- plot_residuals(model_harris, "Harris")


ggsave(
  filename = "trump_residuals.pdf",        # File name
  plot = trump_residuals,                  # Plot object
  path = "other/plots",               # Directory to save (create if it doesn't exist)
  width = 10,                            # Width in inches
  height = 6,                            # Height in inches
  dpi = 300                              # Resolution
)

ggsave(
  filename = "harris_residuals.pdf",        # File name
  plot = harris_residuals,                  # Plot object
  path = "other/plots",               # Directory to save (create if it doesn't exist)
  width = 10,                            # Width in inches
  height = 6,                            # Height in inches
  dpi = 300                              # Resolution
)

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

write_csv(state_win_counts, "other/plots/state_win_counts.csv")


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
candidate_colors <- c("Trump" = "#DD1717",   # Red
                      "Harris" = "#0F4392",  # Blue
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
  filename = "election_map.pdf",        # File name
  plot = election_map,                  # Plot object
  path = "other/plots",               # Directory to save (create if it doesn't exist)
  width = 10,                            # Width in inches
  height = 6,                            # Height in inches
  dpi = 300                              # Resolution
)

