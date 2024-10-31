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
library(brms)
library(dplyr)
library(ggplot2)
library(gridExtra)
#### Read data ####
poll_data <- read_csv("data/cleaned_poll_data.csv")
historical_state_data <- read_csv("data/cleaned_historical_data.csv")
ev_votes <- read_csv("data/electoral_votes.csv")
### Bayes Model ###
#View(historical_state_data)
#View(poll_data)


# Filter data from 2000 onwards
historical_recent <- historical_state_data %>%
  filter(year >= 2000)

# Calculate average support for each party in each state
state_averages <- historical_recent %>%
  group_by(state) %>%
  summarize(
    avg_democrat = mean(democrat, na.rm = TRUE),
    avg_republican = mean(republican, na.rm = TRUE)
  )

# Filter poll data for Trump and Harris
poll_data_trump <- poll_data %>% filter(candidate == "Trump")
poll_data_harris <- poll_data %>% filter(candidate == "Harris")

# Define the model formula
formula <- support ~ days_to_election + sample_size + transparency_score + pollscore

# Fit the model for Trump
model_trump <- stan_glm(
  formula,
  data = poll_data_trump,
  family = gaussian(),
  prior = normal(0, 10),  # Weak prior
  prior_intercept = normal(0, 10),  # Weak prior for intercept
  chains = 4,
  iter = 2000,
  seed = 123
)

# Fit the model for Harris
model_harris <- stan_glm(
  formula,
  data = poll_data_harris,
  family = gaussian(),
  prior = normal(0, 10),
  prior_intercept = normal(0, 10),
  chains = 4,
  iter = 2000,
  seed = 123
)
#https://www.washingtonpost.com/elections/interactive/2024/2024-swing-states-trump-harris/
# Define your list of swing states
swing_states <- c("Pennsylvania", "Georgia", "North Carolina", "Michigan", "Arizona", "Wisconsin", "Nevada")
# Filter state averages for swing states
swing_state_averages <- state_averages %>%
  filter(state %in% swing_states)
# Function to build a model for a candidate in a state
build_swing_state_model <- function(candidate_name, state_name) {
  # Filter poll data for the candidate and state
  poll_data_candidate_state <- poll_data %>%
    filter(candidate == candidate_name, state == state_name)
  
  # Get historical average support for the candidate's party in the state
  if (candidate_name == "Trump") {
    prior_mean <- swing_state_averages %>%
      filter(state == state_name) %>%
      pull(avg_republican)
  } else if (candidate_name == "Harris") {
    prior_mean <- swing_state_averages %>%
      filter(state == state_name) %>%
      pull(avg_democrat)
  } else {
    stop("Candidate not recognized")
  }
  
  # Handle cases where historical averages are missing
  if (is.na(prior_mean)) {
    prior_mean <- 50  # Neutral prior if historical data is missing
    message(paste("Historical average not found for", candidate_name, "in", state_name, ". Using neutral prior of 50%."))
  }
  
  # Check if there is polling data; if not, rely on historical average
  if (nrow(poll_data_candidate_state) == 0) {
    message(paste("No polling data for", candidate_name, "in", state_name, ". Using historical average."))
    return(list(predicted_support = prior_mean))
  } else {
    # Build the Bayesian model with prior on the intercept set to historical average
    model <- stan_glm(
      formula,
      data = poll_data_candidate_state,
      family = gaussian(),
      prior = normal(0, 10),
      prior_intercept = normal(prior_mean, 5),  # Prior centered at historical average
      chains = 4,
      iter = 2000,
      seed = 123
    )
    return(model)
  }
}

# Build models for all swing states and both candidates
swing_state_models <- list()

for (state in swing_states) {
  # Model for Trump
  model_trump_state <- build_swing_state_model("Trump", state)
  swing_state_models[[paste("Trump", state, sep = "_")]] <- model_trump_state
  
  # Model for Harris
  model_harris_state <- build_swing_state_model("Harris", state)
  swing_state_models[[paste("Harris", state, sep = "_")]] <- model_harris_state
}
# Function to display model summaries or predicted support
display_model_result <- function(model_result, candidate_name, state_name) {
  if (is.list(model_result) && "stanreg" %in% class(model_result)) {
    cat("\nSummary of the model for", candidate_name, "in", state_name, ":\n")
    print(summary(model_result))
  } else {
    cat("\nPredicted support for", candidate_name, "in", state_name, "based on historical average:", model_result$predicted_support, "%\n")
  }
}

# Predicting Electoral Votes

# Assuming you have already loaded the libraries and data as in your original code.

# Create a data frame to hold predicted supports and electoral votes
predicted_supports <- ev_votes %>%
  mutate(
    trump_support = NA_real_,
    harris_support = NA_real_
  )

# Function to get predicted support for a candidate in a state
get_candidate_support <- function(candidate_name, state_name) {
  # Filter poll data for the candidate and state
  poll_data_candidate_state <- poll_data %>%
    filter(candidate == candidate_name, state == state_name)
  
  # If there is poll data
  if (nrow(poll_data_candidate_state) > 0) {
    # Use the model to predict support
    # Get the predictors
    predictors <- poll_data_candidate_state %>%
      select(sample_size, days_to_election, transparency_score, pollscore)
    
    # Use the general model
    if (candidate_name == "Trump") {
      model <- model_trump
    } else if (candidate_name == "Harris") {
      model <- model_harris
    } else {
      stop("Candidate not recognized")
    }
    
    # Get posterior predictions
    predictions <- posterior_predict(model, newdata = predictors)
    
    # Take the mean of the predictions
    predicted_support <- mean(predictions)
    
    return(predicted_support)
  } else {
    # No poll data, use historical average
    if (candidate_name == "Trump") {
      avg_support <- state_averages %>%
        filter(state == state_name) %>%
        pull(avg_republican)
    } else if (candidate_name == "Harris") {
      avg_support <- state_averages %>%
        filter(state == state_name) %>%
        pull(avg_democrat)
    } else {
      stop("Candidate not recognized")
    }
    # If historical average is missing, use 50
    if (length(avg_support) == 0 || is.na(avg_support)) {
      avg_support <- 50
    }
    return(avg_support)
  }
}

# Get predicted supports for all states using the general models
for (i in 1:nrow(predicted_supports)) {
  state_name <- predicted_supports$state[i]
  
  # For Trump
  predicted_supports$trump_support[i] <- get_candidate_support("Trump", state_name)
  
  # For Harris
  predicted_supports$harris_support[i] <- get_candidate_support("Harris", state_name)
}

# Overwrite swing states with predictions from swing state models
for (state_name in swing_states) {
  # For Trump
  model_name <- paste("Trump", state_name, sep = "_")
  model <- swing_state_models[[model_name]]
  
  if (is.list(model) && "stanreg" %in% class(model)) {
    # Get posterior predictions
    predictions <- posterior_predict(model)
    predicted_support <- mean(predictions)
    predicted_supports$trump_support[predicted_supports$state == state_name] <- predicted_support
  } else {
    # Use historical average
    predicted_supports$trump_support[predicted_supports$state == state_name] <- model$predicted_support
  }
  
  # For Harris
  model_name <- paste("Harris", state_name, sep = "_")
  model <- swing_state_models[[model_name]]
  
  if (is.list(model) && "stanreg" %in% class(model)) {
    # Get posterior predictions
    predictions <- posterior_predict(model)
    predicted_support <- mean(predictions)
    predicted_supports$harris_support[predicted_supports$state == state_name] <- predicted_support
  } else {
    # Use historical average
    predicted_supports$harris_support[predicted_supports$state == state_name] <- model$predicted_support
  }
}

# Determine the winner in each state
predicted_supports <- predicted_supports %>%
  mutate(
    winner = ifelse(trump_support > harris_support, "Trump", "Harris")
  )

# Sum the electoral votes for each candidate
ev_results <- predicted_supports %>%
  group_by(winner) %>%
  summarize(total_ev = sum(ev))

# Display the electoral vote results
print(ev_results)


#GRAPHING

# Combine the polling data for both candidates
poll_data_both <- poll_data %>% filter(candidate %in% c("Trump", "Harris"))

# Get mean values of predictors for general models
mean_sample_size <- mean(poll_data$sample_size, na.rm = TRUE)
mean_transparency_score <- mean(poll_data$transparency_score, na.rm = TRUE)
mean_pollscore <- mean(poll_data$pollscore, na.rm = TRUE)

# Create a sequence of days_to_election for plotting
days_seq <- seq(min(poll_data$days_to_election, na.rm = TRUE), max(poll_data$days_to_election, na.rm = TRUE), length.out = 100)

# Create new data frame for predictions
new_data <- data.frame(
  sample_size = mean_sample_size,
  days_to_election = days_seq,
  transparency_score = mean_transparency_score,
  pollscore = mean_pollscore
)

# Get predictions from the general models
trump_preds <- posterior_predict(model_trump, newdata = new_data)
trump_mean_preds <- apply(trump_preds, 2, mean)

harris_preds <- posterior_predict(model_harris, newdata = new_data)
harris_mean_preds <- apply(harris_preds, 2, mean)

# Create data frame for plotting the general model lines
model_predictions <- data.frame(
  days_to_election = days_seq,
  trump_support = trump_mean_preds,
  harris_support = harris_mean_preds
)

# Plot the scatter chart with general model lines
ggplot(poll_data_both, aes(x = days_to_election, y = support, color = candidate)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("Trump" = "red", "Harris" = "blue")) +
  geom_line(data = model_predictions, aes(x = days_to_election, y = trump_support), color = "red", size = 1) +
  geom_line(data = model_predictions, aes(x = days_to_election, y = harris_support), color = "blue", size = 1) +
  labs(title = "Polling Data and General Model Predictions for Trump and Harris",
       x = "Days to Election",
       y = "Support (%)") +
  theme_minimal()


#### Simulation of Elections and Win Probability Calculation ####

# Number of simulations
n_sim <- 1000

# Function to simulate one election
simulate_election <- function() {
  # Initialize total electoral votes
  ev_trump <- 0
  ev_harris <- 0
  
  # Loop through each state
  for (i in 1:nrow(ev_votes)) {
    state <- ev_votes$state[i]
    ev <- ev_votes$ev[i]
    
    # Determine if state is a swing state
    if (state %in% swing_states) {
      # Use swing state models
      # Trump
      model_trump_state <- swing_state_models[[paste("Trump", state, sep = "_")]]
      if (is.list(model_trump_state) && "stanreg" %in% class(model_trump_state)) {
        # Check if model has predicted_support (no polling data)
        if ("predicted_support" %in% names(model_trump_state)) {
          support_trump <- model_trump_state$predicted_support
        } else {
          # Sample from posterior
          predictions <- posterior_predict(model_trump_state, draws = 1)
          support_trump <- as.numeric(predictions[1])  # Extract single value
        }
      } else {
        # Use historical average
        support_trump <- model_trump_state$predicted_support
      }
      
      # Harris
      model_harris_state <- swing_state_models[[paste("Harris", state, sep = "_")]]
      if (is.list(model_harris_state) && "stanreg" %in% class(model_harris_state)) {
        # Check if model has predicted_support (no polling data)
        if ("predicted_support" %in% names(model_harris_state)) {
          support_harris <- model_harris_state$predicted_support
        } else {
          # Sample from posterior
          predictions <- posterior_predict(model_harris_state, draws = 1)
          support_harris <- as.numeric(predictions[1])  # Extract single value
        }
      } else {
        # Use historical average
        support_harris <- model_harris_state$predicted_support
      }
      
    } else {
      # Use general models
      # Trump
      if (!is.na(predicted_supports$trump_support[i])) {
        support_trump <- rnorm(1, mean = predicted_supports$trump_support[i], sd = sigma(model_trump))
      } else {
        support_trump <- 50
      }
      
      # Harris
      if (!is.na(predicted_supports$harris_support[i])) {
        support_harris <- rnorm(1, mean = predicted_supports$harris_support[i], sd = sigma(model_harris))
      } else {
        support_harris <- 50
      }
    }
    
    # Determine winner
    if (support_trump > support_harris) {
      ev_trump <- ev_trump + ev
    } else {
      ev_harris <- ev_harris + ev
    }
  }
  
  # Return the winner
  if (ev_trump > ev_harris) {
    return("Trump")
  } else {
    return("Harris")
  }
}

# Vector to store simulation results
simulation_results <- replicate(n_sim, simulate_election())

# Calculate win probabilities
win_prob <- prop.table(table(simulation_results)) * 100

# Create a data frame for win probabilities
win_prob_df <- as.data.frame(win_prob)
colnames(win_prob_df) <- c("Winner", "Probability")

# Display win probabilities
print(win_prob_df)

