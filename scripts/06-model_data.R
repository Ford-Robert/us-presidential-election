#### Preamble ####
# Purpose: Models... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 11 February 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
library(tidyverse)
library(rstanarm)

#### Read data ####
poll_data <- read_csv("data/cleaned_poll_data.csv")

View(poll_data)

#TODO add lm model, response: pct, predictors: everything else

# Then put the polls through the lm to recover the adjusted average pct for each poll

# Do this for each state and each candidate

# Use this to calculate the number of electoral college votes both candidates
# will get




### Model data ####
model <- lm(
)


#### Save model ####
saveRDS(
  first_model,
  file = "models/first_model.rds"
)


