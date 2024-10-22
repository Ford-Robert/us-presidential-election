# A Comprehensive Approach to Modeling the 2024 Presidential Election

## Overview

This repository provides all of the code that went into building the paper, "A Comprehensive Approach to Modeling the 2024 Presidential Election"


## File Structure

The repo is structured as:

-   `data/raw_poll_data.csv` contains the raw data as obtained from X.
-   `data/cleaned_poll_data.csv` contains the cleaned dataset that was constructed.
-   `data/00-simulate_data.csv` contains a simulated dataset used for planning
-   `models` contains fitted models. 
-   `other` contains relevant literature, details about LLM chat interactions, and sketches.
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `scripts` contains the R scripts used to simulate, download, clean, test and model the data.


## Statement on LLM usage

Code related to the cleaning and model building was written with the assistance of ChatGPT-4o, the entire chat history is available in `other/llms_usage`
