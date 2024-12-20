#  Modeling the 2024 Presidential Election

## Overview
This repository provides the scripts, data, and paper used to model and forecast the United States 2024 Presidential Election. Aggregated poll data was used from FiveThirtyEight and generalized linear and Bayesian models were built to predict the winner of presidential candidacy. From our models, we predict Kamala Harris as the 60th president of the United States. 


## File Structure

The repo is structured as:

-   `data` contains the raw data from FiveThirtyEight, analysis data, and simulation data used for this paper. It is in .csv and .parquet form. 
-   `models` contains fitted models. 
-   `other` contains the details about LLM chat interactions and sketches used to facilitate this paper. 
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `scripts` contains the R scripts used to simulate, download, clean, test and model the data.


## Sample Questions for Idealized Poll
A Google form with the proposed questions for an ideal poll that could be employed to collect information about voter opinions can be found [here](https://forms.gle/BAGxHX9dzd4ZZjta6).


## Statement on LLM usage

Code related to the cleaning and model building was written with the assistance of ChatGPT-4o. The model section of the paper was in part written using ChatGPT-4o. The code used to generate the graphs and tables in the Model section, Results section and Appendices 4 through 8 was written in part by ChatGPT-4o.

The entire chat history is available in `other/llms_usage`