# -----------------------------
# R Script to Create a Compact Styled Table with 4 Columns and a Note
# -----------------------------

# -----------------------------
# Step 1: Install and Load Required Packages
# -----------------------------

# Uncomment the following lines to install the packages if they are not already installed
# install.packages("kableExtra")
# install.packages("dplyr")

# Load the necessary libraries
library(kableExtra)
library(dplyr)

# -----------------------------
# Step 2: Define the Data
# -----------------------------

# Create a data frame with the provided data
df <- data.frame(
  State = c("Alabama", "Delaware", "Hawaii", "Idaho", "Illinois",
            "Kentucky", "Louisiana", "Mississippi", "Tennessee", "Wyoming"),
  Party = c("Rep", "Dem", "Dem", "Rep", "Dem",
            "Rep", "Rep", "Rep", "Rep", "Rep"),
  Start_Year = c(1980, 1992, 1998, 1976, 1992,
                 2000, 2000, 1980, 2000, 1976),
  stringsAsFactors = FALSE
)

# -----------------------------
# Step 3: Reshape the Data for a Compact Table
# -----------------------------

# Pair the states into two columns per row
# We'll split the data into two halves
df1 <- df[1:5, ]  # First 5 states
df2 <- df[6:10, ] # Next 5 states

# Combine into a single data frame with 6 columns
compact_df <- data.frame(
  State_1 = df1$State,
  Year_1 = df1$Start_Year,
  Party_1 = df1$Party,
  State_2 = df2$State,
  Year_2 = df2$Start_Year,
  Party_2 = df2$Party,
  stringsAsFactors = FALSE
)

# -----------------------------
# Step 4: Apply Cell-wise Coloring Based on Party
# -----------------------------

# Function to assign colors based on party
assign_color <- function(party) {
  ifelse(party == "Rep", "red", "blue")
}

# Apply cell_spec to each cell that needs coloring
compact_df <- compact_df %>%
  mutate(
    State_1 = cell_spec(State_1, background = assign_color(Party_1), color = "white"),
    Year_1 = cell_spec(Year_1, background = assign_color(Party_1), color = "white"),
    State_2 = cell_spec(State_2, background = assign_color(Party_2), color = "white"),
    Year_2 = cell_spec(Year_2, background = assign_color(Party_2), color = "white")
  )

# -----------------------------
# Step 5: Create the Styled Compact Table with Note
# -----------------------------

# Create and style the table
styled_table <- compact_df %>%
  select(State_1, Year_1, State_2, Year_2) %>%
  kable("html", escape = FALSE, align = "c",
        col.names = c("State", "Last Time Voted for a Different Party",
                      "State", "Last Time Voted for a Different Party")) %>%
  kable_styling("striped", full_width = FALSE, position = "left") %>%
  # Header styling
  row_spec(0, bold = TRUE, background = "#D3D3D3") %>%
  # Add a footnote below the table
  add_footnote("Note: Data collected only went back to 1976, some states may have been voting for the same party farther back than 1976.",
  )

# Display the table
styled_table


# -----------------------------
# R Script to Create a Compact Styled Table with 4 Columns and a Note Using the gt Package
# -----------------------------

# -----------------------------
# Step 1: Install and Load Required Packages
# -----------------------------

# Install the gt package if it's not already installed
if (!require("gt")) {
  install.packages("gt")
}

# Load the necessary library
library(gt)

# -----------------------------
# Step 2: Define the Data
# -----------------------------

# Create a data frame with the provided data
df <- data.frame(
  State = c("Alabama", "Delaware", "Hawaii", "Idaho", "Illinois",
            "Kentucky", "Louisiana", "Mississippi", "Tennessee", "Wyoming"),
  Party = c("Rep", "Dem", "Dem", "Rep", "Dem",
            "Rep", "Rep", "Rep", "Rep", "Rep"),
  Start_Year = c(1980, 1992, 1998, 1976, 1992,
                 2000, 2000, 1980, 2000, 1976),
  stringsAsFactors = FALSE
)

# -----------------------------
# Step 3: Reshape the Data for a Compact Table
# -----------------------------

# Pair the states into two columns per row
# Split the data into two halves
df1 <- df[1:5, ]  # First 5 states
df2 <- df[6:10, ] # Next 5 states

# Combine into a single data frame with 6 columns
compact_df <- data.frame(
  State_1 = df1$State,
  Year_1 = df1$Start_Year,
  Party_1 = df1$Party,
  State_2 = df2$State,
  Year_2 = df2$Start_Year,
  Party_2 = df2$Party,
  stringsAsFactors = FALSE
)

# -----------------------------
# Step 4: Create the Styled Compact Table Using gt
# -----------------------------

# Function to assign colors based on party
assign_color <- function(party) {
  ifelse(party == "Rep", "red", "blue")
}

# Create the gt table
gt_table <- compact_df %>%
  gt() %>%
  # Style for State 1
  tab_style(
    style = list(
      cell_fill(color = assign_color(compact_df$Party_1)),
      cell_text(color = "white")
    ),
    locations = cells_body(
      columns = vars(State_1),
      rows = TRUE
    )
  ) %>%
  # Style for Year 1
  tab_style(
    style = list(
      cell_fill(color = assign_color(compact_df$Party_1)),
      cell_text(color = "white")
    ),
    locations = cells_body(
      columns = vars(Year_1),
      rows = TRUE
    )
  ) %>%
  # Style for State 2
  tab_style(
    style = list(
      cell_fill(color = assign_color(compact_df$Party_2)),
      cell_text(color = "white")
    ),
    locations = cells_body(
      columns = vars(State_2),
      rows = TRUE
    )
  ) %>%
  # Style for Year 2
  tab_style(
    style = list(
      cell_fill(color = assign_color(compact_df$Party_2)),
      cell_text(color = "white")
    ),
    locations = cells_body(
      columns = vars(Year_2),
      rows = TRUE
    )
  ) %>%
  # Rename columns
  cols_label(
    State_1 = "State",
    Year_1 = "Last Time Voted Different Party",
    State_2 = "State",
    Year_2 = "Last Time Voted Different Party"
  ) %>%
  # Hide the Party columns as they are used for styling
  cols_hide(
    columns = vars(Party_1, Party_2)
  ) %>%
  # Add a source note below the table
  tab_source_note(
    source_note = "Note: Data collected only went back to 1976, some states may have been voting for the same party farther back than 1976."
  ) %>%
  # Adjust table options for better appearance
# Display the table
gt_table

# -----------------------------
# End of Script
# -----------------------------
