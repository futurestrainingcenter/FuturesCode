# Load necessary libraries
library(dplyr)

# Set the working directory to the folder containing your CSV files
setwd("/Users/watts/Downloads/Full Team Report - 2023-12-29 - 2023-12-29 - 1706645653") # replace with your folder path

# Get a list of all CSV files in the directory
file_list <- list.files(pattern = "\\.csv$")

# Initialize an empty list to store data frames
all_data <- list()

# Loop through each file
for (file_name in file_list) {
  # Read the CSV file, skipping the first 8 rows
  df <- read_csv(file_name, skip = 8)
  
  if (nrow(df) == 0) {
    next
  }
  
  # Extract the athlete's name from the file name (remove file extension)
  athlete_name <- tools::file_path_sans_ext(file_name)
  
  # Add a new column with the athlete's name
  df$Athlete <- athlete_name
  
  # Append to the list
  all_data[[file_name]] <- df
}

# Combine all data frames into one
master_df <- bind_rows(all_data)

# Save the combined data to a new CSV file
write.csv(master_df, "/Users/watts/Downloads/MasterBlastData.csv", row.names = FALSE)

