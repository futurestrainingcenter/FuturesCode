library(dplyr)
library(readr)

# Set the working directory to where your CSV files are located
# You can change the path to the directory where your CSV files are stored
setwd("/Users/watts/Documents/Futures Performance Center/Data/Trackman Data")

# List all CSV files in the directory
file_list <- list.files(pattern = "\\.csv$")

# Function to read and combine all CSV files
aggregate_csvs <- function(file_list) {
  # Read the first file to create the initial dataframe
  combined_df <- read_csv(file_list[1])
  
  # Loop through the remaining files and bind them row-wise
  for (file in file_list[-1]) {
    temp_df <- read_csv(file)
    combined_df <- bind_rows(combined_df, temp_df)
  }
  
  return(combined_df)
}

# Use the function to combine all CSV files
combined_data <- aggregate_csvs(file_list)

# You can now work with the combined_data dataframe, or you can write it to a new CSV file
write_csv(combined_data, "MasterTrackmanData_october_novemeber.csv")
