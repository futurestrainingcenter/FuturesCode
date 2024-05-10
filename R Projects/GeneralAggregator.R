# Load necessary library
library(dplyr)
library(readr)

# Set the working directory to the folder containing your CSV files
setwd("/Users/watts/Downloads/Proteus Data") # Change this to the path of your CSV files

# List all CSV files in the directory
csv_files <- list.files(pattern = "\\.csv$")

# Read each CSV file and combine them into a single data frame
combined_df <- lapply(csv_files, read_csv) %>% bind_rows()

# Write the combined data frame to a new master CSV file
write.csv(combined_df, "MasterProteusData.csv", row.names = FALSE)
