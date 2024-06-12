# Set the path to the main folder containing year, month, and day folders
main_folder <- "/Users/watts/Documents/Futures Performance Center/Data/practice"

# Create an empty data frame to store the combined data
combined_data <- data.frame()

# List all CSV files in the directory and its subdirectories
csv_files <- list.files(path = main_folder, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)

# Loop through each CSV file and append its contents to the combined data frame
for (file in csv_files) {
  data <- read.csv(file, header = TRUE)  # Assuming CSV files have headers
  combined_data <- rbind(combined_data, data)
}

# Write the combined data frame to a new CSV file
write.csv(combined_data, "/Users/watts/Documents/Futures Performance Center/Data/practice/masterTrackmanData.csv", row.names = FALSE)

# Print a message to confirm the process is completed
cat("All CSV files have been combined into 'masterTrackmanData.csv'.\n")
