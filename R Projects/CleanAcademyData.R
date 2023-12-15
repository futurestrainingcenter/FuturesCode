# Load necessary library
library(readr)

# Read the CSV file (replace 'your_file_path.csv' with the path to your CSV file)
data <- read_csv('your_file_path.csv', col_names = FALSE)

# Initialize variables
athlete_data <- data.frame(Athlete=character(), DateRecorded=character(), Weight=numeric(), stringsAsFactors=FALSE)
current_athlete <- NULL

# Iterate through each row
for (i in 1:nrow(data)) {
  # Check if the row contains a new athlete's name
  if (!is.na(data[i, 1]) && is.na(data[i, 2])) {
    current_athlete <- as.character(data[i, 1])
  } else if (!is.na(data[i, 1]) && !is.na(data[i, 2]) && data[i, 1] != "Date Recorded") {
    # Add the data row with the athlete's name
    athlete_data <- rbind(athlete_data, data.frame(Athlete = current_athlete, 
                                                   DateRecorded = as.character(data[i, 1]), 
                                                   Weight = as.numeric(data[i, 2])))
  }
}

# Optional: Write the cleaned data to a new CSV file
write.csv(athlete_data, 'cleaned_athlete_data.csv', row.names = FALSE)
