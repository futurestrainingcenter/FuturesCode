# Load the necessary libraries
library(dplyr)
library(readr)

# Load the Teambuildr and Blast datasets
teambuildr_file_path <- "/Users/watts/Downloads/Teambuildr Raw Data Report.csv"
blast_file_path <- "/Users/watts/Documents/Futures Performance Center/Data/Blast Master Data - Sheet1.csv"

teambuildr_df <- read_csv(teambuildr_file_path)
blast_df <- read_csv(blast_file_path)

# Combine "First Name" and "Last Name" to create a "Full Name" column in both DataFrames
teambuildr_df <- mutate(teambuildr_df, Name = paste(`First Name`, `Last Name`))

# Merge the DataFrames on the "Full Name" column
merged_df <- merge(teambuildr_df, blast_df, by = "Name")

# Remove rows where either 'Highest Max' or 'Bat Speed (mph)' is NA
merged_df <- filter(merged_df, !is.na(`Highest Max`), !is.na(`Bat Speed (mph)`))

# Filter out rows where 'Highest Max' cannot be converted to a numeric value
merged_df <- filter(merged_df, !is.na(as.numeric(`Highest Max`, suppressWarnings = TRUE)))

# Filter out groups with zero standard deviation before calculating correlation
correlation_by_exercise <- merged_df %>%
  group_by(`Exercise Name`) %>%
  filter(sd(`Highest Max`) != 0, sd(`Bat Speed (mph)`) != 0) %>%
  summarise(Correlation = cor(`Highest Max`, `Bat Speed (mph)`, use = "complete.obs"))

# Sort the results by Correlation
correlation_by_exercise <- arrange(correlation_by_exercise, desc(Correlation))
