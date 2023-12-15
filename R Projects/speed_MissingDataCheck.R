library(dplyr)
library(readr)

clientData <- read.csv("/Users/watts/Downloads/AllClients.csv")
attendanceData <- read.csv("/Users/watts/Downloads/SpeedAttendanceData.csv")
hardNinety <- read.csv("/Users/watts/Downloads/Hard90percentiles.csv")
accelerationData<- read_csv("/Users/watts/Downloads/AccelerationPercentiles.csv")
maxVeloData <- read_csv("/Users/watts/Downloads/MaxVelocityPercentiles.csv")
RSIdata <- read_csv("/Users/watts/Downloads/RSIpercentiles.csv")

# Function to calculate metrics for a client
calculate_metrics <- function(athlete) {
  
  hardNinety_graph_data <- hardNinety %>% 
    filter(FullName == athlete) %>%
    group_by(Date) %>%
    summarize(Cumulative3 = min(Cumulative3, na.rm = TRUE),
              PercentileRank = max(PercentileRank, na.rm = TRUE)) %>%
    ungroup()
  
  
  
  acceleration_graph_data <- accelerationData %>% 
    filter(FullName == athlete) %>%
    group_by(Date) %>%
    summarize(Acceleration = max(Acceleration, na.rm = TRUE),
              PercentileRank = max(PercentileRank, na.rm = TRUE)) %>%
    ungroup()
  

  maxVelo_graph_data <- maxVeloData %>% 
    filter(FullName == athlete) %>%
    group_by(Date) %>%
    summarize(MPH = max(MPH, na.rm = TRUE),
              PercentileRank = max(PercentileRank, na.rm = TRUE)
    ) %>%
    ungroup()
  
  
  RSI_graph_data <- RSIdata %>% 
    filter(FullName == athlete) %>%
    group_by(Date) %>%
    summarize(RSI = max(`Mean RSI (Jump Height/Contact Time) [m/s]`, na.rm = TRUE),
              PercentileRank = max(PercentileRank, na.rm = TRUE)) %>%
    ungroup()
  
  thirtyYardSprint <- ifelse(length(hardNinety_graph_data$PercentileRank) > 0, max(hardNinety_graph_data$PercentileRank, na.rm = TRUE), NA)
  tenYardSprint <- ifelse(length(acceleration_graph_data$PercentileRank) > 0, max(acceleration_graph_data$PercentileRank, na.rm = TRUE), NA)
  flyingTenYardSprint <- ifelse(length(maxVelo_graph_data$PercentileRank) > 0, max(maxVelo_graph_data$PercentileRank, na.rm = TRUE), NA)
  RSItest <- ifelse(length(RSI_graph_data$PercentileRank) > 0, max(RSI_graph_data$PercentileRank, na.rm = TRUE), NA)
  
  
  # Return a list with all the metrics
  return(list(
    thirtyYardSprint = thirtyYardSprint,
    tenYardSprint = tenYardSprint,
    flyingTenYardSprint = flyingTenYardSprint,
    RSItest = RSItest
  ))
}

# Initialize an empty dataframe to store clients with missing data
clients_missing_data <- data.frame(Name = character(), `Missing Data` = character(), stringsAsFactors = FALSE)

# Loop through each client in attendanceData
for (i in 1:nrow(attendanceData)) {
  athlete <- attendanceData$Client.name[i]
  
  # Calculate metrics for this client
  metrics <- calculate_metrics(athlete)
  
  # Check for missing data and record it
  missing_vars <- names(metrics)[sapply(metrics, is.na)]
  if (length(missing_vars) > 0) {
    clients_missing_data <- rbind(clients_missing_data, data.frame(Name = athlete, `Missing Data` = paste(missing_vars, collapse = ", ")))
  }
}

# Write the clients with missing data to a CSV file
write.csv(clients_missing_data, "/Users/watts/Downloads/speed_missing_data.csv", row.names = FALSE)

