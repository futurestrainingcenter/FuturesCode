library(dplyr)
library(readr)

clientData <- read.csv("/Users/watts/Downloads/ClientList.csv")
attendanceData <- read.csv("/Users/watts/Downloads/Client Analysis, October 01, 2023 - November 30, 2023-20231001-20231130.csv")
proteusData<- read_csv("/Users/watts/Downloads/ProteusPercentiles.csv")
teambuildrData <- read_csv("/Users/watts/Downloads/Teambuildr Raw Data Report (Updated) - Sheet1.csv")
CMJdata <- read_csv("/Users/watts/Downloads/CMJpercentiles.csv")
ISOSQTdata <- read_csv("/Users/watts/Downloads/ISO_SquatPercentiles.csv")

# Function to calculate metrics for a client
calculate_metrics <- function(athlete) {
  # Calculate straight_arm_trunk_rotation
  corePower_graph_data <- proteusData %>% 
    filter(Name == athlete & `exercise name` == "Straight Arm Trunk Rotation") %>%
    group_by(`session createdAt`) %>%
    summarize(`power - mean` = max(`power - mean`, na.rm = TRUE),
              PowerPercentileRank = max(PowerPercentileRank, na.rm = TRUE),
              .groups = "drop")
  
  straight_arm_trunk_rotation <- ifelse(length(corePower_graph_data$PowerPercentileRank) > 0, max(corePower_graph_data$PowerPercentileRank, na.rm = TRUE), NA)
  
  # Calculate ISO_SQT
  ISOSQT_graph_data <- ISOSQTdata %>% 
    filter(Name == athlete) %>%
    group_by(Date) %>%
    summarise(`Peak Vertical Force [N]` = max(`Peak Vertical Force [N]`, na.rm = TRUE),
              PercentileRank = max(PercentileRank, na.rm = TRUE),
              `Peak Vertical Force % (Asym) (%)` = max(`Peak Vertical Force % (Asym) (%)`, na.rm = TRUE)) %>%
    ungroup()
  
  ISO_SQT <- ifelse(length(ISOSQT_graph_data$PercentileRank) > 0, max(ISOSQT_graph_data$PercentileRank, na.rm = TRUE), NA)
  
  # Calculate CMJ
  CMJ_graph_data <- CMJdata %>% 
    filter(Name == athlete) %>%
    group_by(Date) %>%
    summarize(`Concentric Peak Force [N]` = max(`Concentric Peak Force [N]`, na.rm = TRUE),
              PercentileRank = max(PercentileRank, na.rm = TRUE),
              `Peak Landing Force % (Asym) (%)` = max(`Peak Landing Force % (Asym) (%)`, na.rm = TRUE)) %>%
    ungroup()
  
  CMJ <- ifelse(length(CMJ_graph_data$PercentileRank) > 0, max(CMJ_graph_data$PercentileRank, na.rm = TRUE), NA)
  
  # Calculate pushpull, flexion, extension, and shotput
  pushpull_power_graph_data <- proteusData %>% 
    filter(Name == athlete & 
             (`exercise name` == "Chest Press (One Hand)" | `exercise name` == "Horizontal Row (One Hand)" | 
                `exercise name` == "PushPull" | `exercise name` == "PNF D2 Extension" | 
                `exercise name` == "PNF D2 Flexion" | `exercise name` == "Shot Put (Countermovement)")) %>%
    group_by(`session createdAt`, `exercise name`) %>%
    summarize(`power - mean` = mean(`power - mean`, na.rm = TRUE),
              PowerPercentileRank = max(PowerPercentileRank, na.rm = TRUE),
              .groups = "drop")
  
  # Extract data for each exercise
  flexion_data <- filter(pushpull_power_graph_data, `exercise name` == "PNF D2 Flexion")
  flexion <- ifelse(length(flexion_data$PowerPercentileRank) > 0, max(flexion_data$PowerPercentileRank, na.rm = TRUE), NA)
  
  extension_data <- filter(pushpull_power_graph_data, `exercise name` == "PNF D2 Extension")
  extension <- ifelse(length(extension_data$PowerPercentileRank) > 0, max(extension_data$PowerPercentileRank, na.rm = TRUE), NA)
  
  shotput_data <- filter(pushpull_power_graph_data, `exercise name` == "Shot Put (Countermovement)")
  shotput <- ifelse(length(shotput_data$PowerPercentileRank) > 0, max(shotput_data$PowerPercentileRank, na.rm = TRUE), NA)
  
  pushpull <- ifelse(length(pushpull_power_graph_data$PowerPercentileRank) > 0, max(pushpull_power_graph_data$PowerPercentileRank, na.rm = TRUE), NA)
  
  # Return a list with all the metrics
  return(list(
    straight_arm_trunk_rotation = straight_arm_trunk_rotation,
    ISO_SQT = ISO_SQT,
    CMJ = CMJ,
    pushpull = pushpull,
    flexion = flexion,
    extension = extension,
    shotput = shotput
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
write.csv(clients_missing_data, "/Users/watts/Downloads/missing_data.csv", row.names = FALSE)

