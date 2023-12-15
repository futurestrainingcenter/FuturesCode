library(dplyr)
library(readxl)
library(readr)
library(lubridate)

clientData <- read.csv("/Users/watts/Downloads/FullClientList.csv") %>% 
  rename(Name = Client, Level = Reporting.Level..Age.Dependent.)

clientData <- clientData %>% 
  select(Name, Level, Gender)
  
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################

# Reading CMJ data
CMJdata <- read_csv("/Users/watts/Downloads/CMJ_data.csv") %>% 
  filter(`Test Type` == "CMJ")
CMJdata <- left_join(CMJdata, clientData, by = "Name")

# Calculating IQR for filtering
Q1 <- quantile(CMJdata$`Concentric Peak Force [N]`, 0.25, na.rm = TRUE)
Q3 <- quantile(CMJdata$`Concentric Peak Force [N]`, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

filtered_data <- CMJdata %>%
  filter(`Concentric Peak Force [N]` >= lower_bound & `Concentric Peak Force [N]` <= upper_bound)

filtered_data["Level"][is.na(filtered_data["Level"])] <- "Unknown"
filtered_data["Gender"][is.na(filtered_data["Gender"])] <- "Unknown"

# Function to calculate all percentiles
calculate_percentiles <- function(data, column) {
  sapply(1:99, function(p) quantile(data[[column]], probs = p/100, na.rm = TRUE))
}

# Calculating percentiles for each level
percentile_data <- filtered_data %>%
  group_by(Level, Gender) %>%
  do(data.frame(Percentile = 1:99, Value = calculate_percentiles(., "Concentric Peak Force [N]")))

assign_percentile <- function(performance, level, gender, percentile_data) {
  # Filter the percentile data for the specific level
  level_data <- percentile_data %>% filter(Level == level & Gender == gender)
  
  # Find the closest percentile
  closest <- which.min(abs(level_data$Value - performance))
  closest_percentile <- level_data$Percentile[closest]
  
  return(paste0(closest_percentile))
}

# Applying the function to each athlete
filtered_data$PercentileRank <- mapply(assign_percentile, filtered_data$`Concentric Peak Force [N]`, filtered_data$Level, filtered_data$Gender, MoreArgs = list(percentile_data))

write_csv(filtered_data, "/Volumes/COLE'S DATA/Data/Physicality Report Data/CMJpercentiles.csv", na = '')

###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################

# Reading squat data and merging with client data
ISO_SquatData <- read_csv("/Users/watts/Downloads/ISOSQT_data.csv")
ISO_SquatData <- left_join(ISO_SquatData, clientData, by = "Name")

# Calculating IQR for filtering
Q1 <- quantile(ISO_SquatData$`Peak Vertical Force [N]`, 0.25, na.rm = TRUE)
Q3 <- quantile(ISO_SquatData$`Peak Vertical Force [N]`, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Filtering the squat data
filtered_data <- ISO_SquatData %>%
  filter(`Peak Vertical Force [N]` >= lower_bound & `Peak Vertical Force [N]` <= upper_bound)

filtered_data["Level"][is.na(filtered_data["Level"])] <- "Unknown"
filtered_data["Gender"][is.na(filtered_data["Gender"])] <- "Unknown"

# Function to calculate all percentiles
calculate_percentiles <- function(data, column) {
  sapply(1:99, function(p) quantile(data[[column]], probs = p/100, na.rm = TRUE))
}

# Calculating percentiles for each level in ISO_SquatData
squat_percentile_data <- filtered_data %>%
  group_by(Level, Gender) %>%
  do(data.frame(Percentile = 1:99, Value = calculate_percentiles(., "Peak Vertical Force [N]")))

# Function to assign percentile based on Level
assign_percentile <- function(performance, level, gender, squat_percentile_data) {
  # Filter the percentile data for the specific level
  level_data <- squat_percentile_data %>% filter(Level == level & Gender == gender)
  
  # Find the closest percentile
  closest <- which.min(abs(level_data$Value - performance))
  closest_percentile <- level_data$Percentile[closest]
  
  return(paste0(closest_percentile))
}

# Applying the function to each athlete in ISO_SquatData
filtered_data$PercentileRank <- mapply(assign_percentile, filtered_data$`Peak Vertical Force [N]`, filtered_data$Level, filtered_data$Gender, MoreArgs = list(squat_percentile_data))

# Writing the output to a CSV file
write_csv(filtered_data, "/Volumes/COLE'S DATA/Data/Physicality Report Data/ISO_SquatPercentiles.csv", na = '')

###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################

# Reading Proteus data and merging with client data
proteusData <- read_csv("/Users/watts/Downloads/Proteus_data.csv") %>% 
  rename(Name = `user name`)
proteusData <- left_join(proteusData, clientData, by = "Name")

# Filtering exercises and summarizing data
proteusData <- proteusData %>% 
  filter(`exercise name` %in% c("Straight Arm Trunk Rotation", "PNF D2 Extension", 
                                "PNF D2 Flexion", "Shot Put (Countermovement)", 
                                "Chest Press (One Hand)", "Horizontal Row (One Hand)")) %>%
  group_by(`session createdAt`, `exercise name`, Name, Level, Gender) %>% 
  summarize(`power - mean` = mean(`power - mean`, na.rm = TRUE),
            `acceleration - mean` = mean(`acceleration - mean`, na.rm = TRUE)) %>%
  ungroup()

# Merging push/pull data
pushPullData <- proteusData %>% 
  filter(`exercise name` %in% c("Chest Press (One Hand)", "Horizontal Row (One Hand)")) %>%
  mutate(`exercise name` = "PushPull") %>%
  group_by(`session createdAt`, `exercise name`, Name, Level, Gender) %>% 
  summarize(`power - mean` = mean(`power - mean`, na.rm = TRUE),
            `acceleration - mean` = mean(`acceleration - mean`, na.rm = TRUE)) %>%
  ungroup()

proteusData <- proteusData %>%
  bind_rows(pushPullData)

proteusData["Level"][is.na(proteusData["Level"])] <- "Unknown"
proteusData["Gender"][is.na(proteusData["Gender"])] <- "Unknown"

# Function to calculate all percentiles for both metrics
calculate_percentiles <- function(data, column) {
  sapply(1:99, function(p) quantile(data[[column]], probs = p/100, na.rm = TRUE))
}

# Calculating percentiles for each exercise, level, gender, and metric
percentile_data_power <- proteusData %>%
  group_by(`exercise name`, Level, Gender) %>%
  do(data.frame(Metric = "power - mean", Percentile = 1:99, Value = calculate_percentiles(., "power - mean")))

percentile_data_acceleration <- proteusData %>%
  group_by(`exercise name`, Level, Gender) %>%
  do(data.frame(Metric = "acceleration - mean", Percentile = 1:99, Value = calculate_percentiles(., "acceleration - mean")))

# Combining both percentile datasets
percentile_data <- rbind(percentile_data_power, percentile_data_acceleration)

# Function to assign percentile based on Level and Gender
assign_percentile <- function(performance, level, gender, exercise, metric, percentile_data) {
  # Filter the percentile data for the specific level, gender, exercise, and metric
  specific_data <- percentile_data %>% filter(Level == level, Gender == gender, `exercise name` == exercise, Metric == metric)
  
  # Find the closest percentile
  closest <- which.min(abs(specific_data$Value - performance))
  closest_percentile <- specific_data$Percentile[closest]
  
  return(paste0(closest_percentile))
}

# Applying the function to each athlete for both metrics
proteusData$PowerPercentileRank <- mapply(assign_percentile, proteusData$`power - mean`, proteusData$Level, proteusData$Gender, proteusData$`exercise name`, MoreArgs = list("power - mean", percentile_data))
proteusData$AccelerationPercentileRank <- mapply(assign_percentile, proteusData$`acceleration - mean`, proteusData$Level, proteusData$Gender, proteusData$`exercise name`, MoreArgs = list("acceleration - mean", percentile_data))

# Writing the output to a CSV file
write_csv(proteusData, "/Volumes/COLE'S DATA/Data/Physicality Report Data/ProteusPercentiles.csv", na = '')

###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################

# Reading squat data and merging with client data
teambuildrData <- read_csv("/Users/watts/Downloads/Teambuildr Raw Data Report.csv") %>%
  mutate(Name = paste(`First Name`, `Last Name`))

merged_data <- left_join(teambuildrData, clientData, by = "Name")

exercises <- c('Barbell Back Squat', 'Trap Bar Deadlift', 'Barbell Bench Press', 
               'Straight Arm Trunk Rotation Max Isometric Test - Crane Scale', 'Cable Lat Pull Down (1)')

merged_data$'Highest Max' <- as.numeric(as.character(merged_data$'Highest Max'))
merged_data$'Highest Max' <- round(merged_data$'Highest Max')

filteredExercise_data <- merged_data %>%
  filter(`Exercise Name` %in% exercises)

for (exercise in exercises) {
  
  exercise_data <- filteredExercise_data %>% filter(`Exercise Name` == exercise)
  
  Q1 <- quantile(exercise_data$`Highest Max`, 0.25, na.rm = TRUE)
  Q3 <- quantile(exercise_data$`Highest Max`, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Filter out the outliers
  filteredExercise_data <- filteredExercise_data %>%
    filter(!(`Exercise Name` == exercise & (`Highest Max` < lower_bound | `Highest Max` > upper_bound)))
} 

filteredExercise_data["Level"][is.na(filteredExercise_data["Level"])] <- "Unknown"
filteredExercise_data["Gender"][is.na(filteredExercise_data["Gender"])] <- "Unknown"

# Function to calculate all percentiles
calculate_percentiles <- function(data, column) {
  sapply(1:99, function(p) quantile(data[[column]], probs = p/100, na.rm = TRUE))
}

# Calculating percentiles for each level in ISO_SquatData
weightroom_percentile_data <- filteredExercise_data %>%
  group_by(Level, Gender) %>%
  do(data.frame(Percentile = 1:99, Value = calculate_percentiles(., "Highest Max")))

# Function to assign percentile based on Level
assign_percentile <- function(performance, level, gender, weightroom_percentile_data) {
  # Filter the percentile data for the specific level
  level_data <- weightroom_percentile_data %>% filter(Level == level & Gender == gender)
  
  # Find the closest percentile
  closest <- which.min(abs(level_data$Value - performance))
  closest_percentile <- level_data$Percentile[closest]
  
  return(paste0(closest_percentile))
}

# Applying the function to each athlete in ISO_SquatData
filteredExercise_data$PercentileRank <- mapply(assign_percentile, filteredExercise_data$`Highest Max`, filteredExercise_data$Level, filteredExercise_data$Gender, MoreArgs = list(weightroom_percentile_data))

# Writing the output to a CSV file
write_csv(filteredExercise_data, "/Volumes/COLE'S DATA/Data/Physicality Report Data/teambuilderPercentiles.csv", na = '')

