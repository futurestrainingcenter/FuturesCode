library(dplyr)
library(readxl)
library(readr)
library(lubridate)

clientData <- read_csv("/Users/watts/Downloads/FullClientList.csv") %>% 
  rename(Name = Client, Level = `Sp Level`) %>% 
  select(Name, Level, Gender)

###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################

# Reading CMJ data
CMJdata <- read_csv("/Users/watts/Downloads/CMJ_data.csv") %>% 
  filter(`Test Type` == "CMJ")
CMJdata$Date <- as.Date(CMJdata$Date, format="%m/%d/%Y")

CMJdata <- CMJdata %>% 
  mutate(Month = month(Date, label = TRUE, abbr = FALSE))

CMJdata <- left_join(CMJdata, clientData, by = "Name") %>% 
  filter(!is.na(Gender), !is.na(Level), Level != "N/A")

# Calculating IQR for filtering
Q1 <- quantile(CMJdata$`Concentric Peak Force [N]`, 0.25, na.rm = TRUE)
Q3 <- quantile(CMJdata$`Concentric Peak Force [N]`, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 2.25 * IQR

filtered_data <- CMJdata %>%
  filter(`Concentric Peak Force [N]` >= lower_bound & `Concentric Peak Force [N]` <= upper_bound)

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

write_csv(filtered_data, "/Users/watts/Documents/Futures Performance Center/Data/Strength Percentiles/CMJpercentiles.csv", na = '')

###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################

# Reading squat data and merging with client data
ISO_SquatData <- read_csv("/Users/watts/Downloads/ISOSQT_data.csv")
ISO_SquatData$Date <- as.Date(ISO_SquatData$Date, format="%m/%d/%Y")

ISO_SquatData <- ISO_SquatData %>% 
  mutate(Month = month(Date, label = TRUE, abbr = FALSE)) %>% 
  left_join(clientData, by = "Name") %>% 
  filter(!is.na(Gender), !is.na(Level), Level != "N/A")

# Calculating IQR for filtering
Q1 <- quantile(ISO_SquatData$`Peak Vertical Force [N]`, 0.25, na.rm = TRUE)
Q3 <- quantile(ISO_SquatData$`Peak Vertical Force [N]`, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 2.25 * IQR

# Filtering the squat data
filtered_data <- ISO_SquatData %>%
  filter(`Peak Vertical Force [N]` >= lower_bound & `Peak Vertical Force [N]` <= upper_bound)

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
write_csv(filtered_data, "/Users/watts/Documents/Futures Performance Center/Data/Strength Percentiles/ISO_SquatPercentiles.csv", na = '')

###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################

# Reading squat data and merging with client data
ISO_BeltSquatData <- read_csv("/Users/watts/Downloads/ISOBelt_data.csv")
ISO_BeltSquatData$Date <- as.Date(ISO_BeltSquatData$Date, format="%m/%d/%Y")

ISO_BeltSquatData <- ISO_BeltSquatData %>% 
  mutate(Month = month(Date, label = TRUE, abbr = FALSE)) %>% 
  left_join(clientData, by = "Name") %>% 
  filter(!is.na(Gender), !is.na(Level), Level != "N/A")

# Calculating IQR for filtering
Q1 <- quantile(ISO_BeltSquatData$`Peak Vertical Force [N]`, 0.25, na.rm = TRUE)
Q3 <- quantile(ISO_BeltSquatData$`Peak Vertical Force [N]`, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 2.25 * IQR

# Filtering the squat data
filtered_data <- ISO_BeltSquatData %>%
  filter(`Peak Vertical Force [N]` >= lower_bound & `Peak Vertical Force [N]` <= upper_bound)

# Function to calculate all percentiles
calculate_percentiles <- function(data, column) {
  sapply(1:99, function(p) quantile(data[[column]], probs = p/100, na.rm = TRUE))
}

# Calculating percentiles for each level in ISO_BeltSquatData
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

# Applying the function to each athlete in ISO_BeltSquatData
filtered_data$PercentileRank <- mapply(assign_percentile, filtered_data$`Peak Vertical Force [N]`, filtered_data$Level, filtered_data$Gender, MoreArgs = list(squat_percentile_data))

# Writing the output to a CSV file
write_csv(filtered_data, "/Users/watts/Documents/Futures Performance Center/Data/Strength Percentiles/ISO_BeltSquatPercentiles.csv", na = '')

###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################

# Reading squat data and merging with client data
SQTJumpData <- read_csv("/Users/watts/Downloads/SQTJump_data.csv") %>% 
  filter(`Test Type` == "SJ")
SQTJumpData$Date <- as.Date(SQTJumpData$Date, format="%m/%d/%Y")

SQTJumpData <- SQTJumpData %>% 
  mutate(Month = month(Date, label = TRUE, abbr = FALSE)) %>% 
  left_join(clientData, by = "Name") %>%
  filter(!is.na(Gender), !is.na(Level), Level != "N/A")

# Calculating IQR for filtering
Q1 <- quantile(SQTJumpData$`Takeoff Peak Force [N]`, 0.25, na.rm = TRUE)
Q3 <- quantile(SQTJumpData$`Takeoff Peak Force [N]`, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 2.25 * IQR

# Filtering the squat data
filtered_data <- SQTJumpData %>%
  filter(`Takeoff Peak Force [N]` >= lower_bound & `Takeoff Peak Force [N]` <= upper_bound)

# Function to calculate all percentiles
calculate_percentiles <- function(data, column) {
  sapply(1:99, function(p) quantile(data[[column]], probs = p/100, na.rm = TRUE))
}

# Calculating percentiles for each level in ISO_SquatData
SQTJump_percentile_data <- filtered_data %>%
  group_by(Level, Gender) %>%
  do(data.frame(Percentile = 1:99, Value = calculate_percentiles(., "Takeoff Peak Force [N]")))

# Function to assign percentile based on Level
assign_percentile <- function(performance, level, gender, SQTJump_percentile_data) {
  # Filter the percentile data for the specific level
  level_data <- SQTJump_percentile_data %>% filter(Level == level & Gender == gender)
  
  # Find the closest percentile
  closest <- which.min(abs(level_data$Value - performance))
  closest_percentile <- level_data$Percentile[closest]
  
  return(paste0(closest_percentile))
}

# Applying the function to each athlete in ISO_SquatData
filtered_data$PercentileRank <- mapply(assign_percentile, filtered_data$`Takeoff Peak Force [N]`, filtered_data$Level, filtered_data$Gender, MoreArgs = list(SQTJump_percentile_data))

# Writing the output to a CSV file
write_csv(filtered_data, "/Users/watts/Documents/Futures Performance Center/Data/Strength Percentiles/SQTJumpPercentiles.csv", na = '')

###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################

# Reading squat data and merging with client data
shoulderData <- read_csv("/Users/watts/Downloads/ShoulderISO_data.csv" )
shoulderData$Date <- as.Date(shoulderData$Date, format="%m/%d/%Y")

shoulderData <- shoulderData %>% 
  mutate(Month = month(Date, label = TRUE, abbr = FALSE)) %>% 
  left_join(clientData, by = "Name") %>% 
  filter(!is.na(Gender), !is.na(Level), Level != "N/A")

# Calculating IQR for filtering
Q1 <- quantile(shoulderData$`Peak Vertical Force [N]`, 0.25, na.rm = TRUE)
Q3 <- quantile(shoulderData$`Peak Vertical Force [N]`, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 2.25 * IQR

# Filtering the squat data
filtered_data <- shoulderData %>%
  filter(`Peak Vertical Force [N]` >= lower_bound & `Peak Vertical Force [N]` <= upper_bound)

filtered_data["Level"][is.na(filtered_data["Level"])] <- "Unknown"
filtered_data["Gender"][is.na(filtered_data["Gender"])] <- "Unknown"

# Function to calculate all percentiles
calculate_percentiles <- function(data, column) {
  sapply(1:99, function(p) quantile(data[[column]], probs = p/100, na.rm = TRUE))
}

# Calculating percentiles for each level in ISO_SquatData
shoulder_percentile_data <- filtered_data %>%
  group_by(Level, Gender) %>%
  do(data.frame(Percentile = 1:99, Value = calculate_percentiles(., "Peak Vertical Force [N]")))

# Function to assign percentile based on Level
assign_percentile <- function(performance, level, gender, shoulder_percentile_data) {
  # Filter the percentile data for the specific level
  level_data <- shoulder_percentile_data %>% filter(Level == level & Gender == gender)
  
  # Find the closest percentile
  closest <- which.min(abs(level_data$Value - performance))
  closest_percentile <- level_data$Percentile[closest]
  
  return(paste0(closest_percentile))
}

# Applying the function to each athlete in ISO_SquatData
filtered_data$PercentileRank <- mapply(assign_percentile, filtered_data$`Peak Vertical Force [N]`, filtered_data$Level, filtered_data$Gender, MoreArgs = list(shoulder_percentile_data))

# Writing the output to a CSV file
write_csv(filtered_data, "/Users/watts/Documents/Futures Performance Center/Data/Strength Percentiles/ShoulderISOPercentiles.csv", na = '')

###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################

# Reading Proteus data and merging with client data
proteusData <- read_csv("/Users/watts/Downloads/Proteus Data/MasterProteusData.csv") %>% 
  rename(Name = `user name`)
proteusData$`session createdAt` <- as.Date(proteusData$`session createdAt`, format="%Y-%m-%dT%H:%M:%S")

proteusData <- proteusData %>% 
  mutate(Month = month(`session createdAt`, label = TRUE, abbr = FALSE)) %>% 
  left_join(clientData, by = "Name") %>% 
  filter(!is.na(Gender), !is.na(Level), Level != "N/A")

# Filtering exercises and summarizing data
proteusData <- proteusData %>% 
  filter(`exercise name` %in% c("Straight Arm Trunk Rotation", "PNF D2 Extension", 
                                "PNF D2 Flexion", "Shot Put (Countermovement)", 
                                "Chest Press (One Hand)", "Horizontal Row (One Hand)")) %>%
  group_by(`session createdAt`, Month, `exercise name`, Name, Level, Gender) %>% 
  summarize(`power - high` = mean(`power - high`, na.rm = TRUE),
            `acceleration - high` = mean(`acceleration - high`, na.rm = TRUE)) %>%
  ungroup()

# Merging push/pull data
pushPullData <- proteusData %>% 
  filter(`exercise name` %in% c("Chest Press (One Hand)", "Horizontal Row (One Hand)")) %>%
  mutate(`exercise name` = "PushPull") %>%
  group_by(`session createdAt`, Month, `exercise name`, Name, Level, Gender) %>% 
  summarize(`power - high` = mean(`power - high`, na.rm = TRUE),
            `acceleration - high` = mean(`acceleration - high`, na.rm = TRUE)) %>%
  ungroup()

summaryData <- proteusData %>%
  mutate(`exercise name` = "Proteus Full Test") %>%
  group_by(`session createdAt`, Month, `exercise name`, Name, Level, Gender) %>% 
  summarize(
    `power - high` = mean(`power - high`, na.rm = TRUE),
    `acceleration - high` = mean(`acceleration - high`, na.rm = TRUE)
  )

proteusData <- proteusData %>%
  bind_rows(pushPullData, summaryData)

# Function to calculate all percentiles for both metrics
calculate_percentiles <- function(data, column) {
  sapply(1:99, function(p) quantile(data[[column]], probs = p/100, na.rm = TRUE))
}

# Calculating percentiles for each exercise, level, gender, and metric
percentile_data_power <- proteusData %>%
  group_by(`exercise name`, Level, Gender) %>%
  do(data.frame(Metric = "power - high", Percentile = 1:99, Value = calculate_percentiles(., "power - high")))

percentile_data_acceleration <- proteusData %>%
  group_by(`exercise name`, Level, Gender) %>%
  do(data.frame(Metric = "acceleration - high", Percentile = 1:99, Value = calculate_percentiles(., "acceleration - high")))

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
proteusData$PowerPercentileRank <- mapply(assign_percentile, proteusData$`power - high`, proteusData$Level, proteusData$Gender, proteusData$`exercise name`, MoreArgs = list("power - high", percentile_data))
proteusData$AccelerationPercentileRank <- mapply(assign_percentile, proteusData$`acceleration - high`, proteusData$Level, proteusData$Gender, proteusData$`exercise name`, MoreArgs = list("acceleration - high", percentile_data))

# Writing the output to a CSV file
write_csv(proteusData, "/Users/watts/Documents/Futures Performance Center/Data/Strength Percentiles/ProteusPercentiles.csv", na = '')

###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################

# Reading squat data and merging with client data
teambuildrData <- read_csv("/Users/watts/Downloads/Teambuildr Raw Data Report.csv") %>%
  mutate(Name = paste(`First Name`, `Last Name`))

merged_data <- left_join(teambuildrData, clientData, by = "Name")

exercises <- c('Barbell Back Squat', 'Trap Bar Deadlift', 'Barbell Bench Press', 'Safety Squat Bar Split Squat',
               'Straight Arm Trunk Rotation Max Isometric Test - Crane Scale', 'Cable Lat Pull Down')

merged_data$'Max Value' <- as.numeric(as.character(merged_data$'Max Value'))
merged_data$'Max Value' <- round(merged_data$'Max Value')

filteredExercise_data <- merged_data %>%
  filter(`Exercise Name` %in% exercises, !is.na(Gender), !is.na(Level), Level != "N/A")

# Filtering step with specific conditions for each exercise
filteredExercise_data <- filteredExercise_data %>%
  filter(!(`Exercise Name` == 'Barbell Back Squat' & `Max Value` < 45 |
             `Exercise Name` == 'Barbell Bench Press' & `Max Value` < 45 |
             `Exercise Name` == 'Safety Squat Bar Split Squat' & `Max Value` < 55 |
             `Exercise Name` == 'Trap Bar Deadlift' & `Max Value` < 60))

for (exercise in exercises) {
  
  exercise_data <- filteredExercise_data %>% filter(`Exercise Name` == exercise)
  
  Q1 <- quantile(exercise_data$`Max Value`, 0.25, na.rm = TRUE)
  Q3 <- quantile(exercise_data$`Max Value`, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 2.25 * IQR
  
  # Filter out the outliers
  filteredExercise_data <- filteredExercise_data %>%
    filter(!(`Exercise Name` == exercise & (`Max Value` < lower_bound | `Max Value` > upper_bound)))
} 

# Adjusting percentile calculation to be done by exercise, level, and gender
calculate_percentiles <- function(data, column) {
  sapply(1:99, function(p) quantile(data[[column]], probs = p/100, na.rm = TRUE))
}

# Adjusted to group by Exercise Name as well
weightroom_percentile_data <- filteredExercise_data %>%
  group_by(`Exercise Name`, Level, Gender) %>%
  do(data.frame(Percentile = 1:99, Value = calculate_percentiles(., "Max Value")))

# Adjusted function to include exercise name
assign_percentile <- function(performance, exercise, level, gender, weightroom_percentile_data) {
  level_data <- weightroom_percentile_data %>%
    filter(`Exercise Name` == exercise & Level == level & Gender == gender)
  
  closest <- which.min(abs(level_data$Value - performance))
  closest_percentile <- level_data$Percentile[closest]
  
  return(paste0(closest_percentile))
}

# Adjusted to include exercise name in the mapply function
filteredExercise_data$PercentileRank <- mapply(assign_percentile, filteredExercise_data$`Max Value`, filteredExercise_data$`Exercise Name`, filteredExercise_data$Level, filteredExercise_data$Gender, MoreArgs = list(weightroom_percentile_data))

# Writing the output to a CSV file
write_csv(filteredExercise_data, "/Users/watts/Documents/Futures Performance Center/Data/Strength Percentiles/teambuilderPercentiles.csv")

