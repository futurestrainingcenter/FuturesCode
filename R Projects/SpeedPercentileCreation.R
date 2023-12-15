library(dplyr)
library(readxl)
library(readr)
library(lubridate)

clientData <- read.csv("/Users/watts/Downloads/FullClientList.csv") %>% 
  rename(FullName = Client, Level = Reporting.Level..Age.Dependent.)

clientData <- clientData %>% 
  select(FullName, Level, Gender)

###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################

hardNinety <- read.csv("/Users/watts/Downloads/FuturesSprint_10_30_40.csv")
hardNinety <- hardNinety %>%
  mutate(FullName = paste(GivenName, FamilyName))

hardNinety <- left_join(hardNinety, clientData, by = "FullName")

Q1 <- quantile(hardNinety$Cumulative2, 0.25, na.rm = TRUE)
Q3 <- quantile(hardNinety$Cumulative2, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

filtered_data <- hardNinety %>%
  filter(Cumulative2 >= lower_bound & Cumulative2 <= upper_bound)

filtered_data["Level"][is.na(filtered_data["Level"])] <- "Unknown"
filtered_data["Gender"][is.na(filtered_data["Gender"])] <- "Unknown"

# Function to calculate all percentiles
calculate_percentiles <- function(data, column) {
  sapply(1:99, function(p) quantile(data[[column]], probs = (100-p)/100, na.rm = TRUE))
}

# Calculating percentiles for each level
percentile_data <- filtered_data %>%
  group_by(Level, Gender) %>%
  do(data.frame(Percentile = 1:99, Value = calculate_percentiles(., "Cumulative2")))

assign_percentile <- function(performance, level, gender, percentile_data) {
  
  # Filter the percentile data for the specific level
  level_data <- percentile_data %>% filter(Level == level, Gender == gender)
  
  # Find the closest percentile
  closest <- which.min(abs(level_data$Value - performance))
  closest_percentile <- level_data$Percentile[closest]
  
  return(paste0(closest_percentile))
}

# Applying the function to each athlete
filtered_data$PercentileRank <- mapply(assign_percentile, filtered_data$Cumulative2, filtered_data$Level, filtered_data$Gender, MoreArgs = list(percentile_data))

write_csv(filtered_data, "/Volumes/COLE'S DATA/Data/Speed Report Data/Hard90percentiles.csv", na = '')

###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################

futuresSprint <- read_csv("/Users/watts/Downloads/FuturesSprint_10_30_40.csv")
futuresSprint <- futuresSprint %>%
  mutate(FullName = paste(GivenName, FamilyName)) %>% 
  select(Date, FullName, Velocity1, Split1)

accelerationData <- read_csv("/Users/watts/Downloads/10ydSprint_data.csv")
accelerationData <- accelerationData %>%
  mutate(FullName = paste(GivenName, FamilyName)) %>% 
  select(Date, FullName, Velocity1, Split1)

combined_acceleration <- bind_rows(accelerationData, futuresSprint)

combined_acceleration <- combined_acceleration %>%
  mutate(Acceleration = round(Velocity1 / Split1, digits = 3))

combined_acceleration <- left_join(combined_acceleration, clientData, by = "FullName")

# Filtering and calculation of percentiles by Level
Q1 <- quantile(combined_acceleration$Acceleration, 0.25, na.rm = TRUE)
Q3 <- quantile(combined_acceleration$Acceleration, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

filtered_data <- combined_acceleration %>%
  filter(Acceleration >= lower_bound & Acceleration <= upper_bound)

filtered_data["Level"][is.na(filtered_data["Level"])] <- "Unknown"
filtered_data["Gender"][is.na(filtered_data["Gender"])] <- "Unknown"

calculate_percentiles <- function(data, column) {
  sapply(1:99, function(p) quantile(data[[column]], probs = p/100, na.rm = TRUE))
}

percentile_data <- filtered_data %>%
  group_by(Level, Gender) %>%
  do(data.frame(Percentile = 1:99, Value = calculate_percentiles(., "Acceleration")))

assign_percentile <- function(performance, level, gender, percentile_data) {
  level_data <- percentile_data %>% filter(Level == level & Gender == gender)
  closest <- which.min(abs(level_data$Value - performance))
  closest_percentile <- level_data$Percentile[closest]
  return(paste0(closest_percentile))
}

filtered_data$PercentileRank <- mapply(assign_percentile, filtered_data$Acceleration, filtered_data$Level, filtered_data$Gender, MoreArgs = list(percentile_data))

write_csv(filtered_data, "/Volumes/COLE'S DATA/Data/Speed Report Data/AccelerationPercentiles.csv", na = '')

###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################


futuresSprint <- read.csv("/Users/watts/Downloads/FuturesSprint_10_30_40.csv")
futuresSprint <- futuresSprint %>%
  mutate(FullName = paste(GivenName, FamilyName),
         `10 yard fly` = Cumulative3 - Cumulative2) %>% 
  select(Date, FullName, Distance1, `10 yard fly`)

maxVeloData <- read_csv("/Users/watts/Downloads/Flying10ydSprint_data.csv")
maxVeloData <- maxVeloData %>%
  rename(`10 yard fly` = Split1) %>% 
  mutate(FullName = paste(GivenName, FamilyName)) %>% 
  select(Date, FullName, Distance1, `10 yard fly`)

combined_maxVelo <- bind_rows(maxVeloData, futuresSprint)

combined_maxVelo <- combined_maxVelo %>% 
  mutate(MPH = round((Distance1 / 1760) * (3600 / `10 yard fly`), digits = 3))

combined_maxVelo <- left_join(combined_maxVelo, clientData, by = "FullName")

# Filtering and calculation of percentiles by Level
Q1 <- quantile(combined_maxVelo$MPH, 0.25, na.rm = TRUE)
Q3 <- quantile(combined_maxVelo$MPH, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

filtered_data <- combined_maxVelo %>%
  filter(MPH >= lower_bound & MPH <= upper_bound)

filtered_data["Level"][is.na(filtered_data["Level"])] <- "Unknown"
filtered_data["Gender"][is.na(filtered_data["Gender"])] <- "Unknown"

calculate_percentiles <- function(data, column) {
  sapply(1:99, function(p) quantile(data[[column]], probs = p/100, na.rm = TRUE))
}

percentile_data <- filtered_data %>%
  group_by(Level, Gender) %>%
  do(data.frame(Percentile = 1:99, Value = calculate_percentiles(., "MPH")))

assign_percentile <- function(performance, level, gender, percentile_data) {
  level_data <- percentile_data %>% filter(Level == level & Gender == gender)
  closest <- which.min(abs(level_data$Value - performance))
  closest_percentile <- level_data$Percentile[closest]
  return(paste0(closest_percentile))
}

filtered_data$PercentileRank <- mapply(assign_percentile, filtered_data$MPH, filtered_data$Level, filtered_data$Gender, MoreArgs = list(percentile_data))

write_csv(filtered_data, "/Volumes/COLE'S DATA/Data/Speed Report Data/MaxVelocityPercentiles.csv", na = '')


###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################

RSIdata <- read_csv("/Users/watts/Downloads/RSIhoptest_data.csv")
RSIdata <- RSIdata %>% 
  rename(FullName = Name)

RSIdata <- left_join(RSIdata, clientData, by = "FullName")

# Filtering and calculation of percentiles by Level
Q1 <- quantile(RSIdata$`Mean RSI (Jump Height/Contact Time) [m/s]`, 0.25, na.rm = TRUE)
Q3 <- quantile(RSIdata$`Mean RSI (Jump Height/Contact Time) [m/s]`, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

filtered_data <- RSIdata %>%
  filter(`Mean RSI (Jump Height/Contact Time) [m/s]` >= lower_bound & `Mean RSI (Jump Height/Contact Time) [m/s]` <= upper_bound)

filtered_data["Level"][is.na(filtered_data["Level"])] <- "Unknown"
filtered_data["Gender"][is.na(filtered_data["Gender"])] <- "Unknown"

calculate_percentiles <- function(data, column) {
  sapply(1:99, function(p) quantile(data[[column]], probs = p/100, na.rm = TRUE))
}

percentile_data <- filtered_data %>%
  group_by(Level, Gender) %>%
  do(data.frame(Percentile = 1:99, Value = calculate_percentiles(., "Mean RSI (Jump Height/Contact Time) [m/s]")))

assign_percentile <- function(performance, level, gender, percentile_data) {
  level_data <- percentile_data %>% filter(Level == level & Gender == gender)
  closest <- which.min(abs(level_data$Value - performance))
  closest_percentile <- level_data$Percentile[closest]
  return(paste0(closest_percentile))
}

filtered_data$PercentileRank <- mapply(assign_percentile, filtered_data$`Mean RSI (Jump Height/Contact Time) [m/s]`, filtered_data$Level, filtered_data$Gender, MoreArgs = list(percentile_data))

write_csv(filtered_data, "/Volumes/COLE'S DATA/Data/Speed Report Data/RSIpercentiles.csv", na = '')

###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################


learningAcademySprint <- read.csv("/Users/watts/Downloads/40yardSprint_data.csv")
learningAcademySprint <- learningAcademySprint %>%
  mutate(FullName = paste(GivenName, FamilyName)) %>% 
  mutate(FullName = trimws(FullName)) %>%  # Trimming any spaces
  select(Date, FullName, Distance1, Split1)

maxVeloData <- read_csv("/Users/watts/Downloads/Flying10ydSprint_data.csv")
maxVeloData <- maxVeloData %>%
  mutate(FullName = paste(GivenName, FamilyName)) %>%
  mutate(FullName = trimws(FullName)) %>%  # Trimming any spaces
  select(Date, FullName, Distance1, Split1)

# Corrected left join
combined_data <- left_join(learningAcademySprint, maxVeloData, by = c("FullName", "Date"))

combined_data <- combined_data %>% 
  group_by(FullName, Date) %>% 
  mutate(`30yard` = Split1.x - Split1.y) %>% 
  na.omit()

final_data <- left_join(combined_data, clientData, by = "FullName")

# Filtering and calculation of percentiles by Level
Q1 <- quantile(final_data$`30yard`, 0.25, na.rm = TRUE)
Q3 <- quantile(final_data$`30yard`, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

filtered_data <- final_data %>%
  filter(`30yard` >= lower_bound & `30yard` <= upper_bound)

filtered_data["Level"][is.na(filtered_data["Level"])] <- "Unknown"
filtered_data["Gender"][is.na(filtered_data["Gender"])] <- "Unknown"

calculate_percentiles <- function(data, column) {
  sapply(1:99, function(p) quantile(data[[column]], probs = (100-p)/100, na.rm = TRUE))
}

percentile_data <- filtered_data %>%
  group_by(Level, Gender) %>%
  do(data.frame(Percentile = 1:99, Value = calculate_percentiles(., "30yard")))

assign_percentile <- function(performance, level, gender, percentile_data) {
  level_data <- percentile_data %>% filter(Level == level & Gender == gender)
  closest <- which.min(abs(level_data$Value - performance))
  closest_percentile <- level_data$Percentile[closest]
  return(paste0(closest_percentile))
}

filtered_data$PercentileRank <- mapply(assign_percentile, filtered_data$`30yard`, filtered_data$Level, filtered_data$Gender, MoreArgs = list(percentile_data))

write_csv(filtered_data, "/Volumes/COLE'S DATA/Data/Speed Report Data/30yard_LearningAcademy.csv", na = '')



