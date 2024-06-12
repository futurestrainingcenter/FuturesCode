library(dplyr)
library(readxl)
library(readr)
library(lubridate)

clientData <- read_csv("/Users/watts/Downloads/FullClientList.csv") %>% 
  rename(FullName = Client, Level = `Sp Level`) %>% 
  select(FullName, Level, Gender)

sprintData <- read.csv("/Users/watts/Downloads/FuturesSprint.csv")
sprintData$Date <- as.Date(sprintData$Date, format="%d/%m/%Y")

###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################

hardNinety <- sprintData %>%
  mutate(FullName = paste(GivenName, FamilyName),
         Month = month(Date, label = TRUE, abbr = FALSE)) %>% 
  select(Date, Month, Name, FullName, Split1, Split2, Split3, Cumulative1, Cumulative2, Cumulative3)
  
hardNinety <- left_join(hardNinety, clientData, by = "FullName") %>% 
  filter(!is.na(Gender), !is.na(Level), Level != "N/A")

Q1 <- quantile(hardNinety$Cumulative2, 0.25, na.rm = TRUE)
Q3 <- quantile(hardNinety$Cumulative2, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

filtered_data <- hardNinety %>%
  filter(Cumulative2 >= lower_bound & Cumulative2 <= upper_bound)

# Function to calculate all percentiles
calculate_percentiles <- function(data, column) {
  sapply(1:99, function(p) quantile(data[[column]], probs = (100-p)/100, na.rm = TRUE))
}

# Calculating percentiles for each level
percentile_data_hardNinety <- filtered_data %>%
  group_by(Level, Gender) %>%
  do(data.frame(Percentile = 1:99, Value = calculate_percentiles(., "Cumulative2")))

assign_percentile <- function(performance, level, gender, percentile_data_hardNinety) {
  
  # Filter the percentile data for the specific level
  level_data <- percentile_data_hardNinety %>% filter(Level == level, Gender == gender)
  
  # Find the closest percentile
  closest <- which.min(abs(level_data$Value - performance))
  closest_percentile <- level_data$Percentile[closest]
  
  return(paste0(closest_percentile))
}

# Applying the function to each athlete
filtered_data$PercentileRank <- mapply(assign_percentile, filtered_data$Cumulative2, filtered_data$Level, filtered_data$Gender, MoreArgs = list(percentile_data_hardNinety))

write_csv(filtered_data, "/Users/watts/Documents/Futures Performance Center/Data/Speed Percentiles/Hard90percentiles.csv", na = '')

###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################


accelerationData <- sprintData %>%
  mutate(FullName = paste(GivenName, FamilyName),
         Month = month(Date, label = TRUE, abbr = FALSE)) %>% 
  select(Date, Month, Name, FullName, Split1)

accelerationData <- left_join(accelerationData, clientData, by = "FullName") %>% 
  filter(!is.na(Gender), !is.na(Level), Level != "N/A")

# Filtering and calculation of percentiles by Level
Q1 <- quantile(accelerationData$Split1, 0.25, na.rm = TRUE)
Q3 <- quantile(accelerationData$Split1, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

filtered_data <- accelerationData %>%
  filter(Split1 >= lower_bound & Split1 <= upper_bound)

calculate_percentiles <- function(data, column) {
  sapply(1:99, function(p) quantile(data[[column]], probs = (100-p)/100, na.rm = TRUE))
}

percentile_data_acc <- filtered_data %>%
  group_by(Level, Gender) %>%
  do(data.frame(Percentile = 1:99, Value = calculate_percentiles(., "Split1")))

assign_percentile <- function(performance, level, gender, percentile_data_acc) {
  
  level_data <- percentile_data_acc %>% filter(Level == level & Gender == gender)
  
  closest <- which.min(abs(level_data$Value - performance))
  closest_percentile <- level_data$Percentile[closest]
  
  return(paste0(closest_percentile))
}

filtered_data$PercentileRank <- mapply(assign_percentile, filtered_data$Split1, filtered_data$Level, filtered_data$Gender, MoreArgs = list(percentile_data_acc))

write_csv(filtered_data, "/Users/watts/Documents/Futures Performance Center/Data/Speed Percentiles/AccelerationPercentiles.csv", na = '')

###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################


maxVeloData <- sprintData %>%
  mutate(FullName = paste(GivenName, FamilyName),
         `10 yard fly` = Cumulative3 - Cumulative2,
         Month = month(Date, label = TRUE, abbr = FALSE)) %>% 
  select(Date, Month, Name, FullName, `10 yard fly`)

maxVeloData <- maxVeloData %>% 
  mutate(MPH = round((10 / 1760) * (3600 / `10 yard fly`), digits = 3))

maxVeloData <- left_join(maxVeloData, clientData, by = "FullName") %>% 
  filter(!is.na(Gender), !is.na(Level), Level != "N/A")

# Filtering and calculation of percentiles by Level
Q1 <- quantile(maxVeloData$MPH, 0.25, na.rm = TRUE)
Q3 <- quantile(maxVeloData$MPH, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

filtered_data <- maxVeloData %>%
  filter(MPH >= lower_bound & MPH <= upper_bound)

calculate_percentiles <- function(data, column) {
  sapply(1:99, function(p) quantile(data[[column]], probs = p/100, na.rm = TRUE))
}

percentile_data_maxVelo <- filtered_data %>%
  group_by(Level, Gender) %>%
  do(data.frame(Percentile = 1:99, Value = calculate_percentiles(., "MPH")))

assign_percentile <- function(performance, level, gender, percentile_data_maxVelo) {
  level_data <- percentile_data_maxVelo %>% filter(Level == level & Gender == gender)
  closest <- which.min(abs(level_data$Value - performance))
  closest_percentile <- level_data$Percentile[closest]
  return(paste0(closest_percentile))
}

filtered_data$PercentileRank <- mapply(assign_percentile, filtered_data$MPH, filtered_data$Level, filtered_data$Gender, MoreArgs = list(percentile_data_maxVelo))

write_csv(filtered_data, "/Users/watts/Documents/Futures Performance Center/Data/Speed Percentiles/MaxVelocityPercentiles.csv", na = '')

###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################


fortyYard <- sprintData %>%
  mutate(FullName = paste(GivenName, FamilyName),
         Month = month(Date, label = TRUE, abbr = FALSE)) %>% 
  select(Date, Month, Name, FullName, Split1, Split2, Split3, Cumulative1, Cumulative2, Cumulative3)

fortyYard <- left_join(fortyYard, clientData, by = "FullName") %>% 
  filter(!is.na(Gender), !is.na(Level), Level != "N/A")

Q1 <- quantile(fortyYard$Cumulative3, 0.25, na.rm = TRUE)
Q3 <- quantile(fortyYard$Cumulative3, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

filtered_data <- fortyYard %>%
  filter(Cumulative3 >= lower_bound & Cumulative3 <= upper_bound)

# Function to calculate all percentiles
calculate_percentiles <- function(data, column) {
  sapply(1:99, function(p) quantile(data[[column]], probs = (100-p)/100, na.rm = TRUE))
}

# Calculating percentiles for each level
percentile_data_fortyDash <- filtered_data %>%
  group_by(Level, Gender) %>%
  do(data.frame(Percentile = 1:99, Value = calculate_percentiles(., "Cumulative3")))

assign_percentile <- function(performance, level, gender, percentile_data_fortyDash) {
  
  # Filter the percentile data for the specific level
  level_data <- percentile_data_fortyDash %>% filter(Level == level, Gender == gender)
  
  # Find the closest percentile
  closest <- which.min(abs(level_data$Value - performance))
  closest_percentile <- level_data$Percentile[closest]
  
  return(paste0(closest_percentile))
}

# Applying the function to each athlete
filtered_data$PercentileRank <- mapply(assign_percentile, filtered_data$Cumulative3, filtered_data$Level, filtered_data$Gender, MoreArgs = list(percentile_data_fortyDash))

write_csv(filtered_data, "/Users/watts/Documents/Futures Performance Center/Data/Speed Percentiles/40yardpercentiles.csv", na = '')

###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################

RSIdata <- read_csv("/Users/watts/Downloads/RSI_data.csv")
RSIdata$Date <- as.Date(RSIdata$Date, format="%m/%d/%Y")

RSIdata <- RSIdata %>% 
  rename(FullName = Name, Name = `Test Type`) %>% 
  mutate(Month = month(Date, label = TRUE, abbr = FALSE)) %>% 
  select(Date, Month, FullName, Name, `BW [KG]`, `Mean RSI (Jump Height/Contact Time) [m/s]`, `Jump Height (Flight Time) [cm]`, `Contact Time [ms]`)

RSIdata <- left_join(RSIdata, clientData, by = "FullName") %>% 
  filter(!is.na(Gender), !is.na(Level), Level != "N/A")

# Filtering and calculation of percentiles by Level
Q1 <- quantile(RSIdata$`Mean RSI (Jump Height/Contact Time) [m/s]`, 0.25, na.rm = TRUE)
Q3 <- quantile(RSIdata$`Mean RSI (Jump Height/Contact Time) [m/s]`, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

filtered_data <- RSIdata %>%
  filter(`Mean RSI (Jump Height/Contact Time) [m/s]` >= lower_bound & `Mean RSI (Jump Height/Contact Time) [m/s]` <= upper_bound)

calculate_percentiles <- function(data, column) {
  sapply(1:99, function(p) quantile(data[[column]], probs = p/100, na.rm = TRUE))
}

percentile_data_RSI <- filtered_data %>%
  group_by(Level, Gender) %>%
  do(data.frame(Percentile = 1:99, Value = calculate_percentiles(., "Mean RSI (Jump Height/Contact Time) [m/s]")))

assign_percentile <- function(performance, level, gender, percentile_data_RSI) {
  level_data <- percentile_data_RSI %>% filter(Level == level & Gender == gender)
  closest <- which.min(abs(level_data$Value - performance))
  closest_percentile <- level_data$Percentile[closest]
  return(paste0(closest_percentile))
}

filtered_data$PercentileRank <- mapply(assign_percentile, filtered_data$`Mean RSI (Jump Height/Contact Time) [m/s]`, filtered_data$Level, filtered_data$Gender, MoreArgs = list(percentile_data_RSI))

write_csv(filtered_data, "/Users/watts/Documents/Futures Performance Center/Data/Speed Percentiles/RSIpercentiles.csv", na = '')

###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################


# learningAcademySprint <- read.csv("/Users/watts/Downloads/40yardSprint_data.csv")
# learningAcademySprint <- learningAcademySprint %>%
#   mutate(FullName = paste(GivenName, FamilyName)) %>% 
#   mutate(FullName = trimws(FullName)) %>%  # Trimming any spaces
#   select(Date, FullName, Distance1, Split1)
# 
# maxVeloData <- read_csv("/Users/watts/Downloads/Flying10ydSprint_data.csv")
# maxVeloData <- maxVeloData %>%
#   mutate(FullName = paste(GivenName, FamilyName)) %>%
#   mutate(FullName = trimws(FullName)) %>%  # Trimming any spaces
#   select(Date, FullName, Distance1, Split1)
# 
# # Corrected left join
# combined_data <- left_join(learningAcademySprint, maxVeloData, by = c("FullName", "Date"))
# 
# combined_data <- combined_data %>% 
#   group_by(FullName, Date) %>% 
#   mutate(`30yard` = Split1.x - Split1.y) %>% 
#   na.omit()
# 
# final_data <- left_join(combined_data, clientData, by = "FullName")
# 
# # Filtering and calculation of percentiles by Level
# Q1 <- quantile(final_data$`30yard`, 0.25, na.rm = TRUE)
# Q3 <- quantile(final_data$`30yard`, 0.75, na.rm = TRUE)
# IQR <- Q3 - Q1
# 
# lower_bound <- Q1 - 1.5 * IQR
# upper_bound <- Q3 + 1.5 * IQR
# 
# filtered_data <- final_data %>%
#   filter(`30yard` >= lower_bound & `30yard` <= upper_bound)
# 
# filtered_data["Level"][is.na(filtered_data["Level"])] <- "Unknown"
# filtered_data["Gender"][is.na(filtered_data["Gender"])] <- "Unknown"
# 
# calculate_percentiles <- function(data, column) {
#   sapply(1:99, function(p) quantile(data[[column]], probs = (100-p)/100, na.rm = TRUE))
# }
# 
# percentile_data <- filtered_data %>%
#   group_by(Level, Gender) %>%
#   do(data.frame(Percentile = 1:99, Value = calculate_percentiles(., "30yard")))
# 
# assign_percentile <- function(performance, level, gender, percentile_data) {
#   level_data <- percentile_data %>% filter(Level == level & Gender == gender)
#   closest <- which.min(abs(level_data$Value - performance))
#   closest_percentile <- level_data$Percentile[closest]
#   return(paste0(closest_percentile))
# }
# 
# filtered_data$PercentileRank <- mapply(assign_percentile, filtered_data$`30yard`, filtered_data$Level, filtered_data$Gender, MoreArgs = list(percentile_data))
# 
# write_csv(filtered_data, "/Users/watts/Documents/Futures Performance Center/Data/Speed Percentiles/30yard_LearningAcademy.csv", na = '')



