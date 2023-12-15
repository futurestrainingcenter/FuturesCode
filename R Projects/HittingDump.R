library(ggplot2)
library(dplyr)
library(readxl)
library(magick)
library(readr)
library(knitr)
library(ggpubr)
library(cowplot)
library(grid)
library(gridExtra)
library(qpdf)
library(pdftools)
library(scales)
library(lubridate)
library(tidyverse)
library(showtext)

blastData <- read_csv("/Volumes/COLE'S DATA/Data/Blast Master Data - Sheet1.csv")
hittraxData <- read_csv("/Volumes/COLE'S DATA/Data/Hittrax Master Data - Sheet1.csv")

blastData <- blastData %>%
  arrange(Name, match(Month, month.name)) %>%
  group_by(Name) %>%
  mutate(
    BatSpeed_Monthly_Change = round(`Bat Speed (mph)` - lag(`Bat Speed (mph)`, order_by = match(Month, month.name)), 1),
    BatSpeed_YTD_Change = round(cumsum(coalesce(`Bat Speed (mph)` - lag(`Bat Speed (mph)`, order_by = match(Month, month.name)), 0)), 1),
    
    Rotation_Monthly_Change = round(`Rotational Acceleration (g)` - lag(`Rotational Acceleration (g)`, order_by = match(Month, month.name)), 1),
    Rotation_YTD_Change = round(cumsum(coalesce(`Rotational Acceleration (g)` - lag(`Rotational Acceleration (g)`, order_by = match(Month, month.name)), 0)), 1),
    
    AttackAngle_Monthly_Change = round(`Attack Angle (deg)` - lag(`Attack Angle (deg)`, order_by = match(Month, month.name)), 1),
    AttackAngle_YTD_Change = round(cumsum(coalesce(`Attack Angle (deg)` - lag(`Attack Angle (deg)`, order_by = match(Month, month.name)), 0)), 1),
    
    Power_Monthly_Change = round(`Power (kW)` - lag(`Power (kW)`, order_by = match(Month, month.name)), 1),
    Power_YTD_Change = round(cumsum(coalesce(`Power (kW)` - lag(`Power (kW)`, order_by = match(Month, month.name)), 0)), 1),
    
    VBA_Monthly_Change = round(`Vertical Bat Angle (deg)` - lag(`Vertical Bat Angle (deg)`, order_by = match(Month, month.name)), 1),
    VBA_YTD_Change = round(cumsum(coalesce(`Vertical Bat Angle (deg)` - lag(`Vertical Bat Angle (deg)`, order_by = match(Month, month.name)), 0)), 1),
    
    OPE_Monthly_Change = round(`On Plane Efficiency (%)` - lag(`On Plane Efficiency (%)`, order_by = match(Month, month.name)), 1),
    OPE_YTD_Change = round(cumsum(coalesce(`On Plane Efficiency (%)` - lag(`On Plane Efficiency (%)`, order_by = match(Month, month.name)), 0)), 1),
    
    EC_Monthly_Change = round(`Early Connection (deg)` - lag(`Early Connection (deg)`, order_by = match(Month, month.name)), 1),
    EC_YTD_Change = round(cumsum(coalesce(`Early Connection (deg)` - lag(`Early Connection (deg)`, order_by = match(Month, month.name)), 0)), 1),
    
    CAI_Monthly_Change = round(`Connection at Impact (deg)` - lag(`Connection at Impact (deg)`, order_by = match(Month, month.name)), 1),
    CAI_YTD_Change = round(cumsum(coalesce(`Connection at Impact (deg)` - lag(`Connection at Impact (deg)`, order_by = match(Month, month.name)), 0)), 1),
  )

# Data manipulation for hittraxData
hittraxData <- hittraxData %>%
  mutate(
    Level = case_when(
      Sport == "Baseball" & Age %in% c("8", "9", "10", "11") ~ "Baseball_L1",
      Sport == "Baseball" & Age %in% c("12", "13", "14") ~ "Baseball_L2",
      Sport == "Baseball" & Age %in% c("15", "16", "17", "18") ~ "Baseball_L3",
      Sport == "Baseball" & TRUE ~ "Collegiate",
      Sport == "Softball" & Age %in% c("8", "9", "10", "11") ~ "Softball_L1",
      Sport == "Softball" & Age %in% c("12", "13", "14") ~ "Softball_L2",
      Sport == "Softball" & Age %in% c("15", "16", "17", "18") ~ "Softball_L3",
      Sport == "Softball" & TRUE ~ "Collegiate"
    )
  ) %>%
  arrange(Name, match(Month, month.name)) %>%
  group_by(Name) %>%
  mutate(
    MaxVel_CumMax = cummax(MaxVel),
    MaxDist_CumMax = cummax(MaxDist),
  ) %>%
  mutate(
    MaxVel_Monthly_Change = round(pmax(MaxVel_CumMax - lag(MaxVel_CumMax, order_by = match(Month, month.name), default = first(MaxVel_CumMax)), 0), 1),
    MaxVel_YTD_Change = round(cumsum(coalesce(pmax(MaxVel_CumMax - lag(MaxVel_CumMax, order_by = match(Month, month.name), default = first(MaxVel_CumMax)), 0), 0)), 1),
    
    AvgVel_Monthly_Change = round(AvgVel - lag(AvgVel, order_by = match(Month, month.name)), 1),
    AvgVel_YTD_Change = round(cumsum(coalesce(AvgVel - lag(AvgVel, order_by = match(Month, month.name)), 0)), 1),
    
    MaxDist_Monthly_Change = round(pmax(MaxDist_CumMax - lag(MaxDist_CumMax, order_by = match(Month, month.name), default = first(MaxDist_CumMax)), 0), 1),
    MaxDist_YTD_Change = round(cumsum(coalesce(pmax(MaxDist_CumMax - lag(MaxDist_CumMax, order_by = match(Month, month.name), default = first(MaxDist_CumMax)), 0), 0)), 1),
    
    AvgDist_Monthly_Change = round(AvgDist - lag(AvgDist, order_by = match(Month, month.name)), 1),
    AvgDist_YTD_Change = round(cumsum(coalesce(AvgDist - lag(AvgDist, order_by = match(Month, month.name)), 0)), 1)
  ) %>%
  ungroup() %>%
  group_by(Sport, Level, Month) %>%
  mutate(
    MaxVel_Rank = rank(-MaxVel_CumMax, ties.method = "min"),
    AvgVel_Rank = rank(-AvgVel, ties.method = "min"),
    MaxDist_Rank = rank(-MaxDist_CumMax, ties.method = "min"),
    AvgDist_Rank = rank(-AvgDist, ties.method = "min"),
    Total_Players = n()
  ) %>%
  ungroup()

combined_data <- left_join(hittraxData, blastData, by = c("Name", "Month"))

# Calculate counts for each metric within the specified ranges, grouped by Level
count_attack_angle_level <- combined_data %>%
  group_by(Level) %>%
  summarise(Count = sum(`Attack Angle (deg)` >= 8 & `Attack Angle (deg)` <= 12, na.rm = TRUE),
            Total = n()) %>%
  mutate(Metric = "Attack Angle",
         Counts = paste(Count, "/", Total)) %>%
  select(Metric, Level, Counts)

count_attack_angle_total <- combined_data %>%
  summarise(Count = sum(`Attack Angle (deg)` >= 8 & `Attack Angle (deg)` <= 12, na.rm = TRUE),
            Total = n()) %>%
  mutate(Metric = "Attack Angle",
         Level = "Total",
         Counts = paste(Count, "/", Total)) %>%
  select(Metric, Level, Counts)

count_attack_angle <- bind_rows(count_attack_angle_level, count_attack_angle_total)

# Calculate counts for Early Connection within the specified ranges, grouped by Level
count_early_connection_level <- combined_data %>%
  group_by(Level) %>%
  summarise(Count = sum(`Early Connection (deg)` >= 85 & `Early Connection (deg)` <= 105, na.rm = TRUE),
            Total = n()) %>%
  mutate(Metric = "Early Connection",
         Counts = paste(Count, "/", Total)) %>%
  select(Metric, Level, Counts)

# Calculate aggregated counts for Early Connection across all levels
count_early_connection_total <- combined_data %>%
  summarise(Count = sum(`Early Connection (deg)` >= 85 & `Early Connection (deg)` <= 105, na.rm = TRUE),
            Total = n()) %>%
  mutate(Metric = "Early Connection",
         Level = "Total",
         Counts = paste(Count, "/", Total)) %>%
  select(Metric, Level, Counts)

# Combine level-specific counts with the aggregated counts for Early Connection
count_early_connection <- bind_rows(count_early_connection_level, count_early_connection_total)

# Calculate counts for Connection at Impact within the specified ranges, grouped by Level
count_connection_at_impact_level <- combined_data %>%
  group_by(Level) %>%
  summarise(Count = sum(`Connection at Impact (deg)` >= 85 & `Connection at Impact (deg)` <= 105, na.rm = TRUE),
            Total = n()) %>%
  mutate(Metric = "Connection at Impact",
         Counts = paste(Count, "/", Total)) %>%
  select(Metric, Level, Counts)

# Calculate aggregated counts for Connection at Impact across all levels
count_connection_at_impact_total <- combined_data %>%
  summarise(Count = sum(`Connection at Impact (deg)` >= 85 & `Connection at Impact (deg)` <= 105, na.rm = TRUE),
            Total = n()) %>%
  mutate(Metric = "Connection at Impact",
         Level = "Total",
         Counts = paste(Count, "/", Total)) %>%
  select(Metric, Level, Counts)

# Combine level-specific counts with the aggregated counts for Connection at Impact
count_connection_at_impact <- bind_rows(count_connection_at_impact_level, count_connection_at_impact_total)

# Calculate counts for Vertical Bat Angle within the specified ranges, grouped by Level
count_vertical_bat_angle_level <- combined_data %>%
  group_by(Level) %>%
  summarise(Count = sum(`Vertical Bat Angle (deg)` >= -37 & `Vertical Bat Angle (deg)` <= -27, na.rm = TRUE),
            Total = n()) %>%
  mutate(Metric = "Vertical Bat Angle",
         Counts = paste(Count, "/", Total)) %>%
  select(Metric, Level, Counts)

# Calculate aggregated counts for Vertical Bat Angle across all levels
count_vertical_bat_angle_total <- combined_data %>%
  summarise(Count = sum(`Vertical Bat Angle (deg)` >= -37 & `Vertical Bat Angle (deg)` <= -27, na.rm = TRUE),
            Total = n()) %>%
  mutate(Metric = "Vertical Bat Angle",
         Level = "Total",
         Counts = paste(Count, "/", Total)) %>%
  select(Metric, Level, Counts)

# Combine level-specific counts with the aggregated counts for Vertical Bat Angle
count_vertical_bat_angle <- bind_rows(count_vertical_bat_angle_level, count_vertical_bat_angle_total)

# Combine all counts into one dataframe
counts_df <- bind_rows(count_attack_angle, count_early_connection, count_connection_at_impact, count_vertical_bat_angle)

athlete_data <- combined_data %>% 
  select("Name", "Month", "Sport", "Level", "MaxVel", "MaxVel_Monthly_Change", "MaxVel_YTD_Change", "MaxVel Rank", "AvgVel", 
         "AvgVel_Monthly_Change", "AvgVel_YTD_Change", "AvgVel Rank", "MaxDist", "MaxDist_Monthly_Change", "MaxDist_YTD_Change", 
         "MaxDist Rank", "AvgDist", "AvgDist_Monthly_Change", "AvgDist_YTD_Change", "AvgDist Rank", "Bat Speed (mph)", 
         "BatSpeed_Monthly_Change", "BatSpeed_YTD_Change", "Rotational Acceleration (g)", "Rotation_Monthly_Change", 
         "Rotation_YTD_Change", "Attack Angle (deg)", "AttackAngle_Monthly_Change", "AttackAngle_YTD_Change", "Power (kW)",
         "Power_Monthly_Change", "Power_YTD_Change", "Vertical Bat Angle (deg)", "VBA_Monthly_Change", "VBA_YTD_Change",
         "On Plane Efficiency (%)", "OPE_Monthly_Change", "OPE_YTD_Change", "Early Connection (deg)", "EC_Monthly_Change",
         "EC_YTD_Change", "Connection at Impact (deg)", "CAI_Monthly_Change", "CAI_YTD_Change")

# Add this new dataframe to the athlete_data dataframe (at the end)
athlete_data_updated <- bind_rows(athlete_data, counts_df)

write_csv(athlete_data_updated, "/Volumes/COLE'S DATA/Data/Data Dump/HittingDataDump.csv", na = '')

# Calculate progress categories for each metric
metrics_monthly <- c("BatSpeed_Monthly_Change", "Rotation_Monthly_Change", "AttackAngle_Monthly_Change", "Power_Monthly_Change", 
                     "VBA_Monthly_Change", "OPE_Monthly_Change", "EC_Monthly_Change", "CAI_Monthly_Change", "MaxVel_Monthly_Change", 
                     "AvgVel_Monthly_Change", "MaxDist_Monthly_Change", "AvgDist_Monthly_Change")

metrics_ytd <- c("BatSpeed_YTD_Change", "Rotation_YTD_Change", "AttackAngle_YTD_Change", "Power_YTD_Change", 
                 "VBA_YTD_Change", "OPE_YTD_Change", "EC_YTD_Change", "CAI_YTD_Change", "MaxVel_YTD_Change", 
                 "AvgVel_YTD_Change", "MaxDist_YTD_Change", "AvgDist_YTD_Change")

# Define the thresholds
thresholds <- list(
  AvgVel_Monthly_Change = 2, AvgVel_YTD_Change = 2,
  AvgDist_Monthly_Change = 8, AvgDist_YTD_Change = 8,
  BatSpeed_Monthly_Change = 1.5, BatSpeed_YTD_Change = 1.5,
  Rotation_Monthly_Change = 0.5, Rotation_YTD_Change = 0.5,
  Power_Monthly_Change = 0.25, Power_YTD_Change = 0.25,
  OPE_Monthly_Change = 4, OPE_YTD_Change = 4
)

# Define the months to be processed
months_to_process <- c("September", "October", "November")

# Process each month separately and store the results in a list
monthly_data_list <- list()

for(month in months_to_process) {
  temp_athlete_data <- athlete_data %>%
    filter(Month == month) %>% 
    mutate(Month = month) # Add a column to identify the month
  
  for(metric in c(metrics_monthly, metrics_ytd)) {
    # If the metric has a defined threshold, use it. Otherwise, default to 0.
    threshold <- ifelse(metric %in% names(thresholds), thresholds[[metric]], 0)
    
    temp_athlete_data <- temp_athlete_data %>%
      mutate(!!paste0(metric, "_Category") := case_when(
        is.na(!!sym(metric)) ~ "No Data",
        !!sym(metric) > threshold ~ "Progressed",
        !!sym(metric) < -threshold ~ "Regressed",
        TRUE ~ "No Gain"
      ))
  }
  
  # Add the processed data for the month to the list
  monthly_data_list[[month]] <- temp_athlete_data
}

# Combine the data for all months
combined_athlete_data <- do.call(rbind, monthly_data_list)

# Calculate percentages for each metric and month
progress_summary <- data.frame(Level = character(),
                               Month = character(), # Add Month column
                               Category = character(),
                               Count = character(),
                               Percentage = double(),
                               Metric = character())

levels <- c("Total", "Baseball_L1", "Baseball_L2", "Baseball_L3", "Softball_L1", "Softball_L2", "Softball_L3", "Collegiate")

for(level in levels) {
  for(month in months_to_process) { # Iterate through each month
    for(metric in c(metrics_monthly, metrics_ytd)) {
      
      if(level == "Total") {
        temp_data <- combined_athlete_data %>% filter(Month == month)
      } else {
        temp_data <- combined_athlete_data %>% filter(Level == level, Month == month)
      }
      
      temp <- temp_data %>%
        group_by(Category = !!sym(paste0(metric, "_Category"))) %>%
        summarise(Count_raw = n()) %>%
        mutate(Percentage = round(Count_raw / sum(Count_raw) * 100, 2), # Round to 2 decimal points
               Metric = metric, 
               Level = level,
               Month = month, # Add Month to the summary
               Count = paste0(Count_raw, "/", sum(Count_raw))) %>% # Add fraction representation
        select(-Count_raw) # Remove the raw count column
      
      progress_summary <- bind_rows(progress_summary, temp)
    }
  }
}

# Write out the results
write_csv(progress_summary, "/Volumes/COLE'S DATA/Data/Data Dump/HittingProgressData.csv", na = '')


#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################

trackmanData <- read_csv("/Users/watts/Downloads/Trackman Master - Sheet1.csv") %>%
  mutate(Name = str_split(Pitcher, pattern = ", ", simplify = TRUE) %>% 
           apply(1, function(x) paste(x[2], x[1])),
         Month = format(ymd(Date), "%B"))

armCareData <- read_csv("/Users/watts/Downloads/Armcare Master - Sheet1.csv")
armCareData$`Exam Date` <- mdy(armCareData$`Exam Date`)

armCareData <- armCareData %>%
  mutate(
    Name = paste(`First Name`, `Last Name`),
    Month = month(`Exam Date`, label = TRUE, abbr = FALSE)) %>%
  filter(
    `Exam Type` %in% c("Fresh - Quick", "Fresh - Full"),
    Month %in% c("October", "November"))

clientData <- read_csv("/Users/watts/Downloads/FullClientList.csv") %>% 
  rename(Name = Client)


pitching_data <- full_join(trackmanData, armCareData, by = "Name")
final_pitching_data <- left_join(pitching_data, clientData, by = "Name")

final_pitching_data <- final_pitching_data %>% 
  select(Name, `Reporting Level (Age-Dependent)`, Month.x, Month.y, TaggedPitchType, RelSpeed, `Arm Score`, `Total Strength`)

write_csv(final_pitching_data, "/Volumes/COLE'S DATA/Data/Data Dump/PitchingDataDump.csv", na = '')


#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################

teambuildrData <- read_csv("/Users/watts/Downloads/Teambuildr Raw Data Report.csv") %>%
  filter(`Exercise Name` %in% c('Trap Bar Deadlift', 'Barbell Bench Press', 'Barbell Back Squat')) %>%
  mutate(Month = format(ymd(`Completed Date`), "%B"),
         Name = paste(`First Name`, `Last Name`))

clientData <- read_csv("/Users/watts/Downloads/FullClientList.csv") %>% 
  rename(Name = Client)

final_teambuildr_data <- left_join(teambuildrData, clientData, by = "Name")%>% 
  select(Name, `Reporting Level (Age-Dependent)`, Month, `Exercise Name`, `Highest Max`)

write_csv(final_teambuildr_data, "/Volumes/COLE'S DATA/Data/Data Dump/TeambuilderDataDump.csv", na = '')

#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################

speedData <- read_csv("/Volumes/COLE'S DATA/Data/Speed Report Data/MaxVelocityPercentiles.csv") %>% 
  mutate(Month = format(dmy(Date), "%B")) %>% 
  select(FullName, Month, Level, `10 yard fly`, MPH)

write_csv(speedData, "/Volumes/COLE'S DATA/Data/Data Dump/SpeedDataDump.csv", na = '')







