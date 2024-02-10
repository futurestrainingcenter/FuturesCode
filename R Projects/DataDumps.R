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

#######################################################################################################
#######################################################################################################
##############################################  HITTING  ##############################################
#######################################################################################################
#######################################################################################################
blastData <- read_csv("/Volumes/COLE'S DATA/Data/Blast Master Data - Sheet1.csv")
hittraxData <- read_csv("/Volumes/COLE'S DATA/Data/Hittrax Master Data - Sheet1.csv")
clientData <- read_csv("/Users/watts/Downloads/FullClientList.csv") %>% 
  rename(Name = Client, Level = `Reporting Level (Age-Dependent)`)

calculate_age <- function(birthdate) {
  if (is.na(birthdate)) {
    return(NA)
  } else {
    birthdate <- mdy(birthdate) # Convert to Date using lubridate
    age <- interval(start = birthdate, end = Sys.Date()) / years(1)
    return(floor(age)) # Floor the age to get complete years
  }
}

clientData$Age <- sapply(clientData$`field-general-7.dl_date`, calculate_age)

blastData <- blastData %>%
  mutate(Date = make_date(Year, match(Month, month.name), 1)) %>% # Create a Date column combining Year and Month
  arrange(Name, Date) %>%
  group_by(Name) %>%
  mutate(
    BatSpeed_Monthly_Change = round(`Bat Speed (mph)` - lag(`Bat Speed (mph)`, order_by = Date), 1),
    BatSpeed_YTD_Change = round(cumsum(coalesce(`Bat Speed (mph)` - lag(`Bat Speed (mph)`, order_by = Date), 0)), 1),
    
    BatSpeed_Monthly_Percent_Change = ifelse(lag(`Bat Speed (mph)`) == 0, NA, 
                                             round(100 * (`Bat Speed (mph)` - lag(`Bat Speed (mph)`)) / lag(`Bat Speed (mph)`), 1)),
    BatSpeed_YTD_Percent_Change = ifelse(lag(`Bat Speed (mph)`) == 0, NA, 
                                         round(100 * (BatSpeed_YTD_Change / lag(`Bat Speed (mph)`)), 1)),
    
    Rotation_Monthly_Change = round(`Rotational Acceleration (g)` - lag(`Rotational Acceleration (g)`, order_by = Date), 1),
    Rotation_YTD_Change = round(cumsum(coalesce(`Rotational Acceleration (g)` - lag(`Rotational Acceleration (g)`, order_by = Date), 0)), 1),
    
    Rotation_Monthly_Percent_Change = ifelse(lag(`Rotational Acceleration (g)`) == 0, NA, 
                                             round(100 * (`Rotational Acceleration (g)` - lag(`Rotational Acceleration (g)`)) / lag(`Rotational Acceleration (g)`), 1)),
    Rotation_YTD_Percent_Change = ifelse(lag(`Rotational Acceleration (g)`) == 0, NA, 
                                         round(100 * (Rotation_YTD_Change / lag(`Rotational Acceleration (g)`)), 1)),
    
    AttackAngle_Monthly_Change = round(`Attack Angle (deg)` - lag(`Attack Angle (deg)`, order_by = Date), 1),
    AttackAngle_YTD_Change = round(cumsum(coalesce(`Attack Angle (deg)` - lag(`Attack Angle (deg)`, order_by = Date), 0)), 1),
    
    AttackAngle_Monthly_Percent_Change = ifelse(lag(`Attack Angle (deg)`) == 0, NA, 
                                             round(100 * (`Attack Angle (deg)` - lag(`Attack Angle (deg)`)) / lag(`Attack Angle (deg)`), 1)),
    AttackAngle_YTD_Percent_Change = ifelse(lag(`Attack Angle (deg)`) == 0, NA, 
                                         round(100 * (AttackAngle_YTD_Change / lag(`Attack Angle (deg)`)), 1)),
    
    Power_Monthly_Change = round(`Power (kW)` - lag(`Power (kW)`, order_by = Date), 1),
    Power_YTD_Change = round(cumsum(coalesce(`Power (kW)` - lag(`Power (kW)`, order_by = Date), 0)), 1),
    
    Power_Monthly_Percent_Change = ifelse(lag(`Power (kW)`) == 0, NA, 
                                                round(100 * (`Power (kW)` - lag(`Power (kW)`)) / lag(`Power (kW)`), 1)),
    Power_YTD_Percent_Change = ifelse(lag(`Power (kW)`) == 0, NA, 
                                            round(100 * (Power_YTD_Change / lag(`Power (kW)`)), 1)),
    
    VBA_Monthly_Change = round(`Vertical Bat Angle (deg)` - lag(`Vertical Bat Angle (deg)`, order_by = Date), 1),
    VBA_YTD_Change = round(cumsum(coalesce(`Vertical Bat Angle (deg)` - lag(`Vertical Bat Angle (deg)`, order_by = Date), 0)), 1),
    
    VBA_Monthly_Percent_Change = ifelse(lag(`Vertical Bat Angle (deg)`) == 0, NA, 
                                          round(100 * (`Vertical Bat Angle (deg)` - lag(`Vertical Bat Angle (deg)`)) / lag(`Vertical Bat Angle (deg)`), 1)),
    VBA_YTD_Percent_Change = ifelse(lag(`Vertical Bat Angle (deg)`) == 0, NA, 
                                      round(100 * (VBA_YTD_Change / lag(`Vertical Bat Angle (deg)`)), 1)),
    
    OPE_Monthly_Change = round(`On Plane Efficiency (%)` - lag(`On Plane Efficiency (%)`, order_by = Date), 1),
    OPE_YTD_Change = round(cumsum(coalesce(`On Plane Efficiency (%)` - lag(`On Plane Efficiency (%)`, order_by = Date), 0)), 1),
    
    OPE_Monthly_Percent_Change = ifelse(lag(`On Plane Efficiency (%)`) == 0, NA, 
                                        round(100 * (`On Plane Efficiency (%)` - lag(`On Plane Efficiency (%)`)) / lag(`On Plane Efficiency (%)`), 1)),
    OPE_YTD_Percent_Change = ifelse(lag(`On Plane Efficiency (%)`) == 0, NA, 
                                    round(100 * (OPE_YTD_Change / lag(`On Plane Efficiency (%)`)), 1)),
    
    EC_Monthly_Change = round(`Early Connection (deg)` - lag(`Early Connection (deg)`, order_by = Date), 1),
    EC_YTD_Change = round(cumsum(coalesce(`Early Connection (deg)` - lag(`Early Connection (deg)`, order_by = Date), 0)), 1),
    
    EC_Monthly_Percent_Change = ifelse(lag(`Early Connection (deg)`) == 0, NA, 
                                        round(100 * (`Early Connection (deg)` - lag(`Early Connection (deg)`)) / lag(`Early Connection (deg)`), 1)),
    EC_YTD_Percent_Change = ifelse(lag(`Early Connection (deg)`) == 0, NA, 
                                    round(100 * (EC_YTD_Change / lag(`Early Connection (deg)`)), 1)),
    
    CAI_Monthly_Change = round(`Connection at Impact (deg)` - lag(`Connection at Impact (deg)`, order_by = Date), 1),
    CAI_YTD_Change = round(cumsum(coalesce(`Connection at Impact (deg)` - lag(`Connection at Impact (deg)`, order_by = Date), 0)), 1),
    
    CAI_Monthly_Percent_Change = ifelse(lag(`Connection at Impact (deg)`) == 0, NA, 
                                       round(100 * (`Connection at Impact (deg)` - lag(`Connection at Impact (deg)`)) / lag(`Connection at Impact (deg)`), 1)),
    CAI_YTD_Percent_Change = ifelse(lag(`Connection at Impact (deg)`) == 0, NA, 
                                   round(100 * (EC_YTD_Change / lag(`Connection at Impact (deg)`)), 1))
  )

hittraxData$`Date of Birth` <- as.Date(hittraxData$`Date of Birth`, format = "%B %d %Y")
hittraxData <- left_join(hittraxData, clientData, by = "Name")

hittraxData <- hittraxData %>%
  mutate(Date = make_date(Year, match(Month, month.name), 1)) %>% # Similar to blastData, create a Date column
  arrange(Name, Date) %>%
  group_by(Name) %>%
  mutate(
    MaxVel_CumMax = cummax(MaxVel),
    MaxDist_CumMax = cummax(MaxDist),
  ) %>%
  mutate(
    MaxVel_Monthly_Change = round(MaxVel - lag(MaxVel, order_by = Date), 1),
    MaxVel_YTD_Change = round(cumsum(coalesce(MaxVel - lag(MaxVel, order_by = Date), 0)), 1),
    
    MaxVel_Monthly_Percent_Change = ifelse(lag(MaxVel) == 0, NA, 
                                           round(100 * (MaxVel - lag(MaxVel)) / lag(MaxVel), 1)),
    MaxVel_YTD_Percent_Change = ifelse(lag(MaxVel) == 0, NA, 
                                       round(100 * (MaxVel_YTD_Change / lag(MaxVel)), 1)),
    
    AvgVel_Monthly_Change = round(AvgVel - lag(AvgVel, order_by = Date), 1),
    AvgVel_YTD_Change = round(cumsum(coalesce(AvgVel - lag(AvgVel, order_by = Date), 0)), 1),
    
    AvgVel_Monthly_Percent_Change = ifelse(lag(AvgVel) == 0, NA, 
                                           round(100 * (AvgVel - lag(AvgVel)) / lag(AvgVel), 1)),
    AvgVel_YTD_Percent_Change = ifelse(lag(AvgVel) == 0, NA, 
                                       round(100 * (AvgVel_YTD_Change / lag(AvgVel)), 1)),
    
    MaxDist_Monthly_Change = round(MaxDist - lag(MaxDist, order_by = Date), 1),
    MaxDist_YTD_Change = round(cumsum(coalesce(MaxDist - lag(MaxDist, order_by = Date), 0)), 1),
    
    MaxDist_Monthly_Percent_Change = ifelse(lag(MaxDist) == 0, NA, 
                                           round(100 * (MaxDist - lag(MaxDist)) / lag(MaxDist), 1)),
    MaxDist_YTD_Percent_Change = ifelse(lag(MaxDist) == 0, NA, 
                                       round(100 * (MaxDist_YTD_Change / lag(MaxDist)), 1)),
    
    AvgDist_Monthly_Change = round(AvgDist - lag(AvgDist, order_by = Date), 1),
    AvgDist_YTD_Change = round(cumsum(coalesce(AvgDist - lag(AvgDist, order_by = Date), 0)), 1),
    
    AvgDist_Monthly_Percent_Change = ifelse(lag(AvgDist) == 0, NA, 
                                            round(100 * (AvgDist - lag(AvgDist)) / lag(AvgDist), 1)),
    AvgDist_YTD_Percent_Change = ifelse(lag(AvgDist) == 0, NA, 
                                        round(100 * (AvgDist_YTD_Change / lag(AvgDist)), 1))
  ) %>%
  ungroup()

attendanceData <- read_csv("/Users/watts/Downloads/Check-Ins, January 01, 2023 - January 28, 2024-20230101-20240128.csv") %>%
  rename(Name = Client) %>% 
  mutate(Date = as.Date(Date, format = "%b %d, %Y"),
         Month = month(Date, label = TRUE, abbr = FALSE)) %>%
  filter(`Service Name` %in% c("Baseball Cage Rental L1", "Baseball Cage Rental L2", "Baseball Cage Rental L3", "Baseball Hitting L1", "Baseball Hitting L2", 
                               "Baseball Hitting L3", "Softball Cage Rental L1", "Softball Cage Rental L2", "Softball Cage Rental L3", "Softball Hitting L1",
                               "Softball Hitting L2", "Softball Hitting L3", "Professional - Facility Access", "Learning Academy Training"))

summary_attendanceData <- attendanceData %>%
  group_by(Name, Month) %>%
  summarise(Attendance = n(), 
            Classes = ifelse("Learning Academy Training" %in% `Service Name`, "Learning Academy Training", toString(unique(`Service Name`))),
            .groups = 'drop')

trainingGroup <- read_csv("/Users/watts/Downloads/Memberships.csv") %>% 
  filter(Membership %in% c("Learning Academy (pre-sale price)", "Learning Academy", "Position Player Skill - Base", "Position Player - Base (annual)",
                           "Position Player Skill Training - 1", "Position Player Skill Training - 1 (Annual)", "Position Player Skill Training - 2",
                           "Position Player Skill Training - 2 (Annual)", "Position Player Skill Training - 3", "Position Player Skill Training - 3 (Annual)",
                           "Position Player Skill Training - 4 (Annual)", "Hitters - Live AB", "Cage Access - Pro", "Professional - MiLB or Independent Baseball", 
                           "Professional - MLB Minimum", "Professional - MiLB with MLB time"))

priority_list <- c("Learning Academy (pre-sale price)", "Learning Academy", "Position Player Skill - Base", "Position Player - Base (annual)",
                   "Position Player Skill Training - 1", "Position Player Skill Training - 1 (Annual)", "Position Player Skill Training - 2",
                   "Position Player Skill Training - 2 (Annual)", "Position Player Skill Training - 3", "Position Player Skill Training - 3 (Annual)",
                   "Position Player Skill Training - 4 (Annual)", "Hitters - Live AB", "Cage Access - Pro", "Professional - MiLB or Independent Baseball", 
                   "Professional - MLB Minimum", "Professional - MiLB with MLB time")

# Filter the data to include only the specified memberships
filtered_memberships <- trainingGroup %>%
  filter(Membership %in% priority_list) %>%
  mutate(
    Membership = factor(Membership, levels = priority_list),
    Name = paste(`First Name`, `Last Name`)
  ) %>%
  arrange(Membership) %>%
  group_by(Name, Email) %>%
  slice(1) %>%
  ungroup()

filtered_memberships <- filtered_memberships %>% 
  select(Name, Membership)

combined_data <- left_join(hittraxData, blastData, by = c("Name", "Month"))

combined_hitting_data <- left_join(combined_data, filtered_memberships, by = "Name")

final_hitting_data <- left_join(combined_hitting_data, summary_attendanceData, by = c("Name", "Month"))

athlete_data <- final_hitting_data %>%
  select(Name, Month, Sport, Membership, Classes, Attendance, Level,
    MaxVel, MaxVel_Monthly_Change, MaxVel_Monthly_Percent_Change, MaxVel_YTD_Change, MaxVel_YTD_Percent_Change, `MaxVel Rank`,
    AvgVel, AvgVel_Monthly_Change, AvgVel_Monthly_Percent_Change, AvgVel_YTD_Change, AvgVel_YTD_Percent_Change, `AvgVel Rank`,
    MaxDist, MaxDist_Monthly_Change, MaxDist_Monthly_Percent_Change, MaxDist_YTD_Change, MaxDist_YTD_Percent_Change, `MaxDist Rank`,
    AvgDist, AvgDist_Monthly_Change, AvgDist_Monthly_Percent_Change, AvgDist_YTD_Change, AvgDist_YTD_Percent_Change, `AvgDist Rank`,
    `Bat Speed (mph)`, BatSpeed_Monthly_Change, BatSpeed_Monthly_Percent_Change, BatSpeed_YTD_Change, BatSpeed_YTD_Percent_Change,
    `Rotational Acceleration (g)`, Rotation_Monthly_Change, Rotation_Monthly_Percent_Change, Rotation_YTD_Change, Rotation_YTD_Percent_Change,
    `Attack Angle (deg)`, AttackAngle_Monthly_Change, AttackAngle_Monthly_Percent_Change, AttackAngle_YTD_Change, AttackAngle_YTD_Percent_Change,
    `Power (kW)`, Power_Monthly_Change, Power_Monthly_Percent_Change, Power_YTD_Change, Power_YTD_Percent_Change,
    `Vertical Bat Angle (deg)`, VBA_Monthly_Change, VBA_Monthly_Percent_Change, VBA_YTD_Change, VBA_YTD_Percent_Change,
    `On Plane Efficiency (%)`, OPE_Monthly_Change, OPE_Monthly_Percent_Change, OPE_YTD_Change, OPE_YTD_Percent_Change,
    `Early Connection (deg)`, EC_Monthly_Change, EC_Monthly_Percent_Change, EC_YTD_Change, EC_YTD_Percent_Change,
    `Connection at Impact (deg)`, CAI_Monthly_Change, CAI_Monthly_Percent_Change, CAI_YTD_Change, CAI_YTD_Percent_Change
  )


write_csv(athlete_data, "/Volumes/COLE'S DATA/Data/Data Dump/HittingFacilityData.csv", na = '')

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
months_to_process <- c("December", "January")

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
                               Counts = character(),
                               Percentage = double(),
                               Metric = character())

levels <- c("Total", "L1", "L2", "L3", "Collegiate", "Professional")

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
               Counts = paste0(Count_raw, "/", sum(Count_raw))) %>% # Add fraction representation
        select(-Count_raw) # Remove the raw count column
      
      progress_summary <- bind_rows(progress_summary, temp)
    }
  }
}

# Calculate counts for each metric within the specified ranges, grouped by Level
count_attack_angle_level <- temp_athlete_data %>%
  group_by(Level) %>%
  summarise(Count = sum(`Attack Angle (deg)` >= 8 & `Attack Angle (deg)` <= 12, na.rm = TRUE),
            Total = n()) %>%
  mutate(Metric = "Attack Angle",
         Counts = paste(Count, "/", Total),
         Percentage = round((Count / Total) * 100, 2)) %>%
  select(Metric, Level, Counts, Percentage)

count_attack_angle_total <- temp_athlete_data %>%
  summarise(Count = sum(`Attack Angle (deg)` >= 8 & `Attack Angle (deg)` <= 12, na.rm = TRUE),
            Total = n()) %>%
  mutate(Metric = "Attack Angle",
         Level = "Total",
         Counts = paste(Count, "/", Total),
         Percentage = round((Count / Total) * 100, 2)) %>%
  select(Metric, Level, Counts, Percentage)

count_attack_angle <- bind_rows(count_attack_angle_level, count_attack_angle_total)

# Calculate counts for Early Connection within the specified ranges, grouped by Level
count_early_connection_level <- temp_athlete_data %>%
  group_by(Level) %>%
  summarise(Count = sum(`Early Connection (deg)` >= 85 & `Early Connection (deg)` <= 105, na.rm = TRUE),
            Total = n()) %>%
  mutate(Metric = "Early Connection",
         Counts = paste(Count, "/", Total),
         Percentage = round((Count / Total) * 100, 2)) %>%
  select(Metric, Level, Counts, Percentage)

# Calculate aggregated counts for Early Connection across all levels
count_early_connection_total <- temp_athlete_data %>%
  summarise(Count = sum(`Early Connection (deg)` >= 85 & `Early Connection (deg)` <= 105, na.rm = TRUE),
            Total = n()) %>%
  mutate(Metric = "Early Connection",
         Level = "Total",
         Counts = paste(Count, "/", Total),
         Percentage = round((Count / Total) * 100, 2)) %>%
  select(Metric, Level, Counts, Percentage)

# Combine level-specific counts with the aggregated counts for Early Connection
count_early_connection <- bind_rows(count_early_connection_level, count_early_connection_total)

# Calculate counts for Connection at Impact within the specified ranges, grouped by Level
count_connection_at_impact_level <- temp_athlete_data %>%
  group_by(Level) %>%
  summarise(Count = sum(`Connection at Impact (deg)` >= 85 & `Connection at Impact (deg)` <= 105, na.rm = TRUE),
            Total = n()) %>%
  mutate(Metric = "Connection at Impact",
         Counts = paste(Count, "/", Total),
         Percentage = round((Count / Total) * 100, 2)) %>%
  select(Metric, Level, Counts, Percentage)

# Calculate aggregated counts for Connection at Impact across all levels
count_connection_at_impact_total <- temp_athlete_data %>%
  summarise(Count = sum(`Connection at Impact (deg)` >= 85 & `Connection at Impact (deg)` <= 105, na.rm = TRUE),
            Total = n()) %>%
  mutate(Metric = "Connection at Impact",
         Level = "Total",
         Counts = paste(Count, "/", Total),
         Percentage = round((Count / Total) * 100, 2)) %>%
  select(Metric, Level, Counts, Percentage)

# Combine level-specific counts with the aggregated counts for Connection at Impact
count_connection_at_impact <- bind_rows(count_connection_at_impact_level, count_connection_at_impact_total)

# Calculate counts for Vertical Bat Angle within the specified ranges, grouped by Level
count_vertical_bat_angle_level <- temp_athlete_data %>%
  group_by(Level) %>%
  summarise(Count = sum(`Vertical Bat Angle (deg)` >= -37 & `Vertical Bat Angle (deg)` <= -27, na.rm = TRUE),
            Total = n()) %>%
  mutate(Metric = "Vertical Bat Angle",
         Counts = paste(Count, "/", Total),
         Percentage = round((Count / Total) * 100, 2)) %>%
  select(Metric, Level, Counts, Percentage)

# Calculate aggregated counts for Vertical Bat Angle across all levels
count_vertical_bat_angle_total <- temp_athlete_data %>%
  summarise(Count = sum(`Vertical Bat Angle (deg)` >= -37 & `Vertical Bat Angle (deg)` <= -27, na.rm = TRUE),
            Total = n()) %>%
  mutate(Metric = "Vertical Bat Angle",
         Level = "Total",
         Counts = paste(Count, "/", Total),
         Percentage = round((Count / Total) * 100, 2)) %>%
  select(Metric, Level, Counts, Percentage)

# Combine level-specific counts with the aggregated counts for Vertical Bat Angle
count_vertical_bat_angle <- bind_rows(count_vertical_bat_angle_level, count_vertical_bat_angle_total)

# Combine all counts into one dataframe
counts_df <- bind_rows(count_attack_angle, count_early_connection, count_connection_at_impact, count_vertical_bat_angle)

progress_summary_updated <- bind_rows(progress_summary, counts_df)

# Write out the results
write_csv(progress_summary_updated, "/Volumes/COLE'S DATA/Data/Data Dump/HittingProgressData.csv", na = '')


#######################################################################################################
#######################################################################################################
###############################################  PITCHING  ############################################
#######################################################################################################
#######################################################################################################

trackmanData <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/practice/combined_data.csv") %>%
  mutate(Name = str_split(Pitcher, pattern = ", ", simplify = TRUE) %>% 
           apply(1, function(x) paste(x[2], x[1])),
         Month = format(mdy(Date), "%B")) %>% 
  select(Name, Month, TaggedPitchType, RelSpeed)

armCareData <- read_csv("/Users/watts/Downloads/ArmCare_data.csv")
armCareData$`Exam Date` <- mdy(armCareData$`Exam Date`)

attendanceData <- read_csv("/Users/watts/Downloads/Check-Ins, January 01, 2023 - January 28, 2024-20230101-20240128.csv") %>%
  rename(Name = Client) %>% 
  mutate(Date = as.Date(Date, format = "%b %d, %Y"),
         Month = month(Date, label = TRUE, abbr = FALSE)) %>%
  filter(`Service Name` %in% c("Academy Pitching G1", "Academy Pitching G2", "Academy Pitching G3", "Baseball Pitching L1", "Baseball Pitching L2",
                               "Baseball Pitching L3", "Pitching 1on1"))

summary_attendanceData <- attendanceData %>%
  group_by(Name, Month) %>%
  summarise(Attendance = n(), 
            Classes = ifelse("Learning Academy Training" %in% `Service Name`, "Learning Academy Training", toString(unique(`Service Name`))),
            .groups = 'drop')

armCareData <- armCareData %>%
  mutate(
    Name = paste(`First Name`, `Last Name`),
    Month = month(`Exam Date`, label = TRUE, abbr = FALSE)) %>%
  filter(
    `Exam Type` %in% c("Fresh - Quick", "Fresh - Full"),
    Month %in% c("November", "December")) %>% 
  select(Name, Month, `Arm Score`, `Total Strength`)

pitching_data <- bind_rows(trackmanData, armCareData)

trainingGroup <- read_csv("/Users/watts/Downloads/Memberships.csv") %>% 
  filter(Membership %in% c("Learning Academy (pre-sale price)", "Learning Academy", "Pitching - Live AB", "Pitching Development - Pro Double",
                           "Pitching Development - Pro Single", "Pitching Development - Pro Single (Annual)", "Pitching 1on1 Membership", 
                           "Professional - MiLB or Independent Baseball", "Professional - MLB Minimum", "Professional - MiLB with MLB time"))

priority_list <- c("Learning Academy (pre-sale price)", "Learning Academy", "Pitching - Live AB", "Pitching Development - Pro Double",
                   "Pitching Development - Pro Single", "Pitching Development - Pro Single (Annual)", "Pitching 1on1 Membership", 
                   "Professional - MiLB or Independent Baseball", "Professional - MLB Minimum", "Professional - MiLB with MLB time")

# Filter the data to include only the specified memberships
filtered_memberships <- trainingGroup %>%
  filter(Membership %in% priority_list) %>%
  mutate(
    Membership = factor(Membership, levels = priority_list),
    Name = paste(`First Name`, `Last Name`)
  ) %>%
  arrange(Membership) %>%
  group_by(Name, Email) %>%
  slice(1) %>%
  ungroup()

filtered_memberships <- filtered_memberships %>% 
  select(Name, Membership)

clientData <- read_csv("/Users/watts/Downloads/FullClientList.csv") %>% 
  rename(Name = Client) %>% 
  select(Name, `Reporting Level (Age-Dependent)`)

summary_pitching_data <- left_join(pitching_data, filtered_memberships, by = "Name")

final_summary_pitching_data <- left_join(summary_pitching_data, clientData, by = "Name")

final_pitching_data <- left_join(final_summary_pitching_data, summary_attendanceData, by = c("Name", "Month"))

final_pitching_data <- final_pitching_data %>%
  rename(Level = `Reporting Level (Age-Dependent)`) %>% 
  select(Name, Month, Membership, Classes, Attendance, Level, TaggedPitchType, RelSpeed, `Arm Score`, `Total Strength`)

write_csv(final_pitching_data, "/Volumes/COLE'S DATA/Data/Data Dump/PitchingFacilityData.csv", na = '')


#######################################################################################################
#######################################################################################################
#############################################  STRENGTH   #############################################
#######################################################################################################
#######################################################################################################

teambuildrData <- read_csv("/Volumes/COLE'S DATA/Data/Physicality Report Data/teambuilderPercentiles.csv") %>%
  filter(`Exercise Name` %in% c('Barbell Back Squat', 'Trap Bar Deadlift', 'Barbell Bench Press', 
                                'Straight Arm Trunk Rotation Max Isometric Test - Crane Scale', 'Cable Lat Pull Down')) %>%
  mutate(`Exercise Type` = "Weightroom",
         Month = format(ymd(`Added Date`), "%B")) %>% 
  rename(Date = `Added Date`) %>% 
  select(Date, Month, Name, Level, Gender, `Exercise Type`, `Exercise Name`, `Max Value`, PercentileRank)

proteusData<- read_csv("/Volumes/COLE'S DATA/Data/Physicality Report Data/ProteusPercentiles.csv") %>%
  mutate(`Exercise Type` = "Proteus") %>% 
  rename(Date = `session createdAt`, `Exercise Name` = `exercise name`)

CMJdata <- read_csv("/Volumes/COLE'S DATA/Data/Physicality Report Data/CMJpercentiles.csv") %>%
  mutate(`Exercise Type` = "ForceDeck: CMJ") %>% 
  rename(`Exercise Name` = `Test Type`) %>% 
  select(-ExternalId, -Time, -`BW [KG]`, -Reps, -Tags, -`Additional Load [lb]`)

ISOSQTdata <- read_csv("/Volumes/COLE'S DATA/Data/Physicality Report Data/ISO_SquatPercentiles.csv") %>%
  mutate(`Exercise Type` = "ForceDeck: ISOSQT") %>% 
  rename(`Exercise Name` = `Test Type`) %>% 
  select(-ExternalId, -Time, -`BW [KG]`, -Reps, -Tags)

SQTJumpData <- read_csv("/Users/watts/Downloads/SQTJump_data.csv") %>% 
  mutate(`Exercise Type` = "ForceDeck: Squat Jump",
         `Exercise Name` = `Test Type`,
         Month = format(mdy(Date), "%B")) %>% 
  select(-ExternalId, -Time, -`BW [KG]`, -Reps, -Tags, -`Additional Load [lb]`)

SQTJumpData$Date <- as.Date(SQTJumpData$Date, format="%m/%d/%Y")

shoulderData <- read_csv("/Users/watts/Downloads/ShoulderISO_data.csv") %>%
  mutate(`Exercise Type` = "ForceDeck: ShoulderISO",
         `Exercise Name` = `Test Type`,
         Month = format(mdy(Date), "%B"))

attendanceData <- read_csv("/Users/watts/Downloads/Check-Ins, January 01, 2023 - January 28, 2024-20230101-20240128.csv") %>%
  rename(Name = Client) %>% 
  mutate(Date = as.Date(Date, format = "%b %d, %Y"),
         Month = month(Date, label = TRUE, abbr = FALSE)) %>%
  filter(`Service Name` %in% c("Level 2 - Strength", "Level 3 - Strength", "Strength Base L3", "Strength Base L3", "Strength Pro L1", "Strength Pro L2", 
                               "Strength Pro L3", "Collegiate/Pro - Strength Training w/ Coach", "Professional - Facility Access", "Learning Academy Training"))

summary_attendanceData <- attendanceData %>%
  group_by(Name, Month) %>%
  summarise(Attendance = n(), 
            Classes = ifelse("Learning Academy Training" %in% `Service Name`, "Learning Academy Training", toString(unique(`Service Name`))),
            .groups = 'drop')

trainingGroup <- read_csv("/Users/watts/Downloads/Memberships.csv") %>% 
  filter(Membership %in% c("Learning Academy (pre-sale price)", "Learning Academy", "Strength Training - Base", "Strength Training - Base (Annual)", 
                           "Strength Training - Pro", "Strength Training - Pro (Annual)", "Professional - MiLB or Independent Baseball", 
                           "Professional - MLB Minimum", "Professional - MiLB with MLB time"))

priority_list <- c("Learning Academy", "Learning Academy (pre-sale price)","Strength Training - Pro (Annual)", "Strength Training - Pro", 
                   "Strength Training - Base (Annual)", "Strength Training - Base", "Professional - MiLB or Independent Baseball", 
                   "Professional - MLB Minimum", "Professional - MiLB with MLB time")

# Filter the data to include only the specified memberships
filtered_memberships <- trainingGroup %>%
  filter(Membership %in% priority_list) %>%
  mutate(
    Membership = factor(Membership, levels = priority_list),
    Name = paste(`First Name`, `Last Name`)
  ) %>%
  arrange(Membership) %>%
  group_by(Name, Email) %>%
  slice(1) %>%
  ungroup()

filtered_memberships <- filtered_memberships %>% 
  select(Name, Membership)

shoulderData$Date <- as.Date(shoulderData$Date, format="%m/%d/%Y")
updated_shoulderData <- shoulderData %>%
  group_by(Name, Date, `Exercise Name`, `Exercise Type`) %>%
  summarise(
    `Peak Vertical Force [N] (L)` = max(`Peak Vertical Force [N] (L)`, na.rm = TRUE),
    `Peak Vertical Force [N] (R)` = max(`Peak Vertical Force [N] (L)`, na.rm = TRUE),
    `RFD - 100ms [N/s] (L)` = max(`RFD - 100ms [N/s] (L)`, na.rm = TRUE),
    `RFD - 100ms [N/s] (R)` = max(`RFD - 100ms [N/s] (R)`, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate_all(~replace(., . == -Inf, NA)) %>% 
  mutate(Month = month(Date, label = TRUE, abbr = FALSE)) %>% 
  select(Name, `Exercise Type`, `Exercise Name`, Date, Month, `Peak Vertical Force [N] (L)`, `Peak Vertical Force [N] (R)`, `RFD - 100ms [N/s] (L)`, `RFD - 100ms [N/s] (R)`)

clientData <- read_csv("/Users/watts/Downloads/FullClientList.csv") %>% 
  rename(Name = Client, Level = `Sports Performance Training/Booking Level`) %>% 
  select(Name, Level, Gender)

final_shoulder_data <- left_join(shoulderData, clientData, by = "Name")

final_SQTJump_data <- left_join(SQTJumpData, clientData, by = "Name")

combined_strength <- bind_rows(proteusData, CMJdata, ISOSQTdata, final_shoulder_data, teambuildrData, final_SQTJump_data)

updated_strength_data <- left_join(combined_strength, summary_attendanceData, by = c("Name", "Month"))

final_strength_data <- left_join(updated_strength_data, filtered_memberships, by = "Name")

final_strength_data <- final_strength_data %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  arrange(Name, `Exercise Type`, `Exercise Name`, Date) %>%
  group_by(Name, `Exercise Type`, `Exercise Name`) %>%
  mutate(
    Value_Power = if_else(`Exercise Type` == "Proteus", `power - mean`, NA_real_),
    Value_Acceleration = if_else(`Exercise Type` == "Proteus", `acceleration - mean`, NA_real_),
    `Monthly Change - Power` = if_else(`Exercise Type` == "Proteus", 
                                       Value_Power - lag(Value_Power, default = first(Value_Power)), NA_real_),
    `Year to Date Change - Power` = if_else(`Exercise Type` == "Proteus", 
                                            Value_Power - first(Value_Power), NA_real_),
    `Monthly Change - Acceleration` = if_else(`Exercise Type` == "Proteus", 
                                              Value_Acceleration - lag(Value_Acceleration, default = first(Value_Acceleration)), NA_real_),
    `Year to Date Change - Acceleration` = if_else(`Exercise Type` == "Proteus", 
                                                   Value_Acceleration - first(Value_Acceleration), NA_real_),
    `Monthly Change % - Power` = if_else(`Exercise Type` == "Proteus" & lag(Value_Power) != 0,
                                         round(((Value_Power - lag(Value_Power, default = first(Value_Power))) / lag(Value_Power)) * 100, 2), NA_real_),
    `Year to Date Change % - Power` = if_else(`Exercise Type` == "Proteus" & first(Value_Power) != 0,
                                              round(((Value_Power - first(Value_Power)) / first(Value_Power)) * 100, 2), NA_real_),
    `Monthly Change % - Acceleration` = if_else(`Exercise Type` == "Proteus" & lag(Value_Acceleration) != 0,
                                                round(((Value_Acceleration - lag(Value_Acceleration, default = first(Value_Acceleration))) / lag(Value_Acceleration)) * 100, 2), NA_real_),
    `Year to Date Change % - Acceleration` = if_else(`Exercise Type` == "Proteus" & first(Value_Acceleration) != 0,
                                                     round(((Value_Acceleration - first(Value_Acceleration)) / first(Value_Acceleration)) * 100, 2), NA_real_)
  ) %>%
  mutate(
    Value = case_when(
      `Exercise Type` == "Weightroom" ~ `Max Value`,
      `Exercise Type` == "ForceDeck: CMJ" ~ `Concentric Peak Force [N]`,
      `Exercise Type` == "ForceDeck: ISOSQT" ~ `Peak Vertical Force [N]`,
      `Exercise Type` == "ForceDeck: ShoulderISO" ~ `Peak Vertical Force [N]`,
      TRUE ~ NA_real_
    ),
    `Monthly Change` = if_else(`Exercise Type` != "Proteus", 
                               Value - lag(Value, default = first(Value)), NA_real_),
    `Year to Date Change` = if_else(`Exercise Type` != "Proteus", 
                                    Value - first(Value), NA_real_),
    `Monthly Change %` = if_else(`Exercise Type` != "Proteus" & lag(Value) != 0,
                                 round(((Value - lag(Value, default = first(Value))) / lag(Value)) * 100, 2), NA_real_),
    `Year to Date Change %` = if_else(`Exercise Type` != "Proteus" & first(Value) != 0,
                                      round(((Value - first(Value)) / first(Value)) * 100, 2), NA_real_)
  ) %>%
  ungroup()
  
final_strength_data <- final_strength_data %>% 
  select("Date", "Month", "Name", "Membership", "Classes", "Attendance", "Level", "Gender", "Exercise Type", "Exercise Name", "power - mean", "acceleration - mean", 
         "Monthly Change - Power", "Monthly Change % - Power", "Year to Date Change - Power", "Year to Date Change % - Power", 
         "Monthly Change - Acceleration", "Monthly Change % - Acceleration", "Year to Date Change - Acceleration", "Year to Date Change % - Acceleration", 
         "PowerPercentileRank", "AccelerationPercentileRank", "Jump Height (Imp-Mom) [cm]", "Jump Height (Imp-Mom) in Inches [in]", 
         "Eccentric Duration [ms]", "Takeoff Peak Force [N]", "Peak Landing Force % (Asym) (%)", "Concentric Peak Force [N]", "Peak Vertical Force [N]", 
         "Peak Vertical Force % (Asym) (%)", "Peak Vertical Force [N] (L)", "Peak Vertical Force [N] (R)", "RFD - 100ms [N/s] (L)", 
         "RFD - 100ms [N/s] (R)", "Max Value", "PercentileRank", "Monthly Change", "Monthly Change %", "Year to Date Change", "Year to Date Change %")

write_csv(final_strength_data, "/Volumes/COLE'S DATA/Data/Data Dump/StrengthFacilityData.csv", na = '')

#######################################################################################################
#######################################################################################################
############################################   SPEED   ################################################
#######################################################################################################
#######################################################################################################

hardNinety <- read_csv("/Volumes/COLE'S DATA/Data/Speed Report Data/Hard90percentiles.csv") %>% 
  rename(`Exercise Type` = Name, Name = FullName) %>% 
  mutate(`Exercise Name` = "Hard 90") %>%
  filter(Month != "July")

accelerationData <- read_csv("/Volumes/COLE'S DATA/Data/Speed Report Data/AccelerationPercentiles.csv") %>% 
  rename(`Exercise Type` = Name, Name = FullName) %>% 
  mutate(`Exercise Name` = "Acceleration") %>%
  filter(Month != "July")

maxVeloData <- read_csv("/Volumes/COLE'S DATA/Data/Speed Report Data/MaxVelocityPercentiles.csv") %>% 
  rename(`Exercise Type` = Name, Name = FullName, Split1 = `10 yard fly`) %>% 
  mutate(`Exercise Name` = "Max Velocity") %>%
  filter(Month != "July")

RSIdata <- read_csv("/Volumes/COLE'S DATA/Data/Speed Report Data/RSIpercentiles.csv") %>% 
  rename(`Exercise Type` = Name, Name = FullName) %>% 
  mutate(`Exercise Name` = "Reactive Strength Index") %>%
  filter(Month != "July")

attendanceData <- read_csv("/Users/watts/Downloads/Check-Ins, January 01, 2023 - January 28, 2024-20230101-20240128.csv") %>%
  rename(Name = Client) %>% 
  mutate(Date = as.Date(Date, format = "%b %d, %Y"),
         Month = month(Date, label = TRUE, abbr = FALSE)) %>%
  filter(`Service Name` %in% c("Speed L1", "Speed L2", "Speed L3", "Professional - Speed Training", "Learning Academy Training"))

summary_attendanceData <- attendanceData %>%
  group_by(Name, Month) %>%
  summarise(Attendance = n(), 
            Classes = ifelse("Learning Academy Training" %in% `Service Name`, "Learning Academy Training", toString(unique(`Service Name`))),
            .groups = 'drop')

trainingGroup <- read_csv("/Users/watts/Downloads/Memberships.csv") %>% 
  filter(Membership %in% c("Learning Academy (pre-sale price)", "Learning Academy", "Speed Training", "Speed Training - Base/Pro", "Speed Training (Annual)",
                           "Professional - MiLB or Independent Baseball", "Professional - MLB Minimum", "Professional - MiLB with MLB time"))

priority_list <- c("Learning Academy", "Learning Academy (pre-sale price)", "Speed Training (Annual)", "Speed Training - Base/Pro", 
                   "Speed Training",  "Professional - MiLB or Independent Baseball", "Professional - MLB Minimum", "Professional - MiLB with MLB time")

# Filter the data to include only the specified memberships
filtered_memberships <- trainingGroup %>%
  filter(Membership %in% priority_list) %>%
  mutate(
    Membership = factor(Membership, levels = priority_list),
    Name = paste(`First Name`, `Last Name`)
  ) %>%
  arrange(Membership) %>%
  group_by(Name, Email) %>%
  slice(1) %>%
  ungroup()

filtered_memberships <- filtered_memberships %>% 
  select(Name, Membership)

speedData <- bind_rows(hardNinety, accelerationData, maxVeloData, RSIdata)

updated_speed_data <- left_join(speedData, summary_attendanceData, by = c("Name", "Month"))

final_speed_data <- left_join(updated_speed_data, filtered_memberships, by = "Name")

final_speed_data <- final_speed_data %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  arrange(Name, `Exercise Name`, Date) %>%
  group_by(Name, `Exercise Name`) %>%
  mutate(
    `Monthly Change` = case_when(
      `Exercise Name` == "Hard 90" ~ lag(Cumulative2, default = first(Cumulative2)) - Cumulative2,
      `Exercise Name` == "Acceleration" ~ lag(Split1, default = first(Split1)) - Split1,
      `Exercise Name` == "Max Velocity" ~ MPH - lag(MPH, default = first(MPH)),
      `Exercise Name` == "Reactive Strength Index" ~ `Mean RSI (Jump Height/Contact Time) [m/s]` - lag(`Mean RSI (Jump Height/Contact Time) [m/s]`, default = first(`Mean RSI (Jump Height/Contact Time) [m/s]`)))
    ) %>%
      mutate(
        `Year to Date Change` = case_when(
          `Exercise Name` == "Hard 90" ~ first(Cumulative2) - Cumulative2,
          `Exercise Name` == "Acceleration" ~ first(Split1) - Split1,
          `Exercise Name` == "Max Velocity" ~ MPH - first(MPH),
          `Exercise Name` == "Reactive Strength Index" ~ `Mean RSI (Jump Height/Contact Time) [m/s]` - first(`Mean RSI (Jump Height/Contact Time) [m/s]`))
      ) %>%
      mutate(
        `Monthly Change %` = round(case_when(
          `Exercise Name` == "Hard 90" & lag(Cumulative2) != 0 ~ ((lag(Cumulative2) - Cumulative2) / lag(Cumulative2)) * 100,
          `Exercise Name` == "Acceleration" & lag(Split1) != 0 ~ ((lag(Split1) - Split1) / lag(Split1)) * 100,
          `Exercise Name` == "Max Velocity" & lag(MPH) != 0 ~ ((MPH - lag(MPH)) / lag(MPH)) * 100,
          `Exercise Name` == "Reactive Strength Index" & lag(`Mean RSI (Jump Height/Contact Time) [m/s]`) != 0 ~ ((`Mean RSI (Jump Height/Contact Time) [m/s]` - lag(`Mean RSI (Jump Height/Contact Time) [m/s]`)) / lag(`Mean RSI (Jump Height/Contact Time) [m/s]`)) * 100,
          TRUE ~ 0
        ), 2),
        `Year to Date Change %` = round(case_when(
          `Exercise Name` == "Hard 90" & first(Cumulative2) != 0 ~ ((first(Cumulative2) - Cumulative2) / first(Cumulative2)) * 100,
          `Exercise Name` == "Acceleration" & first(Split1) != 0 ~ ((first(Split1) - Split1) / first(Split1)) * 100,
          `Exercise Name` == "Max Velocity" & first(MPH) != 0 ~ ((MPH - first(MPH)) / first(MPH)) * 100,
          `Exercise Name` == "Reactive Strength Index" & first(`Mean RSI (Jump Height/Contact Time) [m/s]`) != 0 ~ ((`Mean RSI (Jump Height/Contact Time) [m/s]` - first(`Mean RSI (Jump Height/Contact Time) [m/s]`)) / first(`Mean RSI (Jump Height/Contact Time) [m/s]`)) * 100,
          TRUE ~ 0
        ), 2)
      ) %>%
      ungroup()

final_speed_data <- final_speed_data %>% 
  select("Date", "Month", "Name", "Membership", "Classes", "Attendance", "Level", "Gender", "Exercise Name", "Exercise Type", "Split1", "Split2", "Split3", "Cumulative1", 
         "Cumulative2", "Cumulative3", "MPH", "Mean RSI (Jump Height/Contact Time) [m/s]", "Jump Height (Flight Time) [cm]", "Contact Time [ms]", 
         "PercentileRank", "Monthly Change", "Monthly Change %", "Year to Date Change", "Year to Date Change %")

write_csv(final_speed_data, "/Volumes/COLE'S DATA/Data/Data Dump/SpeedFacilityData.csv", na = '')




