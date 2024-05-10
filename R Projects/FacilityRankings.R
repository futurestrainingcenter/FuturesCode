library(dplyr)
library(readr)

hittingData <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Facility Data/HittingFacilityData.csv")

# Assuming 'hittingData' is your dataframe name
filtered_hittingData <- hittingData %>%
  filter(Month == "March", Level %in% c("L1 (9u-11u)", "L2 (12u-14u)", "L3 (15u-18u)")) %>%
  mutate(Level = case_when(
    Level == "L1 (9u-11u)" ~ "L1",
    Level == "L2 (12u-14u)" ~ "L2",
    Level == "L3 (15u-18u)" ~ "L3",
    TRUE ~ Level # This keeps the original value if none of the above conditions are met
  ))

# Ranking by MaxVel within each Level
ranking_maxvel <- filtered_hittingData %>%
  group_by(Level) %>%
  arrange(Level, desc(MaxVel)) %>%
  mutate(Rank = row_number()) %>%
  select(Name, Level, Rank, MaxVel)

ranking_batspeed <- filtered_hittingData %>%
  group_by(Level) %>%
  arrange(Level, desc(`Bat Speed (mph)`)) %>%
  mutate(Rank = row_number()) %>%
  select(Name, Level, Rank, `Bat Speed (mph)`)


pitchingData <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Facility Data/PitchingFacilityData.csv")

filtered_pitchingData <- pitchingData %>%
  filter(Month == "March", Level %in% c("L1 (9u-11u)", "L2 (12u-14u)", "L3 (15u-18u)"), TaggedPitchType == "Fastball") %>%
  mutate(Level = case_when(
    Level == "L1 (9u-11u)" ~ "L1",
    Level == "L2 (12u-14u)" ~ "L2",
    Level == "L3 (15u-18u)" ~ "L3",
    TRUE ~ Level # This keeps the original value if none of the above conditions are met
  ))

max_velo_per_athlete <- filtered_pitchingData %>%
  group_by(Name, Level) %>%
  summarize(MaxRelSpeed = max(RelSpeed), .groups = 'drop')

# Rank athletes within each Level based on their fastest pitch
ranking_velo <- max_velo_per_athlete %>%
  group_by(Level) %>% 
  arrange(Level, desc(MaxRelSpeed)) %>%
  mutate(Rank = row_number()) %>%
  select(Name, Level, Rank, MaxRelSpeed)


speedData <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Facility Data/SpeedFacilityData.csv")

filtered_speedData <- speedData %>% 
  filter(Month == "March", Level %in% c("L1", "L2", "L3"))

mph_per_athlete <- filtered_speedData %>%
  filter(`Exercise Name` == "Max Velocity") %>% 
  group_by(Name, Level) %>%
  summarize(`Max MPH` = max(MPH), .groups = 'drop')

ranking_mph <- mph_per_athlete %>%
  group_by(Level) %>% 
  arrange(Level, desc(`Max MPH`)) %>%
  mutate(Rank = row_number()) %>%
  select(Name, Level, Rank, `Max MPH`)

acceleration_per_athlete <- filtered_speedData %>%
  filter(`Exercise Name` == "Acceleration") %>% 
  group_by(Name, Level) %>%
  summarize(`Max Acceleration` = max(Split1), .groups = 'drop')

ranking_acceleration <- acceleration_per_athlete %>%
  group_by(Level) %>% 
  arrange(Level, `Max Acceleration`) %>%
  mutate(Rank = row_number()) %>%
  select(Name, Level, Rank, `Max Acceleration`)

hardninety_per_athlete <- filtered_speedData %>%
  filter(`Exercise Name` == "Hard 90") %>% 
  group_by(Name, Level) %>%
  summarize(`Max Hard90` = max(Cumulative2), .groups = 'drop')

ranking_hardninety <- hardninety_per_athlete %>%
  group_by(Level) %>% 
  arrange(Level, `Max Hard90`) %>%
  mutate(Rank = row_number()) %>%
  select(Name, Level, Rank, `Max Hard90`)

RSI_per_athlete <- filtered_speedData %>%
  filter(`Exercise Name` == "Reactive Strength Index") %>% 
  group_by(Name, Level) %>%
  summarize(`Max RSI` = max(`Mean RSI (Jump Height/Contact Time) [m/s]`), .groups = 'drop')

ranking_RSI <- RSI_per_athlete %>%
  group_by(Level) %>% 
  arrange(Level, desc(`Max RSI`)) %>%
  mutate(Rank = row_number()) %>%
  select(Name, Level, Rank, `Max RSI`)

strengthData <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Facility Data/StrengthFacilityData.csv")

filtered_strengthData <- strengthData %>% 
  filter(Month == "March", Level %in% c("L1", "L2", "L3"))

ISOSQT_per_athlete <- filtered_strengthData %>%
  filter(`Exercise Type` == "ForceDeck: ISOSQT") %>% 
  group_by(Name, Level) %>%
  summarize(`Max Peak Vertical Force` = max(`Peak Vertical Force [N]`), .groups = 'drop')

ranking_ISOSQT <- ISOSQT_per_athlete %>%
  group_by(Level) %>% 
  arrange(Level, desc(`Max Peak Vertical Force`)) %>%
  mutate(Rank = row_number()) %>%
  select(Name, Level, Rank, `Max Peak Vertical Force`)

SQTJump_per_athlete <- filtered_strengthData %>%
  filter(`Exercise Type` == "ForceDeck: Squat Jump") %>% 
  group_by(Name, Level) %>%
  summarize(`Max Takeoff Peak Force` = max(`Takeoff Peak Force [N]`), .groups = 'drop')

ranking_SQTJump <- SQTJump_per_athlete %>%
  group_by(Level) %>% 
  arrange(Level, desc(`Max Takeoff Peak Force`)) %>%
  mutate(Rank = row_number()) %>%
  select(Name, Level, Rank, `Max Takeoff Peak Force`)

proteus_power_per_athlete <- filtered_strengthData %>%
  filter(`Exercise Name` == "Proteus Full Test") %>% 
  group_by(Name, Level) %>%
  summarize(Power = max(`power - high`), .groups = 'drop')

ranking_proteus_power <- proteus_power_per_athlete %>%
  group_by(Level) %>% 
  arrange(Level, desc(Power)) %>%
  mutate(Rank = row_number()) %>%
  select(Name, Level, Rank, Power)

proteus_acc_per_athlete <- filtered_strengthData %>%
  filter(`Exercise Name` == "Proteus Full Test") %>% 
  group_by(Name, Level) %>%
  summarize(Acceleration = max(`acceleration - high`), .groups = 'drop')

ranking_proteus_acceleration <- proteus_acc_per_athlete %>%
  group_by(Level) %>% 
  arrange(Level, desc(Acceleration)) %>%
  mutate(Rank = row_number()) %>%
  select(Name, Level, Rank, Acceleration)

# Combine all ranking data frames into one
combined_ranking_data <- bind_rows(
  ranking_maxvel,
  ranking_batspeed,
  ranking_velo,
  ranking_mph,
  ranking_acceleration,
  ranking_hardninety,
  ranking_RSI,
  ranking_ISOSQT,
  ranking_SQTJump,
  ranking_proteus_power,
  ranking_proteus_acceleration
)

# Write the combined data frame to a CSV file
write_csv(combined_ranking_data, "/Users/watts/Documents/Futures Performance Center/Data/Facility Data/PlayerRankings.csv")

