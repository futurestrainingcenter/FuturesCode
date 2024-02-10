library(dplyr)
library(readr)

hittingData <- read_csv("/Volumes/COLE'S DATA/Data/Data Dump/HittingFacilityData.csv")

# Assuming 'hittingData' is your dataframe name
filtered_hittingData <- hittingData %>%
  filter(Month == "January", Level %in% c("L1", "L2", "L3"))

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


pitchingData <- read_csv("/Volumes/COLE'S DATA/Data/Data Dump/PitchingFacilityData.csv")

filtered_pitchingData <- pitchingData %>%
  filter(Month == "January", Level %in% c("L1", "L2", "L3"), TaggedPitchType == "Fastball")

max_velo_per_athlete <- filtered_pitchingData %>%
  group_by(Name, Level) %>%
  summarize(MaxRelSpeed = max(RelSpeed), .groups = 'drop')

# Rank athletes within each Level based on their fastest pitch
ranking_velo <- max_velo_per_athlete %>%
  group_by(Level) %>% 
  arrange(Level, desc(MaxRelSpeed)) %>%
  mutate(Rank = row_number()) %>%
  select(Name, Level, Rank, MaxRelSpeed)


speedData <- read_csv("/Volumes/COLE'S DATA/Data/Data Dump/SpeedFacilityData.csv")

filtered_speedData <- speedData %>% 
  filter(Month == "January", Level %in% c("L1", "L2", "L3"))

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


strengthData <- read_csv("/Volumes/COLE'S DATA/Data/Data Dump/StrengthFacilityData.csv")

filtered_strengthData <- strengthData %>% 
  filter(Month == "January", Level %in% c("L1", "L2", "L3"))

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


proteusData <- read_csv("/Users/watts/Downloads/SummaryProteus_data.csv")

filtered_proteusData <- proteusData %>% 
  filter(Level %in% c("L1", "L2", "L3"))

power_per_athlete <- filtered_proteusData %>%
  group_by(Name, Level) %>%
  summarize(`Max Power` = max(Average_Power_Mean), .groups = 'drop')

ranking_proteus_power <- power_per_athlete %>%
  group_by(Level) %>% 
  arrange(Level, desc(`Max Power`)) %>%
  mutate(Rank = row_number()) %>%
  select(Name, Level, Rank, `Max Power`)

p_acceleration_per_athlete <- filtered_proteusData %>%
  group_by(Name, Level) %>%
  summarize(`Max P_Acceleration` = max(Average_Acceleration_Mean), .groups = 'drop')

ranking_protues_acceleration <- p_acceleration_per_athlete %>%
  group_by(Level) %>% 
  arrange(Level, desc(`Max P_Acceleration`)) %>%
  mutate(Rank = row_number()) %>%
  select(Name, Level, Rank, `Max P_Acceleration`)


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
  ranking_protues_acceleration
)

# Write the combined data frame to a CSV file
write_csv(combined_ranking_data, "/Volumes/COLE'S DATA/Data/Data Dump/CombinedRankingData.csv")

