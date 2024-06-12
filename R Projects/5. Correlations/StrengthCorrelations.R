library(tidyverse)
library(gt)
library(gtExtras)

# Read the datasets
hitting_data <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Facility Data/HittingFacilityData.csv") %>% 
  select(Name, Month, Year, MaxVel, MaxDist, `Bat Speed (mph)`)

hittrax_data <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Hittrax Data/Session/CombinedSessionData.csv") %>% 
  rename(Name = UserName) %>% 
  filter(Type == "8", MinPopT > 1.5) %>% 
  mutate(TS = mdy_hms(TS),
         Month = month(TS, label = TRUE, abbr = FALSE),
         Year = year(TS))

strength_data <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Facility Data/StrengthFacilityData.csv") %>% 
  select(Name, Date, Month, Year, `Exercise Name`, `Max Value`, `Concentric Peak Force [N]`, `Peak Vertical Force [N]`,
         `Takeoff Peak Force [N]`, `power - high`, `acceleration - high`)

pitching_data <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Facility Data/PitchingFacilityData.csv") %>% 
  filter(TaggedPitchType == "Fastball") %>% 
  select(Name, Month, Year, RelSpeed)

speed_data <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Facility Data/SpeedFacilityData.csv")

catching_data <- hittrax_data %>% 
  group_by(Name, Year, Month) %>% 
  summarise(meanPopTime = mean(MinPopT, na.rm = TRUE),
            .groups = "drop")

maxSpeed_data <- speed_data %>% 
  filter(`Exercise Name` == "Max Velocity") %>% 
  group_by(Name, Year, Month) %>% 
  summarise(meanMPH = mean(MPH, na.rm = TRUE),
            .groups = "drop")

acceleration_data <- speed_data %>% 
  filter(`Exercise Name` == "Acceleration") %>% 
  group_by(Name, Year, Month) %>% 
  summarise(meanAcceleration = mean(Split1, na.rm = TRUE),
            .groups = "drop")

forty_data <- speed_data %>% 
  filter(`Exercise Name` == "40 Yard Dash") %>% 
  group_by(Name, Year, Month) %>% 
  summarise(meanForty = mean(Cumulative3, na.rm = TRUE),
            .groups = "drop")

merged_speed_one <- merge(maxSpeed_data, acceleration_data, by = c("Name", "Month", "Year"))
merged_speed_final <- merge(merged_speed_one, forty_data, by = c("Name", "Month", "Year"))

summary_pitch_data <- pitching_data %>% 
  group_by(Name, Year, Month) %>% 
  summarise(meanRelSpeed = mean(RelSpeed, na.rm = TRUE),
            .groups = "drop")

proteus_data <- strength_data %>% 
  filter(`Exercise Name` == "Proteus Full Test") %>%
  mutate(`Exercise Name` = "Proteus Power") %>%
  bind_rows(
    strength_data %>% 
      filter(`Exercise Name` == "Proteus Full Test") %>%
      mutate(`Exercise Name` = "Proteus Acceleration")
  )

# Combine back into the main dataset without the original "Proteus Full Test" rows
strength_data <- strength_data %>%
  filter(`Exercise Name` != "Proteus Full Test") %>%
  bind_rows(proteus_data)

# Filter strength data for the relevant exercises
relevant_exercises <- c("Safety Squat Bar Split Squat", "Barbell Back Squat", "Barbell Bench Press", "Straight Arm Trunk Rotation Max Isometric Test - Crane Scale",
                        "Cable Lat Pull Down", "Trap Bar Deadlift", "CMJ", "IBSQT", "ISOSQT", "SJ", 
                        "SHLDISOY", "Proteus Power", "Proteus Acceleration")

filtered_strength_data <- strength_data %>%
  filter(`Exercise Name` %in% relevant_exercises) %>%
  group_by(Name, Year, Month, `Exercise Name`) %>%
  summarize(AvgMaxValue = round(mean(`Max Value`, na.rm = TRUE)),
            AvgConcentricPeakForce = round(mean(`Concentric Peak Force [N]`, na.rm = TRUE)),
            AvgPeakVerticalForce = round(mean(`Peak Vertical Force [N]`, na.rm = TRUE)),
            AvgTakeoffPeakForce = round(mean(`Takeoff Peak Force [N]`, na.rm = TRUE)),
            AvgPower = round(mean(`power - high`, na.rm = TRUE)),
            AvgAcceleration = round(mean(`acceleration - high`, na.rm = TRUE)),
            .groups = "drop")

strength_averages <- filtered_strength_data %>%
  mutate(Selected_Metric = case_when(
    `Exercise Name` == "CMJ" ~ AvgConcentricPeakForce,
    `Exercise Name` == "IBSQT" ~ AvgPeakVerticalForce,
    `Exercise Name` == "ISOSQT" ~ AvgPeakVerticalForce,
    `Exercise Name` == "SJ" ~ AvgTakeoffPeakForce,
    `Exercise Name` == "SHLDISOY" ~ AvgPeakVerticalForce,
    `Exercise Name` == "Proteus Power" ~ AvgPower,
    `Exercise Name` == "Proteus Acceleration" ~ AvgAcceleration,
    TRUE ~ AvgMaxValue
  ))

# Merge datasets by Name (assuming 'Name' is a common identifier and present in both datasets)
hitting_strength_merge <- merge(strength_averages, hitting_data, by = c("Name", "Month", "Year"))

pitching_strength_merge <- merge(strength_averages, summary_pitch_data, by = c("Name", "Month", "Year"))

speed_strength_merge <- merge(strength_averages, merged_speed_final, by = c("Name", "Month", "Year"))

catching_strength_merge <- merge(strength_averages, catching_data, by = c("Name", "Month", "Year"))

maxVel_results <- list()
maxDist_results <- list()
batSpeed_results <- list()
relSpeed_results <- list()
maxSpeed_results <- list()
acceleration_results <- list()
forty_results <- list()
popTime_results <- list()

for (hitting_exercise in relevant_exercises) {
  hitting_exercise_data <- hitting_strength_merge %>% filter(`Exercise Name` == hitting_exercise)
  
  # Correlation of MaxVel with Max Value
  maxVel_corr <- cor(hitting_exercise_data$MaxVel, hitting_exercise_data$Selected_Metric, use = "complete.obs")
  maxVel_results[[hitting_exercise]] <- maxVel_corr
  
  # Correlation of MaxDist with Max Value
  maxDist_corr <- cor(hitting_exercise_data$MaxDist, hitting_exercise_data$Selected_Metric, use = "complete.obs")
  maxDist_results[[hitting_exercise]] <- maxDist_corr
  
  # Correlation of Bat Speed (mph) with Max Value
  batSpeed_corr <- cor(hitting_exercise_data$`Bat Speed (mph)`, hitting_exercise_data$Selected_Metric, use = "complete.obs")
  batSpeed_results[[hitting_exercise]] <- batSpeed_corr
}

for (pitching_exercise in relevant_exercises) {
  pitching_exercise_data <- pitching_strength_merge %>% filter(`Exercise Name` == pitching_exercise)
  
  relSpeed_corr <- cor(pitching_exercise_data$meanRelSpeed, pitching_exercise_data$Selected_Metric, use = "complete.obs")
  relSpeed_results[[pitching_exercise]] <- relSpeed_corr
}

for (speed_exercise in relevant_exercises) {
  speed_exercise_data <- speed_strength_merge %>% filter(`Exercise Name` == speed_exercise)
  
  maxSpeed_corr <- cor(speed_exercise_data$meanMPH, speed_exercise_data$Selected_Metric, use = "complete.obs")
  maxSpeed_results[[speed_exercise]] <- maxSpeed_corr
  
  # Correlation of meanAcceleration (inverted) with Max Value
  acceleration_corr <- cor(-speed_exercise_data$meanAcceleration, speed_exercise_data$Selected_Metric, use = "complete.obs")
  acceleration_results[[speed_exercise]] <- acceleration_corr
  
  # Correlation of meanForty (inverted) with Max Value
  forty_corr <- cor(-speed_exercise_data$meanForty, speed_exercise_data$Selected_Metric, use = "complete.obs")
  forty_results[[speed_exercise]] <- forty_corr
}

for (catching_exercise in relevant_exercises) {
  catching_exercise_data <- catching_strength_merge %>% filter(`Exercise Name` == catching_exercise)
  
  popTime_corr <- cor(-catching_exercise_data$meanPopTime, catching_exercise_data$Selected_Metric, use = "complete.obs")
  popTime_results[[catching_exercise]] <- popTime_corr
}

# Convert lists to data frames
maxVel_df <- data.frame(Exercise = names(maxVel_results), Correlation = unlist(maxVel_results), row.names = NULL)

maxEV_table <- maxVel_df %>%
  arrange(desc(Correlation)) %>% 
  gt() %>%
  tab_header(
    title = "Max Exit Velocity Correlations",
    subtitle = "Exploring the impact of strength training exercises on hitting exit velocity"
  ) %>% 
  tab_options(
    column_labels.font.weight = "bold",
  ) %>%
  data_color(
    columns = Correlation,
    palette = c("red", "yellow", "green"),
    domain = c(0,1)
  ) %>%
  tab_source_note("Correlations calculated using Pearson method.")


maxDist_df <- data.frame(Exercise = names(maxDist_results), Correlation = unlist(maxDist_results), row.names = NULL)

maxDist_table <- maxDist_df %>%
  arrange(desc(Correlation)) %>% 
  gt() %>%
  tab_header(
    title = "Max Distance Correlations",
    subtitle = "Exploring the impact of strength training exercises on hitting distance"
  ) %>% 
  tab_options(
    column_labels.font.weight = "bold",
  ) %>%
  data_color(
    columns = Correlation,
    palette = c("red", "yellow", "green"),
    domain = c(0,1)
  ) %>%
  tab_source_note("Correlations calculated using Pearson method.")

batSpeed_df <- data.frame(Exercise = names(batSpeed_results), Correlation = unlist(batSpeed_results), row.names = NULL)

batSpeed_table <- batSpeed_df %>%
  arrange(desc(Correlation)) %>% 
  gt() %>%
  tab_header(
    title = "Bat Speed Correlations",
    subtitle = "Exploring the impact of strength training exercises on bat speed"
  ) %>% 
  tab_options(
    column_labels.font.weight = "bold",
  ) %>%
  data_color(
    columns = Correlation,
    palette = c("red", "yellow", "green"),
    domain = c(0,1)
  ) %>%
  tab_source_note("Correlations calculated using Pearson method.")

relSpeed_df <- data.frame(Exercise = names(relSpeed_results), Correlation = unlist(relSpeed_results), row.names = NULL)

relSpeed_table <- relSpeed_df %>%
  arrange(desc(Correlation)) %>% 
  gt() %>%
  tab_header(
    title = "Release Speed Correlations",
    subtitle = "Exploring the impact of strength training exercises on throwing velocity"
  ) %>% 
  tab_options(
    column_labels.font.weight = "bold",
  ) %>%
  data_color(
    columns = Correlation,
    palette = c("red", "yellow", "green"),
    domain = c(0,1)
  ) %>%
  tab_source_note("Correlations calculated using Pearson method.")

maxSpeed_df <- data.frame(Exercise = names(maxSpeed_results), Correlation = unlist(maxSpeed_results), row.names = NULL)

maxSpeed_table <- maxSpeed_df %>%
  arrange(desc(Correlation)) %>% 
  gt() %>%
  tab_header(
    title = "Max Speed Correlations",
    subtitle = "Exploring the impact of strength training exercises on max running speed"
  ) %>% 
  tab_options(
    column_labels.font.weight = "bold",
  ) %>%
  data_color(
    columns = Correlation,
    palette = c("red", "yellow", "green"),
    domain = c(0,1)
  ) %>%
  tab_source_note("Correlations calculated using Pearson method.")

acceleration_df <- data.frame(Exercise = names(acceleration_results), Correlation = unlist(acceleration_results), row.names = NULL)

acceleration_table <- acceleration_df %>%
  arrange(desc(Correlation)) %>% 
  gt() %>%
  tab_header(
    title = "Accelerations Correlations",
    subtitle = "Exploring the impact of strength training exercises on speed acceleration"
  ) %>% 
  tab_options(
    column_labels.font.weight = "bold",
  ) %>%
  data_color(
    columns = Correlation,
    palette = c("red", "yellow", "green"),
    domain = c(0,1)
  ) %>%
  tab_source_note("Correlations calculated using Pearson method.")

forty_df <- data.frame(Exercise = names(forty_results), Correlation = unlist(forty_results), row.names = NULL)

forty_table <- forty_df %>%
  arrange(desc(Correlation)) %>% 
  gt() %>%
  tab_header(
    title = "40 Yard Dash Correlations",
    subtitle = "Exploring the impact of strength training exercises on the 40 yard dash"
  ) %>% 
  tab_options(
    column_labels.font.weight = "bold",
  ) %>%
  data_color(
    columns = Correlation,
    palette = c("red", "yellow", "green"),
    domain = c(0,1)
  ) %>%
  tab_source_note("Correlations calculated using Pearson method.")

popTime_df <- data.frame(Exercise = names(popTime_results), Correlation = unlist(popTime_results), row.names = NULL)

popTime_table <- popTime_df %>%
  arrange(desc(Correlation)) %>% 
  gt() %>%
  tab_header(
    title = "Pop Time Correlations",
    subtitle = "Exploring the impact of strength training exercises on catcher pop times"
  ) %>% 
  tab_options(
    column_labels.font.weight = "bold",
  ) %>%
  data_color(
    columns = Correlation,
    palette = c("red", "yellow", "green"),
    domain = c(-0.25,1)
  ) %>%
  tab_source_note("Correlations calculated using Pearson method.")

# gtsave(maxEV_table, "/Users/watts/Documents/Futures Performance Center/Visualizations/Correlations_MaxEV.png")
# gtsave(maxDist_table, "/Users/watts/Documents/Futures Performance Center/Visualizations/Correlations_MaxDist.png")
# gtsave(batSpeed_table, "/Users/watts/Documents/Futures Performance Center/Visualizations/Correlations_BatSpeed.png")
# gtsave(relSpeed_table, "/Users/watts/Documents/Futures Performance Center/Visualizations/Correlations_RelSpeed.png")
# gtsave(maxSpeed_table, "/Users/watts/Documents/Futures Performance Center/Visualizations/Correlations_MaxSpeed.png")
# gtsave(acceleration_table, "/Users/watts/Documents/Futures Performance Center/Visualizations/Correlations_Acceleration.png")
# gtsave(forty_table, "/Users/watts/Documents/Futures Performance Center/Visualizations/Correlations_FortyYard.png")
# gtsave(popTime_table, "/Users/watts/Documents/Futures Performance Center/Visualizations/Correlations_PopTime.png")



