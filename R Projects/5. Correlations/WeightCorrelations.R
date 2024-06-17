library(dplyr)
library(readxl)
library(readr)
library(lubridate)
library(stringr)
library(ggplot2)

hittraxData <- read_csv("/Volumes/COLE'S DATA/Data/Hittrax Master Data - Sheet1.csv")

grouped_hitting <- hittraxData %>% 
  group_by(Name) %>% 
  summarise(MaxExitVelo = round(mean(MaxVel, na.rm = TRUE), 1),
            AvgExitVelo = round(mean(AvgVel, na.rm = TRUE), 1),
            MaxDistance = round(mean(MaxDist, na.rm = TRUE), 1),
            AvgDistance = round(mean(AvgDist, na.rm = TRUE), 1))

trackmanData <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/practice/masterTrackmanData.csv") %>%
  mutate(Name = str_split(Pitcher, pattern = ", ", simplify = TRUE) %>% 
           apply(1, function(x) paste(x[2], x[1])),
         Date = as.Date(Date, format = "%m/%d/%y"),
         Month = month(Date, label = TRUE, abbr = FALSE)) %>% 
  filter(!is.na(TaggedPitchType) & PitchSession == "Live") %>% 
  select(Name, Date, TaggedPitchType, RelSpeed)

grouped_relSpeed <- trackmanData %>% 
  filter(TaggedPitchType == "Fastball") %>% 
  group_by(Name) %>% 
  summarise(AvgFB = round(mean(RelSpeed, na.rm = TRUE), 1))

ISOBeltSQTdata <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Strength Percentiles/ISO_BeltSquatPercentiles.csv") %>% 
  select(Name, `Peak Vertical Force [N]`)

grouped_BeltSquat <- ISOBeltSQTdata %>% 
  group_by(Name) %>% 
  summarise(`ISO Belt Squat` = round(mean(`Peak Vertical Force [N]`, na.rm = TRUE), 1))

CMJdata <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Strength Percentiles/CMJpercentiles.csv") %>% 
  select(Name, `BW [KG]`)
ISOSQTdata <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Strength Percentiles/ISO_SquatPercentiles.csv") %>% 
  select(Name, `BW [KG]`)
SQTJumpData <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Strength Percentiles/SQTJumpPercentiles.csv") %>% 
  select(Name, `BW [KG]`)

# Combine the datasets
combined_data <- bind_rows(CMJdata, ISOSQTdata, SQTJumpData)

average_BW_by_name <- combined_data %>%
  group_by(Name) %>%
  summarise(avg_BW_KG = mean(`BW [KG]`, na.rm = TRUE)) %>%
  mutate(Weight = round(avg_BW_KG * 2.20462, 1))

hitting_weight_data <- left_join(grouped_hitting, average_BW_by_name, by = "Name")

hitting_weight_data <- hitting_weight_data %>% 
  select(Name, MaxExitVelo, AvgExitVelo, MaxDistance, AvgDistance, Weight)

pitching_weight_data <- left_join(grouped_relSpeed, average_BW_by_name, by = "Name")

pitching_weight_data <- pitching_weight_data %>% 
  select(Name, AvgFB, Weight)

beltSquat_weight_data

final_data <- full_join(hitting_weight_data, pitching_weight_data, by = c("Name", "Weight"))

final_data <- final_data %>% 
  select(Name, Weight, MaxExitVelo, AvgExitVelo, MaxDistance, AvgDistance, AvgFB)

write_csv(final_data, "/Users/watts/Downloads/WeightCorrelations.csv", na = '')
