library(dplyr)
library(readr)

# Read the data from the CSV file
proteus_data <- read_csv("/Users/watts/Downloads/Proteus_data.csv") %>% 
  rename(Name = `user name`)

clientData <- read_csv("/Users/watts/Downloads/FullClientList.csv") %>% 
  rename(Name = Client, Level = `Sports Performance Training/Booking Level`) %>% 
  select(Name, Level, Gender)

# Summarize the data
summary_proteus_data <- proteus_data %>%
  group_by(Name, `session createdAt`, `session name`) %>%
  summarize(
    Average_Power_Mean = mean(`power - high`, na.rm = TRUE),
    Average_Acceleration_Mean = mean(`acceleration - high`, na.rm = TRUE)
  )

combined_proteus_data <- left_join(summary_proteus_data, clientData, by = "Name")

combined_proteus_data <- combined_proteus_data %>% 
  select(Name, `session createdAt`, Level, Gender, `session name`, Average_Power_Mean, Average_Acceleration_Mean)

write_csv(combined_proteus_data, "/Users/watts/Downloads/SummaryProteus_data.csv", na = '')
