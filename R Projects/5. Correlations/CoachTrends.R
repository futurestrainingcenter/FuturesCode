library(tidyverse)
library(gt)
library(gtExtras)
library(lubridate)
library(webshot2)

# Read the datasets
coach_data <- read_csv("/Users/watts/Downloads/CoachAttendance_data.csv") %>% 
  rename(UserName = Client)

hittrax_data <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Hittrax Data/Aggregated Sessions/CombinedSessions.csv") %>% 
  filter(Type != "8")

coach_data$Date <- dmy(coach_data$Date)
hittrax_data$TS <- mdy_hm(hittrax_data$TS)
hittrax_data$Date <- as.Date(hittrax_data$TS)


combined_data <- left_join(coach_data, hittrax_data, by = c("UserName","Date"))

final_data <- combined_data %>% 
  filter(!is.na(Id)) %>% 
  mutate(Month = month(Date, label = TRUE, abbr = FALSE),
         MEV = MEV * 2.23694,
         AEV = AEV * 2.23694,
         AD = AD * 3.28084,
         MD = MD * 3.28084) %>% 
  select(Date, Month, Time, Staff, UserName, Service, Option, MEV, AEV, AD, MD)


# Calculate the percentage of athletes working with a specific coach per month and service
percentage_athletes_per_service <- final_data %>%
  group_by(Month, Service, Staff) %>%
  summarise(total_athletes = n_distinct(UserName)) %>%
  group_by(Month, Service) %>%
  mutate(total_athletes_service = sum(total_athletes),
         Percentage = round((total_athletes / total_athletes_service) * 100, 1)) %>%
  select(Month, Service, Staff, Percentage)

percentage_table <- percentage_athletes_per_service %>%
  gt(rowname_col = "Staff", groupname_col = "Service") %>%
  tab_header(
    title = "Monthly Athlete Participation by Coach",
    subtitle = "Percentage of Athletes Working with Each Coach Across Different Levels"
  ) %>% 
  tab_options(
    column_labels.font.weight = "bold",
  ) %>%
  sub_missing(columns = everything())

gtsave(percentage_table, "/Users/watts/Documents/Futures Performance Center/Visualizations/Skill_Coach_Participation.png")

# Calculate the average improvement in metrics per month, service, and coach
average_improvement <- final_data %>%
  group_by(Service, Staff, Month) %>%
  summarise(
    `Max EV` = round(mean(MEV, na.rm = TRUE), 1),
    `Avg EV` = round(mean(AEV, na.rm = TRUE), 1),
    `Avg Dist` = round(mean(AD, na.rm = TRUE), 1),
    `Max Dist` = round(mean(MD, na.rm = TRUE), 1)
  ) %>%
  arrange(Service, Staff, Month) %>%
  group_by(Service, Staff) %>%
  mutate(
    `Max EV Change` = `Max EV` - lag(`Max EV`),
    `Avg EV Change` = `Avg EV` - lag(`Avg EV`),
    `Avg Dist Change` = `Avg Dist` - lag(`Avg Dist`),
    `Max Dist Change` = `Max Dist` - lag(`Max Dist`)
  ) %>%
  select(Service, Staff, Month, `Max EV`, `Avg EV`, `Max Dist`, `Avg Dist`, `Max EV Change`, `Avg EV Change`, `Max Dist Change`, `Avg Dist Change`)

avgImporvement_table <- average_improvement %>%
  gt(rowname_col = "Staff", groupname_col = "Service") %>%
  tab_header(
    title = "Monthly Performance Improvement Analysis",
    subtitle = "Tracking Changes in Key Hitting Metrics Across Coaches and Levels"
  ) %>% 
  tab_options(
    column_labels.font.weight = "bold",
  ) %>%
  data_color(
    columns = c(`Max EV Change`, `Avg EV Change`, `Max Dist Change`, `Avg Dist Change`),
    palette = c("red", "yellow", "green"),
    na_color = "white",
    domain = NULL
  ) %>% 
  sub_missing(columns = everything())

gtsave(avgImporvement_table, "/Users/watts/Documents/Futures Performance Center/Visualizations/Skill_Coach_Improvements.png")

attendance_trend <- final_data %>%
  group_by(Month, Staff) %>%
  summarise(total_athletes = n_distinct(UserName)) %>%
  group_by(Month) %>%
  mutate(total_athletes_service = sum(total_athletes),
         percentage = round((total_athletes / total_athletes_service) * 100, 1)) %>%
  select(Month, Staff, percentage)


ggplot(attendance_trend, aes(x = Month, y = percentage, color = Staff, group = Staff)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Attendance Trend",
    subtitle = "Percentage of Athletes per Coach per Month",
    x = "Month",
    y = "Percentage of Athletes",
    color = "Coach"
  ) +
  theme_minimal(base_size = 15) +
  scale_color_brewer(palette = "Set2") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12)
  )

