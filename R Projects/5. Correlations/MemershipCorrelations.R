# Load necessary libraries
library(ggplot2)
library(tidyverse)
library(reshape2)

data <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Facility Data/HittingFacilityData.csv") %>% 
  filter(!is.na(Membership), 
         !Membership %in% c("Professional - MLB Minimum", "Professional - MiLB or Independent Baseball"))

# Convert 'FakeDate' to Date
data$FakeDate <- as.Date(data$FakeDate)

data <- data[order(data$Name, data$FakeDate), ]

membership_test <- data %>% 
  group_by(Membership) %>% 
  summarise(`Max Exit Velocity` = mean(MaxVel_Monthly_Percent_Change, na.rm = TRUE),
            `Max Distance` = mean(MaxDist_Monthly_Percent_Change, na.rm = TRUE),
            `Bat Speed` = mean(BatSpeed_Monthly_Percent_Change, na.rm = TRUE),
            .groups = "drop")
  
# membership_progress <- aggregate(cbind(MaxVel_Monthly_Change, MaxDist_Monthly_Change, BatSpeed_Monthly_Change) ~ Membership, data=data, FUN=mean, na.rm=TRUE)

membership_progress_melted <- melt(membership_test, id.vars='Membership', variable.name='Metric', value.name='Average.Progress')

# Plot the average progress for each membership type
ggplot(membership_progress_melted, aes(x = Average.Progress, y = Membership, fill = Metric)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  geom_text(aes(label = round(Average.Progress, 2)), position = position_dodge(width = 0.9)) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = 'Average Monthly Change Percentage by Membership Type',
    x = 'Average Monthly Change (%)',
    y = 'Membership Type',
    fill = 'Metric'
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################

data <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Facility Data/SpeedFacilityData.csv") %>% 
  filter(!is.na(Membership), Membership != "Professional - MiLB or Independent Baseball")

data <- data[order(data$Name, data$Date), ]

maxVelo_data <- data %>% 
  group_by(Membership) %>% 
  filter(`Exercise Name` == "Max Velocity") %>% 
  summarise(`Max Velocity` = mean(`Monthly Change %`, na.rm = TRUE))

acceleration_data <- data %>% 
  group_by(Membership) %>% 
  filter(`Exercise Name` == "Acceleration") %>% 
  summarise(`Acceleration` = mean(`Monthly Change %`, na.rm = TRUE))

forty_data <- data %>% 
  group_by(Membership) %>% 
  filter(`Exercise Name` == "40 Yard Dash") %>% 
  summarise(`40 Yard Dash` = mean(`Monthly Change %`, na.rm = TRUE))

merged_data <- maxVelo_data %>%
  left_join(acceleration_data, by = "Membership") %>%
  left_join(forty_data, by = "Membership") 
  

# membership_progress <- aggregate(cbind(MaxVel_Monthly_Change, MaxDist_Monthly_Change, BatSpeed_Monthly_Change) ~ Membership, data=data, FUN=mean, na.rm=TRUE)

membership_progress_melted <- melt(merged_data, id.vars='Membership', variable.name='Metric', value.name='Average.Progress')

# Plot the average progress for each membership type
ggplot(membership_progress_melted, aes(x = Average.Progress, y = Membership, fill = Metric)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  geom_text(aes(label = round(Average.Progress, 2)), position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = 'Average Monthly Change Percentage by Membership Type',
    x = 'Average Monthly Change (%)',
    y = 'Membership Type',
    fill = 'Metric'
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )
