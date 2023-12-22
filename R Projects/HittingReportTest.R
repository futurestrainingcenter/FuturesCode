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
library(gt)
library(gtExtras)
library(flextable)
library(webshot2)
library(patchwork)

font_add(family = "Good Times", regular = "good times rg.otf")
showtext_auto()

blastData <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Full Team Report - 2023-08-28 - 2023-08-28 - 1696385310/master_data.csv") %>% 
  rename(Name = Athlete)
hittraxData <- read_csv("/Volumes/COLE'S DATA/Data/Hittrax Master Data - Sheet1.csv")
clientData <- read_csv("/Users/watts/Downloads/FullClientList.csv") %>% 
  rename(Name = Client)
attendanceData <- read_csv("/Users/watts/Downloads/HittingAttendance_data.csv")

# Function to calculate age
calculate_age <- function(birthdate) {
  if (is.na(birthdate)) {
    return(NA)
  } else {
    birthdate <- ymd(birthdate) # Convert to Date using lubridate
    age <- interval(start = birthdate, end = Sys.Date()) / years(1)
    return(floor(age)) # Floor the age to get complete years
  }
}

# Create a new column "Age" that calculates their age
clientData$Age <- sapply(clientData$`field-general-7.dl_date`, calculate_age)
colnames(clientData)[colnames(clientData) == "Client"] <- "Name"


blastData$Date <- as.Date(blastData$Date, format = "%b %d, %Y")
blastData$Month <- format(blastData$Date, "%B")
blastData <- left_join(blastData, clientData, by = "Name")

blastData <- blastData %>%
  group_by(Name) %>%
  mutate(MedianBatSpeed = median(`Bat Speed (mph)`, na.rm = TRUE)) %>%
  filter(`Bat Speed (mph)` >= MedianBatSpeed & `Swing Details` %in% c("Front Toss Underhand", "In Game", "Pitching Machine", "Tee"))

# blastData <- blastData %>%
#   arrange(Name, match(Month, month.name)) %>%
#   group_by(Name) %>%
#   mutate(
#     BatSpeed_Monthly_Change = round(`Bat Speed (mph)` - lag(`Bat Speed (mph)`, order_by = match(Month, month.name)), 1),
#     BatSpeed_YTD_Change = round(cumsum(coalesce(`Bat Speed (mph)` - lag(`Bat Speed (mph)`, order_by = match(Month, month.name)), 0)), 1),
#     
#     Rotation_Monthly_Change = round(`Rotational Acceleration (g)` - lag(`Rotational Acceleration (g)`, order_by = match(Month, month.name)), 1),
#     Rotation_YTD_Change = round(cumsum(coalesce(`Rotational Acceleration (g)` - lag(`Rotational Acceleration (g)`, order_by = match(Month, month.name)), 0)), 1),
#     
#     AttackAngle_Monthly_Change = round(`Attack Angle (deg)` - lag(`Attack Angle (deg)`, order_by = match(Month, month.name)), 1),
#     AttackAngle_YTD_Change = round(cumsum(coalesce(`Attack Angle (deg)` - lag(`Attack Angle (deg)`, order_by = match(Month, month.name)), 0)), 1),
#     
#     Power_Monthly_Change = round(`Power (kW)` - lag(`Power (kW)`, order_by = match(Month, month.name)), 1),
#     Power_YTD_Change = round(cumsum(coalesce(`Power (kW)` - lag(`Power (kW)`, order_by = match(Month, month.name)), 0)), 1)
#   )

hittraxData <- left_join(hittraxData, clientData, by = "Name")

hittraxData <- hittraxData %>%
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
  group_by(`Reporting Level (Age-Dependent)`, Gender, Month) %>%
  mutate(
    MaxVel_Rank = rank(-MaxVel_CumMax, ties.method = "min"),
    AvgVel_Rank = rank(-AvgVel, ties.method = "min"),
    MaxDist_Rank = rank(-MaxDist_CumMax, ties.method = "min"),
    AvgDist_Rank = rank(-AvgDist, ties.method = "min"),
    Total_Players = n()
  ) %>%
  ungroup()

col_grid <- rgb(235, 235, 235, 25, maxColorValue = 255)

athletes <- unique(hittraxData$Name)

TemplatePageOne <- image_read_pdf("/Volumes/COLE'S DATA/Templates/Hitting Report Template.pdf")
IndexPage <- image_read_pdf("/Volumes/COLE'S DATA/Templates/Hitting Report Metric Index.pdf")

# Set the working directory
setwd("/Users/watts/Documents/Futures Performance Center/Test")

for (athlete in athletes){
  
  filteredHittrax <- hittraxData %>% 
    filter(Month == "November" & Name == athlete)
  
  # Fetch the group of the current player
  current_level <- unique(filteredHittrax$`Reporting Level (Age-Dependent)`)[1]
  current_gender <- unique(filteredHittrax$Gender)[1]
  
  # Check if current_level is empty or NA, if yes skip to next iteration
  if (is.na(current_level) || length(current_level) == 0) {
    print(paste("No hittrax data:", athlete))
    next
  }
  
  athlete_folder <- paste0("Futures Reports/", athlete)
  if (!dir.exists(athlete_folder)) {
    dir.create(athlete_folder, recursive = TRUE)
  }
  
  player_profile <- clientData %>%
    filter(Name == athlete) %>%
    mutate(GPA = NA, Weight = NA, School = NA, Class = NA, Attendance = NA, HT_WT = paste(Height, " / ", Weight)) %>% 
    select(Name, Age, `Reporting Level (Age-Dependent)`, `Position (Baseball/Softball)`, GPA, HT_WT, School, Class, Attendance)
  names(player_profile) <- c("Name:", "Age:", "Level:", "Position:", "GPA:", "Height/Weight:", "School:", "Class:", "Attendance:")
  
  player_profile <- player_profile %>% 
    mutate(across(c(`Name:`, `Age:`, `Level:`, `Position:`, `GPA:`, `Height/Weight:`, `Attendance:`), as.character)) %>%
    pivot_longer(cols = c(`Name:`, `Age:`, `Level:`, `Position:`, `GPA:`, `Height/Weight:`, `Attendance:`), names_to = "label", values_to = "value")
  
  # Define the coordinates for the labels and values
  left_labels <- c("Name:", "Age:", "Level:")
  middle_labels <- c("School:", "Class:", "Height/Weight:")
  right_labels <- c("GPA:", "Position:", "Attendance:") 
  left_x <- 0.1    # x position for left labels
  middle_x <- 0.35  # x position for middle labels
  right_x <- 0.6   # x position for right labels
  y_positions <- seq(0.9, 0.3, by = -0.1)  # y positions for each label
  
  # Create a blank ggplot object
  p <- ggplot() +
    expand_limits(x = 0.8, y = 0.5) +
    theme_void() 
  
  # Add left labels and their values
  for (i in 1:length(left_labels)) {
    p <- p + 
      annotate("text", x = left_x, y = y_positions[i], label = left_labels[i], 
               hjust = 0, color = "#3d9be9", size = 8, family = "Good Times") +
      annotate("text", x = left_x + 0.05, y = y_positions[i], label = player_profile$value[player_profile$label == left_labels[i]], 
               hjust = 0, color = "white", size = 10, family = "Good Times")
  }
  
  # Add middle labels and their values
  for (i in 1:length(middle_labels)) {
    p <- p + 
      annotate("text", x = middle_x, y = y_positions[i], label = middle_labels[i], 
               hjust = 0, color = "#3d9be9", size = 8, family = "Good Times") +
      annotate("text", x = middle_x + 0.1, y = y_positions[i], label = player_profile$value[player_profile$label == middle_labels[i]], 
               hjust = 0, color = "white", size = 10, family = "Good Times")
  }
  
  # Add right labels and their values
  for (i in 1:length(right_labels)) {
    p <- p + 
      annotate("text", x = right_x, y = y_positions[i], label = right_labels[i], 
               hjust = 0, color = "#3d9be9", size = 8, family = "Good Times") +
      annotate("text", x = right_x + 0.075, y = y_positions[i], label = player_profile$value[player_profile$label == right_labels[i]], 
               hjust = 0, color = "white", size = 11, family = "Good Times")
  }
  
  ggsave(p,file=paste0("Futures Reports Images/",athlete," - playerProfile.png"), width=16,height=3,units="in", dpi = 150)
  playerSummary1 <- image_read(paste0("Futures Reports Images/",athlete," - playerProfile.png"))
  PitchingReport1 <- image_composite(TemplatePageOne, playerSummary1, offset= "+50+525")
  
  maxVel <- filteredHittrax %>% 
    ggplot(aes(x = `MaxVel Rank`, y = "")) +
    geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
    geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
    geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
    geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
    geom_point(aes(fill = `MaxVel Rank`), color = "black", pch = 21, size = 12) +
    geom_text(aes(label = round(`MaxVel Rank`)), size = 10, fontface = "bold") +
    labs(title = paste(round(filteredHittrax$MaxVel_CumMax, digits = 1), "MPH"),
         subtitle = paste("MOM Change:", filteredHittrax$MaxVel_Monthly_Change, 
                          "| YTD Change:", filteredHittrax$MaxVel_YTD_Change,
                          "\nLevel Rank:", filteredHittrax$MaxVel_Rank, "/", filteredHittrax$Total_Players)) +
    scale_fill_gradient2(low = "#FF0000", mid = "#FFFF00", high = "#00FF00", midpoint = 50, limits = c(0, 100), na.value = "grey") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.background =  element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5, vjust = -7, face = "bold", size = 38, color = "white"),
          plot.subtitle = element_text(hjust = 0.5, vjust = -10, size = 32, color = "white")
    )
  
  avgVel <- filteredHittrax %>% 
    ggplot(aes(x = `AvgVel Rank`, y = "")) +
    geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
    geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
    geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
    geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
    geom_point(aes(fill = `AvgVel Rank`), color = "black", pch = 21, size = 12) +
    geom_text(aes(label = round(`AvgVel Rank`)), size = 10, fontface = "bold") +
    labs(title = paste(round(filteredHittrax$AvgVel, digits = 1), "MPH"),
         subtitle = paste("MOM Change:", filteredHittrax$AvgVel_Monthly_Change, 
                          "| YTD Change:", filteredHittrax$AvgVel_YTD_Change,
                          "\nLevel Rank:", filteredHittrax$AvgVel_Rank, "/", filteredHittrax$Total_Players)) +
    scale_fill_gradient2(low = "#FF0000", mid = "#FFFF00", high = "#00FF00", midpoint = 50, limits = c(0, 100), na.value = "grey") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.background =  element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5, vjust = -7, face = "bold", size = 38, color = "white"),
          plot.subtitle = element_text(hjust = 0.5, vjust = -10, size = 32, color = "white")
    )
  
  maxDist <- filteredHittrax %>% 
    ggplot(aes(x = `MaxDist Rank`, y = "")) +
    geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
    geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
    geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
    geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
    geom_point(aes(fill = `MaxDist Rank`), color = "black", pch = 21, size = 12) +
    geom_text(aes(label = round(`MaxDist Rank`)), size = 10, fontface = "bold") +
    labs(title = paste(round(filteredHittrax$MaxDist_CumMax, digits = 1), "Feet"),
         subtitle = paste("MOM Change:", filteredHittrax$MaxDist_Monthly_Change, 
                          "| YTD Change:", filteredHittrax$MaxDist_YTD_Change,
                          "\nLevel Rank:", filteredHittrax$MaxDist_Rank, "/", filteredHittrax$Total_Players)) +
    scale_fill_gradient2(low = "#FF0000", mid = "#FFFF00", high = "#00FF00", midpoint = 50, limits = c(0, 100), na.value = "grey") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.background =  element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5, vjust = -7, face = "bold", size = 38, color = "white"),
          plot.subtitle = element_text(hjust = 0.5, vjust = -10, size = 32, color = "white")
    )
  
  avgDist <- filteredHittrax %>% 
    ggplot(aes(x = `AvgDist Rank`, y = "")) +
    geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
    geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
    geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
    geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
    geom_point(aes(fill = `AvgDist Rank`), color = "black", pch = 21, size = 12) +
    geom_text(aes(label = round(`AvgDist Rank`)), size = 10, fontface = "bold") +
    labs(title = paste(round(filteredHittrax$AvgDist, digits = 1), "Feet"),
         subtitle = paste("MOM Change:", filteredHittrax$AvgDist_Monthly_Change, 
                          "| YTD Change:", filteredHittrax$AvgDist_YTD_Change,
                          "\nLevel Rank:", filteredHittrax$AvgDist_Rank, "/", filteredHittrax$Total_Players)) +
    scale_fill_gradient2(low = "#FF0000", mid = "#FFFF00", high = "#00FF00", midpoint = 50, limits = c(0, 100), na.value = "grey") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.background =  element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5, vjust = -7, face = "bold", size = 38, color = "white"),
          plot.subtitle = element_text(hjust = 0.5, vjust = -10, size = 32, color = "white")
    )
  
  ggsave(maxVel,file=paste0("Futures Reports Images/",athlete," - playerMaxEVPercentiles.png"), width=5,height=3,units="in", dpi = 170)
  pitchCharts1 <- image_read(paste0("Futures Reports Images/",athlete," - playerMaxEVPercentiles.png"))
  PitchingReport2 <- image_composite(PitchingReport1, pitchCharts1, offset= "+250+985")
  
  ggsave(maxDist,file=paste0("Futures Reports Images/",athlete," - playerMaxDistPercentiles.png"), width=5,height=3,units="in", dpi = 170)
  pitchCharts2 <- image_read(paste0("Futures Reports Images/",athlete," - playerMaxDistPercentiles.png"))
  PitchingReport3 <- image_composite(PitchingReport2, pitchCharts2, offset= "+250+1360")
  
  ggsave(avgVel,file=paste0("Futures Reports Images/",athlete," - playerAvgEVPercentiles.png"), width=5,height=3,units="in", dpi = 170)
  pitchCharts3 <- image_read(paste0("Futures Reports Images/",athlete," - playerAvgEVPercentiles.png"))
  PitchingReport4 <- image_composite(PitchingReport3, pitchCharts3, offset= "+1450+985")
  
  ggsave(avgDist,file=paste0("Futures Reports Images/",athlete," - playerAvgDistPercentiles.png"), width=5,height=3,units="in", dpi = 170)
  pitchCharts4 <- image_read(paste0("Futures Reports Images/",athlete," - playerAvgDistPercentiles.png"))
  PitchingReport5 <- image_composite(PitchingReport4, pitchCharts4, offset= "+1450+1360")
  
  filteredBlast <- blastData %>% 
    filter(Month == "September" & Name == athlete)
  
  player_data <- filteredBlast %>%
    group_by(`Swing Details`) %>% 
    summarise(
      `Bat Speed (mph)` = round(mean(`Bat Speed (mph)`, na.rm = TRUE), 1),
      `Peak Hand Speed (mph)` = round(mean(`Peak Hand Speed (mph)`, na.rm = TRUE), 1),
      `Rotational Acceleration (g)` = round(mean(`Rotational Acceleration (g)`, na.rm = TRUE), 1),
      `Power (kW)` = round(mean(`Power (kW)`, na.rm = TRUE), 1),
      `On Plane Efficiency (%)` = round(mean(`On Plane Efficiency (%)`, na.rm = TRUE), 1),
      `Attack Angle (deg)` = round(mean(`Attack Angle (deg)`, na.rm = TRUE), 1),
      `Early Connection (deg)` = round(mean(`Early Connection (deg)`, na.rm = TRUE), 1),
      `Connection at Impact (deg)` = round(mean(`Connection at Impact (deg)`, na.rm = TRUE), 1),
      `Vertical Bat Angle (deg)` = round(mean(`Vertical Bat Angle (deg)`, na.rm = TRUE), 1)
    ) %>%
    select("Swing Details", "Bat Speed (mph)", "Rotational Acceleration (g)", "Power (kW)", "On Plane Efficiency (%)", 
           "Attack Angle (deg)", "Early Connection (deg)", "Connection at Impact (deg)", "Vertical Bat Angle (deg)")
  
  # Initialize goals_data with the same columns as player_data, filled with NA values
  goals_data <- as.data.frame(matrix(NA, ncol=ncol(player_data), nrow=1))
  colnames(goals_data) <- colnames(player_data)
  
  # Fetch the group of the current player
  current_level_blast <- unique(filteredBlast$`Reporting Level (Age-Dependent)`)[1]
  current_gender_blast <- unique(filteredBlast$Gender)[1]
  
  goals_data$`Swing Details` <- "Goals"
  
  if(current_gender_blast == "Male" && current_level_blast %in% c("Professional", "Collegiate", " L1", "L2", "L3")) {
    if(current_level_blast %in% c("Collegiate", "Professional")) {
      goals_data$`Bat Speed (mph)` <- "75"
      goals_data$`Rotational Acceleration (g)` <- "16"
      goals_data$`On Plane Efficiency (%)` <- "85"
      goals_data$`Power (kW)` <- "6.0"
      goals_data$`Attack Angle (deg)` <- "6 - 10"
      goals_data$`Early Connection (deg)` <- "85 - 105"
      goals_data$`Connection at Impact (deg)` <- "90 - 95"
      goals_data$`Vertical Bat Angle (deg)` <- "-27 to -37"
    } else if(current_level_blast == "L3") {
      goals_data$`Bat Speed (mph)` <- "65"
      goals_data$`Rotational Acceleration (g)` <- "13"
      goals_data$`On Plane Efficiency (%)` <- "80"
      goals_data$`Power (kW)` <- "4.5"
      goals_data$`Attack Angle (deg)` <- "8 - 12"
      goals_data$`Early Connection (deg)` <- "85 - 105"
      goals_data$`Connection at Impact (deg)` <- "90 - 95"
      goals_data$`Vertical Bat Angle (deg)` <- "-27 to -37"
    } else if(current_level_blast == "L2") {
      goals_data$`Bat Speed (mph)` <- "55"
      goals_data$`Rotational Acceleration (g)` <- "10"
      goals_data$`On Plane Efficiency (%)` <- "70"
      goals_data$`Power (kW)` <- "2.75"
      goals_data$`Attack Angle (deg)` <- "8 - 12"
      goals_data$`Early Connection (deg)` <- "85 - 110"
      goals_data$`Connection at Impact (deg)` <- "85 - 95"
      goals_data$`Vertical Bat Angle (deg)` <- "-20 to -30"
    } else if(current_level_blast == " L1") {
      goals_data$`Bat Speed (mph)` <- "45"
      goals_data$`Rotational Acceleration (g)` <- "7.5"
      goals_data$`On Plane Efficiency (%)` <- "60"
      goals_data$`Power (kW)` <- "1.0"
      goals_data$`Attack Angle (deg)` <- "5 - 15"
      goals_data$`Early Connection (deg)` <- "80 - 110"
      goals_data$`Connection at Impact (deg)` <- "80 - 100"
      goals_data$`Vertical Bat Angle (deg)` <- "-15 to -25"
    }
  } else if(current_gender_blast == "Female" && current_level_blast %in% c("Collegiate", " L1", "L2", "L3")) {
    if(current_level_blast %in% c("Collegiate", "Professional")) {
      goals_data$`Bat Speed (mph)` <- "75"
      goals_data$`Rotational Acceleration (g)` <- "16"
      goals_data$`On Plane Efficiency (%)` <- "85"
      goals_data$`Power (kW)` <- "6.0"
      goals_data$`Attack Angle (deg)` <- "6 - 10"
      goals_data$`Early Connection (deg)` <- "85 - 105"
      goals_data$`Connection at Impact (deg)` <- "90 - 95"
      goals_data$`Vertical Bat Angle (deg)` <- "-27 to -37"
    } else if(current_level_blast == "L3") {
      goals_data$`Bat Speed (mph)` <- "65"
      goals_data$`Rotational Acceleration (g)` <- "12"
      goals_data$`On Plane Efficiency (%)` <- "80"
      goals_data$`Power (kW)` <- "3.5"
      goals_data$`Attack Angle (deg)` <- "6 - 10"
      goals_data$`Early Connection (deg)` <- "85 - 105"
      goals_data$`Connection at Impact (deg)` <- "90 - 95"
      goals_data$`Vertical Bat Angle (deg)` <- "-27 to -37"
    } else if(current_level_blast == "L2") {
      goals_data$`Bat Speed (mph)` <- "55"
      goals_data$`Rotational Acceleration (g)` <- "10"
      goals_data$`On Plane Efficiency (%)` <- "70"
      goals_data$`Power (kW)` <- "1.75"
      goals_data$`Attack Angle (deg)` <- "6 - 10"
      goals_data$`Early Connection (deg)` <- "85 - 110"
      goals_data$`Connection at Impact (deg)` <- "85 - 95"
      goals_data$`Vertical Bat Angle (deg)` <- "-20 to -30"
    } else if(current_level_blast == " L1") {
      goals_data$`Bat Speed (mph)` <- "45"
      goals_data$`Rotational Acceleration (g)` <- "6"
      goals_data$`On Plane Efficiency (%)` <- "60"
      goals_data$`Power (kW)` <- "1.0"
      goals_data$`Attack Angle (deg)` <- "6 - 14"
      goals_data$`Early Connection (deg)` <- "80 - 110"
      goals_data$`Connection at Impact (deg)` <- "80 - 100"
      goals_data$`Vertical Bat Angle (deg)` <- "-15 to -25"
    }
  } else {
    player_data <- NULL
  }
  
  combined_player_data <- rbind(player_data, goals_data)
  
  gt_table <- combined_player_data %>%
    gt(rowname_col = "Swing Details") %>%
    cols_align(
      align = "center",
      columns = everything()
    ) %>%
    tab_header(
      title = html('<p style="font-family:Good Times">Blast Motion Data</p>')
    ) %>%
    tab_options(
      heading.align = "left",
      column_labels.font.weight = "bold",
      heading.title.font.size = 30,
      table.background.color = "black",
      table.font.color = "white"
    ) %>%
    opt_table_lines() %>%
    tab_style(
      style = cell_text(color = "#3d9be9"),
      locations = cells_column_labels()
    ) %>% 
    tab_style(
      style = list(
        cell_borders(
          sides = c("top"),
          color = "#3d9be9",
          weight = px(5)
        )
      ),
      locations = list(
        cells_body(
          columns = c("Bat Speed (mph)", "Rotational Acceleration (g)", "Power (kW)", "On Plane Efficiency (%)"),
          rows = "Goals"
        )
      )
    ) %>% 
    tab_style(
      style = list(
        cell_borders(
          sides = c("top"),
          color = "white",
          weight = px(5)
        )
      ),
      locations = list(
        cells_body(
          columns = c("Attack Angle (deg)", "Early Connection (deg)", "Connection at Impact (deg)", "Vertical Bat Angle (deg)"),
          rows = "Goals"
        )
      )
    ) %>% 
    tab_options(
      table.border.top.style = "hidden",
      heading.border.lr.style = "hidden"
    )
  
  gtsave(gt_table, file = paste0("Futures Reports Images/ ",athlete, "- hittingSummary.png"), vwidth = 1200, vheight = 500, expand = 0)
  
  playerSummary2 <- image_read(paste0("Futures Reports Images/ ",athlete,"- hittingSummary.png"))
  playerSummary2 <- playerSummary2 %>% 
    image_transparent(color = "black")
  PitchingReport6 <- image_composite(PitchingReport5,playerSummary2,offset= "+100+1850")
  
  if (!is.null(player_data)) {
    
    red_ranges <- list()
    if (current_level_blast == " L1") {
      red_ranges$bat_speed = list(xmin = 30, xmax = -Inf)
      red_ranges$rot_accel = list(ymin = 2, ymax = -Inf)
      red_ranges$on_plane_eff = list(xmin = 60, xmax = -Inf)
      red_ranges$attack_angle = list(ymin = 5, ymax = -Inf, ymin2 = 18, ymax2 = Inf)
      red_ranges$vertical_angle = list(ymin = -Inf, ymax = 15, ymin2 = 40, ymax2 = Inf)
      red_ranges$early_connection = list(ymin = -Inf, ymax = 80, ymin2 = 110, ymax2 = Inf)
      red_ranges$connection_impact = list(ymin = -Inf, ymax = 75, ymin2 = 100, ymax2 = Inf)
    } else if (current_level_blast == "L2") {
      red_ranges$bat_speed = list(xmin = 40, xmax = -Inf)
      red_ranges$rot_accel = list(ymin = 4, ymax = -Inf)
      red_ranges$on_plane_eff = list(xmin = 60, xmax = -Inf)
      red_ranges$attack_angle = list(ymin = 5, ymax = -Inf, ymin2 = 18, ymax2 = Inf)
      red_ranges$vertical_angle = list(ymin = -Inf, ymax = 15, ymin2 = 45, ymax2 = Inf)
      red_ranges$early_connection = list(ymin = -Inf, ymax = 85, ymin2 = 110, ymax2 = Inf)
      red_ranges$connection_impact = list(ymin = -Inf, ymax = 80, ymin2 = 95, ymax2 = Inf)
    } else if (current_level_blast %in% c("L3", "Collegiate", "Professional")) {
      red_ranges$bat_speed = list(xmin = 50, xmax = -Inf)
      red_ranges$rot_accel = list(ymin = 8, ymax = -Inf)
      red_ranges$on_plane_eff = list(xmin = 60, xmax = -Inf)
      red_ranges$attack_angle = list(ymin = 5, ymax = -Inf, ymin2 = 18, ymax2 = Inf)
      red_ranges$vertical_angle = list(ymin = -Inf, ymax = 22, ymin2 = 42, ymax2 = Inf)
      red_ranges$early_connection = list(ymin = -Inf, ymax = 85, ymin2 = 115, ymax2 = Inf)
      red_ranges$connection_impact = list(ymin = -Inf, ymax = 85, ymin2 = 95, ymax2 = Inf)
    }
    
    if (current_level_blast %in% c(" L1")) {
      contact_xlim <- c(20, 80)
      contact_ylim <- c(-10, 30)
    } else {
      contact_xlim <- c(30, 80)
      contact_ylim <- c(0, 30)
    }
    
    
    
    bat_speed_goal <- as.numeric(goals_data$`Bat Speed (mph)`)
    rot_accel_goal <- as.numeric(goals_data$`Rotational Acceleration (g)`)
    
    power_graph <- ggplot(player_data) +
      coord_cartesian(xlim = contact_xlim, ylim = contact_ylim) +
      annotate("rect", xmin = bat_speed_goal, xmax = Inf, ymin = rot_accel_goal, ymax = Inf, fill = "#00FF00", alpha = 0.25) +
      annotate("rect", xmin = red_ranges$bat_speed$xmin, xmax = red_ranges$bat_speed$xmax, ymin = red_ranges$rot_accel$ymin, ymax = red_ranges$rot_accel$ymax, fill = "#FF2400", alpha = 0.15) +
      geom_point(aes(x = `Bat Speed (mph)`, y = `Rotational Acceleration (g)`, color = `Swing Details`))
    
    attack_angle_goal <- strsplit(as.character(goals_data$`Attack Angle (deg)`), ' - ')
    attack_angle_min <- as.numeric(attack_angle_goal[[1]][1])
    attack_angle_max <- as.numeric(attack_angle_goal[[1]][2])
    on_plane_eff_goal <- as.numeric(goals_data$`On Plane Efficiency (%)`)
    
    contact_graph <- ggplot(player_data) +
      coord_cartesian(xlim = c(20, 100), ylim = c(-5, 25)) +
      annotate("rect", xmin = on_plane_eff_goal, xmax = Inf, ymin = attack_angle_min, ymax = attack_angle_max, fill = "#00FF00", alpha = 0.25) +
      annotate("rect", xmin = red_ranges$on_plane_eff$xmin, xmax = red_ranges$on_plane_eff$xmax, ymin = red_ranges$attack_angle$ymin, ymax = red_ranges$attack_angle$ymax, fill = "#FF2400", alpha = 0.15) +
      annotate("rect", xmin = red_ranges$on_plane_eff$xmin, xmax = red_ranges$on_plane_eff$xmax, ymin = red_ranges$attack_angle$ymin2, ymax = red_ranges$attack_angle$ymax2, fill = "#FF2400", alpha = 0.15) +
      geom_point(aes(x = `On Plane Efficiency (%)`, y = `Attack Angle (deg)`, color = `Swing Details`))
    
    early_connection_goal <- strsplit(as.character(goals_data$`Early Connection (deg)`), ' - ')
    early_connection_min <- as.numeric(early_connection_goal[[1]][1])
    early_connection_max <- as.numeric(early_connection_goal[[1]][2])
    
    load_graph <- ggplot(player_data) +
      coord_cartesian(xlim = c(0, -60), ylim = c(60, 140)) +
      annotate("rect", xmin = Inf, xmax = -Inf, ymin = early_connection_min, ymax = early_connection_max, fill = "#00FF00", alpha = 0.25) +
      annotate("rect", xmin = Inf, xmax = -Inf, ymin = red_ranges$early_connection$ymin, ymax = red_ranges$early_connection$ymax, fill = "#FF2400", alpha = 0.15) +
      annotate("rect", xmin = Inf, xmax = -Inf, ymin = red_ranges$early_connection$ymin2, ymax = red_ranges$early_connection$ymax2, fill = "#FF2400", alpha = 0.15) +
      geom_point(aes(x = `Vertical Bat Angle (deg)`, y = `Early Connection (deg)`, color = `Swing Details`))
    
    connection_impact_goal <- strsplit(as.character(goals_data$`Connection at Impact (deg)`), ' - ')
    connection_impact_min <- as.numeric(connection_impact_goal[[1]][1])
    connection_impact_max <- as.numeric(connection_impact_goal[[1]][2])
    
    impact_graph <- ggplot(player_data) +
      coord_cartesian(xlim = c(0, -60), ylim = c(60, 110)) +
      annotate("rect", xmin = Inf, xmax = -Inf, ymin = connection_impact_min, ymax = connection_impact_max, fill = "#00FF00", alpha = 0.25) +
      annotate("rect", xmin = Inf, xmax = -Inf, ymin = red_ranges$connection_impact$ymin, ymax = red_ranges$connection_impact$ymax, fill = "#FF2400", alpha = 0.15) +
      annotate("rect", xmin = Inf, xmax = -Inf, ymin = red_ranges$connection_impact$ymin2, ymax = red_ranges$connection_impact$ymax2, fill = "#FF2400", alpha = 0.15) +
      geom_point(aes(x = `Vertical Bat Angle (deg)`, y = `Connection at Impact (deg)`, color = `Swing Details`))
  } else {

    empty_core_df <- data.frame()
    
    power_graph <- empty_core_df %>% 
      ggplot() + 
      geom_point() + 
      xlim(0, 10) + 
      ylim(0, 100) +
      annotate("text", x = 5, y = 50, label = "No Blast Data Collected", 
               size = 8, color = "white", hjust = 0.5, vjust = 0.5)
    
    contact_graph <- empty_core_df %>% 
      ggplot() + 
      geom_point() + 
      xlim(0, 10) + 
      ylim(0, 100) +
      annotate("text", x = 5, y = 50, label = "No Blast Data Collected", 
               size = 8, color = "white", hjust = 0.5, vjust = 0.5)
    
    load_graph <- empty_core_df %>% 
      ggplot() + 
      geom_point() + 
      xlim(0, 10) + 
      ylim(0, 100) +
      annotate("text", x = 5, y = 50, label = "No Blast Data Collected", 
               size = 8, color = "white", hjust = 0.5, vjust = 0.5)
    
    impact_graph <- empty_core_df %>% 
      ggplot() + 
      geom_point() + 
      xlim(0, 10) + 
      ylim(0, 100) +
      annotate("text", x = 5, y = 50, label = "No Blast Data Collected", 
               size = 8, color = "white", hjust = 0.5, vjust = 0.5)
  }
  
  combined_plot <- (power_graph | contact_graph | load_graph | impact_graph) +
    plot_layout(guides = "collect", nrow = 1) &
    theme_minimal() +
    theme(legend.position = 'bottom',
          legend.title = element_text(color = "white", size = 20),
          legend.text = element_text(color = "white", size = 18),
          panel.grid = element_line(color = col_grid),
          axis.text=element_text(color = "white", size = 12),
          axis.title = element_text(color = "white", size = 14))
  
  ggsave(combined_plot,file=paste0("Futures Reports Images/",athlete," - swingProfile.png"), width=11,height=3.40,units="in", dpi = 215)
  pitchCharts5 <- image_read(paste0("Futures Reports Images/",athlete," - swingProfile.png"))
  PitchingReport7 <- image_composite(PitchingReport6,pitchCharts5, offset= "+100+2550")
  
  attendance_plot_data <- attendanceData %>%
    filter(`Client name` == athlete) %>% 
    mutate(`Attendance Score` = round(pmin((Attended / 4) * 100, 100), digits = 2))
  
  attendance_score <- max(attendance_plot_data$`Attendance Score`, na.rm = TRUE)
  
  get_color <- function(score) {
    if (score < 33) {
      return("red")
    } else if (score >= 33 & score < 66) {
      return("#FFA500")
    } else if (score >= 66 & score < 90) {
      return("green")
    } else {
      return("#3d9be9")
    }
  }
  
  # Check if all attendance scores are NA
  if (all(is.na(attendance_plot_data$`Attendance Score`))) {
    # Handle the scenario where all scores are missing
    attendance_plot <- ggplot(data.frame(ClientName = "N/A", y = 50), aes(x = ClientName, y = y)) +
      geom_col(aes(y = 100), alpha = 0.5, color = "black") +
      geom_text(aes(y = 50, label = "NA"), size = 12, color = "white") +
      coord_flip() +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
      scale_fill_identity()
  } else {
    # Existing code for when there are valid attendance scores
    attendance_plot <- attendance_plot_data %>% 
      ggplot(aes(x = `Client name`, y = `Attendance Score`)) +
      geom_col(aes(fill = get_color(`Attendance Score`))) +
      geom_col(aes(y = 100), alpha = 0.5, color = "black") +
      geom_text(aes(y = 50, label = paste(attendance_score, "%")), size = 14, fontface = "bold", color = "white") +
      coord_flip() +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
      scale_fill_identity()
  }
  
  
  ggsave(attendance_plot,file=paste0("Futures Reports Images/",athlete,"_attendancePlot.png"), width=2.90,height=0.75,units="in", dpi = 175)
  attendancePlot <- image_read(paste0("Futures Reports Images/",athlete,"_attendancePlot.png"))
  PitchingReport8 <- image_composite(PitchingReport7, attendancePlot, offset= "+1965+685")
  
  image_write(PitchingReport8,path = "page1.pdf",format="pdf",quality=100,density=300)
  image_write(IndexPage,path = "page2.pdf",format="pdf",quality=100,density=300)
  
  
  qpdf::pdf_combine(input = c("page1.pdf", "page2.pdf"),
                    output = paste0(athlete_folder, "/", "Futures Hitting Report.pdf")) 
}
