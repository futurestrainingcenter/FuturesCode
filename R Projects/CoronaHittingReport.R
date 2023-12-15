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

font_add(family = "Good Times", regular = "good times rg.otf")
showtext_auto()

blastData <- read_csv("/Users/watts/Downloads/Corona HS Blast Data - Sheet1.csv")
hittraxData <- read_csv("/Volumes/COLE'S DATA/Data/Hittrax Master Data - Sheet1.csv")
clientData <- read_csv("/Users/watts/Downloads/FullClientList.csv")
attendanceData <- read_csv("/Users/watts/Downloads/HittingAttendance_data.csv")
coronaHS <- read_csv("/Users/watts/Downloads/CoronaHSPlayers.csv") %>%
  mutate(Name = paste(`First Name`, `Last Name`))


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
    Power_YTD_Change = round(cumsum(coalesce(`Power (kW)` - lag(`Power (kW)`, order_by = match(Month, month.name)), 0)), 1)
  )

# Data manipulation for hittraxData
hittraxData <- hittraxData %>%
  arrange(Name, match(Month, month.name)) %>%
  filter(Month == "November" | Month == first(Month[Month %in% c("August", "September", "October", "November")])) %>%
  group_by(Name) %>%
  mutate(
    MaxVel_Aug_Nov_Change = pmax(0, ifelse(Month == "November", MaxVel - lag(MaxVel), NA), na.rm = TRUE),
    AvgVel_Aug_Nov_Change = pmax(0, ifelse(Month == "November", AvgVel - lag(AvgVel), NA), na.rm = TRUE),
    MaxDist_Aug_Nov_Change = pmax(0, ifelse(Month == "November", MaxDist - lag(MaxDist), NA), na.rm = TRUE),
    AvgDist_Aug_Nov_Change = pmax(0, ifelse(Month == "November", AvgDist - lag(AvgDist), NA), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(Sport, Month) %>%
  mutate(
    MaxVel_Rank = rank(-MaxVel, ties.method = "min"),
    AvgVel_Rank = rank(-AvgVel, ties.method = "min"),
    MaxDist_Rank = rank(-MaxDist, ties.method = "min"),
    AvgDist_Rank = rank(-AvgDist, ties.method = "min"),
    Total_Players = n()
  ) %>%
  ungroup()


athletes <- intersect(hittraxData$Name, coronaHS$Name)

TemplatePageOne <- image_read_pdf("/Volumes/COLE'S DATA/Templates/Corona Hitting Template.pdf")
IndexPage <- image_read_pdf("/Volumes/COLE'S DATA/Templates/Hitting Report Metric Index.pdf")

# Set the working directory
setwd("/Users/watts/Documents/Futures Performance Center/Test")

for (athlete in athletes){
  
  hittrax_plot_data <- hittraxData %>% 
    filter(Name == athlete, Month == "November")
  
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
               hjust = 0, color = "#ff3131", size = 8, family = "Good Times") +
      annotate("text", x = left_x + 0.05, y = y_positions[i], label = player_profile$value[player_profile$label == left_labels[i]], 
               hjust = 0, color = "white", size = 10, family = "Good Times")
  }
  
  # Add middle labels and their values
  for (i in 1:length(middle_labels)) {
    p <- p + 
      annotate("text", x = middle_x, y = y_positions[i], label = middle_labels[i], 
               hjust = 0, color = "#ff3131", size = 8, family = "Good Times") +
      annotate("text", x = middle_x + 0.1, y = y_positions[i], label = player_profile$value[player_profile$label == middle_labels[i]], 
               hjust = 0, color = "white", size = 10, family = "Good Times")
  }
  
  # Add right labels and their values
  for (i in 1:length(right_labels)) {
    p <- p + 
      annotate("text", x = right_x, y = y_positions[i], label = right_labels[i], 
               hjust = 0, color = "#ff3131", size = 8, family = "Good Times") +
      annotate("text", x = right_x + 0.075, y = y_positions[i], label = player_profile$value[player_profile$label == right_labels[i]], 
               hjust = 0, color = "white", size = 11, family = "Good Times")
  }
  
  ggsave(p,file=paste0("Futures Reports Images/",athlete," - playerProfile.png"), width=16,height=3,units="in", dpi = 150)
  playerSummary1 <- image_read(paste0("Futures Reports Images/",athlete," - playerProfile.png"))
  PitchingReport1 <- image_composite(TemplatePageOne, playerSummary1, offset= "+50+525")
  
  maxVel <- hittrax_plot_data %>%
    ggplot(aes(x = `MaxVel Rank`, y = "")) +
    geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
    geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
    geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
    geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
    geom_point(aes(fill = `MaxVel Rank`), color = "black", pch = 21, size = 12) +
    geom_text(aes(label = round(`MaxVel Rank`)), size = 10, fontface = "bold") +
    labs(title = paste(round(hittrax_plot_data$MaxVel, digits = 1), "MPH"),
         subtitle = paste("Fall Program Change:", round(hittrax_plot_data$MaxVel_Aug_Nov_Change, digits = 1))) +
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
  
  avgVel <- hittrax_plot_data %>%
    ggplot(aes(x = `AvgVel Rank`, y = "")) +
    geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
    geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
    geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
    geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
    geom_point(aes(fill = `AvgVel Rank`), color = "black", pch = 21, size = 12) +
    geom_text(aes(label = round(`AvgVel Rank`)), size = 10, fontface = "bold") +
    labs(title = paste(round(hittrax_plot_data$AvgVel, digits = 1), "MPH"),
         subtitle = paste("Fall Program Change:", round(hittrax_plot_data$AvgVel_Aug_Nov_Change, digits = 1))) +
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
  
  maxDist <- hittrax_plot_data %>%
    ggplot(aes(x = `MaxDist Rank`, y = "")) +
    geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
    geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
    geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
    geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
    geom_point(aes(fill = `MaxDist Rank`), color = "black", pch = 21, size = 12) +
    geom_text(aes(label = round(`MaxDist Rank`)), size = 10, fontface = "bold") +
    labs(title = paste(round(hittrax_plot_data$MaxDist, digits = 1), "Feet"),
         subtitle = paste("Fall Program Change:", round(hittrax_plot_data$MaxDist_Aug_Nov_Change, digits = 1))) +
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
  
  avgDist <- hittrax_plot_data %>%
    ggplot(aes(x = `AvgDist Rank`, y = "")) +
    geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
    geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
    geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
    geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
    geom_point(aes(fill = `AvgDist Rank`), color = "black", pch = 21, size = 12) +
    geom_text(aes(label = round(`AvgDist Rank`)), size = 10, fontface = "bold") +
    labs(title = paste(round(hittrax_plot_data$AvgDist, digits = 1), "Feet"),
         subtitle = paste("Fall Program Change:", round(hittrax_plot_data$AvgDist_Aug_Nov_Change, digits = 1))) +
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
  PitchingReport2 <- image_composite(PitchingReport1, pitchCharts1, offset= "+250+1000")
  
  ggsave(maxDist,file=paste0("Futures Reports Images/",athlete," - playerMaxDistPercentiles.png"), width=5,height=3,units="in", dpi = 170)
  pitchCharts2 <- image_read(paste0("Futures Reports Images/",athlete," - playerMaxDistPercentiles.png"))
  PitchingReport3 <- image_composite(PitchingReport2, pitchCharts2, offset= "+250+1375")
  
  ggsave(avgVel,file=paste0("Futures Reports Images/",athlete," - playerAvgEVPercentiles.png"), width=5,height=3,units="in", dpi = 170)
  pitchCharts3 <- image_read(paste0("Futures Reports Images/",athlete," - playerAvgEVPercentiles.png"))
  PitchingReport4 <- image_composite(PitchingReport3, pitchCharts3, offset= "+1450+1000")
  
  ggsave(avgDist,file=paste0("Futures Reports Images/",athlete," - playerAvgDistPercentiles.png"), width=5,height=3,units="in", dpi = 170)
  pitchCharts4 <- image_read(paste0("Futures Reports Images/",athlete," - playerAvgDistPercentiles.png"))
  PitchingReport5 <- image_composite(PitchingReport4, pitchCharts4, offset= "+1450+1375")
  
  finalBlastData <- left_join(blastData, clientData, by = "Name")
  
  current_level <- unique(finalBlastData$`Reporting Level (Age-Dependent)`)[1]
  
  # Define the columns
  columns <- c("Bat Speed (mph)", "Rotational Acceleration (g)", "Power (kW)", "On Plane Efficiency (%)", 
               "Attack Angle (deg)", "Early Connection (deg)", "Connection at Impact (deg)", "Vertical Bat Angle (deg)")
  
  # Create empty data frames for August and November
  empty_august <- data.frame(matrix(NA, ncol = length(columns), nrow = 1))
  names(empty_august) <- columns
  
  empty_november <- data.frame(matrix(NA, ncol = length(columns), nrow = 1))
  names(empty_november) <- columns
  
  # Filter and select data for August
  august_data <- blastData %>%
    ungroup() %>% 
    filter(Month == "August" & Name == athlete) %>% 
    select(all_of(columns))
  
  # Use empty data frame if august_data is empty
  if (nrow(august_data) == 0) {
    august_data <- empty_august
  }
  
  november_data <- blastData %>%
    ungroup() %>% 
    filter(Month == "November" & Name == athlete) %>% 
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
    select(all_of(columns))
  
  
  # Use empty data frame if november_data is empty
  if (nrow(november_data) == 0) {
    november_data <- empty_november
  }
  
  # Combine the data frames
  august_november_data <- rbind(august_data, november_data)
  
  # Rename columns
  names(august_november_data) <- c("Bat Speed\n(mph)", "Rotational\nAcceleration (g)", "Power\n(kW)", 
                                   "On Plane\nEfficiency (%)", "Attack Angle\n(deg)", "Early Connection\n(deg)", 
                                   "Connection at\nImpact (deg)", "Vertical Bat\nAngle (deg)")
  
  
  
  august_november_data$`Bat Speed\n(mph)` <- as.character(august_november_data$`Bat Speed\n(mph)`)
  august_november_data$`Rotational\nAcceleration (g)` <- as.character(august_november_data$`Rotational\nAcceleration (g)`)
  august_november_data$`Power\n(kW)` <- as.character(august_november_data$`Power\n(kW)`)
  august_november_data$`On Plane\nEfficiency (%)` <- as.character(august_november_data$`On Plane\nEfficiency (%)`)
  august_november_data$`Attack Angle\n(deg)` <- as.character(august_november_data$`Attack Angle\n(deg)`)
  august_november_data$`Early Connection\n(deg)` <- as.character(august_november_data$`Early Connection\n(deg)`)
  august_november_data$`Connection at\nImpact (deg)` <- as.character(august_november_data$`Connection at\nImpact (deg)`)
  august_november_data$`Vertical Bat\nAngle (deg)` <- as.character(august_november_data$`Vertical Bat\nAngle (deg)`)
  
  # Initialize goals_data with the same columns as august_november_data, filled with NA values
  goals_data <- as.data.frame(matrix(NA, ncol=ncol(august_november_data), nrow=1))
  colnames(goals_data) <- colnames(august_november_data)
  
  if(current_level %in% c("Collegiate")) {
    goals_data$`Bat Speed\n(mph)` <- "75"
    goals_data$`Rotational\nAcceleration (g)` <- "16"
    goals_data$`On Plane\nEfficiency (%)` <- "85"
    goals_data$`Power\n(kW)` <- "6.0"
    goals_data$`Attack Angle\n(deg)` <- "6 - 10"
    goals_data$`Early Connection\n(deg)` <- "85 - 105"
    goals_data$`Connection at\nImpact (deg)` <- "90 - 95"
    goals_data$`Vertical Bat\nAngle (deg)` <- "-27 to -37"
  } else if(current_level %in% c("L3")) {
    goals_data$`Bat Speed\n(mph)` <- "65"
    goals_data$`Rotational\nAcceleration (g)` <- "13"
    goals_data$`On Plane\nEfficiency (%)` <- "80"
    goals_data$`Power\n(kW)` <- "4.5"
    goals_data$`Attack Angle\n(deg)` <- "8 - 12"
    goals_data$`Early Connection\n(deg)` <- "85 - 105"
    goals_data$`Connection at\nImpact (deg)` <- "90 - 95"
    goals_data$`Vertical Bat\nAngle (deg)` <- "-27 to -37"
  } else if(current_level %in% c("L2")) {
    goals_data$`Bat Speed\n(mph)` <- "55"
    goals_data$`Rotational\nAcceleration (g)` <- "10"
    goals_data$`On Plane\nEfficiency (%)` <- "70"
    goals_data$`Power\n(kW)` <- "2.75"
    goals_data$`Attack Angle\n(deg)` <- "8 - 12"
    goals_data$`Early Connection\n(deg)` <- "85 - 110"
    goals_data$`Connection at\nImpact (deg)` <- "85 - 95"
    goals_data$`Vertical Bat\nAngle (deg)` <- "-20 to -30"
  } else if(current_level %in% c("L1")) {
    goals_data$`Bat Speed\n(mph)` <- "45"
    goals_data$`Rotational\nAcceleration (g)` <- "7.5"
    goals_data$`On Plane\nEfficiency (%)` <- "60"
    goals_data$`Power\n(kW)` <- "1.0"
    goals_data$`Attack Angle\n(deg)` <- "5 - 15"
    goals_data$`Early Connection\n(deg)` <- "80 - 110"
    goals_data$`Connection at\nImpact (deg)` <- "80 - 100"
    goals_data$`Vertical Bat\nAngle (deg)` <- "-15 to -25"
  } else {
    # If none of the conditions are met, set goals_data to NULL
    goals_data <- NULL
  }
  
  # Combine the dataframes only if goals_data is not NULL
  if (!is.null(goals_data)) {
    full_combined_data <- rbind(august_november_data, goals_data)
  } else {
    full_combined_data <- august_november_data
  }
  
  # Assuming combined_data is the final dataframe that needs the new column
  row_descriptions <- c("Pre-Fall", "Post-Fall", "Goals")
  
  # Add the new column to the front of the dataframe
  combined_player_data <- cbind(" " = row_descriptions, full_combined_data)
  
  transparent_theme <- ttheme_minimal(
    core = list(fg_params = list(col = "white"), bg_params = list(fill = "transparent", col = NA)),
    colhead = list(fg_params = list(col = "white"), bg_params = list(fill = "transparent", col = NA)),
    rowhead = list(fg_params = list(col = "white"), bg_params = list(fill = "transparent", col = NA)),
    padding = unit(c(4, 4), "mm")
  )
  
  png(paste0("Futures Reports Images/",athlete,"- hittingSummary.png"),height=1.5, width=10.5,units = "in",res = 225, bg = "transparent")
  table2 <- tableGrob(combined_player_data, rows = NULL, theme = transparent_theme)
  grid.arrange(table2)
  dev.off()
  
  playerSummary2 <- image_read(paste0("Futures Reports Images/",athlete,"- hittingSummary.png"))
  PitchingReport6 <- image_composite(PitchingReport5,playerSummary2,offset= "+100+1915")
  
  
  if (!is.null(goals_data)) {
    
    red_ranges <- list()
    if (current_level %in% c("L1", "Softball_L1")) {
      red_ranges$bat_speed = list(xmin = 30, xmax = -Inf)
      red_ranges$rot_accel = list(ymin = 2, ymax = -Inf)
      red_ranges$on_plane_eff = list(xmin = 60, xmax = -Inf)
      red_ranges$attack_angle = list(ymin = 5, ymax = -Inf, ymin2 = 18, ymax2 = Inf)
      red_ranges$vertical_angle = list(ymin = -Inf, ymax = 15, ymin2 = 40, ymax2 = Inf)
      red_ranges$early_connection = list(ymin = -Inf, ymax = 80, ymin2 = 110, ymax2 = Inf)
      red_ranges$connection_impact = list(ymin = -Inf, ymax = 75, ymin2 = 100, ymax2 = Inf)
    } else if (current_level %in% c("L2", "Softball_L2")) {
      red_ranges$bat_speed = list(xmin = 40, xmax = -Inf)
      red_ranges$rot_accel = list(ymin = 4, ymax = -Inf)
      red_ranges$on_plane_eff = list(xmin = 60, xmax = -Inf)
      red_ranges$attack_angle = list(ymin = 5, ymax = -Inf, ymin2 = 18, ymax2 = Inf)
      red_ranges$vertical_angle = list(ymin = -Inf, ymax = 15, ymin2 = 45, ymax2 = Inf)
      red_ranges$early_connection = list(ymin = -Inf, ymax = 85, ymin2 = 110, ymax2 = Inf)
      red_ranges$connection_impact = list(ymin = -Inf, ymax = 80, ymin2 = 95, ymax2 = Inf)
    } else if (current_level %in% c("L3", "Collegiate")) {
      red_ranges$bat_speed = list(xmin = 50, xmax = -Inf)
      red_ranges$rot_accel = list(ymin = 8, ymax = -Inf)
      red_ranges$on_plane_eff = list(xmin = 60, xmax = -Inf)
      red_ranges$attack_angle = list(ymin = 5, ymax = -Inf, ymin2 = 18, ymax2 = Inf)
      red_ranges$vertical_angle = list(ymin = -Inf, ymax = 22, ymin2 = 42, ymax2 = Inf)
      red_ranges$early_connection = list(ymin = -Inf, ymax = 85, ymin2 = 115, ymax2 = Inf)
      red_ranges$connection_impact = list(ymin = -Inf, ymax = 85, ymin2 = 95, ymax2 = Inf)
    }
    
    if (current_level %in% c("L1", "Softball_L1")) {
      contact_xlim <- c(20, 80)
      contact_ylim <- c(-10, 30)
    } else {
      contact_xlim <- c(30, 80)
      contact_ylim <- c(0, 30)
    }
    
    col_grid <- rgb(235, 235, 235, 25, maxColorValue = 255)
    
    bat_speed_goal <- as.numeric(goals_data$`Bat Speed\n(mph)`)
    rot_accel_goal <- as.numeric(goals_data$`Rotational\nAcceleration (g)`)
    
    graph_blast_data <- blastData %>%
      group_by(Month) %>% 
      filter(Name == athlete & Month %in% c("August", "November")) %>% 
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
      )
      
    power_graph <- ggplot(graph_blast_data) +
      coord_cartesian(xlim = contact_xlim, ylim = contact_ylim) +
      geom_rect(aes(xmin = bat_speed_goal, xmax = Inf, ymin = rot_accel_goal, ymax = Inf), fill = "#00FF00", alpha = 0.1) +
      geom_rect(aes(xmin = red_ranges$bat_speed$xmin, xmax = red_ranges$bat_speed$xmax, ymin = red_ranges$rot_accel$ymin, ymax = red_ranges$rot_accel$ymax), fill = "#FF2400", alpha = 0.075) +
      geom_point(aes(x = `Bat Speed (mph)`, y = `Rotational Acceleration (g)`, color = Month), size = 4) +
      scale_color_manual(values = c("August" = "#F8766D", "November" = "#00BFC4")) +      
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(color = "white", size = 17),
            panel.grid = element_line(color = col_grid),
            axis.text=element_text(color = "white", size = 12),
            axis.title = element_text(color = "white", size = 14),
            plot.title = element_text(color = "white", size = 18),
            plot.subtitle = element_text(color = "white", size = 16))
    
    attack_angle_goal <- strsplit(as.character(goals_data$`Attack Angle\n(deg)`), ' - ')
    attack_angle_min <- as.numeric(attack_angle_goal[[1]][1])
    attack_angle_max <- as.numeric(attack_angle_goal[[1]][2])
    on_plane_eff_goal <- as.numeric(goals_data$`On Plane\nEfficiency (%)`)
    
    contact_graph <- ggplot(graph_blast_data) +
      coord_cartesian(xlim = c(20, 100), ylim = c(-5, 25)) +
      geom_rect(aes(xmin = on_plane_eff_goal, xmax = Inf, ymin = attack_angle_min, ymax = attack_angle_max), fill = "#00FF00", alpha = 0.1) +
      geom_rect(aes(xmin = red_ranges$on_plane_eff$xmin, xmax = red_ranges$on_plane_eff$xmax, ymin = red_ranges$attack_angle$ymin, ymax = red_ranges$attack_angle$ymax), fill = "#FF2400", alpha = 0.075) +
      geom_rect(aes(xmin = red_ranges$on_plane_eff$xmin, xmax = red_ranges$on_plane_eff$xmax, ymin = red_ranges$attack_angle$ymin2, ymax = red_ranges$attack_angle$ymax2), fill = "#FF2400", alpha = 0.075) +
      geom_point(aes(x = `On Plane Efficiency (%)`, y = `Attack Angle (deg)`, color = Month), size = 4) +
      scale_color_manual(values = c("August" = "#F8766D", "November" = "#00BFC4")) +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(color = "white", size = 17),
            panel.grid = element_line(color = col_grid),
            axis.text=element_text(color = "white", size = 12),
            axis.title = element_text(color = "white", size = 14),
            plot.title = element_text(color = "white", size = 18),
            plot.subtitle = element_text(color = "white", size = 16))
    
    early_connection_goal <- strsplit(as.character(goals_data$`Early Connection\n(deg)`), ' - ')
    early_connection_min <- as.numeric(early_connection_goal[[1]][1])
    early_connection_max <- as.numeric(early_connection_goal[[1]][2])
    
    load_graph <- ggplot(graph_blast_data) +
      coord_cartesian(xlim = c(0, -60), ylim = c(60, 140)) +
      geom_rect(aes(xmin = Inf, xmax = -Inf, ymin = early_connection_min, ymax = early_connection_max), fill = "#00FF00", alpha = 0.1) +
      geom_rect(aes(xmin = Inf, xmax = -Inf, ymin = red_ranges$early_connection$ymin, ymax = red_ranges$early_connection$ymax), fill = "#FF2400", alpha = 0.075) +
      geom_rect(aes(xmin = Inf, xmax = -Inf, ymin = red_ranges$early_connection$ymin2, ymax = red_ranges$early_connection$ymax2), fill = "#FF2400", alpha = 0.075) +
      geom_point(aes(x = `Vertical Bat Angle (deg)`, y = `Early Connection (deg)`, color = Month), size = 4) +
      scale_color_manual(values = c("August" = "#F8766D", "November" = "#00BFC4")) +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(color = "white", size = 17),
            panel.grid = element_line(color = col_grid),
            axis.text=element_text(color = "white", size = 12),
            axis.title = element_text(color = "white", size = 14),
            plot.title = element_text(color = "white", size = 18),
            plot.subtitle = element_text(color = "white", size = 16))
    
    connection_impact_goal <- strsplit(as.character(goals_data$`Connection at\nImpact (deg)`), ' - ')
    connection_impact_min <- as.numeric(connection_impact_goal[[1]][1])
    connection_impact_max <- as.numeric(connection_impact_goal[[1]][2])
    
    impact_graph <- ggplot(graph_blast_data) +
      coord_cartesian(xlim = c(0, -60), ylim = c(60, 110)) +
      geom_rect(aes(xmin = Inf, xmax = -Inf, ymin = connection_impact_min, ymax = connection_impact_max), fill = "#00FF00", alpha = 0.1) +
      geom_rect(aes(xmin = Inf, xmax = -Inf, ymin = red_ranges$connection_impact$ymin, ymax = red_ranges$connection_impact$ymax), fill = "#FF2400", alpha = 0.075) +
      geom_rect(aes(xmin = Inf, xmax = -Inf, ymin = red_ranges$connection_impact$ymin2, ymax = red_ranges$connection_impact$ymax2), fill = "#FF2400", alpha = 0.075) +
      geom_point(aes(x = `Vertical Bat Angle (deg)`, y = `Connection at Impact (deg)`, color = Month), size = 4) +
      scale_color_manual(values = c("August" = "#F8766D", "November" = "#00BFC4")) +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(color = "white", size = 17),
            panel.grid = element_line(color = col_grid),
            axis.text=element_text(color = "white", size = 12),
            axis.title = element_text(color = "white", size = 14),
            plot.title = element_text(color = "white", size = 18),
            plot.subtitle = element_text(color = "white", size = 16))
  } else {
    power_graph <- ggplot(graph_blast_data) +
      geom_point(aes(x = `Bat Speed (mph)`, y = `Rotational Acceleration (g)`)) +
      coord_cartesian(xlim = c(30, 80), ylim = c(0, 30)) +
      theme_minimal() +
      theme(legend.position = "none")
    
    contact_graph <- ggplot(graph_blast_data) +
      geom_point(aes(x = `On Plane Efficiency (%)`, y = `Attack Angle (deg)`)) +
      coord_cartesian(xlim = c(20, 100), ylim = c(-5, 25)) +
      theme_minimal() +
      theme(legend.position = "none")
    
    load_graph <- ggplot(graph_blast_data) +
      geom_point(aes(x = `Vertical Bat Angle (deg)`, y = `Early Connection (deg)`)) +
      coord_cartesian(xlim = c(0, -60), ylim = c(60, 140)) +
      theme_minimal() +
      theme(legend.position = "none")
    
    impact_graph <- ggplot(graph_blast_data) +
      geom_point(aes(x = `Vertical Bat Angle (deg)`, y = `Connection at Impact (deg)`)) +
      coord_cartesian(xlim = c(0, -60), ylim = c(60, 110)) +
      theme_minimal() +
      theme(legend.position = "none")
  }
  
  plots_row <- ggarrange(power_graph, contact_graph, load_graph, impact_graph, nrow = 1)
  ggsave(plots_row,file=paste0("Futures Reports Images/",athlete," - swingProfile.png"), width=12,height=3.35,units="in", dpi = 195)
  pitchCharts5 <- image_read(paste0("Futures Reports Images/",athlete," - swingProfile.png"))
  PitchingReport7 <- image_composite(PitchingReport6,pitchCharts5, offset= "+100+2600")
  
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
