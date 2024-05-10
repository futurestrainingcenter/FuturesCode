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

clientData <- read_csv("/Users/watts/Downloads/FullClientList.csv") %>% 
  rename(Name = Client)
hardNinety <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Speed Percentiles/Hard90percentiles.csv") %>% 
  filter(Month %in% c("March", "April"))
accelerationData<- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Speed Percentiles/AccelerationPercentiles.csv") %>% 
  filter(Month %in% c("March", "April"))
maxVeloData <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Speed Percentiles/MaxVelocityPercentiles.csv") %>% 
  filter(Month %in% c("March", "April"))
RSIdata <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Speed Percentiles/RSIpercentiles.csv") %>% 
  filter(Month %in% c("March", "April"))

LA_attendanceData <- read_csv("/Users/watts/Downloads/Learning_Academy_Body_Weight.csv") %>% 
  mutate(Date = as.Date(Date, format="%m/%d/%y"),
         Month = month(Date, label = TRUE, abbr = FALSE))

attendanceData <- read_csv("/Users/watts/Downloads/CheckIns.csv") %>%
  rename(Name = Client) %>% 
  mutate(Date = as.Date(Date, format = "%b %d, %Y"),
         Month = month(Date, label = TRUE, abbr = FALSE),
         LearningBlock = case_when(
           `Service Name` == "Learning Academy - Block 1" ~ "Learning Academy - Attended",
           `Service Name` == "Learning Academy - Block 2" ~ "Learning Academy - Attended",
           TRUE ~ `Service Name`
         )) %>%
  filter(`Service Name` %in% c("Speed L1", "Speed L2", "Speed L3", "Professional - Speed Training", "Learning Academy - Attended")) %>% 
  distinct(Name, Date, LearningBlock, .keep_all = TRUE)

summary_attendanceData <- attendanceData %>%
  filter(Month %in% c("March", "April")) %>%
  group_by(Name) %>%
  summarise(Attendance = n(), 
            .groups = 'drop')

# hardNinety$Date <- as.Date(hardNinety$Date, format="%y-%m-%d")
# accelerationData$Date <- as.Date(accelerationData$Date, format="%y-%m-%d")
# maxVeloData$Date <- as.Date(maxVeloData$Date, format="%y-%m-%d")
# RSIdata$Date <- as.Date(RSIdata$Date, format="%m/%d/%Y")

clientNames <- clientData$Name

# # Compare and extract non-matching names
# nonMatchingVALD <- setdiff(hardNinety$FullName, clientNames)
# 
# # Combine non-matching names into one data frame
# nonMatching <- rbind(
#   data.frame(Name = nonMatchingVALD, Source = 'VALD')
# )
# 
# # Write the non-matching names to a new CSV file
# write_csv(nonMatching, "/Users/watts/Downloads/missing_speed_data.csv")

RSIdata <- RSIdata %>%
  mutate(Weight = round(`BW [KG]` * 2.20462, digits = 1))

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

TemplatePageOne <- image_read_pdf("/Volumes/COLE'S DATA/Templates/Speed Report Template.pdf")
IndexPage <- image_read_pdf("/Volumes/COLE'S DATA/Templates/Speed Report Index.pdf")

setwd("/Users/watts/Documents/Futures Performance Center/Test")

athletes <- unique(hardNinety$FullName)

col_grid <- rgb(235, 235, 235, 50, maxColorValue = 255)

low_attendance_athletes <- c()

for (athlete in athletes){
  
  ########################################################################################################
  #############################################  ATTENDANCE  #############################################
  ########################################################################################################
  
  attendance_plot_data <- summary_attendanceData %>%
    filter(Name == athlete) %>% 
    mutate(`Total Weeks` = 8, # Adjust this number based on the exact number of weeks in the 2-month period
           `Attendance Score` = round(Attendance / `Total Weeks`, digits = 1))
  
  attendance_score <- max(attendance_plot_data$`Attendance Score`, na.rm = TRUE)
  
  if (attendance_score == -Inf || is.na(attendance_score) || attendance_score <= 0.1) {
    low_attendance_athletes <- c(low_attendance_athletes, athlete)
    next
  }
  
  athlete_folder <- paste0("Futures Reports/", athlete)
  if (!dir.exists(athlete_folder)) {
    dir.create(athlete_folder, recursive = TRUE)
  }
  
  get_color <- function(scores) {
    sapply(scores, function(score) {
      if (is.na(score)) {
        return(NA)
      } else if (score < 0.5) {
        return("#FF0000")
      } else if (score >= 0.5 & score < 1) {
        return("#FFA500")
      } else if (score >= 1 & score < 1.5) {
        return("green")
      } else {
        return("#3d9be9")
      }
    })
  }
  
  attendance_plot <- ggplot(attendance_plot_data, aes(x = Name, y = `Attendance Score`)) +
    geom_col(aes(fill = get_color(`Attendance Score`))) +
    geom_col(aes(y = 2), alpha = 0.5, color = "black") +
    geom_text(aes(y = 1, label = paste(attendance_score)), size = 14, fontface = "bold", color = "white") +
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
  
  ggsave(attendance_plot,file=paste0("Futures Reports Images/",athlete,"_attendancePlot.png"), width=6,height=1,units="in", dpi = 150)
  attendancePlot <- image_read(paste0("Futures Reports Images/",athlete,"_attendancePlot.png"))
  PitchingReport0 <- image_composite(TemplatePageOne, attendancePlot, offset= "+215+1340")
  
  ########################################################################################################
  #############################################PLAYER PROFILE#############################################
  ########################################################################################################
  
  if(!any(clientData$Name == athlete)) {
    player_profile <- data.frame(
      Name = NA, 
      Age = NA, 
      Level = NA,
      Position = NA, 
      Height = NA,
      stringsAsFactors = FALSE
    )
    names(player_profile) <- c("Name:", "Age:",  "Level:", "Position:", "Height:")
  } else {
    player_profile <- clientData %>%
      filter(Name == athlete) %>%
      select(Name, Age, `Sports Performance Training/Booking Level`, `Position (Baseball/Softball)`, Height)
    names(player_profile) <- c("Name:", "Age:", "Level:", "Position:", "Height:")
  }
  
  if (!any(LA_attendanceData$Name == athlete)) {
    if (!any(RSIdata$FullName == athlete)) {
      player_profile_two <- data.frame(
        Weight = NA,
        stringsAsFactors = FALSE
      )
      names(player_profile_two) <- c("Weight:")
      
      combined_profile <- cbind(player_profile, player_profile_two)
    } else {
      player_profile_two <- RSIdata %>%
        filter(FullName == athlete) %>%
        select(Weight) %>%
        slice(n())
      names(player_profile_two) <- c("Weight:")
      
      combined_profile <- cbind(player_profile, player_profile_two)
      
      combined_profile$`Weight:` <- paste(combined_profile$`Weight:`, "lbs")
    }
  } else {
    player_profile_two <- LA_attendanceData %>%
      filter(Name == athlete) %>%
      select(Weight) %>%
      slice(n())
    names(player_profile_two) <- c("Weight:")
    
    combined_profile <- cbind(player_profile, player_profile_two)
    
    combined_profile$`Weight:` <- paste(combined_profile$`Weight:`, "lbs")  
  }
  
  combined_profile <- combined_profile %>% 
    mutate(across(c(`Name:`, `Age:`, `Level:`, `Position:`, `Weight:`, `Height:`), as.character)) %>%
    pivot_longer(cols = c(`Name:`, `Age:`, `Level:`, `Position:`, `Weight:`, `Height:`), names_to = "label", values_to = "value")
  
  # Define the coordinates for the labels and values
  left_labels <- c("Name:", "Age:", "Height:", "Position:")
  right_labels <- c("Level:", "Weight:")
  left_x <- 0.1  # x position for left labels
  right_x <- 0.353 # x position for right labels
  y_positions <- seq(0.9, 0.3, by = -0.1)  # y positions for each label
  
  # Create a blank ggplot object
  p <- ggplot() +
    expand_limits(x = 0.575) +
    theme_void() 
  
  # Add left labels and their values
  for (i in 1:length(left_labels)) {
    p <- p + 
      annotate("text", x = left_x, y = y_positions[i], label = left_labels[i], 
               hjust = 0, color = "#3d9be9", size = 8, family = "Good Times") +
      annotate("text", x = left_x + 0.1, y = y_positions[i], label = combined_profile$value[combined_profile$label == left_labels[i]], 
               hjust = 0, color = "white", size = 11, family = "Good Times")
  }
  
  # Add right labels and their values
  for (i in 1:length(right_labels)) {
    p <- p + 
      annotate("text", x = right_x, y = y_positions[i + 1], label = right_labels[i], 
               hjust = 0, color = "#3d9be9", size = 8, family = "Good Times") +
      annotate("text", x = right_x + 0.1, y = y_positions[i + 1], label = combined_profile$value[combined_profile$label == right_labels[i]], 
               hjust = 0, color = "white", size = 11, family = "Good Times")
  }
  
  ggsave(p,file=paste0("Futures Reports Images/",athlete,"_playerSummary.png"), width=6,height=2.75,units="in", dpi = 175)
  strengthSummarys <- image_read(paste0("Futures Reports Images/",athlete,"_playerSummary.png"))
  PitchingReport1 <- image_composite(PitchingReport0,strengthSummarys,offset= "+125+550")
  
  
  ########################################################################################################
  #############################################   Hard 90   ##############################################
  ########################################################################################################
  
  if(any(hardNinety$FullName == athlete)) {
    
    hardNinety_graph_data <- hardNinety %>% 
      filter(FullName == athlete) %>%
      group_by(Date, Level, Gender) %>%
      summarize(Cumulative2 = min(Cumulative2, na.rm = TRUE),
                PercentileRank = max(PercentileRank, na.rm = TRUE),
                .groups = "drop")
    
    minHardNinety <- min(hardNinety_graph_data$Cumulative2, na.rm = TRUE)
    
    hardNinety_percentile_graph <- hardNinety_graph_data %>% 
      ggplot(aes(x = max(PercentileRank), y = "")) +
      geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
      geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(fill = max(PercentileRank)), color = "black", pch = 21, size = 12) +
      geom_text(aes(label = round(max(PercentileRank))), size = 8, fontface = "bold") +
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
            axis.title.x = element_blank()) +
      annotate("text", x = 0, y = 1, label = "90 Feet Sprint: ", hjust = 0, vjust = -2.5, color = "white", size = 12, family = "Good Times") +
      annotate("text", x = 100, y = 1, label = paste(round(minHardNinety, digits = 1), " S", sep = ""), hjust = 1, vjust = -2.5, color = "white", size = 12, family = "Good Times")
    
    hardNinety_level <- unique(hardNinety_graph_data$Level)[1]
    hardNinety_gender <- unique(hardNinety_graph_data$Gender)[1]
    
    hardNinety_ylim_data <- hardNinety %>% 
      filter(Level == hardNinety_level, Gender == hardNinety_gender)
    
    hardNinety_ylim_min <- min(hardNinety_ylim_data$Cumulative2, na.rm = TRUE)
    hardNinety_ylim_max <- max(hardNinety_ylim_data$Cumulative2, na.rm = TRUE)
    
    hardNinety_graph <- hardNinety_graph_data %>% 
      ggplot(aes(x = Date, y = Cumulative2)) +
      geom_line(linewidth = 2, color = "#FF0000") +
      geom_point(size = 4, color = "#FF0000") +
      scale_y_reverse() +
      ylim(hardNinety_ylim_max, hardNinety_ylim_min) +
      labs(y = "Seconds", caption = "*Y-axis inverted for visual purposes") +
      theme_minimal() +
      theme(legend.position = "none",
            panel.grid = element_line(color = col_grid),
            axis.title.y = element_text(color = "white", size = 20),
            axis.title.x = element_blank(),
            axis.text = element_text(color = "white", size = 15),
            plot.caption = element_text(color = "white", size = 18)
      )
    
    hardNinety_percentile_score <- max(hardNinety_graph_data$PercentileRank, na.rm = TRUE)
    
    ggsave(hardNinety_graph,file=paste0("Futures Reports Images/",athlete,"_hardNinetyPlot.png"), width=6.75,height=3,units="in", dpi = 150)
    strengthCharts1 <- image_read(paste0("Futures Reports Images/",athlete,"_hardNinetyPlot.png"))
    PitchingReport2 <- image_composite(PitchingReport1, strengthCharts1, offset= "+1350+885")
    
    ggsave(hardNinety_percentile_graph,file=paste0("Futures Reports Images/",athlete,"_hardNinetyPercentiles.png"), width=7,height=2,units="in", dpi = 150)
    strengthCharts2 <- image_read(paste0("Futures Reports Images/",athlete,"_hardNinetyPercentiles.png"))
    PitchingReport3 <- image_composite(PitchingReport2, strengthCharts2, offset= "+1340+1360")
  } else {
    
    empty_hardNinety_df <- data.frame()
    
    empty_percentile_graph <- empty_hardNinety_df %>% 
      ggplot() +
      geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
      geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
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
            axis.title.x = element_blank()
      )
    
    empty_hardNinety_plot <- empty_hardNinety_df %>% 
      ggplot() + 
      geom_point() + 
      xlim(0, 10) + 
      ylim(0, 100) +
      annotate("text", x = 5, y = 50, label = "No data collected.\nPlease bring report to Futures Coaches to test.", 
               size = 8, color = "white", hjust = 0.5, vjust = 0.5) +
      theme_minimal() +
      theme(panel.grid = element_line(color = col_grid),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    
    hardNinety_percentile_score <- 0
    
    ggsave(empty_hardNinety_plot,file=paste0("Futures Reports Images/", athlete,"_hardNinetyPlot.png"), width=6,height=3,units="in", dpi = 150)
    strengthCharts1 <- image_read(paste0("Futures Reports Images/", athlete,"_hardNinetyPlot.png"))
    PitchingReport2 <- image_composite(PitchingReport1, strengthCharts1, offset= "+1425+885")
    
    ggsave(empty_percentile_graph,file=paste0("Futures Reports Images/", athlete,"_hardNinetyPercentiles.png"), width=7,height=2,units="in", dpi = 150)
    strengthCharts2 <- image_read(paste0("Futures Reports Images/", athlete,"_hardNinetyPercentiles.png"))
    PitchingReport3 <- image_composite(PitchingReport2, strengthCharts2, offset= "+1350+1360")
    
  }
  
  ########################################################################################################
  ############################################# Acceleration #############################################
  ########################################################################################################
  
  if(any(accelerationData$FullName == athlete)) {
    
    acceleration_graph_data <- accelerationData %>% 
      filter(FullName == athlete) %>%
      group_by(Date, Level, Gender) %>%
      summarize(Split1 = min(Split1, na.rm = TRUE),
                PercentileRank = max(PercentileRank, na.rm = TRUE),
                .groups = "drop")
    
    maxAcceleration <- min(acceleration_graph_data$Split1, na.rm = TRUE)
    
    acceleration_percentile_graph <- acceleration_graph_data %>% 
      ggplot(aes(x = max(PercentileRank), y = "")) +
      geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
      geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(fill = max(PercentileRank)), color = "black", pch = 21, size = 12) +
      geom_text(aes(label = round(max(PercentileRank))), size = 8, fontface = "bold") +
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
            axis.title.x = element_blank()) +
      annotate("text", x = 0, y = 1, label = "10 Yard Sprint: ", hjust = 0, vjust = -2.5, color = "white", size = 12, family = "Good Times") +
      annotate("text", x = 100, y = 1, label = paste(round(maxAcceleration, digits = 1), " S", sep = ""), hjust = 1, vjust = -2.5, color = "white", size = 12, family = "Good Times")
    
    acceleration_level <- unique(acceleration_graph_data$Level)[1]
    acceleration_gender <- unique(acceleration_graph_data$Gender)[1]
    
    acceleration_ylim_data <- accelerationData %>% 
      filter(Level == acceleration_level, Gender == acceleration_gender)
    
    acceleration_ylim_min <- min(acceleration_ylim_data$Split1, na.rm = TRUE)
    acceleration_ylim_max <- max(acceleration_ylim_data$Split1, na.rm = TRUE)
    
    acceleration_graph <- acceleration_graph_data %>% 
      ggplot(aes(x = Date , y = Split1)) +
      geom_line(linewidth = 2, color = "#FF0000") +
      geom_point(size = 4, color = "#FF0000") +
      scale_y_reverse() +
      ylim(acceleration_ylim_max, acceleration_ylim_min) +
      labs(y = "Seconds", caption = "*Y-axis inverted for visual purposes") +
      theme_minimal() +
      theme(legend.position = "none",
            panel.grid = element_line(color = col_grid),
            axis.title.y = element_text(color = "white", size = 20),
            axis.title.x = element_blank(),
            axis.text=element_text(color = "white", size = 15),
            plot.caption = element_text(color = "white", size = 18)
      )
    
    acceleration_percentile_score <- max(acceleration_graph_data$PercentileRank, na.rm = TRUE)
    
    ggsave(acceleration_graph,file=paste0("Futures Reports Images/",athlete,"_accelerationPlot.png"), width=6.75,height=3,units="in", dpi = 150)
    strengthCharts3 <- image_read(paste0("Futures Reports Images/",athlete,"_accelerationPlot.png"))
    PitchingReport4 <- image_composite(PitchingReport3, strengthCharts3, offset= "+150+1775")
    
    ggsave(acceleration_percentile_graph,file=paste0("Futures Reports Images/",athlete,"_accelerationPercentiles.png"), width=7,height=2,units="in", dpi = 150)
    strengthCharts4 <- image_read(paste0("Futures Reports Images/",athlete,"_accelerationPercentiles.png"))
    PitchingReport5 <- image_composite(PitchingReport4, strengthCharts4, offset= "+140+2250")
  } else {
    
    empty_acceleration_df <- data.frame()
    
    empty_percentile_graph <- empty_acceleration_df %>% 
      ggplot() +
      geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
      geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
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
            axis.title.x = element_blank()
      )
    
    empty_plot <- empty_acceleration_df %>% 
      ggplot() + 
      geom_point() + 
      xlim(0, 10) + 
      ylim(0, 100) +
      annotate("text", x = 5, y = 50, label = "No data collected.\nPlease bring report to Futures Coaches to test.", 
               size = 8, color = "white", hjust = 0.5, vjust = 0.5) +
      theme_minimal() +
      theme(panel.grid = element_line(color = col_grid),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    
    acceleration_percentile_score <- 0
    
    ggsave(empty_plot,file=paste0("Futures Reports Images/", athlete,"_accelerationPlot.png"), width=6,height=3,units="in", dpi = 150)
    strengthCharts3 <- image_read(paste0("Futures Reports Images/", athlete,"_accelerationPlot.png"))
    PitchingReport4 <- image_composite(PitchingReport3, strengthCharts3, offset= "+175+1775")
    
    ggsave(empty_percentile_graph,file=paste0("Futures Reports Images/", athlete,"_accelerationPercentiles.png"), width=7,height=2,units="in", dpi = 150)
    strengthCharts4 <- image_read(paste0("Futures Reports Images/", athlete,"_accelerationPercentiles.png"))
    PitchingReport5 <- image_composite(PitchingReport4, strengthCharts4, offset= "+115+2250")
    
  }
  
  ########################################################################################################
  ############################################# Max Velocity #############################################
  ########################################################################################################
  
  if(any(maxVeloData$FullName == athlete)) {
    
    maxVelo_graph_data <- maxVeloData %>% 
      filter(FullName == athlete) %>%
      group_by(Date, Level, Gender) %>%
      summarize(MPH = max(MPH, na.rm = TRUE),
                PercentileRank = max(PercentileRank, na.rm = TRUE),
                .groups = "drop")
    
    maxMaxVelo <- max(maxVelo_graph_data$MPH, na.rm = TRUE)
    
    maxVelo_percentile_graph <- maxVelo_graph_data %>% 
      ggplot(aes(x = max(PercentileRank), y = "")) +
      geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
      geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(fill = max(PercentileRank)), color = "black", pch = 21, size = 12) +
      geom_text(aes(label = round(max(PercentileRank))), size = 8, fontface = "bold") +
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
            axis.title.x = element_blank()) +
      annotate("text", x = 0, y = 1, label = "Flying 10y Sprint: ", hjust = 0, vjust = -2.5, color = "white", size = 12, family = "Good Times") +
      annotate("text", x = 100, y = 1, label = paste(round(maxMaxVelo, digits = 1), " MPH", sep = ""), hjust = 1, vjust = -2.5, color = "white", size = 12, family = "Good Times")
    
    maxVelo_level <- unique(maxVelo_graph_data$Level)[1]
    maxVelo_gender <- unique(maxVelo_graph_data$Gender)[1]
    
    maxVelo_ylim_data <- maxVeloData %>% 
      filter(Level == maxVelo_level, Gender == maxVelo_gender)
    
    maxVelo_ylim_min <- min(maxVelo_ylim_data$MPH, na.rm = TRUE)
    maxVelo_ylim_max <- max(maxVelo_ylim_data$MPH, na.rm = TRUE)
    
    maxVelo_graph <- maxVelo_graph_data %>% 
      ggplot(aes(x = Date , y = MPH)) +
      geom_line(linewidth = 2, color = "#FF0000") +
      geom_point(size = 4, color = "#FF0000") +
      ylim(maxVelo_ylim_min, maxVelo_ylim_max) +
      labs(y = "MPH") +
      theme_minimal() +
      theme(legend.position = "none",
            panel.grid = element_line(color = col_grid),
            axis.title.y = element_text(color = "white", size = 20),
            axis.title.x = element_blank(),
            axis.text=element_text(color = "white", size = 15),
            plot.title = element_text(hjust = 0.5, color = "white", size = 18, family = "Good Times")
      )
    
    maxVelo_percentile_score <- max(maxVelo_graph_data$PercentileRank, na.rm = TRUE)
    
    ggsave(maxVelo_graph,file=paste0("Futures Reports Images/",athlete,"_maxVeloPlot.png"), width=6.75,height=3,units="in", dpi = 150)
    strengthCharts5 <- image_read(paste0("Futures Reports Images/",athlete,"_maxVeloPlot.png"))
    PitchingReport6 <- image_composite(PitchingReport5, strengthCharts5, offset= "+1350+1775")
    
    ggsave(maxVelo_percentile_graph,file=paste0("Futures Reports Images/",athlete,"_maxVeloPercentiles.png"), width=7,height=2,units="in", dpi = 150)
    strengthCharts6 <- image_read(paste0("Futures Reports Images/",athlete,"_maxVeloPercentiles.png"))
    PitchingReport7 <- image_composite(PitchingReport6, strengthCharts6, offset= "+1340+2250")
  } else {
    
    empty_maxVelo_df <- data.frame()
    
    empty_percentile_graph <- empty_maxVelo_df %>% 
      ggplot() +
      geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
      geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
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
            axis.title.x = element_blank()
      )
    
    empty_plot <- empty_maxVelo_df %>% 
      ggplot() + 
      geom_point() + 
      xlim(0, 10) + 
      ylim(0, 100) +
      annotate("text", x = 5, y = 50, label = "No data collected.\nPlease bring report to Futures Coaches to test.", 
               size = 8, color = "white", hjust = 0.5, vjust = 0.5) +
      theme_minimal() +
      theme(panel.grid = element_line(color = col_grid),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    
    maxVelo_percentile_score <- 0
    
    ggsave(empty_plot,file=paste0("Futures Reports Images/", athlete,"_maxVeloPlot.png"), width=6,height=3,units="in", dpi = 150)
    strengthCharts5 <- image_read(paste0("Futures Reports Images/", athlete,"_maxVeloPlot.png"))
    PitchingReport6 <- image_composite(PitchingReport5, strengthCharts5, offset= "+1425+1775")
    
    ggsave(empty_percentile_graph,file=paste0("Futures Reports Images/", athlete,"_maxVeloPercentiles.png"), width=7,height=2,units="in", dpi = 150)
    strengthCharts6 <- image_read(paste0("Futures Reports Images/", athlete,"_maxVeloPercentiles.png"))
    PitchingReport7 <- image_composite(PitchingReport6, strengthCharts6, offset= "+1350+2250")
  }
  
  ########################################################################################################
  ##############################################     RSI     #############################################
  ########################################################################################################
  
  if(any(RSIdata$FullName == athlete)) {
    
    RSI_graph_data <- RSIdata %>% 
      filter(FullName == athlete) %>%
      group_by(Date, Level, Gender) %>%
      summarize(RSI = max(`Mean RSI (Jump Height/Contact Time) [m/s]`, na.rm = TRUE),
                PercentileRank = max(PercentileRank, na.rm = TRUE),
                .groups = "drop")
    
    maxRSI <- max(RSI_graph_data$RSI, na.rm = TRUE)
    
    RSI_percentile_graph <- RSI_graph_data %>% 
      ggplot(aes(x = max(PercentileRank), y = "")) +
      geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
      geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(fill = max(PercentileRank)), color = "black", pch = 21, size = 12) +
      geom_text(aes(label = round(max(PercentileRank))), size = 8, fontface = "bold") +
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
            axis.title.x = element_blank()) +
      annotate("text", x = 0, y = 1, label = "Repeat Hop Test - RSI: ", hjust = 0, vjust = -3.5, color = "white", size = 12, family = "Good Times") +
      annotate("text", x = 0, y = 1, label = "(Jump Height / Ground Contact Time)", hjust = 0, vjust = -4.5, color = "white", size = 6, family = "Good Times") +
      annotate("text", x = 100, y = 1, label = paste(round(maxRSI, digits = 1), " M/S", sep = ""), hjust = 1, vjust = -3.5, color = "white", size = 12, family = "Good Times")
    
    RSI_level <- unique(RSI_graph_data$Level)[1]
    RSI_gender <- unique(RSI_graph_data$Gender)[1]
    
    RSI_ylim_data <- RSIdata %>% 
      filter(Level == RSI_level, Gender == RSI_gender)
    
    RSI_ylim_min <- min(RSI_ylim_data$`Mean RSI (Jump Height/Contact Time) [m/s]`, na.rm = TRUE)
    RSI_ylim_max <- max(RSI_ylim_data$`Mean RSI (Jump Height/Contact Time) [m/s]`, na.rm = TRUE)
    
    RSI_graph <- RSI_graph_data %>% 
      ggplot(aes(x = Date , y = RSI)) +
      geom_line(linewidth = 2, color = "#FF0000") +
      geom_point(size = 4, color = "#FF0000") +
      ylim(RSI_ylim_min, RSI_ylim_max) +
      labs(y = "Meters per second") +
      theme_minimal() +
      theme(legend.position = "none",
            panel.grid = element_line(color = col_grid),
            axis.title.y = element_text(color = "white", size = 20),
            axis.title.x = element_blank(),
            axis.text=element_text(color = "white", size = 15),
            plot.title = element_text(hjust = 0.5, color = "white", size = 18, family = "Good Times")
      )
    
    RSI_percentile_score <- max(RSI_graph_data$PercentileRank, na.rm = TRUE)
    
    ggsave(RSI_graph,file=paste0("Futures Reports Images/",athlete,"_RSIplot.png"), width=7,height=3.15,units="in", dpi = 150)
    strengthCharts7 <- image_read(paste0("Futures Reports Images/",athlete,"_RSIplot.png"))
    PitchingReport8 <- image_composite(PitchingReport7, strengthCharts7, offset= "+1315+2700")
    
    ggsave(RSI_percentile_graph,file=paste0("Futures Reports Images/",athlete,"_RSIpercentiles.png"), width=7,height=3,units="in", dpi = 150)
    strengthCharts8 <- image_read(paste0("Futures Reports Images/",athlete,"_RSIpercentiles.png"))
    PitchingReport9 <- image_composite(PitchingReport8, strengthCharts8, offset= "+140+2750")
  } else {
    
    empty_RSI_df <- data.frame()
    
    empty_percentile_graph <- empty_RSI_df %>% 
      ggplot() +
      geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
      geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
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
            axis.title.x = element_blank()
      )
    
    empty_plot <- empty_RSI_df %>% 
      ggplot() + 
      geom_point() + 
      xlim(0, 10) + 
      ylim(0, 100) +
      annotate("text", x = 5, y = 50, label = "No data collected.\nPlease bring report to Futures Coaches to test.", 
               size = 8, color = "white", hjust = 0.5, vjust = 0.5) +
      theme_minimal() +
      theme(panel.grid = element_line(color = col_grid),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    
    RSI_percentile_score <- 0
    
    ggsave(empty_plot,file=paste0("Futures Reports Images/", athlete,"_RSIplot.png"), width=6,height=3,units="in", dpi = 150)
    strengthCharts7 <- image_read(paste0("Futures Reports Images/", athlete,"_RSIplot.png"))
    PitchingReport8 <- image_composite(PitchingReport7, strengthCharts7, offset= "+1425+2700")
    
    ggsave(empty_percentile_graph,file=paste0("Futures Reports Images/", athlete,"_RSIpercentiles.png"), width=7,height=3,units="in", dpi = 150)
    strengthCharts8 <- image_read(paste0("Futures Reports Images/", athlete,"_RSIpercentiles.png"))
    PitchingReport9 <- image_composite(PitchingReport8, strengthCharts8, offset= "+140+2750")
  }
  
  ########################################################################################################
  ############################################  Speed Score  #############################################
  ########################################################################################################
  
  scores <- c(hardNinety_percentile_score, acceleration_percentile_score, maxVelo_percentile_score, RSI_percentile_score)
  
  speedScore <- round(mean(scores, na.rm = TRUE), digits = 1)
  
  get_color_two <- function(score) {
    if (score < 50) {
      return("#FF0000")
    } else if (score >= 50 & score < 70) {
      return("#FFA500")
    } else if (score >= 70 & score < 90) {
      return("green")
    } else {
      return("#3d9be9")
    }
  }
  
  if (any(scores == 0 | is.infinite(scores) & scores < 0, na.rm = TRUE)) {
    speedScore_plot <- ggplot(data.frame(Name = "N/A", y = 50), aes(x = Name, y = y)) +
      geom_col(aes(y = 100), alpha = 0, color = "black") +
      geom_text(aes(label = "One or more scores are zero.\nPlease bring report to Futures Coaches to test."), size = 8, color = "white") +
      coord_flip() +
      theme_void()
  } else {
    speedScore_plot <- attendance_plot_data %>% 
      ggplot(aes(x = Name, y = speedScore)) +
      geom_col(aes(fill = get_color_two(speedScore))) +
      geom_col(aes(y = 100), alpha = 0.5, color = "black") +
      geom_text(aes(y = 50, label = paste(speedScore)), size = 14, fontface = "bold", color = "white") +
      coord_flip() +
      theme_minimal() +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) +
      scale_fill_identity()
  }
  
  ggsave(speedScore_plot,file=paste0("Futures Reports Images/",athlete,"_speedScore.png"), width=6,height=1,units="in", dpi = 150)
  speedScorePlot <- image_read(paste0("Futures Reports Images/",athlete,"_speedScore.png"))
  PitchingReport10 <- image_composite(PitchingReport9, speedScorePlot, offset= "+1425+475")
  
  image_write(PitchingReport10,path = "page1.pdf",format="pdf",quality=100,density=300)
  image_write(IndexPage,path = "page2.pdf",format="pdf",quality=100,density=300)
  
  qpdf::pdf_combine(input = c("page1.pdf", "page2.pdf"),
                    output = paste0(athlete_folder, "/", "Futures Speed Report.pdf"))  
}