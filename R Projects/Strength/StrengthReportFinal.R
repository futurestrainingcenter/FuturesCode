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
proteusData<- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Strength Percentiles/ProteusPercentiles.csv") %>% 
  filter(Month %in% c("March", "April"))
teambuildrData <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Strength Percentiles/teambuilderPercentiles.csv")
SJdata <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Strength Percentiles/SQTJumpPercentiles.csv") %>% 
  filter(Month %in% c("March", "April"))
ISOSQTdata <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Strength Percentiles/ISO_BeltSquatPercentiles.csv") %>% 
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
  filter(`Service Name` %in% c("Level 2 - Strength", "Level 3 - Strength", "Strength Base L3", "Strength Base L2", "Strength Pro L3", 
                               "Strength Pro L2", "Collegiate/Pro - Strength Training w/ Coach", "Professional - Facility Access", "Learning Academy - Attended")) %>% 
  distinct(Name, Date, LearningBlock, .keep_all = TRUE)

summary_attendanceData <- attendanceData %>%
  filter(Month %in% c("March", "April")) %>%
  group_by(Name) %>%
  summarise(Attendance = n(), 
            .groups = 'drop')

#ISOSQTdata$Date <- as.Date(ISOSQTdata$Date, format="%Y-%m-%d")
SJdata$Date <- as.Date(SJdata$Date, format="%y-%m-%d")
#proteusData$`session createdAt` <- as.Date(proteusData$`session createdAt`, format="%m/%d/%y")

# Extract names from clientData
clientNames <- clientData$Name

# # Compare and extract non-matching names
# nonMatchingVALD <- setdiff(SJdata$Name, clientNames)
# nonMatchingProteus <- setdiff(proteusData$Name, clientNames)
# nonMatchingTeambuildr <- setdiff(teambuildrData$Name, clientNames)
# 
# # Combine non-matching names into one data frame
# nonMatching <- rbind(
#   data.frame(Name = nonMatchingVALD, Source = 'VALD'),
#   data.frame(Name = nonMatchingProteus, Source = 'Proteus'),
#   data.frame(Name = nonMatchingTeambuildr, Source = 'Teambuildr')
# )
# 
# # Write the non-matching names to a new CSV file
# write_csv(nonMatching, "/Users/watts/Downloads/missing_strength_data.csv")

SJdata <- SJdata %>%
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

merged_data <- left_join(teambuildrData, clientData, by="Name")

TemplatePageOne <- image_read_pdf("/Volumes/COLE'S DATA/Templates/Strength Report Template.pdf")
IndexPage <- image_read_pdf("/Volumes/COLE'S DATA/Templates/Physicality Report Index.pdf")

setwd("/Users/watts/Documents/Futures Performance Center/Test")

athletes <- unique(proteusData$Name)

col_grid <- rgb(235, 235, 235, 50, maxColorValue = 255)

low_attendance_athletes <- c()

for (athlete in athletes){
  
  finalExercise_data <- teambuildrData %>% 
    filter(Name == athlete)
  
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
      } else if (score < 1) {
        return("#FF0000")
      } else if (score >= 1 & score < 1.75) {
        return("#FFA500")
      } else if (score >= 1.75 & score < 2.5) {
        return("green")
      } else {
        return("#3d9be9")
      }
    })
  }
  
  attendance_plot <- ggplot(attendance_plot_data, aes(x = Name, y = `Attendance Score`)) +
    geom_col(aes(fill = get_color(`Attendance Score`))) +
    geom_col(aes(y = 3), alpha = 0.5, color = "black") +
    geom_text(aes(y = 1.5, label = paste(attendance_score)), size = 14, fontface = "bold", color = "white") +
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
  
  ggsave(attendance_plot,file=paste0("Futures Reports Images/", athlete,"_attendancePlot.png"), width=6,height=1,units="in", dpi = 150)
  attendancePlot <- image_read(paste0("Futures Reports Images/", athlete,"_attendancePlot.png"))
  PitchingReport0 <- image_composite(TemplatePageOne, attendancePlot, offset= "+225+1700")
  
  #######################################################################################################
  ############################################PLAYER PROFILE#############################################
  #######################################################################################################
  
  if(!any(clientData$Name == athlete)) {
    player_profile <- data.frame(
      Name = NA, 
      Age = NA, 
      `Sports Performance Training/Booking Level` = NA,
      `Position (Baseball/Softball)` = NA, 
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
    if (!any(SJdata$Name == athlete)) {
      player_profile_two <- data.frame(
        Weight = NA,
        stringsAsFactors = FALSE
      )
      names(player_profile_two) <- c("Weight:")
      
      combined_profile <- cbind(player_profile, player_profile_two)
    } else {
      player_profile_two <- SJdata %>%
        filter(Name == athlete) %>%
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
    expand_limits(x = 0.575, y = 0.5) +
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
      annotate("text", x = right_x + 0.09, y = y_positions[i + 1], label = combined_profile$value[combined_profile$label == right_labels[i]], 
               hjust = 0, color = "white", size = 11, family = "Good Times")
  }
  
  # Assuming LA_attendanceData and CMJdata are already read in
  if (any(LA_attendanceData$Name == athlete)) {
    data_to_use <- LA_attendanceData
  } else {
    data_to_use <- SJdata
  }
  
  first_weight_value <- data_to_use %>%
    filter(Name == athlete) %>%
    arrange(Date) %>%
    .$Weight %>%
    first()
  
  y_min_weight <- round(first_weight_value - 8)
  y_max_weight <- round(first_weight_value + 8)
  
  weight_plot <- data_to_use %>%
    filter(Name == athlete) %>% 
    ggplot(aes(x = Date, y = Weight)) +
    geom_line(linewidth = 2, color = "#FF0000") +
    geom_point(size = 4, color = "#FF0000") +
    labs(title = "Weight Trends") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid = element_line(color = col_grid),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text = element_text(color = "white", size = 15),
          plot.title = element_text(hjust = 0.5, color = "white", size = 22, family = "Good Times")) +
    scale_y_continuous(limits = c(y_min_weight, y_max_weight))
  
  full_player_profile <- ggarrange(p, weight_plot, nrow = 2)
  ggsave(full_player_profile,file=paste0("Futures Reports Images/", athlete,"_playerSummary.png"), width=6.15,height=5.5,units="in", dpi = 175)
  strengthSummarys <- image_read(paste0("Futures Reports Images/", athlete,"_playerSummary.png"))
  PitchingReport1 <- image_composite(PitchingReport0,strengthSummarys,offset= "+125+525")
  
  
  ########################################################################################################
  #############################################     CORE     #############################################
  ########################################################################################################
  filtered_proteusData <- proteusData %>% 
    filter(Name == athlete)
  
  if (any(filtered_proteusData$`exercise name` == "Straight Arm Trunk Rotation")) {
    
    core_graph_data <- filtered_proteusData %>% 
      filter(`exercise name` == "Straight Arm Trunk Rotation") %>%
      group_by(`session createdAt`, Level, Gender) %>%
      summarize(`power - high` = round(max(`power - high`, na.rm = TRUE)),
                PowerPercentileRank = max(PowerPercentileRank, na.rm = TRUE),
                `acceleration - high` = round(max(`acceleration - high`, na.rm = TRUE)),
                AccelerationPercentileRank = max(AccelerationPercentileRank, na.rm = TRUE),
                .groups = "drop")
    
    core_level <- unique(core_graph_data$Level)[1]
    core_gender <- unique(core_graph_data$Gender)[1]
    
    core_ylim_data <- proteusData %>% 
      filter(Level == core_level, Gender == core_gender)
    
    core_ylim_min <- round(min(core_ylim_data$`power - high`[core_ylim_data$`exercise name` == "Straight Arm Trunk Rotation"], na.rm = TRUE))
    core_ylim_max <- round(max(core_ylim_data$`power - high`[core_ylim_data$`exercise name` == "Straight Arm Trunk Rotation"], na.rm = TRUE))
    
    maxPower <- max(core_graph_data$`power - high`, na.rm = TRUE)
    
    core_power_percentile_graph <- core_graph_data %>% 
      ggplot(aes(x = max(PowerPercentileRank), y = "")) +
      geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
      geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(fill = max(PowerPercentileRank)), color = "black", pch = 21, size = 12) +
      geom_text(aes(label = round(max(PowerPercentileRank))), size = 8, fontface = "bold") +
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
      annotate("text", x = 0, y = 1, label = "Strength Max: ", hjust = 0, vjust = -2.5, color = "#3d9be9", size = 12, family = "Good Times") +
      annotate("text", x = 100, y = 1, label = paste(round(maxPower, digits = 1), " W", sep = ""), hjust = 1, vjust = -2.5, color = "white", size = 12, family = "Good Times")
    
    maxAcc <- max(core_graph_data$`acceleration - high`, na.rm = TRUE)
    
    transformation_factor = round(maxPower / maxAcc, digits = 3)
    
    corePower_graph <- core_graph_data %>%
      ggplot(aes(x = `session createdAt`)) +
      geom_line(aes(y = `power - high`, color = "Straight Arm Trunk Rotation (Strength)"), linewidth = 2) +
      geom_point(aes(y = `power - high`, color = "Straight Arm Trunk Rotation (Strength)"), size = 4) +
      geom_line(aes(y = `acceleration - high` * transformation_factor, color = "Straight Arm Trunk Rotation (Speed)"), linewidth = 2) +
      geom_point(aes(y = `acceleration - high` * transformation_factor, color = "Straight Arm Trunk Rotation (Speed)"), size = 4) +
      labs(y = "Strength (W)\n") +
      scale_y_continuous(
        limits = c(core_ylim_min, core_ylim_max),
        sec.axis = sec_axis(~ . / transformation_factor, name = "Speed (m/s²)\n")
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(color = "white", size = 17),
        panel.grid = element_line(color = col_grid),
        axis.title.x = element_blank(),
        axis.title.y.left = element_text(color = "#3d9be9", size = 20),
        axis.title.y.right = element_text(color = "#FF0000", size = 20),
        axis.text.y.left = element_text(color = "white", size = 15),
        axis.text.y.right = element_text(color = "white", size = 15),
        axis.text.x = element_text(color = "white", size = 15)
      ) +
      scale_color_manual(values = c("Straight Arm Trunk Rotation (Strength)" = "#3d9be9", 
                                    "Straight Arm Trunk Rotation (Speed)" = "#FF0000"),
                         breaks = c("Straight Arm Trunk Rotation (Strength)", 
                                    "Straight Arm Trunk Rotation (Speed)"))
    
    core_acc_percentile_graph <- core_graph_data %>% 
      ggplot(aes(x = max(AccelerationPercentileRank), y = "")) +
      geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
      geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(fill = max(AccelerationPercentileRank)), color = "black", pch = 21, size = 12) +
      geom_text(aes(label = round(max(AccelerationPercentileRank))), size = 8, fontface = "bold") +
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
      annotate("text", x = 0, y = 1, label = "Speed Max: ", hjust = 0, vjust = -2.5, color = "#FF0000", size = 12, family = "Good Times") +
      annotate("text", x = 100, y = 1, label = paste(round(maxAcc, digits = 1), " m/s²", sep = ""), hjust = 1, vjust = -2.5, color = "white", size = 12, family = "Good Times")
    
    core_power_percentile_score <- max(core_graph_data$PowerPercentileRank, na.rm = TRUE)
    core_acc_percentile_score <- max(core_graph_data$AccelerationPercentileRank, na.rm = TRUE)
    
    ggsave(corePower_graph,file=paste0("Futures Reports Images/", athlete,"_coreStrengthPlot.png"), width=7.5,height=3.5,units="in", dpi = 150)
    strengthCharts1 <- image_read(paste0("Futures Reports Images/", athlete,"_coreStrengthPlot.png"))
    PitchingReport2 <- image_composite(PitchingReport1, strengthCharts1, offset= "+1320+800")
    
    ggsave(core_power_percentile_graph,file=paste0("Futures Reports Images/", athlete,"_coreStrengthPercentiles.png"), width=7,height=3,units="in", dpi = 150)
    strengthCharts2 <- image_read(paste0("Futures Reports Images/", athlete,"_coreStrengthPercentiles.png"))
    PitchingReport3 <- image_composite(PitchingReport2, strengthCharts2, offset= "+1350+1300")
    
    ggsave(core_acc_percentile_graph,file=paste0("Futures Reports Images/", athlete,"_coreSpeedPercentiles.png"), width=7,height=3,units="in", dpi = 150)
    strengthCharts3 <- image_read(paste0("Futures Reports Images/", athlete,"_coreSpeedPercentiles.png"))
    PitchingReport4 <- image_composite(PitchingReport3, strengthCharts3, offset= "+1350+1535")
  } else {
    
    empty_core_df <- data.frame()
    
    empty_percentile_graph <- empty_core_df %>% 
      ggplot() +
      geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
      geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
      theme_void()
    
    empty_core_plot <- empty_core_df %>% 
      ggplot() + 
      geom_point() + 
      xlim(0, 10) + 
      ylim(0, 100) +
      annotate("text", x = 5, y = 50, label = "No data collected.\nPlease bring report to Futures Coaches to test.", 
               size = 10, color = "white", hjust = 0.5, vjust = 0.5) +
      theme_minimal() +
      theme(panel.grid = element_line(color = col_grid),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    
    core_power_percentile_score <- 0
    core_acc_percentile_score <- 0
    
    ggsave(empty_core_plot,file=paste0("Futures Reports Images/", athlete,"_coreStrengthPlot.png"), width=6,height=3,units="in", dpi = 150)
    strengthCharts1 <- image_read(paste0("Futures Reports Images/", athlete,"_coreStrengthPlot.png"))
    PitchingReport2 <- image_composite(PitchingReport1, strengthCharts1, offset= "+1425+950")
    
    ggsave(empty_percentile_graph,file=paste0("Futures Reports Images/", athlete,"_coreStrengthPercentiles.png"), width=6,height=1,units="in", dpi = 150)
    strengthCharts2 <- image_read(paste0("Futures Reports Images/", athlete,"_coreStrengthPercentiles.png"))
    PitchingReport3 <- image_composite(PitchingReport2, strengthCharts2, offset= "+1425+1425")
    
    ggsave(empty_percentile_graph,file=paste0("Futures Reports Images/", athlete,"_coreSpeedPercentiles.png"), width=6,height=1,units="in", dpi = 150)
    strengthCharts3 <- image_read(paste0("Futures Reports Images/", athlete,"_coreSpeedPercentiles.png"))
    PitchingReport4 <- image_composite(PitchingReport3, strengthCharts3, offset= "+1425+1635")
  }
  ########################################################################################################
  #############################################     LEGS     #############################################
  ########################################################################################################
  
  athlete_in_ISOSQTdata <- any(ISOSQTdata$Name == athlete)
  athlete_in_SJdata <- any(SJdata$Name == athlete)
  
  # Functions to generate empty plots
  generate_empty_percentile_graph <- function() {
    empty_legs_df <- data.frame()
    empty_legs_percentile_graph <- empty_legs_df %>% 
      ggplot() +
      geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
      geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
      theme_void()
     
    return(empty_legs_percentile_graph)
  }
  
  generate_empty_legs_plot <- function() {
    empty_legs_df <- data.frame()
    empty_legs_plot <- empty_legs_df %>% 
      ggplot() + 
      geom_point() + 
      xlim(0, 10) + 
      ylim(0, 100) +
      annotate("text", x = 5, y = 50, label = "No data collected.\nPlease bring report to Futures Coaches to test.", 
               size = 10, color = "white", hjust = 0.5, vjust = 0.5) +
      theme_minimal() +
      theme(panel.grid = element_line(color = col_grid),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    
    return(empty_legs_plot)
  }
  
  # New if statement to check if both datasets contain the athlete
  if (athlete_in_ISOSQTdata && athlete_in_SJdata) {
    
    ISOSQT_graph_data <- ISOSQTdata %>% 
      filter(Name == athlete) %>%
      select(Date, Level, Gender, Name, `Peak Vertical Force [N]`, PercentileRank)
    
    ISOSQT_max_data <- ISOSQT_graph_data %>% 
      slice_max(order_by = `Peak Vertical Force [N]`, n = 1, with_ties = FALSE)
    
    legs_level <- unique(ISOSQT_graph_data$Level)[1]
    legs_gender <- unique(ISOSQT_graph_data$Gender)[1]
    
    legs_ylim_data <- ISOSQTdata %>% 
      filter(Level == legs_level, Gender == legs_gender)
    
    legs_ylim_min <- round(min(legs_ylim_data$`Peak Vertical Force [N]`, na.rm = TRUE))
    legs_ylim_max <- round(max(legs_ylim_data$`Peak Vertical Force [N]`, na.rm = TRUE))
    
    maxISOSQT <- max(ISOSQT_graph_data$`Peak Vertical Force [N]`, na.rm = TRUE)
    
    ISOSQT_percentile_graph <- ISOSQT_graph_data %>% 
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
      annotate("text", x = 0, y = 1, label = "Strength Max: ", hjust = 0, vjust = -2.5, color = "#3d9be9", size = 12, family = "Good Times") +
      annotate("text", x = 100, y = 1, label = paste(round(maxISOSQT, digits = 1), " N", sep = ""), hjust = 1, vjust = -2.5, color = "white", size = 12, family = "Good Times")
    
    SJ_graph_data <- SJdata %>% 
      filter(Name == athlete) %>%
      select(Date, Level, Gender, Name, `Takeoff Peak Force [N]`, PercentileRank)
    
    maxSJ <- max(SJ_graph_data$`Takeoff Peak Force [N]`, na.rm = TRUE)
    
    SJ_percentile_graph <- SJ_graph_data %>% 
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
      annotate("text", x = 0, y = 1, label = "Speed Max: ", hjust = 0, vjust = -2.5, color = "#FF0000", size = 12, family = "Good Times") +
      annotate("text", x = 100, y = 1, label = paste(round(maxSJ, digits = 1), " N", sep = ""), hjust = 1, vjust = -2.5, color = "white", size = 12, family = "Good Times")
    
    legs_acc_percentile_score <- max(SJ_graph_data$PercentileRank, na.rm = TRUE)
    
    transformation_factor_three <- round(maxISOSQT / maxSJ, digits = 3)
    
    combined_legs_plot <- ISOSQT_graph_data %>%
      ggplot(aes(x = Date)) +
      geom_line(aes(y = `Peak Vertical Force [N]`, color = "Isometric Belt Squat (Strength)"), linewidth = 2) +
      geom_point(aes(y = `Peak Vertical Force [N]`, color = "Isometric Belt Squat (Strength)"), size = 4) +
      geom_line(data = SJ_graph_data, aes(y = `Takeoff Peak Force [N]` * transformation_factor_three, color = "Squat Jump (Speed)"), linewidth = 2) +
      geom_point(data = SJ_graph_data, aes(y = `Takeoff Peak Force [N]` * transformation_factor_three, color = "Squat Jump (Speed)"), size = 4) +
      labs(y = "Strength (N)\n") +
      scale_y_continuous(
        limits = c(legs_ylim_min, legs_ylim_max),
        sec.axis = sec_axis(~ . / transformation_factor_three, name = "Speed (N)\n")
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(color = "white", size = 17),
        panel.grid = element_line(color = col_grid),
        axis.title.x = element_blank(),
        axis.title.y.left = element_text(color = "#3d9be9", size = 20),
        axis.title.y.right = element_text(color = "#FF0000", size = 20),
        axis.text.y.left = element_text(color = "white", size = 15),
        axis.text.y.right = element_text(color = "white", size = 15),
        axis.text.x = element_text(color = "white", size = 15)
      ) +
      scale_color_manual(values = c("Isometric Belt Squat (Strength)" = "#3d9be9", 
                                    "Squat Jump (Speed)" = "#FF0000"),
                         breaks = c("Isometric Belt Squat (Strength)", 
                                    "Squat Jump (Speed)"))
    
    legs_power_percentile_score <- max(ISOSQT_graph_data$PercentileRank, na.rm = TRUE)
    legs_acc_percentile_score <- max(SJ_graph_data$PercentileRank, na.rm = TRUE)
    
    ggsave(combined_legs_plot,file=paste0("Futures Reports Images/", athlete,"_legsPlot.png"), width=7.5,height=3.5,units="in", dpi = 150)
    strengthCharts4 <- image_read(paste0("Futures Reports Images/", athlete,"_legsPlot.png"))
    PitchingReport5 <- image_composite(PitchingReport4, strengthCharts4, offset= "+115+2100")
    
    ggsave(ISOSQT_percentile_graph,file=paste0("Futures Reports Images/", athlete,"_ISOPercentiles.png"), width=7,height=3,units="in", dpi = 150)
    strengthCharts5 <- image_read(paste0("Futures Reports Images/", athlete,"_ISOPercentiles.png"))
    PitchingReport6 <- image_composite(PitchingReport5, strengthCharts5, offset= "+140+2600")
    
    ggsave(SJ_percentile_graph,file=paste0("Futures Reports Images/", athlete,"_SquatJumpPercentiles.png"), width=7,height=3,units="in", dpi = 150)
    strengthCharts6 <- image_read(paste0("Futures Reports Images/", athlete,"_SquatJumpPercentiles.png"))
    PitchingReport7 <- image_composite(PitchingReport6, strengthCharts6, offset= "+140+2835")
    
  } else if (athlete_in_ISOSQTdata && !athlete_in_SJdata) {
    
    legs_ylim_min <- min(ISOSQTdata$`Peak Vertical Force [N]`, na.rm = TRUE)
    legs_ylim_max <- max(ISOSQTdata$`Peak Vertical Force [N]`, na.rm = TRUE)
    
    ISOSQT_graph_data <- ISOSQTdata %>% 
      filter(Name == athlete) %>%
      select(Date, Level, Gender, Name, `Peak Vertical Force [N]`, PercentileRank)
    
    ISOSQT_max_data <- ISOSQT_graph_data %>% 
      slice_max(order_by = `Peak Vertical Force [N]`, n = 1, with_ties = FALSE)
    
    legs_level <- unique(ISOSQT_graph_data$Level)[1]
    legs_gender <- unique(ISOSQT_graph_data$Gender)[1]
    
    legs_ylim_data <- ISOSQTdata %>% 
      filter(Level == legs_level, Gender == legs_gender)
    
    legs_ylim_min <- round(min(legs_ylim_data$`Peak Vertical Force [N]`, na.rm = TRUE))
    legs_ylim_max <- round(max(legs_ylim_data$`Peak Vertical Force [N]`, na.rm = TRUE))
    
    maxISOSQT <- max(ISOSQT_graph_data$`Peak Vertical Force [N]`, na.rm = TRUE)
    
    ISOSQT_percentile_graph <- ISOSQT_graph_data %>% 
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
      annotate("text", x = 0, y = 1, label = "Strength Max: ", hjust = 0, vjust = -2.5, color = "#3d9be9", size = 12, family = "Good Times") +
      annotate("text", x = 100, y = 1, label = paste(round(maxISOSQT, digits = 1), " N", sep = ""), hjust = 1, vjust = -2.5, color = "white", size = 12, family = "Good Times")
    
    # Extract the last two data points from the `ISOSQT_graph_data` dataframe
    last_two_points_isosqt <- tail(ISOSQT_graph_data$`Peak Vertical Force [N]`, 2)
    
    # Check if the most recent data point is less than the previous data point
    set_ylim_isosqt <- if (length(last_two_points_isosqt) == 2 && last_two_points_isosqt[2] < last_two_points_isosqt[1]) TRUE else FALSE
    
    # Build the plot
    ISOSQT_graph <- ISOSQT_graph_data %>%
      ggplot(aes(x = Date, y = `Peak Vertical Force [N]`, color = "Isometric Belt Squat")) +
      geom_line(linewidth = 2) +
      geom_point(size = 4) +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(color = "white", size = 17),
            panel.grid = element_line(color = col_grid),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text=element_text(color = "white", size = 15)) +
      scale_color_manual(values = c("Isometric Belt Squat" = "#3d9be9"),
                         breaks = c("Isometric Belt Squat"))
    
    # Apply ylim conditionally
    if (set_ylim_isosqt) {
      ISOSQT_graph <- ISOSQT_graph + ylim(legs_ylim_min, legs_ylim_max)
    }
    
    legs_power_percentile_score <- max(ISOSQT_graph_data$PercentileRank, na.rm = TRUE)
    legs_acc_percentile_score <- 0
    
    ggsave(ISOSQT_graph,file=paste0("Futures Reports Images/", athlete,"_ISOSQTplot.png"), width=7.5,height=3.5,units="in", dpi = 150)
    strengthCharts4 <- image_read(paste0("Futures Reports Images/", athlete,"_ISOSQTplot.png"))
    PitchingReport5 <- image_composite(PitchingReport4, strengthCharts4, offset= "+115+2100")
    
    ggsave(ISOSQT_percentile_graph,file=paste0("Futures Reports Images/", athlete,"_ISOSQTperctiles.png"), width=7,height=3,units="in", dpi = 150)
    strengthCharts5 <- image_read(paste0("Futures Reports Images/", athlete,"_ISOSQTperctiles.png"))
    PitchingReport6 <- image_composite(PitchingReport5, strengthCharts5, offset= "+140+2600")
    
    empty_legs_percentile_graph <- generate_empty_percentile_graph()
    ggsave(empty_legs_percentile_graph, file=paste0("Futures Reports Images/", athlete, "_SquatJumpPercentiles.png"), width=7, height=3, units="in", dpi=150)
    strengthCharts6 <- image_read(paste0("Futures Reports Images/", athlete, "_SquatJumpPercentiles.png"))
    PitchingReport7 <- image_composite(PitchingReport6, strengthCharts6, offset= "+140+2835")
    
  } else if (!athlete_in_ISOSQTdata && athlete_in_SJdata) {
    
    SJ_ylim_min <- min(SJdata$`Takeoff Peak Force [N]`, na.rm = TRUE)
    SJ_ylim_max <- max(SJdata$`Takeoff Peak Force [N]`, na.rm = TRUE)
    
    SJ_graph_data <- SJdata %>% 
      filter(Name == athlete) %>%
      select(Date, Level, Gender, Name, `Takeoff Peak Force [N]`, PercentileRank)
    
    maxSJ <- max(SJ_graph_data$`Takeoff Peak Force [N]`, na.rm = TRUE)
    
    SJ_percentile_graph <- SJ_graph_data %>% 
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
      annotate("text", x = 0, y = 1, label = "Speed Max: ", hjust = 0, vjust = -2.5, color = "#FF0000", size = 12, family = "Good Times") +
      annotate("text", x = 100, y = 1, label = paste(round(maxSJ, digits = 1), " N", sep = ""), hjust = 1, vjust = -2.5, color = "white", size = 12, family = "Good Times")
    
    # Extract the last two data points from the `ISOSQT_graph_data` dataframe
    last_two_points_SJ <- tail(SJ_graph_data$`Takeoff Peak Force [N]`, 2)
    
    # Check if the most recent data point is less than the previous data point
    set_ylim_SJ <- if (length(last_two_points_SJ) == 2 && last_two_points_SJ[2] < last_two_points_SJ[1]) TRUE else FALSE
    
    # Build the plot
    SJ_graph <- SJ_graph_data %>%
      ggplot(aes(x = Date, y = `Takeoff Peak Force [N]`, color = "SJ")) +
      geom_line(linewidth = 2) +
      geom_point(size = 4) +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(color = "white", size = 17),
            panel.grid = element_line(color = col_grid),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text=element_text(color = "white", size = 15)) +
      scale_color_manual(values = c("SJ" = "#FF0000"),
                         breaks = c("SJ"))
    
    # Apply ylim conditionally
    if (set_ylim_SJ) {
      SJ_graph <- SJ_graph + ylim(SJ_ylim_min, SJ_ylim_max)
    }
    
    legs_acc_percentile_score <- max(SJ_graph_data$PercentileRank, na.rm = TRUE)
    legs_power_percentile_score <- 0
    
    ggsave(SJ_graph,file=paste0("Futures Reports Images/", athlete,"_SJplot.png"), width=7.5,height=3.5,units="in", dpi = 150)
    strengthCharts4 <- image_read(paste0("Futures Reports Images/", athlete,"_SJplot.png"))
    PitchingReport5 <- image_composite(PitchingReport4, strengthCharts4, offset= "+115+2100")
    
    empty_legs_percentile_graph <- generate_empty_percentile_graph()
    ggsave(empty_legs_percentile_graph, file=paste0("Futures Reports Images/", athlete, "_ISOSQTperctiles.png"), width=7, height=3, units="in", dpi=150)
    strengthCharts5 <- image_read(paste0("Futures Reports Images/", athlete, "_ISOSQTperctiles.png"))
    PitchingReport6 <- image_composite(PitchingReport5, strengthCharts5, offset= "+140+2600")
    
    ggsave(SJ_percentile_graph,file=paste0("Futures Reports Images/", athlete,"_SquatJumpPercentiles.png"), width=7,height=3,units="in", dpi = 150)
    strengthCharts6 <- image_read(paste0("Futures Reports Images/", athlete,"_SquatJumpPercentiles.png"))
    PitchingReport7 <- image_composite(PitchingReport6, strengthCharts6, offset= "+140+2835")
  } else {
    
    legs_power_percentile_score <- 0
    legs_acc_percentile_score <- 0
    
    empty_legs_plot <- generate_empty_legs_plot()
    ggsave(empty_legs_plot, file=paste0("Futures Reports Images/", athlete, "_SJplot.png"), width=7.5, height=3.5, units="in", dpi=150)
    strengthCharts4 <- image_read(paste0("Futures Reports Images/", athlete, "_SJplot.png"))
    PitchingReport5 <- image_composite(PitchingReport4, strengthCharts4, offset= "+115+2100")
    
    empty_legs_percentile_graph <- generate_empty_percentile_graph()
    ggsave(empty_legs_percentile_graph, file=paste0("Futures Reports Images/", athlete, "_ISOSQTperctiles.png"), width=7, height=3, units="in", dpi=150)
    strengthCharts5 <- image_read(paste0("Futures Reports Images/", athlete, "_ISOSQTperctiles.png"))
    PitchingReport6 <- image_composite(PitchingReport5, strengthCharts5, offset= "+140+2600")
    
    empty_legs_percentile_graph <- generate_empty_percentile_graph()
    ggsave(empty_legs_percentile_graph, file=paste0("Futures Reports Images/", athlete, "_SquatJumpPercentiles.png"), width=7, height=3, units="in", dpi=150)
    strengthCharts6 <- image_read(paste0("Futures Reports Images/", athlete, "_SquatJumpPercentiles.png"))
    PitchingReport7 <- image_composite(PitchingReport6, strengthCharts6, offset= "+140+2835")
  }
  
  ########################################################################################################
  ############################################# UPPER BODY  ##############################################
  ########################################################################################################
  
  if(any(proteusData$Name == athlete)) {
    
    pushpull_graph_data <- proteusData %>% 
      filter(Name == athlete & 
               (`exercise name` == "Chest Press (One Hand)" | `exercise name` == "Horizontal Row (One Hand)" | `exercise name` == "PushPull" |
                  `exercise name` == "PNF D2 Extension" | `exercise name` == "PNF D2 Flexion" | `exercise name` == "Shot Put (Countermovement)")) %>%
      group_by(`session createdAt`, `exercise name`, Level, Gender) %>%
      summarize(`power - high` = round(mean(`power - high`, na.rm = TRUE)),
                PowerPercentileRank = max(PowerPercentileRank, na.rm = TRUE),
                `acceleration - high` = round(mean(`acceleration - high`, na.rm = TRUE)),
                AccelerationPercentileRank = max(AccelerationPercentileRank, na.rm = TRUE),
                .groups = "drop")
    
    arms_level <- unique(pushpull_graph_data$Level)[1]
    arms_gender <- unique(pushpull_graph_data$Gender)[1]
    
    arms_ylim_data <- proteusData %>% 
      filter(Level == arms_level, Gender == arms_gender)
    
    arms_ylim_min <- round(min(arms_ylim_data$`power - high`[arms_ylim_data$`exercise name` == "PushPull"], na.rm = TRUE))
    arms_ylim_max <- round(max(arms_ylim_data$`power - high`[arms_ylim_data$`exercise name` == "PushPull"], na.rm = TRUE))
    
    # Calculating max values for each exercise and dominance type
    max_power_pushpull <- max(pushpull_graph_data$`power - high`[pushpull_graph_data$`exercise name` == "PushPull"], na.rm = TRUE)
    
    pushpull_power_percentile_graph <- pushpull_graph_data %>%
      filter(`exercise name` == "PushPull") %>% 
      ggplot(aes(x = max(PowerPercentileRank), y = "")) +
      geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
      geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(fill = max(PowerPercentileRank)), color = "black", pch = 21, size = 12) +
      geom_text(aes(label = round(max(PowerPercentileRank))), size = 8, fontface = "bold") +
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
      annotate("text", x = 0, y = 1, label = "Strength Max: ", hjust = 0, vjust = -2.5, color = "#3d9be9", size = 12, family = "Good Times") +
      annotate("text", x = 100, y = 1, label = paste(round(max_power_pushpull, digits = 1), "W"), hjust = 1, vjust = -2.5, color = "white", size = 12, family = "Good Times")
    
    max_acc_pushpull <- max(pushpull_graph_data$`acceleration - high`[pushpull_graph_data$`exercise name` == "PushPull"], na.rm = TRUE)
    
    pushpull_acc_percentile_graph <- pushpull_graph_data %>%
      filter(`exercise name` == "PushPull") %>% 
      ggplot(aes(x = max(AccelerationPercentileRank), y = "")) +
      geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
      geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(fill = max(AccelerationPercentileRank)), color = "black", pch = 21, size = 12) +
      geom_text(aes(label = round(max(AccelerationPercentileRank))), size = 8, fontface = "bold") +
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
      annotate("text", x = 0, y = 1, label = "Speed Max: ", hjust = 0, vjust = -2.5, color = "#FF0000", size = 12, family = "Good Times") +
      annotate("text", x = 100, y = 1, label = paste(round(max_acc_pushpull, digits = 1), "m/s²"), hjust = 1, vjust = -2.5, color = "white", size = 12, family = "Good Times")
    
    transformation_factor_two = round(max_power_pushpull / max_acc_pushpull, digits = 1)
    
    pushpull_power_graph <- pushpull_graph_data %>%
      filter(`exercise name` == "PushPull") %>% 
      ggplot(aes(x = `session createdAt`)) +
      geom_line(aes(y = `power - high`, color = "Push/Pull (Strength)"), linewidth = 2) +
      geom_point(aes(y = `power - high`, color = "Push/Pull (Strength)"), size = 4) +
      geom_line(aes(y = `acceleration - high` * transformation_factor_two, color = "Push/Pull (Speed)"), linewidth = 2) +
      geom_point(aes(y = `acceleration - high` * transformation_factor_two, color = "Push/Pull (Speed)"), size = 4) +
      labs(y = "Strength (W)\n") +
      scale_y_continuous(
        limits = c(arms_ylim_min, arms_ylim_max),
        sec.axis = sec_axis(~ . / transformation_factor_two, name = "Speed (m/s²)\n")
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.box = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(color = "white", size = 17),
        panel.grid = element_line(color = col_grid),
        axis.title.x = element_blank(),
        axis.title.y.left = element_text(color = "#3d9be9", size = 20),
        axis.title.y.right = element_text(color = "#FF0000", size = 20),
        axis.text.y.left = element_text(color = "white", size = 15),
        axis.text.y.right = element_text(color = "white", size = 15),
        axis.text.x = element_text(color = "white", size = 15)
      ) +
      scale_color_manual(values = c("Push/Pull (Strength)" = "#3d9be9", 
                                    "Push/Pull (Speed)" = "#FF0000"),
                         breaks = c("Push/Pull (Strength)", 
                                    "Push/Pull (Speed)"))
    
    pushpull_power_percentile_score <- max(pushpull_graph_data$PowerPercentileRank, na.rm = TRUE)
    pushpull_acc_percentile_score <- max(pushpull_graph_data$AccelerationPercentileRank, na.rm = TRUE)
    
    # Calculate max power and acceleration percentile scores for Shot Put (Countermovement)
    shotput_data <- filter(pushpull_graph_data, `exercise name` == "Shot Put (Countermovement)")
    shotput_power_percentile_score <- max(shotput_data$PowerPercentileRank, na.rm = TRUE)
    shotput_data <- filter(pushpull_graph_data, `exercise name` == "Shot Put (Countermovement)")
    shotput_acc_percentile_score <- max(shotput_data$AccelerationPercentileRank, na.rm = TRUE)
    
    ggsave(pushpull_power_graph,file=paste0("Futures Reports Images/", athlete,"_pushpullPlot.png"), width=7.5,height=3.5,units="in", dpi = 150)
    strengthCharts7 <- image_read(paste0("Futures Reports Images/", athlete,"_pushpullPlot.png"))
    PitchingReport8 <- image_composite(PitchingReport7, strengthCharts7, offset= "+1320+2100")
    
    ggsave(pushpull_power_percentile_graph,file=paste0("Futures Reports Images/", athlete,"_pushpullStrengthPercentile.png"), width=7,height=3,units="in", dpi = 150)
    strengthCharts8 <- image_read(paste0("Futures Reports Images/", athlete,"_pushpullStrengthPercentile.png"))
    PitchingReport9 <- image_composite(PitchingReport8, strengthCharts8, offset= "+1350+2600")
    
    ggsave(pushpull_acc_percentile_graph,file=paste0("Futures Reports Images/", athlete,"_pushpullSpeedPercentile.png"), width=7,height=3,units="in", dpi = 150)
    strengthCharts9 <- image_read(paste0("Futures Reports Images/", athlete,"_pushpullSpeedPercentile.png"))
    PitchingReport10 <- image_composite(PitchingReport9, strengthCharts9, offset= "+1350+2835")
  }else{
    
    empty_arms_df <- data.frame()
    
    empty_arms_percentile_graph <- empty_arms_df %>% 
      ggplot() +
      geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
      geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
      theme_void()
    
    empty_arms_plot <- empty_arms_df %>% 
      ggplot() + 
      geom_point() + 
      xlim(0, 10) + 
      ylim(0, 100)  +
      annotate("text", x = 5, y = 50, label = "No data collected.\nPlease bring report to Futures Coaches to test.", 
               size = 10, color = "white", hjust = 0.5, vjust = 0.5) +
      theme_minimal() +
      theme(panel.grid = element_line(color = col_grid),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    
    pushpull_power_percentile_score <- 0
    pushpull_acc_percentile_score <- 0
    shotput_power_percentile_score <- 0
    shotput_acc_percentile_score <- 0
    
    ggsave(empty_arms_plot,file=paste0("Futures Reports Images/", athlete,"_pushpullPlot.png"), width=7.5,height=3.5,units="in", dpi = 150)
    strengthCharts7 <- image_read(paste0("Futures Reports Images/", athlete,"_pushpullPlot.png"))
    PitchingReport8 <- image_composite(PitchingReport7, strengthCharts7, offset= "+1320+2100")
    
    ggsave(empty_arms_percentile_graph,file=paste0("Futures Reports Images/", athlete,"_pushpullStrengthPercentile.png"), width=7,height=3,units="in", dpi = 150)
    strengthCharts8 <- image_read(paste0("Futures Reports Images/", athlete,"_pushpullStrengthPercentile.png"))
    PitchingReport9<- image_composite(PitchingReport8, strengthCharts8, offset= "+1350+2600")
    
    ggsave(empty_arms_percentile_graph,file=paste0("Futures Reports Images/", athlete,"_pushpullSpeedPercentile.png"), width=7,height=3,units="in", dpi = 150)
    strengthCharts9 <- image_read(paste0("Futures Reports Images/", athlete,"_pushpullSpeedPercentile.png"))
    PitchingReport10 <- image_composite(PitchingReport9, strengthCharts9, offset= "+1350+2835")
  }
  
  ########################################################################################################
  ###########################################  Weightroom text  ##########################################
  ########################################################################################################
  
  core_subset <- finalExercise_data$`Max Value`[finalExercise_data$`Exercise Name` == "Straight Arm Trunk Rotation Max Isometric Test - Crane Scale"]
  max_core <- if (all(is.na(core_subset))) NA else max(core_subset, na.rm = TRUE)
  
  core_text <- ggplot() + 
    annotate("text", x = 0, y = 0, label = paste("Core Rotation ISO Max:", max_core, "N"), size = 7, family = "Good Times", color = "white") + 
    theme_void() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "lines"))
  
  ggsave(core_text,file=paste0("Futures Reports Images/", athlete,"_coreText.png"), width=5,height=1,units="in", dpi = 150)
  strengthCharts10 <- image_read(paste0("Futures Reports Images/", athlete,"_coreText.png"))
  PitchingReport11 <- image_composite(PitchingReport10, strengthCharts10, offset= "+1500+1800")
  
  deadlift_subset <- finalExercise_data$`Max Value`[finalExercise_data$`Exercise Name` == "Trap Bar Deadlift"]
  max_deadlift <- if (all(is.na(deadlift_subset))) NA else max(deadlift_subset, na.rm = TRUE)
  squat_subset <- finalExercise_data$`Max Value`[finalExercise_data$`Exercise Name` == "Barbell Back Squat"]
  max_squat <- if (all(is.na(squat_subset))) NA else max(squat_subset, na.rm = TRUE)
  
  lowerbody_text <- ggplot() + 
    annotate("text", x = 0, y = 0, label = paste("Trap Bar Deadift Max:", max_deadlift, "LBS  -  ", 
                                                 "Squat Max:", max_squat, "LBS"), size = 7, family = "Good Times", color = "white") + 
    theme_void() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "lines"))
  
  ggsave(lowerbody_text,file=paste0("Futures Reports Images/", athlete,"_lowerbodyText.png"), width=8,height=1,units="in", dpi = 150)
  strengthCharts11 <- image_read(paste0("Futures Reports Images/", athlete,"_lowerbodyText.png"))
  PitchingReport12 <- image_composite(PitchingReport11, strengthCharts11, offset= "+65+3100")
  
  bench_subset <- finalExercise_data$`Max Value`[finalExercise_data$`Exercise Name` == "Barbell Bench Press"]
  max_bench <- if (all(is.na(bench_subset))) NA else max(bench_subset, na.rm = TRUE)
  pulldown_subset <- finalExercise_data$`Max Value`[finalExercise_data$`Exercise Name` == "Cable Lat Pull Down"]
  max_pulldown <- if (all(is.na(pulldown_subset))) NA else max(pulldown_subset, na.rm = TRUE)
  
  upperbody_text <- ggplot() + 
    annotate("text", x = 0, y = 0, label = paste("Bench Press Max:", max_bench, "LBS  -  ", 
                                                 "Lat Pulldown Max:", max_pulldown, "LBS"), size = 7, family = "Good Times", color = "white") + 
    theme_void() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "lines"))
  
  ggsave(upperbody_text,file=paste0("Futures Reports Images/", athlete,"_upperbodyText.png"), width=8,height=1,units="in", dpi = 150)
  strengthCharts12 <- image_read(paste0("Futures Reports Images/", athlete,"_upperbodyText.png"))
  PitchingReport13 <- image_composite(PitchingReport12, strengthCharts12, offset= "+1285+3100")
  
  ########################################################################################################
  ###########################################  Strength Score  ###########################################
  ########################################################################################################
  
  scores <- c(core_power_percentile_score, core_acc_percentile_score, legs_power_percentile_score, legs_acc_percentile_score, 
              pushpull_power_percentile_score, pushpull_acc_percentile_score,shotput_power_percentile_score, shotput_acc_percentile_score)
  
  strengthScore <- round(mean(scores, na.rm = TRUE), digits = 1)
  
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
    strengthScore_plot <- ggplot(data.frame(ClientName = "N/A", y = 50), aes(x = ClientName, y = y)) +
      geom_col(aes(y = 100), alpha = 0, color = "black") +
      geom_text(aes(label = "One or more scores are zero.\nPlease bring report to Futures Coaches to test."), size = 8, color = "white") +
      coord_flip() +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
  } else {
    strengthScore_plot <- attendance_plot_data %>% 
      ggplot(aes(x = Name, y = strengthScore)) +
      geom_col(aes(fill = get_color_two(strengthScore))) +
      geom_col(aes(y = 100), alpha = 0.5, color = "black") +
      geom_text(aes(y = 50, label = paste(strengthScore)), size = 14, fontface = "bold", color = "white") +
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
  
  ggsave(strengthScore_plot,file=paste0("Futures Reports Images/", athlete,"_strengthScore.png"), width=6,height=1,units="in", dpi = 150)
  strengthScorePlot <- image_read(paste0("Futures Reports Images/", athlete,"_strengthScore.png"))
  PitchingReport14 <- image_composite(PitchingReport13, strengthScorePlot, offset= "+1425+450")
  
  image_write(PitchingReport14,path = "page1.pdf",format="pdf",quality=100,density=300)
  image_write(IndexPage,path = "page2.pdf",format="pdf",quality=100,density=300)
  
  qpdf::pdf_combine(input = c("page1.pdf", "page2.pdf"),
                    output = paste0(athlete_folder, "/", "Futures Physicality Report.pdf"))
}