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

clientData <- read.csv("/Users/watts/Downloads/FullClientList.csv")
attendanceData <- read.csv("/Users/watts/Downloads/Learning_Academy_Body_Weight - Sheet1.csv")
proteusData<- read_csv("/Volumes/COLE'S DATA/Data/Physicality Report Data/ProteusPercentiles.csv")
teambuildrData <- read_csv("/Volumes/COLE'S DATA/Data/Physicality Report Data/teambuilderPercentiles.csv")
CMJdata <- read_csv("/Volumes/COLE'S DATA/Data/Physicality Report Data/CMJpercentiles.csv")
ISOSQTdata <- read_csv("/Volumes/COLE'S DATA/Data/Physicality Report Data/ISO_SquatPercentiles.csv")

CMJdata <- CMJdata %>%
  mutate(`Weight (lbs)` = round(`BW [KG]` * 2.20462, digits = 1))

# Function to calculate age
calculate_age <- function(birthdate) {
  if (is.na(birthdate)) {
    return(NA)
  } else {
    birthdate <- mdy(birthdate) # Convert to Date using lubridate
    age <- interval(start = birthdate, end = Sys.Date()) / years(1)
    return(floor(age)) # Floor the age to get complete years
  }
}

# Create a new column "Age" that calculates their age
clientData$Age <- sapply(clientData$`field.general.7.dl_date`, calculate_age)
colnames(clientData)[colnames(clientData) == "Client"] <- "Name"

TemplatePageOne <- image_read_pdf("/Volumes/COLE'S DATA/Templates/Strength Report Template.pdf")
IndexPage <- image_read_pdf("/Volumes/COLE'S DATA/Templates/Physicality Report Index.pdf")

setwd("/Users/watts/Documents/Futures Performance Center/Test")

athletes <- unique(attendanceData$Athlete)

for (athlete in athletes){
  
  athlete_folder <- paste0("Futures Reports/", athlete)
  if (!dir.exists(athlete_folder)) {
    dir.create(athlete_folder, recursive = TRUE)
  }
  
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
      select(Name, Age, Reporting.Level..Age.Dependent., Position..Baseball.Softball., Height)
    names(player_profile) <- c("Name:", "Age:", "Level:", "Position:", "Height:")
  }
  
  player_profile_two <- attendanceData %>%
    filter(Athlete == athlete) %>%
    select(Weight) %>%
    slice(n())
  names(player_profile_two) <- c("Weight:")
  
  combined_profile <- cbind(player_profile, player_profile_two)
  
  combined_profile$`Weight:` <- paste(combined_profile$`Weight:`, "lbs")
  
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
               hjust = 0, color = "#3d9be9", size = 8, fontface = "bold", family = "Good Times") +
      annotate("text", x = left_x + 0.1, y = y_positions[i], label = combined_profile$value[combined_profile$label == left_labels[i]], 
               hjust = 0, color = "white", size = 9, family = "Good Times")
  }
  
  # Add right labels and their values
  for (i in 1:length(right_labels)) {
    p <- p + 
      annotate("text", x = right_x, y = y_positions[i + 1], label = right_labels[i], 
               hjust = 0, color = "#3d9be9", size = 8, fontface = "bold", family = "Good Times") +
      annotate("text", x = right_x + 0.1, y = y_positions[i + 1], label = combined_profile$value[combined_profile$label == right_labels[i]], 
               hjust = 0, color = "white", size = 9, family = "Good Times")
  }
  
  attendanceData$Date.Recorded <- as.Date(attendanceData$Date.Recorded, format="%m/%d/%y")
  
  weight_plot <- attendanceData %>%
    filter(Athlete == athlete) %>% 
    ggplot(aes(x = Date.Recorded , y = Weight, color = "")) +
    geom_line(size = 2) +
    geom_point(size = 4) +
    labs(title = "Weight Trends") +
    theme_minimal() +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text=element_text(color = "white", size = 15),
          plot.title = element_text(hjust = 0.5, color = "white", size = 18, family = "Good Times")
    )
  
  full_player_profile <- ggarrange(p, weight_plot, nrow = 2)
  ggsave(full_player_profile,file=paste0("Futures Reports Images/", athlete,"_playerSummary.png"), width=6,height=5,units="in", dpi = 175)
  strengthSummarys <- image_read(paste0("Futures Reports Images/", athlete,"_playerSummary.png"))
  PitchingReport0 <- image_composite(TemplatePageOne,strengthSummarys,offset= "+100+525")
  
  ########################################################################################################
  #############################################  ATTENDANCE  #############################################
  ########################################################################################################
  
  attendance_plot_data <- attendanceData %>%
    filter(Athlete == athlete) %>%
    summarize(Attendance_Count = n()) %>%
    mutate(`Attendance Score` = round((Attendance_Count / 26) * 3, digits = 1))
  
  attendance_score <- max(attendance_plot_data$`Attendance Score`, na.rm = TRUE)
  
  get_color <- function(score) {
    if (score < 1.5) {
      return("red")
    } else if (score >= 1.5 & score < 2) {
      return("#FFA500")
    } else if (score >= 2 & score < 2.5) {
      return("green")
    } else {
      return("#3d9be9")
    }
  }
  
  attendance_plot <- attendance_plot_data %>% 
    ggplot(aes(x = "", y = `Attendance Score`)) +
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
  PitchingReport1 <- image_composite(PitchingReport0, attendancePlot, offset= "+200+1665")
  
  ########################################################################################################
  #############################################     CORE     #############################################
  ########################################################################################################
  
  if(any(proteusData$Name == athlete)) {
    
    proteusData$`session createdAt` <- as.Date(proteusData$`session createdAt`, format="%Y-%m-%dT%H:%M:%S")
    
    core_ylim_min <- min(proteusData$`power - mean`[proteusData$`exercise name` == "Straight Arm Trunk Rotation"], na.rm = TRUE)
    core_ylim_max <- max(proteusData$`power - mean`[proteusData$`exercise name` == "Straight Arm Trunk Rotation"], na.rm = TRUE)
    
    corePower_graph_data <- proteusData %>% 
      filter(Name == athlete & `exercise name` == "Straight Arm Trunk Rotation") %>%
      group_by(`session createdAt`) %>%
      summarize(`power - mean` = max(`power - mean`, na.rm = TRUE),
                PowerPercentileRank = max(PowerPercentileRank, na.rm = TRUE),
                .groups = "drop")
    
    maxPower <- max(corePower_graph_data$`power - mean`, na.rm = TRUE)
    
    core_power_percentile_graph <- corePower_graph_data %>% 
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
      annotate("text", x = 0, y = 1, label = "Strength Max: ", hjust = 0, vjust = -2.5, color = "white", size = 12, family = "Good Times") +
      annotate("text", x = 100, y = 1, label = paste(round(maxPower, digits = 1), " W", sep = ""), hjust = 1, vjust = -2.5, color = "white", size = 12, family = "Good Times")
    
    # Assuming `corePower_graph_data` is your dataframe and `power - mean` is the column you're interested in
    last_two_points <- tail(corePower_graph_data$`power - mean`, 2)
    
    # Check if the most recent data point is less than the previous data point
    set_ylim <- if (length(last_two_points) == 2 && last_two_points[2] < last_two_points[1]) TRUE else FALSE
    
    # Plot
    corePower_graph <- corePower_graph_data %>% 
      ggplot(aes(x = `session createdAt`, y = `power - mean`, color = "Straight Arm Trunk Rotation")) +
      geom_line(linewidth = 2) +
      geom_point(size = 4) +
      labs(title = "Core Strength  vs  Time") +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(color = "white", size = 17),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text=element_text(color = "white", size = 15),
            plot.title = element_text(hjust = 0.5, color = "white", size = 18, family = "Good Times")
      )
    
    # Apply ylim conditionally
    if (set_ylim) {
      corePower_graph <- corePower_graph + ylim(core_ylim_min, core_ylim_max)
    }
    
    coreAcc_graph_data <- proteusData %>%
      filter(Name == athlete & `exercise name` == "Straight Arm Trunk Rotation") %>%
      group_by(`session createdAt`) %>%
      summarize(`acceleration - mean` = max(`acceleration - mean`, na.rm = TRUE),
                AccelerationPercentileRank = max(AccelerationPercentileRank, na.rm = TRUE),
                .groups = "drop")
    
    maxAcc <- max(coreAcc_graph_data$`acceleration - mean`, na.rm = TRUE)
    
    core_acc_percentile_graph <- coreAcc_graph_data %>% 
      ggplot(aes(x = max(AccelerationPercentileRank), y = "")) +
      geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
      geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
      geom_point(aes(fill = max(AccelerationPercentileRank)), color = "black", pch = 21, size = 12) +
      geom_text(aes(label = round(max(AccelerationPercentileRank))), size = 8, fontface = "bold") +
      labs(title = paste("Speed Max: ", round(maxAcc, digits = 1), " m/s2", sep = "")) +
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
      annotate("text", x = 0, y = 1, label = "Speed Max: ", hjust = 0, vjust = -2.5, color = "white", size = 12, family = "Good Times") +
      annotate("text", x = 100, y = 1, label = paste(round(maxAcc, digits = 1), " m/s2", sep = ""), hjust = 1, vjust = -2.5, color = "white", size = 12, family = "Good Times")
    
    core_power_percentile_score <- max(corePower_graph_data$PowerPercentileRank, na.rm = TRUE)
    core_acc_percentile_score <- max(coreAcc_graph_data$AccelerationPercentileRank, na.rm = TRUE)
    
    ggsave(corePower_graph,file=paste0("Futures Reports Images/", athlete,"_coreStrengthPlot.png"), width=6,height=3,units="in", dpi = 150)
    strengthCharts1 <- image_read(paste0("Futures Reports Images/", athlete,"_coreStrengthPlot.png"))
    PitchingReport2 <- image_composite(PitchingReport1, strengthCharts1, offset= "+1425+900")
    
    ggsave(core_power_percentile_graph,file=paste0("Futures Reports Images/", athlete,"_coreStrengthPercentiles.png"), width=7,height=2,units="in", dpi = 150)
    strengthCharts2 <- image_read(paste0("Futures Reports Images/", athlete,"_coreStrengthPercentiles.png"))
    PitchingReport3 <- image_composite(PitchingReport2, strengthCharts2, offset= "+1350+1375")
    
    ggsave(core_acc_percentile_graph,file=paste0("Futures Reports Images/", athlete,"_coreSpeedPercentiles.png"), width=7,height=2,units="in", dpi = 150)
    strengthCharts3 <- image_read(paste0("Futures Reports Images/", athlete,"_coreSpeedPercentiles.png"))
    PitchingReport4 <- image_composite(PitchingReport3, strengthCharts3, offset= "+1350+1575")
  } else {
    
    empty_core_df <- data.frame()
    
    empty_percentile_graph <- empty_core_df %>% 
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
    
    empty_core_plot <- empty_core_df %>% 
      ggplot() + 
      geom_point() + 
      xlim(0, 10) + 
      ylim(0, 100) +
      annotate("text", x = 5, y = 50, label = "No data collected.\nPlease bring report to Futures Coaches to test.", 
               size = 8, color = "white", hjust = 0.5, vjust = 0.5) +
      theme_minimal() +
      theme(axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text=element_text(color = "white", size = 15)
      )
    
    core_power_percentile_score <- 0
    core_acc_percentile_score <- 0
    
    ggsave(empty_core_plot,file=paste0("Futures Reports Images/", athlete,"_coreStrengthPlot.png"), width=6,height=3,units="in", dpi = 150)
    strengthCharts1 <- image_read(paste0("Futures Reports Images/", athlete,"_coreStrengthPlot.png"))
    PitchingReport2 <- image_composite(PitchingReport1, strengthCharts1, offset= "+1425+950")
    
    ggsave(empty_percentile_graph,file=paste0("Futures Reports Images/", athlete,"_coreStrengthPercentiles.png"), width=6,height=1,units="in", dpi = 150)
    strengthCharts2 <- image_read(paste0("Futures Reports Images/", athlete,"_coreStrengthPercentiles.png"))
    PitchingReport3 <- image_composite(PitchingReport2, strengthCharts2, offset= "+1425+1450")
    
    ggsave(empty_percentile_graph,file=paste0("Futures Reports Images/", athlete,"_coreSpeedPercentiles.png"), width=6,height=1,units="in", dpi = 150)
    strengthCharts3 <- image_read(paste0("Futures Reports Images/", athlete,"_coreSpeedPercentiles.png"))
    PitchingReport4 <- image_composite(PitchingReport3, strengthCharts3, offset= "+1425+1650")
  }
  ########################################################################################################
  #############################################     LEGS     #############################################
  ########################################################################################################
  
  # Separate checks for athlete's presence in each dataset
  #athlete_in_ISOSQTdata <- any(ISOSQTdata$Name == athlete)
  athlete_in_CMJdata <- any(CMJdata$Name == athlete)
  
  # Convert dates to Date format for both datasets
  #ISOSQTdata$Date <- as.Date(ISOSQTdata$Date, format="%m/%d/%Y")
  CMJdata$Date <- as.Date(CMJdata$Date, format="%m/%d/%Y")
  teambuildrData$`Completed Date` <- as.Date(teambuildrData$`Completed Date`, format="%m/%d/%y")
  
  
  # legs_ylim_min <- min(ISOSQTdata$`Peak Vertical Force [N]`, na.rm = TRUE)
  # legs_ylim_max <- max(ISOSQTdata$`Peak Vertical Force [N]`, na.rm = TRUE)
  
  # Functions to generate empty plots
  generate_empty_percentile_graph <- function() {
    # [Insert your existing code for empty percentile graph here]
    empty_legs_df <- data.frame()
    empty_legs_percentile_graph <- empty_legs_df %>%
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
    return(empty_legs_percentile_graph)
  }
  
  generate_empty_legs_plot <- function() {
    # [Insert your existing code for empty legs plot here]
    empty_legs_df <- data.frame()
    empty_legs_plot <- empty_legs_df %>%
      ggplot() +
      geom_point() +
      xlim(0, 10) +
      ylim(0, 100) +
      annotate("text", x = 5, y = 50, label = "No data collected.\nPlease bring report to Futures Coaches to test.",
               size = 8, color = "white", hjust = 0.5, vjust = 0.5) +
      theme_minimal() +
      theme(axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text=element_text(color = "white", size = 15)
      )
    return(empty_legs_plot)
  }
  
  # # Process and plot ISOSQTdata if athlete is present
  # if (athlete_in_ISOSQTdata) {
    teambuilder_graph_data <- teambuildrData %>%
      filter(Name == athlete & `Exercise Name` == "Trap Bar Deadlift") %>%
      group_by(`Completed Date`) %>%
      summarise(`Highest Max` = max(`Highest Max`, na.rm = TRUE),
                PercentileRank = max(PercentileRank)) %>%
      ungroup()

    maxDeadlift <- max(teambuilder_graph_data$`Highest Max`, na.rm = TRUE)

    teambuilder_percentile_graph <- teambuilder_graph_data %>%
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
      annotate("text", x = 0, y = 1, label = "Strength Max: ", hjust = 0, vjust = -2.5, color = "white", size = 12, family = "Good Times") +
      annotate("text", x = 100, y = 1, label = paste(round(maxDeadlift, digits = 1), " LBS", sep = ""), hjust = 1, vjust = -2.5, color = "white", size = 12, family = "Good Times")
  
  legs_graph <- teambuilder_graph_data %>% 
    ggplot(aes(x = `Completed Date`, y = `Highest Max`, color = "Trap Bar Deadlift")) +
    geom_line(linewidth = 2) +
    geom_point(size = 4) +
    labs(title = "Deadlift Max  vs  Time") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(color = "white", size = 17),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text=element_text(color = "white", size = 15),
          plot.title = element_text(hjust = 0.5, color = "white", size = 18, family = "Good Times")
    )    
  
  legs_power_percentile_score <- max(teambuilder_graph_data$PercentileRank, na.rm = TRUE)
  
  ggsave(legs_graph,file=paste0("Futures Reports Images/", athlete,"_legsPlot.png"), width=6,height=3,units="in", dpi = 150)
  strengthCharts4 <- image_read(paste0("Futures Reports Images/", athlete,"_legsPlot.png"))
  PitchingReport5 <- image_composite(PitchingReport4, strengthCharts4, offset= "+175+2175")
  
  ggsave(teambuilder_percentile_graph,file=paste0("Futures Reports Images/", athlete,"_legPerctiles.png"), width=7,height=3,units="in", dpi = 150)
  strengthCharts5 <- image_read(paste0("Futures Reports Images/", athlete,"_legPerctiles.png"))
  PitchingReport6 <- image_composite(PitchingReport5, strengthCharts5, offset= "+110+2600")
  
  # # Extract the last two data points from the `ISOSQT_graph_data` dataframe
  # last_two_points_isosqt <- tail(ISOSQT_graph_data$`Peak Vertical Force [N]`, 2)
  # 
  # # Check if the most recent data point is less than the previous data point
  # set_ylim_isosqt <- if (length(last_two_points_isosqt) == 2 && last_two_points_isosqt[2] < last_two_points_isosqt[1]) TRUE else FALSE
  # 
  # # Build the plot
  # ISOSQT_graph <- ISOSQT_graph_data %>%
  #   ggplot(aes(x = Date, y = `Peak Vertical Force [N]`, color = "Isometric Belt Squat")) +
  #   geom_line(linewidth = 2) +
  #   geom_point(size = 4) +
  #   labs(title = "Lower Body Strength  vs  Time") +
  #   theme_minimal() +
  #   theme(legend.position = "bottom",
  #         legend.title = element_blank(),
  #         legend.text = element_text(color = "white", size = 17),
  #         axis.title.y = element_blank(),
  #         axis.title.x = element_blank(),
  #         axis.text=element_text(color = "white", size = 15),
  #         plot.title = element_text(hjust = 0.5, color = "white", size = 18, family = "Good Times"))
  # 
  # # Apply ylim conditionally
  # if (set_ylim_isosqt) {
  #   ISOSQT_graph <- ISOSQT_graph + ylim(legs_ylim_min, legs_ylim_max)
  # }
  
  #legs_power_percentile_score <- max(ISOSQT_graph_data$PercentileRank, na.rm = TRUE)
  
  # ggsave(ISOSQT_graph,file=paste0("Futures Reports Images/", athlete,"_legsPlot.png"), width=6,height=3,units="in", dpi = 150)
  # strengthCharts4 <- image_read(paste0("Futures Reports Images/", athlete,"_legsPlot.png"))
  # PitchingReport5 <- image_composite(PitchingReport4, strengthCharts4, offset= "+175+2175")
  
  # ggsave(ISOSQT_percentile_graph,file=paste0("Futures Reports Images/", athlete,"_ISOSqtPerctiles.png"), width=7,height=3,units="in", dpi = 150)
  # strengthCharts5 <- image_read(paste0("Futures Reports Images/", athlete,"_ISOSqtPerctiles.png"))
  # PitchingReport6 <- image_composite(PitchingReport5, strengthCharts5, offset= "+110+2600")
  # } else {
  # 
  #   legs_power_percentile_score <- 0
  # 
  #   # Use empty plots for ISOSQT
  #   empty_legs_plot <- generate_empty_legs_plot()
  #   ggsave(empty_legs_plot, file=paste0("Futures Reports Images/", athlete, "_legsPlot.png"), width=6, height=3, units="in", dpi=150)
  #   strengthCharts4 <- image_read(paste0("Futures Reports Images/", athlete, "_legsPlot.png"))
  #   PitchingReport5 <- image_composite(PitchingReport4, strengthCharts4, offset= "+175+2200")
  # 
  #   empty_legs_percentile_graph <- generate_empty_percentile_graph()
  #   ggsave(empty_legs_percentile_graph, file=paste0("Futures Reports Images/", athlete, "_ISOSqtPerctiles.png"), width=6, height=1, units="in", dpi=150)
  #   strengthCharts5 <- image_read(paste0("Futures Reports Images/", athlete, "_ISOSqtPerctiles.png"))
  #   PitchingReport6 <- image_composite(PitchingReport5, strengthCharts5, offset= "+175+2700")
  # }
  
  # Process and plot CMJdata if athlete is present
  if (athlete_in_CMJdata) {
    CMJ_graph_data <- CMJdata %>%
      filter(Name == athlete) %>%
      group_by(Date) %>%
      summarize(`Concentric Peak Force [N]` = max(`Concentric Peak Force [N]`, na.rm = TRUE),
                PercentileRank = max(PercentileRank),
                `Peak Landing Force % (Asym) (%)` = max(`Peak Landing Force % (Asym) (%)`, na.rm = TRUE)) %>%
      ungroup()
    
    maxCMJ <- max(CMJ_graph_data$`Concentric Peak Force [N]`, na.rm = TRUE)
    
    # Processing the asymmetry data
    CMJ_graph_data$AsymmetryValue <- as.numeric(gsub("[^0-9.]", "", CMJ_graph_data$`Peak Landing Force % (Asym) (%)`))
    CMJ_graph_data$AsymmetrySide <- ifelse(grepl("R", CMJ_graph_data$`Peak Landing Force % (Asym) (%)`), "% R", "% L")
    
    max_asymmetry_value <- max(CMJ_graph_data$AsymmetryValue[CMJ_graph_data$AsymmetryValue > 15], na.rm = TRUE)
    max_asymmetry_side <- CMJ_graph_data$AsymmetrySide[which.max(CMJ_graph_data$AsymmetryValue == max_asymmetry_value)]
    max_asymmetry_info <- paste(max_asymmetry_value, max_asymmetry_side)
    
    has_asymmetry_data <- max_asymmetry_value != -Inf
    vjust_value <- if (has_asymmetry_data) -4 else -2.5
    
    asymmetry_annotations <- if(max_asymmetry_value != -Inf) {
      list(
        annotate("text", x = 0, y = 1, label = "Asymmetry:", hjust = 0, vjust = -3, color = "white", size = 8, family = "Good Times"),
        annotate("text", x = 100, y = 1, label = max_asymmetry_info, hjust = 1, vjust = -3, color = "white", size = 8, family = "Good Times")
      )
    } else {
      list()
    }
    
    CMJ_percentile_graph <- CMJ_graph_data %>%
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
      annotate("text", x = 0, y = 1, label = "Speed Max: ", hjust = 0, vjust = vjust_value, color = "white", size = 12, family = "Good Times") +
      annotate("text", x = 100, y = 1, label = paste(round(maxCMJ, digits = 1), " N", sep = ""), hjust = 1, vjust = vjust_value, color = "white", size = 12, family = "Good Times") +
      asymmetry_annotations
    
    legs_acc_percentile_score <- max(CMJ_graph_data$PercentileRank, na.rm = TRUE)
    
    ggsave(CMJ_percentile_graph,file=paste0("Futures Reports Images/", athlete,"_CMJPercentiles.png"), width=7,height=3,units="in", dpi = 150)
    strengthCharts6 <- image_read(paste0("Futures Reports Images/", athlete,"_CMJPercentiles.png"))
    PitchingReport7 <- image_composite(PitchingReport6, strengthCharts6, offset= "+110+2825")
  } else {
    
    legs_acc_percentile_score <- 0
    
    # Use empty plot for CMJ
    empty_legs_percentile_graph <- generate_empty_percentile_graph()
    ggsave(empty_legs_percentile_graph, file=paste0("Futures Reports Images/", athlete, "_CMJPercentiles.png"), width=6, height=1, units="in", dpi=150)
    strengthCharts6 <- image_read(paste0("Futures Reports Images/", athlete, "_CMJPercentiles.png"))
    PitchingReport7 <- image_composite(PitchingReport6, strengthCharts6, offset= "+175+2900")
  }
  
  
  ########################################################################################################
  ############################################# UPPER BODY  ##############################################
  ########################################################################################################
  
  if(any(proteusData$Name == athlete)) {
    
    arms_ylim_min <- min(proteusData$`power - mean`[proteusData$`exercise name` == "Horizontal Row (One Hand)"], na.rm = TRUE)
    arms_ylim_max <- max(proteusData$`power - mean`[proteusData$`exercise name` == "Horizontal Row (One Hand)"], na.rm = TRUE)
    
    pushpull_power_graph_data <- proteusData %>% 
      filter(Name == athlete & 
               (`exercise name` == "Chest Press (One Hand)" | `exercise name` == "Horizontal Row (One Hand)" | `exercise name` == "PushPull" |
                  `exercise name` == "PNF D2 Extension" | `exercise name` == "PNF D2 Flexion" | `exercise name` == "Shot Put (Countermovement)")) %>%
      group_by(`session createdAt`, `exercise name`) %>%
      summarize(`power - mean` = mean(`power - mean`, na.rm = TRUE),
                PowerPercentileRank = max(PowerPercentileRank, na.rm = TRUE),
                .groups = "drop")
    
    # Calculating max values for each exercise and dominance type
    max_power_push <- max(pushpull_power_graph_data$`power - mean`[pushpull_power_graph_data$`exercise name` == "Chest Press (One Hand)"], na.rm = TRUE)
    
    max_power_pull <- max(pushpull_power_graph_data$`power - mean`[pushpull_power_graph_data$`exercise name` == "Horizontal Row (One Hand)"], na.rm = TRUE)
    
    pushpull_power_percentile_graph <- pushpull_power_graph_data %>%
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
      annotate("text", x = 0, y = 1, label = "Strength Max: ", hjust = 0, vjust = -2.5, color = "white", size = 12, family = "Good Times") +
      annotate("text", x = 100, y = 1, label = paste("Push:", round(max_power_push, digits = 1), "W\n",
                                                     "Pull:", round(max_power_pull, digits = 1), "W"), hjust = 1, vjust = -0.65, color = "white", size = 8, family = "Good Times")
    
    
    last_two_points_chest_press <- pushpull_power_graph_data %>% 
      filter(`exercise name` == "Chest Press (One Hand)") %>%
      tail(2) %>% 
      pull(`power - mean`)
    
    last_two_points_horizontal_row <- pushpull_power_graph_data %>% 
      filter(`exercise name` == "Horizontal Row (One Hand)") %>%
      tail(2) %>% 
      pull(`power - mean`)
    
    # Check if the most recent data points are less than the previous points for both exercises
    set_ylim_chest_press <- if (length(last_two_points_chest_press) == 2 && 
                                last_two_points_chest_press[2] < last_two_points_chest_press[1]) TRUE else FALSE
    set_ylim_horizontal_row <- if (length(last_two_points_horizontal_row) == 2 && 
                                   last_two_points_horizontal_row[2] < last_two_points_horizontal_row[1]) TRUE else FALSE
    
    # Determine if ylim should be set (true if either exercise needs ylim set)
    set_ylim <- set_ylim_chest_press || set_ylim_horizontal_row
    
    # Build the plot
    pushpull_power_graph <- pushpull_power_graph_data %>%
      filter(`exercise name` == "Chest Press (One Hand)" | `exercise name` == "Horizontal Row (One Hand)") %>% 
      ggplot(aes(x = `session createdAt`, group = `exercise name`)) +
      geom_point(aes(y = `power - mean`, color = `exercise name`), size = 4) +
      geom_line(aes(y = `power - mean`, color = `exercise name`), linewidth = 2) +
      labs(title = "Upper Body Strength  vs  Time") +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(color = "white", size = 17),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text=element_text(color = "white", size = 15),
            plot.title = element_text(hjust = 0.5, color = "white", size = 18, family = "Good Times"))
    
    # Apply ylim conditionally
    if (set_ylim) {
      pushpull_power_graph <- pushpull_power_graph + ylim(arms_ylim_min, arms_ylim_max)
    }
    
    pushpull_acc_graph_data <- proteusData %>% 
      filter(Name == athlete & 
               (`exercise name` == "Chest Press (One Hand)" | `exercise name` == "Horizontal Row (One Hand)" | `exercise name` == "PushPull" |
                  `exercise name` == "PNF D2 Extension" | `exercise name` == "PNF D2 Flexion" | `exercise name` == "Shot Put (Countermovement)")) %>%
      group_by(`session createdAt`, `exercise name`) %>%
      summarize(`acceleration - mean` = mean(`acceleration - mean`, na.rm = TRUE),
                AccelerationPercentileRank = max(AccelerationPercentileRank, na.rm = TRUE),
                .groups = "drop")
    
    max_acc_push <- max(pushpull_acc_graph_data$`acceleration - mean`[pushpull_acc_graph_data$`exercise name` == "Chest Press (One Hand)"], na.rm = TRUE)
    
    max_acc_pull <- max(pushpull_acc_graph_data$`acceleration - mean`[pushpull_acc_graph_data$`exercise name` == "Horizontal Row (One Hand)"], na.rm = TRUE)
    
    pushpull_acc_percentile_graph <- pushpull_acc_graph_data %>%
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
      annotate("text", x = 0, y = 1, label = "Speed Max: ", hjust = 0, vjust = -2.5, color = "white", size = 12, family = "Good Times") +
      annotate("text", x = 100, y = 1, label = paste("Push: ", round(max_acc_push, digits = 1), "m/s2\n",
                                                     "Pull: ", round(max_acc_pull, digits = 1), "m/s2"), hjust = 1, vjust = -0.65, color = "white", size = 8, family = "Good Times")
    
    pushpull_power_percentile_score <- max(pushpull_power_graph_data$PowerPercentileRank, na.rm = TRUE)
    pushpull_acc_percentile_score <- max(pushpull_acc_graph_data$AccelerationPercentileRank, na.rm = TRUE)
    
    # Calculate max power and acceleration percentile scores for PNF D2 Flexion
    flexion_data <- filter(pushpull_power_graph_data, `exercise name` == "PNF D2 Flexion")
    flexion_power_percentile_score <- max(flexion_data$PowerPercentileRank, na.rm = TRUE)
    flexion_data <- filter(pushpull_acc_graph_data, `exercise name` == "PNF D2 Flexion")
    flexion_acc_percentile_score <- max(flexion_data$AccelerationPercentileRank, na.rm = TRUE)
    
    # Calculate max power and acceleration percentile scores for PNF D2 Extension
    extension_data <- filter(pushpull_power_graph_data, `exercise name` == "PNF D2 Extension")
    extension_power_percentile_score <- max(extension_data$PowerPercentileRank, na.rm = TRUE)
    extension_data <- filter(pushpull_acc_graph_data, `exercise name` == "PNF D2 Extension")
    extension_acc_percentile_score <- max(extension_data$AccelerationPercentileRank, na.rm = TRUE)
    
    # Calculate max power and acceleration percentile scores for Shot Put (Countermovement)
    shotput_data <- filter(pushpull_power_graph_data, `exercise name` == "Shot Put (Countermovement)")
    shotput_power_percentile_score <- max(shotput_data$PowerPercentileRank, na.rm = TRUE)
    shotput_data <- filter(pushpull_acc_graph_data, `exercise name` == "Shot Put (Countermovement)")
    shotput_acc_percentile_score <- max(shotput_data$AccelerationPercentileRank, na.rm = TRUE)
    
    ggsave(pushpull_power_graph,file=paste0("Futures Reports Images/", athlete,"_pushpullPlot.png"), width=6,height=3,units="in", dpi = 150)
    strengthCharts7 <- image_read(paste0("Futures Reports Images/", athlete,"_pushpullPlot.png"))
    PitchingReport8 <- image_composite(PitchingReport7, strengthCharts7, offset= "+1425+2175")
    
    ggsave(pushpull_power_percentile_graph,file=paste0("Futures Reports Images/", athlete,"_pushpullStrengthPercentile.png"), width=7,height=3,units="in", dpi = 150)
    strengthCharts8 <- image_read(paste0("Futures Reports Images/", athlete,"_pushpullStrengthPercentile.png"))
    PitchingReport9 <- image_composite(PitchingReport8, strengthCharts8, offset= "+1350+2600")
    
    ggsave(pushpull_acc_percentile_graph,file=paste0("Futures Reports Images/", athlete,"_pushpullSpeedPercentile.png"), width=7,height=3,units="in", dpi = 150)
    strengthCharts9 <- image_read(paste0("Futures Reports Images/", athlete,"_pushpullSpeedPercentile.png"))
    PitchingReport10 <- image_composite(PitchingReport9, strengthCharts9, offset= "+1350+2825")
  }else{
    
    empty_arms_df <- data.frame()
    
    empty_arms_percentile_graph <- empty_arms_df %>% 
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
    
    empty_arms_plot <- empty_arms_df %>% 
      ggplot() + 
      geom_point() + 
      xlim(0, 10) + 
      ylim(0, 100) +
      annotate("text", x = 5, y = 50, label = "No data collected.\nPlease bring report to Futures Coaches to test.", 
               size = 8, color = "white", hjust = 0.5, vjust = 0.5) +
      theme_minimal() +
      theme(axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text=element_text(color = "white", size = 15)
      )
    
    pushpull_power_percentile_score <- 0
    pushpull_acc_percentile_score <- 0
    flexion_power_percentile_score <- 0
    flexion_acc_percentile_score <- 0
    extension_power_percentile_score <- 0
    extension_acc_percentile_score <- 0
    shotput_power_percentile_score <- 0
    shotput_acc_percentile_score <- 0
    
    ggsave(empty_arms_plot,file=paste0("Futures Reports Images/", athlete,"_pushpullPlot.png"), width=6,height=3,units="in", dpi = 150)
    strengthCharts7 <- image_read(paste0("Futures Reports Images/", athlete,"_pushpullPlot.png"))
    PitchingReport8 <- image_composite(PitchingReport7, strengthCharts7, offset= "+1425+2200")
    
    ggsave(empty_arms_percentile_graph,file=paste0("Futures Reports Images/", athlete,"_pushpullStrengthPercentile.png"), width=6,height=1,units="in", dpi = 150)
    strengthCharts8 <- image_read(paste0("Futures Reports Images/", athlete,"_pushpullStrengthPercentile.png"))
    PitchingReport9 <- image_composite(PitchingReport8, strengthCharts8, offset= "+1425+2700")
    
    ggsave(empty_arms_percentile_graph,file=paste0("Futures Reports Images/", athlete,"_pushpullSpeedPercentile.png"), width=6,height=1,units="in", dpi = 150)
    strengthCharts9 <- image_read(paste0("Futures Reports Images/", athlete,"_pushpullSpeedPercentile.png"))
    PitchingReport10 <- image_composite(PitchingReport9, strengthCharts9, offset= "+1425+2900")
  }
  
  ########################################################################################################
  ###########################################  Weightroom text  ##########################################
  ########################################################################################################
  weightroom_data <- teambuildrData %>% 
    filter(Name == athlete)
  
  core_subset <- weightroom_data$`Highest Max`[weightroom_data$`Exercise Name` == "Straight Arm Trunk Rotation Max Isometric Test - Crane Scale"]
  max_core <- if (all(is.na(core_subset))) NA else max(core_subset, na.rm = TRUE)
  
  core_text <- ggplot() + 
    annotate("text", x = 0, y = 0, label = paste("Core Rotation ISO Max:", max_core, "N"), size = 7, family = "Good Times", color = "white") + 
    theme_void() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "lines"))
  
  ggsave(core_text,file=paste0("Futures Reports Images/", athlete,"_coreText.png"), width=5,height=1,units="in", dpi = 150)
  strengthCharts10 <- image_read(paste0("Futures Reports Images/", athlete,"_coreText.png"))
  PitchingReport11 <- image_composite(PitchingReport10, strengthCharts10, offset= "+1500+1775")
  
  # deadlift_subset <- teambuildrData$`Highest Max`[teambuildrData$`Exercise Name` == "Trap Bar Deadlift"]
  # max_deadlift <- if (all(is.na(deadlift_subset))) NA else max(deadlift_subset, na.rm = TRUE)
  squat_subset <- weightroom_data$`Highest Max`[weightroom_data$`Exercise Name` == "Barbell Back Squat"]
  max_squat <- if (all(is.na(squat_subset))) NA else max(squat_subset, na.rm = TRUE)
  
  lowerbody_text <- ggplot() +
    # annotate("text", x = 0, y = 0, label = paste("Trap Bar Deadift Max:", max_deadlift, "LBS  -  ", 
    #                                              "Squat Max:", max_squat, "LBS"), size = 7, family = "Good Times", color = "white") + 
    annotate("text", x = 0, y = 0, label = paste("Squat Max:", max_squat, "LBS"), size = 7, family = "Good Times", color = "white") + 
    theme_void() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "lines"))
  
  ggsave(lowerbody_text,file=paste0("Futures Reports Images/", athlete,"_lowerbodyText.png"), width=8,height=1,units="in", dpi = 150)
  strengthCharts11 <- image_read(paste0("Futures Reports Images/", athlete,"_lowerbodyText.png"))
  PitchingReport12 <- image_composite(PitchingReport11, strengthCharts11, offset= "+40+3075")
  
  bench_subset <- weightroom_data$`Highest Max`[weightroom_data$`Exercise Name` == "Barbell Bench Press"]
  max_bench <- if (all(is.na(bench_subset))) NA else max(bench_subset, na.rm = TRUE)
  pulldown_subset <- weightroom_data$`Highest Max`[weightroom_data$`Exercise Name` == "Cable Lat Pull Down (1)"]
  max_pulldown <- if (all(is.na(pulldown_subset))) NA else max(pulldown_subset, na.rm = TRUE)
  
  upperbody_text <- ggplot() + 
    annotate("text", x = 0, y = 0, label = paste("Bench Press Max:", max_bench, "LBS  -  ", 
                                                 "Lat Pulldown Max:", max_pulldown, "LBS"), size = 7, family = "Good Times", color = "white") + 
    theme_void() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "lines"))
  
  ggsave(upperbody_text,file=paste0("Futures Reports Images/", athlete,"_upperbodyText.png"), width=8,height=1,units="in", dpi = 150)
  strengthCharts12 <- image_read(paste0("Futures Reports Images/", athlete,"_upperbodyText.png"))
  PitchingReport13 <- image_composite(PitchingReport12, strengthCharts12, offset= "+1285+3075")
  
  ########################################################################################################
  ###########################################  Strength Score  ###########################################
  ########################################################################################################
  
  scores <- c(core_power_percentile_score, core_acc_percentile_score, legs_power_percentile_score,
              legs_acc_percentile_score, pushpull_power_percentile_score, pushpull_acc_percentile_score,
              flexion_power_percentile_score, flexion_acc_percentile_score, extension_power_percentile_score,
              extension_acc_percentile_score, shotput_power_percentile_score, shotput_acc_percentile_score)
  
  
  strengthScore <- round(mean(scores, na.rm = TRUE), digits = 1)
  
  get_color_two <- function(score) {
    if (score < 50) {
      return("red")
    } else if (score >= 50 & score < 70) {
      return("#FFA500")
    } else if (score >= 70 & score < 90) {
      return("green")
    } else {
      return("#3d9be9")
    }
  }
  
  if (any(scores == 0, na.rm = TRUE)) {
    strengthScore_plot <- ggplot(data.frame(Client.name = "N/A", y = 50), aes(x = Client.name, y = y)) +
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
      ggplot(aes(x = "", y = strengthScore)) +
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
  PitchingReport14 <- image_composite(PitchingReport13, strengthScorePlot, offset= "+1425+475")
  
  image_write(PitchingReport14,path = "page1.pdf",format="pdf",quality=100,density=300)
  image_write(IndexPage,path = "page2.pdf",format="pdf",quality=100,density=300)
  
  qpdf::pdf_combine(input = c("page1.pdf", "page2.pdf"),
                    output = paste0(athlete_folder, "/", "Futures Physicality Report.pdf"))  
}
