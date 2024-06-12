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
library(sportyR)

teamInfo <- read_csv("/Users/watts/Downloads/Ohana Tigers OTQ Info Form (Responses) - Form Responses 1.csv")
hittraxData <- read_csv("/Users/watts/Downloads/data.csv")
speedData <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Facility Data/SpeedFacilityData.csv")
proteusData <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Strength Percentiles/ProteusPercentiles.csv")
SJ_data <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Strength Percentiles/SQTJumpPercentiles.csv")
armcare_data <- read_csv("/Users/watts/Downloads/ArmCare_TeamTesting.csv")
blastData <- read_csv("/Users/watts/Downloads/Full Team Report - 2024-05-18 - 2024-05-18 - 1716575333/Brandon Oliver.csv")
trackmanData <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Pitching_2024-05-18T203259_verified.csv")

athletes <- unique(teamInfo$`Athlete First & Last Name`)

calculate_age <- function(birthdate) {
  if (is.na(birthdate)) {
    return(NA)
  } else {
    birthdate <- mdy(birthdate) # Convert to Date using lubridate
    age <- interval(start = birthdate, end = Sys.Date()) / years(1)
    return(floor(age)) # Floor the age to get complete years
  }
}

teamInfo$Age <- sapply(teamInfo$`Athlete DOB`, calculate_age)

hittraxData <- hittraxData %>%
  filter(Name != "Softball Dummy") %>% 
  mutate(
    MaxVel_Rank_Facility = rank(-MaxVel, ties.method = "min"),
    AvgVel_Rank_Facility = rank(-AvgVel, ties.method = "min"),
    MaxDist_Rank_Facility = rank(-MaxDist, ties.method = "min"),
    AvgDist_Rank_Facility = rank(-AvgDist, ties.method = "min"),
    Total_Players_Facility = n())
    

filteredHittrax <- hittraxData %>% 
  filter(Name %in% athletes) %>% 
  mutate(
    MaxVel_Rank_Team = rank(-MaxVel, ties.method = "min"),
    AvgVel_Rank_Team = rank(-AvgVel, ties.method = "min"),
    MaxDist_Rank_Team = rank(-MaxDist, ties.method = "min"),
    AvgDist_Rank_Team = rank(-AvgDist, ties.method = "min"),
    Total_Players_Team = n())

filtered_armcare <- armcare_data %>% 
  mutate(
    Name = paste(`First Name`, `Last Name`)) %>% 
  filter(Name %in% athletes) 
  
unique_data <- speedData %>%
  group_by(Name, Month, Year, `Exercise Name`, Level, Gender) %>%
  summarize(
    Cumulative3 = min(Cumulative3, na.rm = TRUE),
    Split1 = min(Split1, na.rm = TRUE),
    Cumulative2 = min(Split1 + Split3, na.rm = TRUE),
    MPH = max(MPH, na.rm = TRUE),
    PercentileRank = max(PercentileRank, na.rm = TRUE),
    .groups = 'drop'
  )

ranked_data <- unique_data %>%
  group_by(Level, Gender, Month, Year, `Exercise Name`) %>%
  mutate(Facility_Rank = case_when(
    `Exercise Name` == "40 Yard Dash" ~ rank(Cumulative3, ties.method = "min"),
    `Exercise Name` == "Acceleration" ~ rank(Split1, ties.method = "min"),
    `Exercise Name` == "Hard 90" ~ rank(Cumulative2, ties.method = "min"),
    `Exercise Name` == "Max Velocity" ~ rank(desc(MPH), ties.method = "min")),
    Total_Players_Facility = n()) %>%
  ungroup()

combined_speed <- ranked_data %>%
  filter(Name %in% athletes) %>% 
  group_by(Level, Gender, Month, Year, `Exercise Name`) %>%
  mutate(Team_Rank = case_when(
    `Exercise Name` == "40 Yard Dash" ~ rank(Cumulative3, ties.method = "min"),
    `Exercise Name` == "Acceleration" ~ rank(Split1, ties.method = "min"),
    `Exercise Name` == "Hard 90" ~ rank(Cumulative2, ties.method = "min"),
    `Exercise Name` == "Max Velocity" ~ rank(desc(MPH), ties.method = "min")),
    Total_Players_Team = n()) %>%
  ungroup()

proteusData <- proteusData %>%
  filter(`exercise name` == "Proteus Full Test") %>% 
  group_by(Month, Name, Level, Gender) %>% 
  summarise(`power - high` = round(mean(`power - high`, na.rm = TRUE), 1),
            `acceleration - high` = round(mean(`acceleration - high`, na.rm = TRUE), 1),
            PowerPercetileRank = round(mean(PowerPercentileRank, na.rm = TRUE), 1),
            AccelerationPercentileRank = round(mean(AccelerationPercentileRank, na.rm = TRUE), 1),
            .groups = "drop")

proteusRanked <- proteusData %>%
  group_by(Level, Gender, Month) %>% 
  mutate(
    Power_Rank_Facility = rank(-`power - high`, ties.method = "min"),
    Acceleration_Rank_Facility = rank(-`acceleration - high`, ties.method = "min"),
    Total_Players_Facility = n()) %>% 
  ungroup()

filtered_proteus <- proteusRanked %>%
  filter(Name %in% athletes, Month == "May") %>% 
  mutate(
    Power_Rank_Team = rank(-`power - high`, ties.method = "min"),
    Acceleration_Rank_Team = rank(-`acceleration - high`, ties.method = "min"),
    Total_Players_Team = n()) %>% 
  ungroup()

SJ_data <- SJ_data %>%
  group_by(Month, Name, Level, Gender) %>% 
  summarise(`Jump Height (in)` = round(mean(`Jump Height (Imp-Mom) in Inches [in]`, na.rm = TRUE), 1),
            Value = round(mean(`Takeoff Peak Force [N]`, na.rm = TRUE), 1),
            `Perc.` = round(mean(PercentileRank, na.rm = TRUE), 1),
            .groups = "drop")

SJ_ranked <- SJ_data %>% 
  group_by(Level, Gender, Month) %>% 
  mutate(
    SJ_Facility_Rank = rank(-Value, ties.method = "min"),
    Total_Players_Facility = n())

filtered_SJ <- SJ_ranked %>% 
  filter(Name %in% athletes, Month == "May") %>% 
  mutate(
    SJ_Team_Rank = rank(-Value, ties.method = "min"),
    Total_Players_Team = n())

TemplatePageOne <- image_read_pdf("/Volumes/COLE'S DATA/Templates/Team Report Template.pdf")

swingOrder <- c("Tee", "Front Toss Underhand", "Pitching Machine", "In Game", "Goals")

setwd("/Users/watts/Documents/Futures Performance Center/Test")

for (athlete in athletes){
  
  player_profile <- teamInfo %>%
    filter(`Athlete First & Last Name` == athlete) %>%
    mutate(Position = NA, GPA = NA, Height = NA, Weight = NA, Class = NA, HT_WT = paste(Height, " / ", Weight)) %>% 
    select(`Athlete First & Last Name`, Age, `Athlete Age Group`, Position, GPA, HT_WT, `Athlete High School`, Class)
  names(player_profile) <- c("Name:", "Age:", "Level:", "Position:", "GPA:", "HT / WT:", "School:", "Class:")
  
  player_profile <- player_profile %>% 
    mutate(across(c(`Name:`, `Age:`, `Level:`, `Position:`, `School:`, `Class:`, `GPA:`, `HT / WT:`), as.character)) %>%
    pivot_longer(cols = c(`Name:`, `Age:`, `Level:`, `Position:`, `School:`, `Class:`, `GPA:`, `HT / WT:`), names_to = "label", values_to = "value")
  
  # Define the coordinates for the labels and values
  left_labels <- c("Name:", "Age:", "Level:")
  middle_labels <- c("Class:", "GPA:", "School:")
  right_labels <- c("HT / WT:", "Position:") 
  left_x <- 0.1    # x position for left labels
  middle_x <- 0.35  # x position for middle labels
  right_x <- 0.60   # x position for right labels
  y_positions <- seq(0.9, 0.3, by = -0.1)  # y positions for each label
  
  # Create a blank ggplot object
  p <- ggplot() +
    expand_limits(x = 0.8, y = 0.5) +
    theme_void() 
  
  # Add left labels and their values
  for (i in 1:length(left_labels)) {
    p <- p + 
      annotate("text", x = left_x, y = y_positions[i], label = left_labels[i], 
               hjust = 0, color = "#3d9be9", size = 5, family = "Good Times") +
      annotate("text", x = left_x + 0.05, y = y_positions[i], label = player_profile$value[player_profile$label == left_labels[i]], 
               hjust = 0, color = "white", size = 6, family = "Good Times")
  }
  
  # Add middle labels and their values
  for (i in 1:length(middle_labels)) {
    p <- p + 
      annotate("text", x = middle_x, y = y_positions[i], label = middle_labels[i], 
               hjust = 0, color = "#3d9be9", size = 5, family = "Good Times") +
      annotate("text", x = middle_x + 0.07, y = y_positions[i], label = player_profile$value[player_profile$label == middle_labels[i]], 
               hjust = 0, color = "white", size = 6, family = "Good Times")
  }
  
  # Add right labels and their values
  for (i in 1:length(right_labels)) {
    p <- p + 
      annotate("text", x = right_x, y = y_positions[i], label = right_labels[i], 
               hjust = 0, color = "#3d9be9", size = 5, family = "Good Times") +
      annotate("text", x = right_x + 0.075, y = y_positions[i], label = player_profile$value[player_profile$label == right_labels[i]], 
               hjust = 0, color = "white", size = 6, family = "Good Times")
  }
  
  ggsave(p,file=paste0("Futures Reports Images/",athlete," - playerProfile.png"), width=16,height=2.25,units="in", dpi = 150)
  playerSummary1 <- image_read(paste0("Futures Reports Images/",athlete," - playerProfile.png"))
  teamReport1 <- image_composite(TemplatePageOne, playerSummary1, offset= "+50+465")
  
  #Hittrax Section
  metrics <- c('Max EV', 'Avg EV', 'Max Dist', 'Avg Dist')
  originalMetrics <- c('MaxVel', 'AvgVel', 'MaxDist', 'AvgDist')
  percentiles <- c('MaxVel Rank', 'AvgVel Rank', 'MaxDist Rank', 'AvgDist Rank')
  facility_ranks <- c('MaxVel_Rank_Facility', 'AvgVel_Rank_Facility', 'MaxDist_Rank_Facility', 'AvgDist_Rank_Facility')
  team_ranks <- c('MaxVel_Rank_Team', 'AvgVel_Rank_Team', 'MaxDist_Rank_Team', 'AvgDist_Rank_Team')
  
  final_hittrax <- filteredHittrax %>% 
    filter(Name == athlete)
  
  if (nrow(final_hittrax) == 0) {
    next  # Skip to the next athlete if final_hittrax is empty
  }
  
  hittrax_datatable <- data.frame(Metric = character(), 
                          Value = numeric(), 
                          `Facility Rank` = character(),
                          `Team Rank` = character(),
                          `Perc.` = numeric(), 
                          stringsAsFactors = FALSE,
                          check.names = FALSE)
  
  total_players_facility <- final_hittrax$Total_Players_Facility[1]
  total_players_team <- final_hittrax$Total_Players_Team[1]
  
  for (i in 1:length(metrics)) {
    facility_rank <- paste(final_hittrax[[facility_ranks[i]]][1], "/", total_players_facility, sep="")
    team_rank <- paste(final_hittrax[[team_ranks[i]]][1], "/", total_players_team, sep="")
    hittrax_datatable <- rbind(hittrax_datatable, data.frame(
      Metric = metrics[i],
      Value = final_hittrax[[originalMetrics[i]]][1],
      `Facility Rank` = facility_rank,
      `Team Rank` = team_rank,
      `Perc.` = final_hittrax[[percentiles[i]]][1],
      check.names = FALSE
    ))
  }
  
  hittrax_table <- hittrax_datatable %>% 
    gt(rowname_col = "Metric") %>%
    cols_align(
      align = "center",
      columns = everything()
    ) %>%
    tab_options(
      column_labels.font.weight = "bold",
      table.background.color = "black",
      table.font.color = "white"
    ) %>%
    opt_table_lines() %>%
    tab_style(
      style = cell_text(size = px(18), color = "#3d9be9"),
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = cell_text(size = px(18), weight = "bold"),
      locations = cells_stub()
    ) %>%
    data_color(
      columns = `Perc.`,
      palette = c("red", "yellow", "green"),
      domain = c(0,100)
    ) %>% 
    cols_width(
      Metric ~ px(90),
      Value ~ px(85),
      `Facility Rank` ~ px(120),
      `Team Rank` ~ px(120),
      `Perc.` ~ px(85)
    ) %>% 
    tab_options(
      table.border.top.style = "hidden",
      heading.border.lr.style = "hidden",
      heading.border.bottom.style = "hidden",
      column_labels.vlines.style = "hidden"
    )
  
  gtsave(hittrax_table, file = paste0("Futures Reports Images/ ",athlete, "- hittraxSummary.png"), vwidth = 1200, expand = 0)
  
  playerSummary2 <- image_read(paste0("Futures Reports Images/ ",athlete,"- hittraxSummary.png"))
  playerSummary2 <- playerSummary2 %>% 
    image_transparent(color = "black")
  teamReport2 <- image_composite(teamReport1,playerSummary2,offset= "+250+800")

  #Infield Release Speed
  final_teamInfo <- teamInfo %>% 
    filter(`Athlete First & Last Name` == athlete)
  
  relSpeed <- mean(final_teamInfo$RelSpeed, na.rm = TRUE)
  
  relSpeed_plot <- ggplot() +
    geom_text(aes(x = 0.5, y = 0.5, label = paste(round(relSpeed, digits = 1))),
              size = 15, color = "white", family = "Good Times") +
    theme_void() +
    theme(plot.margin = margin(1, 1, 1, 1, "cm"))
  
  ggsave(relSpeed_plot,file=paste0("Futures Reports Images/", athlete," - relSpeed.png"),width=5,height=5.15,units="in", dpi = 150)
  playerSummary3<- image_read(paste0("Futures Reports Images/", athlete," - relSpeed.png"))
  teamReport3 <- image_composite(teamReport2,playerSummary3,offset= "+1300+550")
  
  
  #Shoulder balance
  final_armcare <- filtered_armcare %>% 
    filter(Name == athlete)
  
  shoulderBalanceAVG <- mean(final_armcare$`Shoulder Balance`, na.rm = TRUE)
  
  shoulderBalance_plot <- ggplot() +
    geom_text(aes(x = 0.5, y = 0.5, label = paste(round(shoulderBalanceAVG, digits = 2))),
              size = 15, color = "white", family = "Good Times") +
    theme_void() +
    theme(plot.margin = margin(1, 1, 1, 1, "cm"))
  
  ggsave(shoulderBalance_plot,file=paste0("Futures Reports Images/", athlete," - shoulderBalance.png"),width=5,height=5.15,units="in", dpi = 150)
  playerSummary4<- image_read(paste0("Futures Reports Images/", athlete," - shoulderBalance.png"))
  teamReport4 <- image_composite(teamReport3,playerSummary4,offset= "+1850+550")
  
  #Speed Section
  final_speed_data <- combined_speed %>% 
    filter(Name == athlete)

  final_speed_data <- final_speed_data %>%
    pivot_longer(cols = c(Cumulative3, Split1, Cumulative2, MPH),
                 names_to = "Metric_Type",
                 values_to = "Value") %>%
    filter((`Exercise Name` == "Acceleration" & Metric_Type == "Split1") |
             (`Exercise Name` == "Hard 90" & Metric_Type == "Cumulative2") |
             (`Exercise Name` == "40 Yard Dash" & Metric_Type == "Cumulative3") |
             (`Exercise Name` == "Max Velocity" & Metric_Type == "MPH")) %>%
    mutate(`Exercise Name` = case_when(
      `Exercise Name` == "Acceleration" ~ "10 Yard Split (sec)",
      `Exercise Name` == "Hard 90" ~ "20 Yard Split (sec)",
      `Exercise Name` == "40 Yard Dash" ~ "40 Yard Split (sec)",
      `Exercise Name` == "Max Velocity" ~ "Max Velocity (MPH)",
      TRUE ~ `Exercise Name`
    )) %>%
    mutate(`Facility Rank` = paste(Facility_Rank, "/", Total_Players_Facility, sep = ""),
           `Team Rank` = paste(Team_Rank, "/", Total_Players_Team, sep = "")) %>%
    select(`Exercise Name`, Value, `Facility Rank`, `Team Rank`, PercentileRank) %>%
    arrange((`Exercise Name`)) %>% 
    rename(`Perc.` = PercentileRank)
  
  speed_table <- final_speed_data %>% 
    gt(rowname_col = "Exercise Name") %>%
    cols_align(
      align = "center",
      columns = everything()
    ) %>%
    tab_options(
      column_labels.font.weight = "bold",
      table.background.color = "black",
      table.font.color = "white"
    ) %>%
    opt_table_lines() %>%
    tab_style(
      style = cell_text(size = px(18), color = "#3d9be9"),
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = cell_text(size = px(18), weight = "bold"),
      locations = cells_stub()
    ) %>%
    data_color(
      columns = `Perc.`,
      palette = c("red", "yellow", "green"),
      domain = c(0,100)
    ) %>% 
    cols_width(
      `Exercise Name` ~ px(130),
      Value ~ px(85),
      `Facility Rank` ~ px(120),
      `Team Rank` ~ px(120),
      `Perc.` ~ px(85)
    ) %>% 
    tab_options(
      table.border.top.style = "hidden",
      heading.border.lr.style = "hidden",
      heading.border.bottom.style = "hidden",
      column_labels.vlines.style = "hidden"
    )
  
  gtsave(speed_table, file = paste0("Futures Reports Images/ ",athlete, "- speedSummary.png"), vwidth = 1200, expand = 0)
  
  playerSummary5 <- image_read(paste0("Futures Reports Images/ ",athlete,"- speedSummary.png"))
  playerSummary5 <- playerSummary5 %>% 
    image_transparent(color = "black")
  teamReport5 <- image_composite(teamReport4,playerSummary5,offset= "+1350+2575")
  
  #Proteus Data
  final_proteus <- filtered_proteus %>% 
    filter(Name == athlete)
  
  metrics <- c('Proteus Power (W)', 'Proteus Acceleration (m/s²)')
  originalMetrics <- c('power - high', 'acceleration - high')
  percentiles <- c('PowerPercetileRank', 'AccelerationPercentileRank')
  facility_ranks <- c('Power_Rank_Facility', 'Acceleration_Rank_Facility')
  team_ranks <- c('Power_Rank_Team', 'Acceleration_Rank_Team')
  
  proteus_datatable <- data.frame(Metric = character(), 
                          Value = numeric(), 
                          `Facility Rank` = character(),
                          `Team Rank` = character(),
                          `Perc.` = numeric(), 
                          stringsAsFactors = FALSE,
                          check.names = FALSE)
  
  total_facility_proteus <- final_proteus$Total_Players_Facility[1]
  total_team_proteus <- final_proteus$Total_Players_Team[1]
  
  for (i in 1:length(metrics)) {
    facility_rank <- paste(final_proteus[[facility_ranks[i]]][1], "/", total_facility_proteus, sep="")
    team_rank <- paste(final_proteus[[team_ranks[i]]][1], "/", total_team_proteus, sep="")
    proteus_datatable <- rbind(proteus_datatable, data.frame(
      Metric = metrics[i],
      Value = final_proteus[[originalMetrics[i]]][1],
      `Facility Rank` = facility_rank,
      `Team Rank` = team_rank,
      `Perc.` = final_proteus[[percentiles[i]]][1],
      check.names = FALSE
    ))
  }
  
  proteus_table <- proteus_datatable %>% 
    gt(rowname_col = "Metric") %>%
    cols_align(
      align = "center",
      columns = everything()
    ) %>%
    tab_options(
      column_labels.font.weight = "bold",
      table.background.color = "black",
      table.font.color = "white"
    ) %>%
    opt_table_lines() %>%
    tab_style(
      style = cell_text(size = px(18), color = "#3d9be9"),
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = cell_text(size = px(18), weight = "bold"),
      locations = cells_stub()
    ) %>%
    data_color(
      columns = `Perc.`,
      palette = c("red", "yellow", "green"),
      domain = c(0,100)
    ) %>% 
    cols_width(
      Metric ~ px(125),
      Value ~ px(110),
      `Facility Rank` ~ px(120),
      `Team Rank` ~ px(120),
      `Perc.` ~ px(85)
    ) %>% 
    tab_options(
      table.border.top.style = "hidden",
      heading.border.lr.style = "hidden",
      heading.border.bottom.style = "hidden",
      column_labels.vlines.style = "hidden"
    )
  
  gtsave(proteus_table, file = paste0("Futures Reports Images/ ",athlete, "- proteusSummary.png"), vwidth = 1200, expand = 0)
  
  playerSummary6 <- image_read(paste0("Futures Reports Images/ ",athlete,"- proteusSummary.png"))
  playerSummary6 <- playerSummary6 %>% 
    image_transparent(color = "black")
  teamReport6 <- image_composite(teamReport5,playerSummary6,offset= "+105+2550")
  
  #Squat Jump Data
  final_SJ_data <- filtered_SJ %>% 
    filter(Name == athlete) %>% 
    mutate(
      Test = "Squat Jump (N)",
      `Facility Rank` = paste(SJ_Facility_Rank, "/", Total_Players_Facility),
      `Team Rank` = paste(SJ_Team_Rank, "/", Total_Players_Team)) %>%
    ungroup() %>% 
    select(Test, Value, `Facility Rank`, `Team Rank`, `Perc.`)
  
  if (nrow(final_SJ_data) == 0) {
    noData_plot <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No Squat Jump Data", size = 10, color = "#3d9be9", hjust = 0.5, vjust = 0.5) +
      theme_void() +
      theme(
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black")
      )
    
    ggsave(noData_plot,file=paste0("Futures Reports Images/", athlete," - emptyPlot.png"), width=5,height=1,units="in", dpi = 150)
    playerSummary7 <- image_read(paste0("Futures Reports Images/", athlete ," - emptyPlot.png"))
    teamReport7 <- image_composite(teamReport8,playerSummary9, offset= "+150+2950")
    
  } else {
    
  SJ_datatable <- final_SJ_data %>% 
    gt(rowname_col = "Test") %>%
    cols_align(
      align = "center",
      columns = everything()
    ) %>%
    tab_options(
      column_labels.font.weight = "bold",
      table.background.color = "black",
      table.font.color = "white"
    ) %>%
    opt_table_lines() %>%
    tab_style(
      style = cell_text(size = px(18), color = "#3d9be9"),
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = cell_text(size = px(18), weight = "bold"),
      locations = cells_stub()
    ) %>%
    data_color(
      columns = `Perc.`,
      palette = c("red", "yellow", "green"),
      domain = c(0,100)
    ) %>% 
    cols_width(
      Test ~ px(125),
      Value ~ px(110),
      `Facility Rank` ~ px(120),
      `Team Rank` ~ px(120),
      `Perc.` ~ px(85)
    ) %>% 
    tab_options(
      table.border.top.style = "hidden",
      heading.border.lr.style = "hidden",
      heading.border.bottom.style = "hidden",
      column_labels.vlines.style = "hidden"
    )
  
  gtsave(SJ_datatable, file = paste0("Futures Reports Images/ ",athlete, "- SJsummary.png"), vwidth = 1200, expand = 0)
  
  playerSummary7 <- image_read(paste0("Futures Reports Images/ ",athlete,"- SJsummary.png"))
  playerSummary7 <- playerSummary7 %>% 
    image_transparent(color = "black")
  teamReport7 <- image_composite(teamReport6,playerSummary7,offset= "+105+2950")
  }
  
  #Blast Section
  filteredBlast <- blastData %>%
    filter(Name == athlete)
  
  player_data <- filteredBlast %>%
    filter(`Swing Details` %in% c("Tee", "Front Toss Underhand", "Pitching Machine")) %>%
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
  current_level <- "L3"
  current_gender_blast <- "Female"
  
  goals_data$`Swing Details` <- "Goals"
  
  if(current_gender_blast == "Male" && current_level %in% c("Professional", "Collegiate", "L1", "L2", "L3")) {
    if(current_level %in% c("Collegiate", "Professional")) {
      goals_data$`Bat Speed (mph)` <- "75"
      goals_data$`Rotational Acceleration (g)` <- "16"
      goals_data$`On Plane Efficiency (%)` <- "85"
      goals_data$`Power (kW)` <- "6.0"
      goals_data$`Attack Angle (deg)` <- "6 - 10"
      goals_data$`Early Connection (deg)` <- "85 - 105"
      goals_data$`Connection at Impact (deg)` <- "90 - 95"
      goals_data$`Vertical Bat Angle (deg)` <- "-27 to -37"
    } else if(current_level == "L3") {
      goals_data$`Bat Speed (mph)` <- "65"
      goals_data$`Rotational Acceleration (g)` <- "13"
      goals_data$`On Plane Efficiency (%)` <- "80"
      goals_data$`Power (kW)` <- "4.5"
      goals_data$`Attack Angle (deg)` <- "8 - 12"
      goals_data$`Early Connection (deg)` <- "85 - 105"
      goals_data$`Connection at Impact (deg)` <- "90 - 95"
      goals_data$`Vertical Bat Angle (deg)` <- "-27 to -37"
    } else if(current_level == "L2") {
      goals_data$`Bat Speed (mph)` <- "55"
      goals_data$`Rotational Acceleration (g)` <- "10"
      goals_data$`On Plane Efficiency (%)` <- "70"
      goals_data$`Power (kW)` <- "2.75"
      goals_data$`Attack Angle (deg)` <- "8 - 12"
      goals_data$`Early Connection (deg)` <- "85 - 110"
      goals_data$`Connection at Impact (deg)` <- "85 - 95"
      goals_data$`Vertical Bat Angle (deg)` <- "-20 to -30"
    } else if(current_level == "L1") {
      goals_data$`Bat Speed (mph)` <- "45"
      goals_data$`Rotational Acceleration (g)` <- "7.5"
      goals_data$`On Plane Efficiency (%)` <- "60"
      goals_data$`Power (kW)` <- "1.0"
      goals_data$`Attack Angle (deg)` <- "5 - 15"
      goals_data$`Early Connection (deg)` <- "80 - 110"
      goals_data$`Connection at Impact (deg)` <- "80 - 100"
      goals_data$`Vertical Bat Angle (deg)` <- "-15 to -25"
    }
  } else if(current_gender_blast == "Female" && current_level %in% c("Collegiate", "L1", "L2", "L3")) {
    if(current_level %in% c("Collegiate", "Professional")) {
      goals_data$`Bat Speed (mph)` <- "75"
      goals_data$`Rotational Acceleration (g)` <- "16"
      goals_data$`On Plane Efficiency (%)` <- "85"
      goals_data$`Power (kW)` <- "6.0"
      goals_data$`Attack Angle (deg)` <- "6 - 10"
      goals_data$`Early Connection (deg)` <- "85 - 105"
      goals_data$`Connection at Impact (deg)` <- "90 - 95"
      goals_data$`Vertical Bat Angle (deg)` <- "-27 to -37"
    } else if(current_level == "L3") {
      goals_data$`Bat Speed (mph)` <- "65"
      goals_data$`Rotational Acceleration (g)` <- "12"
      goals_data$`On Plane Efficiency (%)` <- "80"
      goals_data$`Power (kW)` <- "3.5"
      goals_data$`Attack Angle (deg)` <- "6 - 10"
      goals_data$`Early Connection (deg)` <- "85 - 105"
      goals_data$`Connection at Impact (deg)` <- "90 - 95"
      goals_data$`Vertical Bat Angle (deg)` <- "-27 to -37"
    } else if(current_level == "L2") {
      goals_data$`Bat Speed (mph)` <- "55"
      goals_data$`Rotational Acceleration (g)` <- "10"
      goals_data$`On Plane Efficiency (%)` <- "70"
      goals_data$`Power (kW)` <- "1.75"
      goals_data$`Attack Angle (deg)` <- "6 - 10"
      goals_data$`Early Connection (deg)` <- "85 - 110"
      goals_data$`Connection at Impact (deg)` <- "85 - 95"
      goals_data$`Vertical Bat Angle (deg)` <- "-20 to -30"
    } else if(current_level == "L1") {
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
  
  if (!is.null(player_data) && ncol(player_data) > 0) {
    in_game_data <- as.data.frame(matrix(NA, ncol = ncol(player_data), nrow = 1))
    colnames(in_game_data) <- colnames(player_data)
    in_game_data$`Swing Details` <- "In Game"
    
    combined_player_data <- rbind(player_data, in_game_data, goals_data)
  } else {
    combined_player_data <- rbind(player_data, goals_data)
  }
  
  blast_table <- combined_player_data %>%
    slice(match(swingOrder, `Swing Details`)) %>%
    gt(rowname_col = "Swing Details") %>%
    cols_align(
      align = "center",
      columns = everything()
    ) %>%
    tab_options(
      column_labels.font.weight = "bold",
      table.background.color = "black",
      table.font.color = "white"
    ) %>%
    opt_table_lines() %>%
    tab_style(
      style = cell_text(color = "#3d9be9"),
      locations = cells_column_labels()
    ) %>%
    tab_spanner(
      label = "Progressive",
      columns = c("Bat Speed (mph)", "Rotational Acceleration (g)", "Power (kW)", "On Plane Efficiency (%)")
    ) %>%
    tab_spanner(
      label = "Range-based",
      columns = c("Attack Angle (deg)", "Early Connection (deg)", "Connection at Impact (deg)", "Vertical Bat Angle (deg)")
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_stub()
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
          columns = everything(),
          rows = "Goals"
        )
      )
    ) %>%
    tab_options(
      table.border.top.style = "hidden",
      table.border.left.style = "solid",
      table.border.right.style = "solid",
      column_labels.border.lr.style = "hidden",
      column_labels.vlines.style = "hidden",
      heading.border.lr.style = "hidden",
      heading.border.bottom.style = "hidden",
      table_body.vlines.style = "solid"
    ) %>%
    sub_missing(
      columns = everything()
    )
  
  gtsave(blast_table, file = paste0("Futures Reports Images/ ",athlete, "- blastSummary.png"), vwidth = 1200, vheight = 500, expand = 0)
  
  playerSummary8 <- image_read(paste0("Futures Reports Images/ ",athlete,"- blastSummary.png"))
  playerSummary8 <- playerSummary8 %>%
    image_transparent(color = "black")
  teamReport8 <- image_composite(teamReport7,playerSummary8,offset= "+93+1325")
  
  #Trackman Section
  filteredTrackmanData <- trackmanData %>% 
    filter(Pitcher == "Addison Wong")
  
  if (nrow(filteredTrackmanData) == 0) {
    noData_plot <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No Pitching Data", size = 20, color = "#3d9be9", hjust = 0.5, vjust = 0.5) +
      theme_void() +
      theme(
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black")
      )
    
    ggsave(noData_plot,file=paste0("Futures Reports Images/", athlete," - emptyPlot.png"), width=8,height=3,units="in", dpi = 150)
    playerSummary9 <- image_read(paste0("Futures Reports Images/", athlete ," - emptyPlot.png"))
    teamReport9 <- image_composite(teamReport8,playerSummary9, offset= "+700+1885")
    
  } else {
  total_filteredTrackmanData <- nrow(filteredTrackmanData)
  
  pitchStats <- filteredTrackmanData %>%
    group_by(TaggedPitchType) %>%
    summarize(`#` = n(),
              `%` = format(round((`#` / total_filteredTrackmanData) * 100, 1)),
              AvgVelo = format(round(mean(RelSpeed, na.rm = TRUE), 1)),
              MaxVelo = format(round(max(RelSpeed, na.rm = TRUE), 1)),
              AvgIVB = format(round(mean(InducedVertBreak, na.rm = TRUE), 1)),
              AvgHB = format(round(mean(HorzBreak, na.rm = TRUE), 1)),
              AvgSpinRate = format(round(mean(SpinRate, na.rm = TRUE), 0)),
              Height = format(round(mean(RelHeight, na.rm = TRUE), 1)),
              Side = format(round(mean(RelSide, na.rm = TRUE), 1))) %>% 
    arrange(desc(AvgVelo)) %>% 
    select(-`#`)
  colnames(pitchStats) <- c('Pitch Type', "%", 'Avg Velo (mph)', 'Max Velo (mph)', 'IVB (in)', 'HB (in)', 'Spin (rpm)', "Height (in)", "Side (in)") 
  
  pitch_table <- pitchStats %>% 
    gt(rowname_col = "Pitch Type") %>% 
    cols_align(
      align = "center",
      columns = everything()
    ) %>%
    tab_options(
      column_labels.font.weight = "bold",
      table.background.color = "black",
      table.font.color = "white"
    ) %>%
    tab_spanner(
      label = "Metrics",
      columns = c('Avg Velo (mph)', 'Max Velo (mph)', 'IVB (in)', 'HB (in)', 'Spin (rpm)')
    ) %>% 
    tab_spanner(
      label = "Release",
      columns = c("Height (in)", "Side (in)")
    ) %>% 
    tab_style(
      style = cell_text(size = px(18), color = "#3d9be9"),
      locations = cells_column_labels()
    ) %>% 
    tab_style(
      style = cell_text(size = px(18), weight = "bold"),
      locations = cells_stub()
    ) %>%
    tab_options(
      table.border.top.style = "hidden",
      table.border.left.style = "solid",
      table.border.right.style = "solid",
      column_labels.border.lr.style = "hidden",
      heading.border.lr.style = "hidden",
      heading.border.bottom.style = "hidden",
      table_body.vlines.style = "solid"
    ) %>% 
    cols_width(
      `Pitch Type` ~ px(125),
      `%` ~ px(50),
      everything() ~ px(105)
    )
  
  gtsave(pitch_table, file = paste0("Futures Reports Images/ ",athlete, "- pitchingSummary.png"), vwidth = 1200, expand = 5)
  
  playerSummary9 <- image_read(paste0("Futures Reports Images/ ",athlete,"- pitchingSummary.png"))
  playerSummary9 <- playerSummary9 %>% 
    image_transparent(color = "black") %>% 
    image_trim()
  teamReport9 <- image_composite(teamReport8,playerSummary9,offset= "+365+1885")
  
  }
  
  image_write(teamReport9, path = paste(athlete, "Report.pdf"), format = "pdf", quality = 100, density = 300)
  
}
  