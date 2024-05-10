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

font_add(family = "Good Times", regular = "good times rg.otf")
showtext_auto()

blastData <- read_csv("/Users/watts/Downloads/MasterBlastData.csv") %>% 
  rename(Name = Athlete)

hittraxData <- read_csv("/Volumes/COLE'S DATA/Data/Hittrax Master Data - Sheet1.csv")
hittraxSession <- read.csv("/Users/watts/Documents/Futures Performance Center/Data/Hittrax Data/SessionExport_2024-02-28-08-20-06_UTC.CSV")
hittraxPlays <- read.csv("/Users/watts/Documents/Futures Performance Center/Data/Hittrax Data/PlaysExport_2024-02-28-08-20-06_UTC.CSV")

clientData <- read_csv("/Users/watts/Downloads/FullClientList.csv") %>% 
  rename(Name = Client)

attendanceData <- read_csv("/Users/watts/Downloads/CheckIns.csv") %>%
  rename(Name = Client) %>% 
  mutate(Date = as.Date(Date, format = "%b %d, %Y"),
         Month = month(Date, label = TRUE, abbr = FALSE),
         LearningBlock = case_when(
           `Service Name` == "Learning Academy - Block 1" ~ "Learning Academy - Attended",
           `Service Name` == "Learning Academy - Block 2" ~ "Learning Academy - Attended",
           TRUE ~ `Service Name`
         )) %>%
  filter(LearningBlock %in% c("Baseball Cage Rental L1", "Baseball Cage Rental L2", "Baseball Cage Rental L3", 
                              "Baseball Hitting L1", "Baseball Hitting L2", "Baseball Hitting L3", 
                              "Softball Cage Rental L1", "Softball Cage Rental L2", "Softball Cage Rental L3", 
                              "Softball Hitting L1", "Softball Hitting L2", "Softball Hitting L3", 
                              "Professional - Facility Access", "Learning Academy - Attended")) %>% 
  distinct(Name, Date, LearningBlock, .keep_all = TRUE)

summary_attendanceData <- attendanceData %>%
  filter(Month == "April") %>% 
  group_by(Name) %>%
  summarise(Attendance = n(),
            .groups = 'drop')

clientNames <- clientData$Name

#  # Compare and extract non-matching names
#  nonMatchingBlast <- setdiff(blastData$Name, clientNames)
#  nonMatchingHittrax <- setdiff(hittraxData$Name, clientNames)
# 
#  # Combine non-matching names into one data frame
#  nonMatching <- rbind(
#    data.frame(Name = nonMatchingBlast, Source = 'Blast'),
#    data.frame(Name = nonMatchingHittrax, Source = 'Hittrax')
#  )
# 
# # Write the non-matching names to a new CSV file
#  write_csv(nonMatching, "/Users/watts/Downloads/missing_hitting_data.csv")

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

blastData$Date <- as.Date(blastData$Date, format = "%m/%d/%y")
blastData$Month <- format(blastData$Date, "%B")
blastData <- left_join(blastData, clientData, by = "Name")

blastData <- blastData %>%
  group_by(Name, `Swing Details`) %>%
  mutate(MedianBatSpeed = median(`Bat Speed (mph)`, na.rm = TRUE)) %>%
  filter(`Bat Speed (mph)` >= MedianBatSpeed)

hittraxData$`Date of Birth` <- as.Date(hittraxData$`Date of Birth`, format = "%B %d %Y")
hittraxData <- left_join(hittraxData, clientData, by = "Name")

# Calculate Monthly and Year-to-Date Changes
hittraxData <- hittraxData %>%
  mutate(Date = make_date(Year, match(Month, month.name), 1)) %>%
  arrange(Name, Date) %>%
  group_by(Name) %>%
  mutate(
    MaxVel_CumMax = cummax(MaxVel),
    MaxDist_CumMax = cummax(MaxDist),
    MaxVelRank_CumMax = cummax(`MaxVel Rank`),
    MaxDistRank_CumMax = cummax(`MaxDist Rank`),
  ) %>%
  mutate(
    MaxVel_Monthly_Change = round(pmax(MaxVel_CumMax - lag(MaxVel_CumMax, order_by = Date, default = first(MaxVel_CumMax)), 0), 1),
    MaxVel_YTD_Change = round(cumsum(coalesce(pmax(MaxVel_CumMax - lag(MaxVel_CumMax, order_by = Date, default = first(MaxVel_CumMax)), 0), 0)), 1),
    
    AvgVel_Monthly_Change = round(AvgVel - lag(AvgVel, order_by = Date), 1),
    AvgVel_YTD_Change = round(cumsum(coalesce(AvgVel - lag(AvgVel, order_by = Date), 0)), 1),
    
    MaxDist_Monthly_Change = round(pmax(MaxDist_CumMax - lag(MaxDist_CumMax, order_by = Date, default = first(MaxDist_CumMax)), 0), 1),
    MaxDist_YTD_Change = round(cumsum(coalesce(pmax(MaxDist_CumMax - lag(MaxDist_CumMax, order_by = Date, default = first(MaxDist_CumMax)), 0), 0)), 1),
    
    AvgDist_Monthly_Change = round(AvgDist - lag(AvgDist, order_by = Date), 1),
    AvgDist_YTD_Change = round(cumsum(coalesce(AvgDist - lag(AvgDist, order_by = Date), 0)), 1)
  ) %>%
  ungroup() %>%
  group_by(`Skill Development Training/Booking Level`, Gender, Date) %>%
  mutate(
    MaxVel_Rank = rank(-MaxVel_CumMax, ties.method = "min"),
    AvgVel_Rank = rank(-AvgVel, ties.method = "min"),
    MaxDist_Rank = rank(-MaxDist_CumMax, ties.method = "min"),
    AvgDist_Rank = rank(-AvgDist, ties.method = "min"),
    Total_Players = n()
  ) %>%
  ungroup()

session_names <- hittraxSession %>%
  select(UsId, UserName) %>%
  distinct()

# Left join to keep all records from PlaysExport and add UserName where available
combined_data <- hittraxPlays %>%
  left_join(session_names, by = "UsId")

cleaned_data <- combined_data %>%
  distinct(Id, .keep_all = TRUE)

cleaned_data <- cleaned_data %>%
  mutate(
    EBV1 = EBV1 * 2.23694,
    EBV2 = EBV2 * 2.23694,
    EBV3 = EBV3 * 2.23694,
    Intersect1 = Intersect1 * 39.3701,
    Intersect2 = Intersect2 * 39.3701,
    Intersect3 = Intersect3 * 39.3701,
    PE1 = PE1 * 3.28084,
    PE2 = PE2 * 3.28084,
    PE3 = PE3 * 3.28084
  )

# Assuming your dataframe is named 'data'
cleaned_data <- cleaned_data %>%
  mutate(HitType = case_when(
    HT == 1 ~ "Ground Ball",
    HT == 2 ~ "Line Drive",
    HT == 3 ~ "Fly Ball",
    HT == 4 ~ "Baltimore Chop",
    HT == 5 ~ "Catcher's Throw",
    TRUE ~ NA_character_  # Handles any other case not defined
  ))

# Assuming your dataframe is named 'data'
cleaned_data <- cleaned_data %>%
  mutate(Outcome = case_when(
    Res == -6 ~ "Wild pitch",
    Res == -5 ~ "Swing and miss",
    Res == -3 ~ "Foul ball",
    Res == -2 ~ "Hit by pitch",
    Res == -8 ~ "Line drive out, caught in the air",
    Res == -1 ~ "Fly ball out",
    Res == 0  ~ "Out",
    Res == 1  ~ "Single",
    Res == 2  ~ "Double",
    Res == 3  ~ "Triple",
    Res == 4  ~ "Home Run",
    Res == 5  ~ "Caught Stealing",
    TRUE ~ NA_character_  # Handles any other case not defined
  ))

col_grid <- rgb(235, 235, 235, 25, maxColorValue = 255)

athletes <- unique(hittraxData$Name)

TemplatePageOne <- image_read_pdf("/Volumes/COLE'S DATA/Templates/Hitting Report Template.pdf")
IndexPage <- image_read_pdf("/Volumes/COLE'S DATA/Templates/Hitting Report Metric Index.pdf")

swingOrder <- c("Tee", "Front Toss Underhand", "Pitching Machine", "In Game", "Goals")
 
blastColors <- c("Tee"="#FF0000", "Front Toss Underhand"="#AD0AFD", "Pitching Machine"= "#3d9be9", "In Game"="#FFA500")
sprayChart_colors <- c("LDP" = "#FF0000", "FBP" = "#3d9be9", "GBP" = "#FFA500")

# Set the working directory
setwd("/Users/watts/Documents/Futures Performance Center/Test")

for (athlete in athletes){
  
  filteredHittrax <- hittraxData %>% 
    filter(Month == "April" & Year == "2024" & Name == athlete)
  
  # Fetch the group of the current player
  current_level <- unique(filteredHittrax$`Skill Development Training/Booking Level`)[1]
  current_gender <- unique(filteredHittrax$Gender)[1]
  
  attendance_plot_data <- summary_attendanceData %>%
    filter(Name == athlete) %>% 
    mutate(`Total Weeks` = 4, # Adjust this number based on the exact number of weeks in the 2-month period
           `Attendance Score` = round(Attendance / `Total Weeks`, digits = 1))
  
  attendance_score <- max(attendance_plot_data$`Attendance Score`)
  
  if (attendance_score == -Inf || is.na(attendance_score) || attendance_score <= 0.1) {
    next
  }
  
  athlete_folder <- paste0("Futures Reports/", athlete)
  if (!dir.exists(athlete_folder)) {
    dir.create(athlete_folder, recursive = TRUE)
  }
  
  # Revised get_color function to handle vector inputs
  get_color <- function(scores) {
    sapply(scores, function(score) {
      if (is.na(score)) {
        return(NA)
      } else if (score < 1.5) {
        return("#FF0000")
      } else if (score >= 1.5 & score < 2.5) {
        return("#FFA500")
      } else if (score >= 2.5 & score < 3.5) {
        return("green")
      } else {
        return("#3d9be9")
      }
    })
  }
  
  # Plotting
  attendance_plot <- ggplot(attendance_plot_data, aes(x = Name, y = `Attendance Score`)) +
    geom_col(aes(fill = get_color(`Attendance Score`))) +
    geom_col(aes(y = 4), alpha = 0.5, color = "black") +
    geom_text(aes(y = 2, label = paste(attendance_score)), size = 12, fontface = "bold", color = "white") +
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
  
  ggsave(attendance_plot,file=paste0("Futures Reports Images/",athlete,"_attendancePlot.png"), width=2.65,height=0.65,units="in", dpi = 175)
  attendancePlot <- image_read(paste0("Futures Reports Images/",athlete,"_attendancePlot.png"))
  PitchingReport1 <- image_composite(TemplatePageOne, attendancePlot, offset= "+1965+575")
  
  player_profile <- clientData %>%
    filter(Name == athlete) %>%
    mutate(Attendance = NA, HT_WT = paste(Height, " / ", Weight)) %>% 
    select(Name, Age, `Skill Development Training/Booking Level`, `Position (Baseball/Softball)`, Gpa, HT_WT, `High School`, `Graduating Class`, Attendance)
  names(player_profile) <- c("Name:", "Age:", "Level:", "Position:", "GPA:", "HT / WT:", "School:", "Class:", "Attendance:")
  
  player_profile <- player_profile %>% 
    mutate(across(c(`Name:`, `Age:`, `Level:`, `Position:`, `School:`, `Class:`, `GPA:`, `HT / WT:`, `Attendance:`), as.character)) %>%
    pivot_longer(cols = c(`Name:`, `Age:`, `Level:`, `Position:`, `School:`, `Class:`, `GPA:`, `HT / WT:`, `Attendance:`), names_to = "label", values_to = "value")
  
  # Define the coordinates for the labels and values
  left_labels <- c("Name:", "Age:", "Level:")
  middle_labels <- c("School:", "Class:", "GPA:")
  right_labels <- c("HT / WT:", "Position:", "Attendance:") 
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
               hjust = 0, color = "#3d9be9", size = 8, family = "Good Times") +
      annotate("text", x = left_x + 0.05, y = y_positions[i], label = player_profile$value[player_profile$label == left_labels[i]], 
               hjust = 0, color = "white", size = 10, family = "Good Times")
  }
  
  # Add middle labels and their values
  for (i in 1:length(middle_labels)) {
    p <- p + 
      annotate("text", x = middle_x, y = y_positions[i], label = middle_labels[i], 
               hjust = 0, color = "#3d9be9", size = 8, family = "Good Times") +
      annotate("text", x = middle_x + 0.07, y = y_positions[i], label = player_profile$value[player_profile$label == middle_labels[i]], 
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
  
  ggsave(p,file=paste0("Futures Reports Images/",athlete," - playerProfile.png"), width=16,height=2.25,units="in", dpi = 150)
  playerSummary1 <- image_read(paste0("Futures Reports Images/",athlete," - playerProfile.png"))
  PitchingReport2 <- image_composite(PitchingReport1, playerSummary1, offset= "+50+465")
  
  
  plot_hitting_metrics <- function(metrics) {
    plot_data <- hittraxData %>%
      filter(Name == athlete) %>%
      mutate(Month = factor(Month, levels = month.name),
             YearMonth = make_date(Year, match(Month, month.name), 1)) %>%
      select(Name, YearMonth, !!rlang::sym(metrics))
    
    # Plotting
    ggplot(plot_data, aes(x = YearMonth, y = !!rlang::sym(metrics), group = 1)) +
      geom_line(linewidth = 8, color = "#3d9be9") +
      geom_point(size = 12, color = "#3d9be9") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      theme_void() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  metrics <- c('Max EV', 'Avg EV', 'Max Dist', 'Avg Dist')
  originalMetrics <- c('MaxVel_CumMax', 'AvgVel', 'MaxDist_CumMax', 'AvgDist')
  ranks <- c('MaxVelRank_CumMax', 'AvgVel Rank', 'MaxDistRank_CumMax', 'AvgDist Rank')
  facility_ranks <- c('MaxVel_Rank', 'AvgVel_Rank', 'MaxDist_Rank', 'AvgDist_Rank')
  monthly_changes <- c('MaxVel_Monthly_Change', 'AvgVel_Monthly_Change', 
                       'MaxDist_Monthly_Change', 'AvgDist_Monthly_Change')
  ytd_changes <- c('MaxVel_YTD_Change', 'AvgVel_YTD_Change', 
                   'MaxDist_YTD_Change', 'AvgDist_YTD_Change')
  
  # Creating a new dataframe
  datatable <- data.frame(Metric = character(), 
                          Value = numeric(), 
                          `Monthly` = numeric(), 
                          `YTD` = numeric(),
                          `Rank` = character(),
                          `Perc.` = numeric(), 
                          stringsAsFactors = FALSE,
                          check.names = FALSE)
  
  # Total players
  total_players <- filteredHittrax$Total_Players[1]
  
  # Populating the dataframe
  for (i in 1:length(metrics)) {
    level_rank <- paste(filteredHittrax[[facility_ranks[i]]][1], "/", total_players, sep="")
    datatable <- rbind(datatable, data.frame(
      Metric = metrics[i],
      Value = filteredHittrax[[originalMetrics[i]]][1],
      `Monthly` = filteredHittrax[[monthly_changes[i]]][1],
      `YTD` = filteredHittrax[[ytd_changes[i]]][1],
      `Rank` = level_rank,
      `Perc.` = filteredHittrax[[ranks[i]]][1],
      check.names = FALSE
    ))
  }
  
  arrow_icon <- function(value) {
    if (value > 0.01) {
      return(html(paste("<span style='color:green;'>&#9650;</span>", value)))
    } else if (value < 0) {
      return(html(paste("<span style='color:red;'>&#9660;</span>", value)))
    } else {
      return(html("<span>&#8212;</span>"))
    }
  }
  
  hittrax_table <- datatable %>% 
    mutate(Trend = c("MaxVel", "AvgVel", "MaxDist", "AvgDist")) %>% 
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
    tab_style(
      style = cell_text(color = "red"),
      locations = cells_body(columns = `Monthly`,
                             rows = `Monthly` < 0)
    ) %>% 
    tab_style(
      style = cell_text(color = "green4"),
      locations = cells_body(columns = `Monthly`,
                             rows = `Monthly` > 0)
    ) %>% 
    tab_style(
      style = cell_text(color = "red"),
      locations = cells_body(columns = `YTD`,
                             rows = `YTD` < 0)
    ) %>% 
    tab_style(
      style = cell_text(color = "green4"),
      locations = cells_body(columns = `YTD`,
                             rows = `YTD` > 0)
    ) %>% 
    text_transform(
      locations = cells_body(columns = `Monthly`),
      fn = function(x) {
        map_chr(x, ~arrow_icon(.x))
      }
    ) %>%
    text_transform(
      locations = cells_body(columns = `YTD`),
      fn = function(x) {
        map_chr(x, ~arrow_icon(.x))
      }
    ) %>%
    data_color(
      columns = `Perc.`,
      palette = c("red", "yellow", "green"),
      domain = c(0,100)
    ) %>% 
    cols_width(
      Metric ~ px(90),
      Value ~ px(85),
      `Rank` ~ px(85),
      `Monthly` ~ px(85),
      `YTD` ~ px(85),
      `Perc.` ~ px(85)
    ) %>%
    text_transform(
      locations = cells_body(columns = "Trend"),
      fn = function(column){
        map(column, plot_hitting_metrics) %>% 
          ggplot_image(height = px(50), aspect_ratio = 3)
      }
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
  PitchingReport3 <- image_composite(PitchingReport2,playerSummary2,offset= "+100+840")

##################################################################################################################
##################################################################################################################
##################################################################################################################
  
  filteredBlast <- blastData %>% 
    filter(Name == athlete)
  
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
  current_level <- unique(filteredBlast$`Skill Development Training/Booking Level`)[1]
  current_gender_blast <- unique(filteredBlast$Gender)[1]
  
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
  
  apply_tab_style_if_exists <- function(blast_table, swing_type, color) {
    if (swing_type %in% combined_player_data$`Swing Details`) {
      # Apply background color
      blast_table <- blast_table %>%
        tab_style(
          style = cell_fill(color = color),
          locations = cells_stub(rows = swing_type)
        )

      if (swing_type == "In Game") {
        blast_table <- blast_table %>%
          tab_style(
            style = cell_text(color = "black"),
            locations = cells_stub(rows = swing_type)
          )
      }
    }
    return(blast_table)
  }

  swing_types_colors <- list(
    "Tee" = "#FF0000",
    "Front Toss Underhand" = "#AD0AFD",
    "Pitching Machine" = "#3d9be9",
    "In Game" = "#FFA500"
  )

  for (swing_type in names(swing_types_colors)) {
    blast_table <- apply_tab_style_if_exists(blast_table, swing_type, swing_types_colors[[swing_type]])
  }
  
  gtsave(blast_table, file = paste0("Futures Reports Images/ ",athlete, "- hittingSummary.png"), vwidth = 1200, vheight = 500, expand = 0)
  
  playerSummary3 <- image_read(paste0("Futures Reports Images/ ",athlete,"- hittingSummary.png"))
  playerSummary3 <- playerSummary3 %>% 
    image_transparent(color = "black")
  PitchingReport4 <- image_composite(PitchingReport3,playerSummary3,offset= "+93+1650")
  
  if (!is.null(player_data)) {
    
    red_ranges <- list()
    if (current_level == "L1") {
      red_ranges$bat_speed = list(xmin = 30, xmax = -Inf)
      red_ranges$rot_accel = list(ymin = 2, ymax = -Inf)
      red_ranges$on_plane_eff = list(xmin = 60, xmax = -Inf)
      red_ranges$attack_angle = list(ymin = 5, ymax = -Inf, ymin2 = 18, ymax2 = Inf)
      red_ranges$vertical_angle = list(ymin = -Inf, ymax = 15, ymin2 = 40, ymax2 = Inf)
      red_ranges$early_connection = list(ymin = -Inf, ymax = 80, ymin2 = 110, ymax2 = Inf)
      red_ranges$connection_impact = list(ymin = -Inf, ymax = 75, ymin2 = 100, ymax2 = Inf)
    } else if (current_level == "L2") {
      red_ranges$bat_speed = list(xmin = 40, xmax = -Inf)
      red_ranges$rot_accel = list(ymin = 4, ymax = -Inf)
      red_ranges$on_plane_eff = list(xmin = 60, xmax = -Inf)
      red_ranges$attack_angle = list(ymin = 5, ymax = -Inf, ymin2 = 18, ymax2 = Inf)
      red_ranges$vertical_angle = list(ymin = -Inf, ymax = 15, ymin2 = 45, ymax2 = Inf)
      red_ranges$early_connection = list(ymin = -Inf, ymax = 85, ymin2 = 110, ymax2 = Inf)
      red_ranges$connection_impact = list(ymin = -Inf, ymax = 80, ymin2 = 95, ymax2 = Inf)
    } else if (current_level %in% c("L3", "Collegiate", "Professional")) {
      red_ranges$bat_speed = list(xmin = 50, xmax = -Inf)
      red_ranges$rot_accel = list(ymin = 8, ymax = -Inf)
      red_ranges$on_plane_eff = list(xmin = 60, xmax = -Inf)
      red_ranges$attack_angle = list(ymin = 5, ymax = -Inf, ymin2 = 18, ymax2 = Inf)
      red_ranges$vertical_angle = list(ymin = -Inf, ymax = 22, ymin2 = 42, ymax2 = Inf)
      red_ranges$early_connection = list(ymin = -Inf, ymax = 85, ymin2 = 115, ymax2 = Inf)
      red_ranges$connection_impact = list(ymin = -Inf, ymax = 85, ymin2 = 95, ymax2 = Inf)
    }
    
    if (current_level %in% c("L1")) {
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
      geom_point(aes(x = `Bat Speed (mph)`, y = `Rotational Acceleration (g)`, fill = `Swing Details`), size = 4, shape = 21) +
      scale_fill_manual(values = blastColors,
                        breaks = names(blastColors))
      
    attack_angle_goal <- strsplit(as.character(goals_data$`Attack Angle (deg)`), ' - ')
    attack_angle_min <- as.numeric(attack_angle_goal[[1]][1])
    attack_angle_max <- as.numeric(attack_angle_goal[[1]][2])
    on_plane_eff_goal <- as.numeric(goals_data$`On Plane Efficiency (%)`)
    
    contact_graph <- ggplot(player_data) +
      coord_cartesian(xlim = c(20, 100), ylim = c(-5, 25)) +
      annotate("rect", xmin = on_plane_eff_goal, xmax = Inf, ymin = attack_angle_min, ymax = attack_angle_max, fill = "#00FF00", alpha = 0.25) +
      annotate("rect", xmin = red_ranges$on_plane_eff$xmin, xmax = red_ranges$on_plane_eff$xmax, ymin = red_ranges$attack_angle$ymin, ymax = red_ranges$attack_angle$ymax, fill = "#FF2400", alpha = 0.15) +
      annotate("rect", xmin = red_ranges$on_plane_eff$xmin, xmax = red_ranges$on_plane_eff$xmax, ymin = red_ranges$attack_angle$ymin2, ymax = red_ranges$attack_angle$ymax2, fill = "#FF2400", alpha = 0.15) +
      geom_point(aes(x = `On Plane Efficiency (%)`, y = `Attack Angle (deg)`, fill = `Swing Details`), size = 4, shape = 21) +
      scale_fill_manual(values = blastColors,
                        breaks = names(blastColors))
    
    early_connection_goal <- strsplit(as.character(goals_data$`Early Connection (deg)`), ' - ')
    early_connection_min <- as.numeric(early_connection_goal[[1]][1])
    early_connection_max <- as.numeric(early_connection_goal[[1]][2])
    
    load_graph <- ggplot(player_data) +
      coord_cartesian(xlim = c(0, -60), ylim = c(60, 140)) +
      annotate("rect", xmin = Inf, xmax = -Inf, ymin = early_connection_min, ymax = early_connection_max, fill = "#00FF00", alpha = 0.25) +
      annotate("rect", xmin = Inf, xmax = -Inf, ymin = red_ranges$early_connection$ymin, ymax = red_ranges$early_connection$ymax, fill = "#FF2400", alpha = 0.15) +
      annotate("rect", xmin = Inf, xmax = -Inf, ymin = red_ranges$early_connection$ymin2, ymax = red_ranges$early_connection$ymax2, fill = "#FF2400", alpha = 0.15) +
      geom_point(aes(x = `Vertical Bat Angle (deg)`, y = `Early Connection (deg)`, fill = `Swing Details`), size = 4, shape = 21) +
      scale_fill_manual(values = blastColors,
                        breaks = names(blastColors))
    
    connection_impact_goal <- strsplit(as.character(goals_data$`Connection at Impact (deg)`), ' - ')
    connection_impact_min <- as.numeric(connection_impact_goal[[1]][1])
    connection_impact_max <- as.numeric(connection_impact_goal[[1]][2])
    
    impact_graph <- ggplot(player_data) +
      coord_cartesian(xlim = c(0, -60), ylim = c(60, 110)) +
      annotate("rect", xmin = Inf, xmax = -Inf, ymin = connection_impact_min, ymax = connection_impact_max, fill = "#00FF00", alpha = 0.25) +
      annotate("rect", xmin = Inf, xmax = -Inf, ymin = red_ranges$connection_impact$ymin, ymax = red_ranges$connection_impact$ymax, fill = "#FF2400", alpha = 0.15) +
      annotate("rect", xmin = Inf, xmax = -Inf, ymin = red_ranges$connection_impact$ymin2, ymax = red_ranges$connection_impact$ymax2, fill = "#FF2400", alpha = 0.15) +
      geom_point(aes(x = `Vertical Bat Angle (deg)`, y = `Connection at Impact (deg)`, fill = `Swing Details`), size = 4, shape = 21) +
      scale_fill_manual(values = blastColors,
                        breaks = names(blastColors))
    
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
    pitchCharts1 <- image_read(paste0("Futures Reports Images/",athlete," - swingProfile.png"))
    PitchingReport5 <- image_composite(PitchingReport4,pitchCharts1, offset= "+100+2550")
    
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
    
    combined_empty_plot <- (power_graph | contact_graph | load_graph | impact_graph) +
      plot_layout(guides = "collect", nrow = 1) &
      theme_minimal() +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_line(color = col_grid))
    
    ggsave(combined_empty_plot,file=paste0("Futures Reports Images/",athlete," - swingProfile.png"), width=11,height=3.25,units="in", dpi = 215)
    pitchCharts1 <- image_read(paste0("Futures Reports Images/",athlete," - swingProfile.png"))
    PitchingReport5 <- image_composite(PitchingReport4,pitchCharts1, offset= "+100+2525")
  }
  
  filtered_depth_spray <- cleaned_data %>%
    filter(UserName == athlete, !is.na(Outcome)) %>%
    mutate(ExitVelocityGroup = case_when(
      EBV3 < 40 ~ "<40",
      EBV3 >= 40 & EBV3 < 50 ~ "40-49",
      EBV3 >= 50 & EBV3 < 60 ~ "50-59",
      EBV3 >= 60 & EBV3 < 70 ~ "60-69",
      EBV3 >= 70 & EBV3 < 80 ~ "70-79",
      EBV3 >= 80 & EBV3 < 90 ~ "80-89",
      EBV3 >= 90 ~ ">90"
    ),
    GroupID = as.numeric(factor(ExitVelocityGroup, levels = c("<40", "40-49", "50-59", "60-69", "70-79", "80-89", ">90")))) %>%
    group_by(GroupID) %>%
    summarise(
      Mean_Intersect1= mean(Intersect1, na.rm = TRUE),
      Mean_Intersect3 = mean(Intersect3, na.rm = TRUE)
    )
  
  colors <- c("#00008b", "#4747ab", "#9999d0", "white", "#dcb3b3", "#b25757", "#8b0000")
  breaks <- seq(1, 7, length.out = length(colors))
  
  depth_plot <- filtered_depth_spray %>%
    ggplot(aes(Mean_Intersect1, Mean_Intersect3))+
    scale_x_continuous(limit= c(-20,20))+
    scale_y_continuous(limit= c(0,36), breaks=seq(0,36 ,6), labels = function(x) paste(x, "in")) +
    #geom_text(aes(label = round(ExitSpeed)), size = 3, check_overlap = TRUE, colour = 'black')+
    # home plate outline
    geom_segment(aes(x = -8.5, y = 8.5, xend = -8.5, yend = 17), linewidth = 1, alpha = 1, color = "white")+ # left side
    geom_segment(aes(x = 8.5, y = 8.5, xend = 8.5, yend = 17), linewidth = 1, alpha = 1, color = "white")+ # right side
    geom_segment(aes(x = -8.5, y = 17, xend = 8.5, yend = 17), linewidth = 1, alpha = 1, color = "white")+ # front plate
    geom_segment(aes(x = -8.5, y = 8.5, xend = 0, yend = 0), linewidth = 1, alpha = 1, color = "white")+ # back left
    geom_segment(aes(x = 8.5, y = 8.5, xend = 0, yend = 0), linewidth = 1, alpha = 1, color = "white")+ #back right
    geom_hline(yintercept=seq(0, 36, by = 6), linetype="dashed", color = "#3d9be9", linewidth = 1, alpha = 0.25)+
    geom_point(aes(fill = GroupID), shape = 21, size = 3, alpha = 0.75)+
    scale_fill_gradientn(colours = colors, breaks = breaks, labels = c("<40", "40", "50", "60", "70", "80", "90+"), name = "Exit Velocity") +
    theme_minimal() +
    theme(legend.position="bottom",
          legend.title = element_text(color = "white", size = 13),
          legend.text = element_text(color = "white", size = 7),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(vjust = -1, margin = margin(l = 20, r = -25), color = "white", size = 15),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  ggsave(depth_plot,file=paste0("Futures Reports Images/",athlete," - depthplot.png"), width=3,height=3.25,units="in", dpi = 150)
  pitchCharts2 <- image_read(paste0("Futures Reports Images/",athlete," - depthplot.png"))
  PitchingReport6 <- image_composite(PitchingReport5,pitchCharts2, offset= "+1495+850")
  
  spray_plot <- geom_baseball(league = "mlb", color_updates = list(
    plot_background = "black",
    infield_dirt = "#9b7653",
    infield_grass = "black",
    pitchers_mound = "#9b7653",
    base = "#ffffff",
    pitchers_plate = "#ffffff",
    batters_box = "#ffffff",
    catchers_box = "#ffffff",
    foul_line = "#ffffff",
    running_lane = "#ffffff"
  )) +
    geom_point(data = filtered_depth_spray, aes(x = PE1, y = PE3), fill = "white", shape = 21, size = 2, alpha = 0.75)
  
  ggsave(spray_plot,file=paste0("Futures Reports Images/",athlete," - spraychart.png"), width=3,height=2,units="in", dpi = 200)
  pitchCharts3 <- image_read(paste0("Futures Reports Images/",athlete," - spraychart.png"))
  cropped_image <- pitchCharts3 %>%
  image_crop("400x400+100+75")
  PitchingReport7 <- image_composite(PitchingReport6,cropped_image, offset= "+2035+825")
  
  depthAVG <- mean(filtered_depth_spray$Intersect3, na.rm = TRUE)
  
  depth_label <- ggplot() +
    geom_text(aes(x = 0.5, y = 0.5), 
              label = paste("Avg POI:", round(depthAVG, digits = 1), "\""),
              size = 10, color = "white", family = "Good Times") +
    theme_void() +
    theme(plot.margin = margin(1, 1, 1, 1, "cm"))

  ggsave(depth_label,file=paste0("Futures Reports Images/",athlete," - depthlabel.png"), width=3.5,height=2,units="in", dpi = 150)
  pitchCharts4 <- image_read(paste0("Futures Reports Images/",athlete," - depthlabel.png"))
  PitchingReport8 <- image_composite(PitchingReport7,pitchCharts4, offset= "+1475+1250")
  
  sprayChart_metrics <- hittraxSession %>%
    filter(UserName == athlete, !(LDP == 0 & FBP == 0 & GBP == 0)) %>%
    select(LDP, FBP, GBP) %>%
    summarise(
      LD = round(mean(LDP, na.rm = TRUE),1),
      FB = round(mean(FBP, na.rm = TRUE),1),
      GB = round(mean(GBP, na.rm = TRUE),1)
    )
  
  sprayChart_table <- sprayChart_metrics %>%
    gt() %>% 
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
    tab_options(
      table.border.left.style = "solid",
      table.border.right.style = "solid",
      table_body.vlines.style = "solid"
    ) %>% 
    cols_width(
      LD ~ px(70),
      FB ~ px(70),
      GB ~ px(70),
    ) %>% 
    sub_missing(
      columns = everything()
    )
  
  gtsave(sprayChart_table, file = paste0("Futures Reports Images/ ",athlete, "- sprayChart_table.png"), vwidth = 1200, expand = 0)
  
  playerSummary4 <- image_read(paste0("Futures Reports Images/ ",athlete,"- sprayChart_table.png"))
  playerSummary4 <- playerSummary4 %>% 
    image_transparent(color = "black")
  PitchingReport9 <- image_composite(PitchingReport8,playerSummary4,offset= "+2025+1175")
  
  sprayChart_metrics_two <- hittraxSession %>%
    filter(UserName == athlete, !(LDP == 0 & FBP == 0 & GBP == 0)) %>%
    mutate(
      Left = (LIP + LOP),
      Center = (CIP + COP),
      Right = (RIP + ROP)
    ) %>%
    select(Left, Center, Right) %>%
    summarise(
      Left = round(mean(Left, na.rm = TRUE),1),
      Center = round(mean(Center, na.rm = TRUE),1),
      Right = round(mean(Right, na.rm = TRUE),1)
    )
  
  sprayChart_table_two <- sprayChart_metrics_two %>%
    gt() %>% 
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
    tab_options(
      table.border.left.style = "solid",
      table.border.right.style = "solid",
      table_body.vlines.style = "solid"
    ) %>% 
    cols_width(
      Left ~ px(70),
      Center ~ px(70),
      Right ~ px(70),
    ) %>% 
    sub_missing(
      columns = everything()
    )
  
  gtsave(sprayChart_table_two, file = paste0("Futures Reports Images/ ",athlete, "- sprayChart_table2.png"), vwidth = 1200, expand = 0)
  
  playerSummary5 <- image_read(paste0("Futures Reports Images/ ",athlete,"- sprayChart_table2.png"))
  playerSummary5 <- playerSummary5 %>% 
    image_transparent(color = "black")
  PitchingReport10 <- image_composite(PitchingReport9,playerSummary5,offset= "+2025+1335")

  #image_write(PitchingReport5, path = paste0(athlete_folder, "/", "Futures Hitting Report.pdf"), format="pdf", quality=100, density=300)
  
  image_write(PitchingReport10,path = "page1.pdf",format="pdf",quality=100,density=300)
  image_write(IndexPage,path = "page2.pdf",format="pdf",quality=100,density=300)
  
  qpdf::pdf_combine(input = c("page1.pdf", "page2.pdf"),
                    output = paste0(athlete_folder, "/", "Futures Hitting Report.pdf")) 
}
