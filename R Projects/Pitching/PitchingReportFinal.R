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

font_add(family = "Good Times", regular = "good times rg.otf")
showtext_auto()

trackmanData <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/practice/combined_data.csv")
trackmanData$Date <- mdy(trackmanData$Date)

trackmanData <- trackmanData %>% 
  mutate(ZoneCheck = case_when(between(PlateLocHeight, 1.65, 3.65) & between(PlateLocSide, -0.75, 0.75) ~ TRUE, TRUE ~ FALSE),
         Month = month(Date, label = TRUE, abbr = FALSE),
         Pitcher = str_split(Pitcher, pattern = ", ", simplify = TRUE) %>% 
           apply(1, function(x) paste(x[2], x[1]))) %>%
  filter(!is.na(TaggedPitchType) & Month == "January" & PitchSession == "Live")

armCareData <- read_csv("/Users/watts/Downloads/ArmCare_data.csv")
armCareData$`Exam Date` <- mdy(armCareData$`Exam Date`)

armCareData <- armCareData %>%
  mutate(
    Name = paste(`First Name`, `Last Name`),
    Month = month(`Exam Date`, label = TRUE, abbr = FALSE)) %>%
  filter(
    `Exam Type` %in% c("Fresh - Quick", "Fresh - Full"),
    Month == "January")

clientData <- read_csv("/Users/watts/Downloads/FullClientList.csv") %>% 
  rename(Name = Client)
attendanceData <- read_csv("/Users/watts/Downloads/PitchingAttendance_data.csv")

# Extract names from clientData
clientNames <- clientData$Name

# Compare and extract non-matching names
nonMatchingTrackman <- setdiff(trackmanData$Pitcher, clientNames)
nonMatchingArmCare <- setdiff(armCareData$Name, clientNames)

# Combine non-matching names into one data frame
nonMatching <- rbind(
  data.frame(Name = nonMatchingTrackman, Source = 'Trackman'),
  data.frame(Name = nonMatchingArmCare, Source = 'ArmCare')
)

# Write the non-matching names to a new CSV file
write_csv(nonMatching, "/Users/watts/Downloads/missing_pitching_data.csv")

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
clientData$Age <- sapply(clientData$`field-general-7.dl_date`, calculate_age)
colnames(clientData)[colnames(clientData) == "Client"] <- "Name"

pitchers <- unique(trackmanData$Pitcher)

TemplatePageOne <- image_read_pdf("/Volumes/COLE'S DATA/Templates/Pitching Report Template.pdf")
IndexPage <- image_read_pdf("/Volumes/COLE'S DATA/Templates/Pitching Report Metric Index.pdf")

USABaseball <- c("ChangeUp"="#3d9be9", "Curveball"="#00FF00", "Cutter"="#FFFF00", "Fastball"="#FF0000", "Sinker"="#FFA500", "Slider"="#800080", "Splitter"="#FF69B4")

setwd("/Users/watts/Documents/Futures Performance Center/Test")

col_grid <- rgb(235, 235, 235, 25, maxColorValue = 255)

for (pitcher in pitchers){
  
  filteredTrackmanData <- trackmanData %>% 
    filter(Pitcher == pitcher & (sum(!is.na(Flag)) > 0 | is.na(Flag)))
  
  attendance_plot_data <- attendanceData %>%
    filter(`Client name` == pitcher) %>% 
    mutate(`Total Weeks` = 4, # Adjust this number based on the exact number of weeks in the 2-month period
           `Total Days Open` = `Total Weeks` * 6, # Assuming the facility is open 6 days a week
           `Attendance Score` = round(Attended / `Total Weeks`, digits = 1))
  
  attendance_score <- max(attendance_plot_data$`Attendance Score`, na.rm = TRUE)
  
  if (attendance_score == -Inf || is.na(attendance_score) || attendance_score <= 0.1) {
    next
  }
  
  pitcher_folder <- paste0("Futures Reports/", pitcher)
  if (!dir.exists(pitcher_folder)) {
    dir.create(pitcher_folder, recursive = TRUE)
  }
  
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
  attendance_plot <- ggplot(attendance_plot_data, aes(x = `Client name`, y = `Attendance Score`)) +
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
  
  ggsave(attendance_plot,file=paste0("Futures Reports Images/",pitcher,"_attendancePlot.png"), width=2.65,height=0.65,units="in", dpi = 175)
  attendancePlot <- image_read(paste0("Futures Reports Images/",pitcher,"_attendancePlot.png"))
  PitchingReport1 <- image_composite(TemplatePageOne, attendancePlot, offset= "+1965+585")
  
  player_profile <- clientData %>%
    filter(Name == pitcher) %>%
    mutate(Attendance = NA, HT_WT = paste(Height, " / ", Weight)) %>% 
    select(Name, Age, `Reporting Level (Age-Dependent)`, `Position (Baseball/Softball)`, Gpa, HT_WT, `High School`, `Graduating Class`, Attendance)
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
  
  ggsave(p,file=paste0("Futures Reports Images/",pitcher," - playerProfile.png"), width=16,height=2.25,units="in", dpi = 150)
  playerSummary1 <- image_read(paste0("Futures Reports Images/",pitcher," - playerProfile.png"))
  PitchingReport2 <- image_composite(PitchingReport1, playerSummary1, offset= "+50+475")
  
  armCarePlotData <- armCareData %>% 
    filter(Name == pitcher) %>%
    select(`Exam Date`, `Arm Score`, `IRTARM RS`, `ERTARM RS`, `STARM RS`, `GTARM RS`, `Shoulder Balance`) %>%
    mutate(`IRTARM RS` = `IRTARM RS` * 100,
           `ERTARM RS` = `ERTARM RS` * 100,
           `STARM RS` = `STARM RS` * 100,
           `GTARM RS` = `GTARM RS` * 100) %>%
    arrange(`Exam Date`) %>% 
    na.omit()
  names(armCarePlotData) <- c("Exam Date", "Arm Score", "IR", "ER", "Scaption", "Grip", "Shoulder Balance")
  
  shoulderBalanceAVG <- mean(armCarePlotData$`Shoulder Balance`, na.rm = TRUE)
  
  armStrengthPlot <- armCarePlotData %>%
    ggplot(aes(x = `Exam Date`, y = `Arm Score`, color = "Arm Score")) +
    geom_hline(yintercept = 70, linetype="dashed", color="green") +
    geom_line(linewidth = 2, color = "#3d9be9") +
    geom_point(size = 4, color = "#3d9be9") +
    labs(y = "% BW", x = NULL) +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid = element_line(color = col_grid),
          axis.text=element_text(color = "white", size = 15),
          axis.title = element_text(color = "white", size = 15))
  
  ggsave(armStrengthPlot,file=paste0("Futures Reports Images/", pitcher," - armStrengthPlot.png"),width=7.25,height=2.85,units="in", dpi = 150)
  pitchCharts1<- image_read(paste0("Futures Reports Images/", pitcher," - armStrengthPlot.png"))
  PitchingReport3 <- image_composite(PitchingReport2,pitchCharts1,offset= "+100+2805")
  
  armCarePlot_one <- armCarePlotData %>%
    ggplot(aes(x = `Exam Date`)) +
    geom_hline(yintercept = 20, linetype="dashed", color="green") +
    geom_line(aes(y = IR, color = "IR"), linewidth = 2) + 
    geom_line(aes(y = ER, color = "ER"), linewidth = 2) + 
    geom_point(aes(y = IR, color = "IR"), size = 4) + 
    geom_point(aes(y = ER, color = "ER"), size = 4) + 
    labs(y = "% BW", x = NULL) +
    scale_color_manual(values = c("IR" = "#FF0000", "ER" = "#3d9be9")) +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title= element_blank(),
          legend.text = element_text(color = "white", size = 20),
          panel.grid = element_line(color = col_grid),
          axis.text=element_text(color = "white", size = 15),
          axis.title = element_text(color = "white", size = 15))
  
  
  armCarePlot_two <- armCarePlotData %>%
    ggplot(aes(x = `Exam Date`)) +
    geom_hline(yintercept = 15, linetype="dashed", color="green") +
    geom_line(aes(y = Scaption, color = "Scaption"), linewidth = 2) +
    geom_line(aes(y = Grip, color = "Grip"), linewidth = 2) +
    geom_point(aes(y = Scaption, color = "Scaption"), size = 4) +
    geom_point(aes(y = Grip, color = "Grip"), size = 4) +
    labs(y = "% BW", x = NULL) +
    scale_color_manual(values = c("Scaption" = "#FF0000", "Grip" = "#3d9be9")) +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title= element_blank(),
          legend.text = element_text(color = "white", size = 20),
          panel.grid = element_line(color = col_grid),
          axis.text=element_text(color = "white", size = 15),
          axis.title = element_text(color = "white", size = 15))
  
  
  ggsave(armCarePlot_one,file=paste0("Futures Reports Images/", pitcher," - armcarePlot1.png"),width=5.7,height=3,units="in", dpi = 150)
  pitchCharts2<- image_read(paste0("Futures Reports Images/", pitcher," - armcarePlot1.png"))
  PitchingReport4 <- image_composite(PitchingReport3,pitchCharts2,offset= "+100+2200")
  
  ggsave(armCarePlot_two,file=paste0("Futures Reports Images/", pitcher," - armcarePlot2.png"),width=5.7,height=3,units="in", dpi = 150)
  pitchCharts3<- image_read(paste0("Futures Reports Images/", pitcher," - armcarePlot2.png"))
  PitchingReport5 <- image_composite(PitchingReport4,pitchCharts3,offset= "+1000+2200")
  
  SVRplotData <- armCareData %>%
    filter(Name == pitcher) %>%
    select(`Exam Date`, "SVR") %>% 
    arrange(`Exam Date`) %>% 
    na.omit()
  
  SVRplot <- SVRplotData %>%
    ggplot(aes(x = `Exam Date`, y = SVR, color = "")) +
    geom_hline(yintercept = 1.60, linetype="dashed", color="green") +
    geom_hline(yintercept = 1.40, linetype="dashed", color="red") +
    geom_line(linewidth = 2, color = "#3d9be9") +
    geom_point(size = 4, color = "#3d9be9") +
    labs(y = "SVR", x = NULL) + # Add label for the color legend
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid = element_line(color = col_grid),
          axis.text=element_text(color = "white", size = 15),
          axis.title = element_text(color = "white", size = 15))
  
  ggsave(SVRplot,file=paste0("Futures Reports Images/", pitcher," - SVRplot.png"),width=7.25,height=2.85,units="in", dpi = 150)
  pitchCharts4<- image_read(paste0("Futures Reports Images/", pitcher," - SVRplot.png"))
  PitchingReport6 <- image_composite(PitchingReport5,pitchCharts4,offset= "+1315+2805")
  
  shoulderBalance_plot <- ggplot() +
    geom_text(aes(x = 0.5, y = 0.5, label = paste(round(shoulderBalanceAVG, digits = 2))),
              size = 25, color = "white", family = "Good Times") +
    theme_void() +
    theme(plot.margin = margin(1, 1, 1, 1, "cm"))
  
  ggsave(shoulderBalance_plot,file=paste0("Futures Reports Images/", pitcher," - shoulderBalance.png"),width=5,height=5.15,units="in", dpi = 150)
  pitchCharts5<- image_read(paste0("Futures Reports Images/", pitcher," - shoulderBalance.png"))
  PitchingReport7 <- image_composite(PitchingReport6,pitchCharts5,offset= "+1825+1975")
  
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
              AvgGyro = format(round(mean(SpinAxis3dLongitudinalAngle, na.rm = TRUE), 1)),
              Zone = format(round(mean(ZoneCheck, na.rm = TRUE) * 100, 1)),
              Height = format(round(mean(RelHeight, na.rm = TRUE), 1)),
              Side = format(round(mean(RelSide, na.rm = TRUE), 1)),
              Extension = format(round(mean(Extension, na.rm = TRUE), 1))) %>%
    arrange(desc(AvgVelo)) %>% 
    select(-`#`)
  colnames(pitchStats) <- c('Pitch Type', "%", 'Avg Velo', 'Max Velo', 'Vert', 'Horz', 'Avg Spin', 'Avg Gyro', "K%", "Height", "Side", "Extension") 
  
  gt_table <- pitchStats %>% 
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
      columns = c('Avg Velo', 'Max Velo', 'Vert', 'Horz', 'Avg Spin', 'Avg Gyro', 'K%')
    ) %>% 
    tab_spanner(
      label = "Release",
      columns = c("Height", "Side", "Extension")
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
      `K%` ~ px(50),
      Height ~ px(75),
      Side ~ px(75),
      Extension ~ px(75),
      everything() ~ px(90)
    ) %>% 
    cols_label(
      Extension = "Ext."
    )
  
  apply_tab_style_if_exists <- function(gt_table, pitch_type, color) {
    if (pitch_type %in% pitchStats$`Pitch Type`) {
      # Apply background color
      gt_table <- gt_table %>%
        tab_style(
          style = cell_fill(color = color), 
          locations = cells_stub(rows = pitch_type)
        )
      
      # Apply text color for Curveball and Cutter
      if (pitch_type %in% c("Curveball", "Cutter", "Sinker")) {
        gt_table <- gt_table %>%
          tab_style(
            style = cell_text(color = "black"), 
            locations = cells_stub(rows = pitch_type)
          )
      }
    }
    return(gt_table)
  }
  
  pitch_types_colors <- list(
    "ChangeUp" = "#007FFF",
    "Curveball" = "#00FF00",
    "Cutter" = "#FFFF00",
    "Fastball" = "#FF0000",
    "Sinker" = "#FFA500",
    "Slider" = "#800080",
    "Splitter" = "#FF69B4"
  )
  
  for (pitch_type in names(pitch_types_colors)) {
    gt_table <- apply_tab_style_if_exists(gt_table, pitch_type, pitch_types_colors[[pitch_type]])
  }
  
  gtsave(gt_table, file = paste0("Futures Reports Images/ ",pitcher, "- pitchingSummary.png"), vwidth = 1200, expand = 5)
  
  pitchSummaries1 <- image_read(paste0("Futures Reports Images/ ",pitcher,"- pitchingSummary.png"))
  pitchSummaries1 <- pitchSummaries1 %>% 
    image_transparent(color = "black") %>% 
    image_trim()
  PitchingReport8 <- image_composite(PitchingReport7,pitchSummaries1,offset= "+310+765")
  
  meanMovement <- filteredTrackmanData %>% 
    group_by(TaggedPitchType) %>% 
    summarise(`Avg HorzBreak` = mean(HorzBreak, na.rm = TRUE),
              `Avg VertBreak` = mean(InducedVertBreak, na.rm = TRUE))
  
  movement_plot <- filteredTrackmanData %>% 
    ggplot(aes(x = HorzBreak, y = InducedVertBreak, fill = TaggedPitchType)) +
    geom_jitter(shape = 21, size = 2, alpha = 0.75, stroke = 0) +
    geom_point(data = meanMovement, aes(x = `Avg HorzBreak`, y = `Avg VertBreak`, fill = TaggedPitchType), shape = 21, size = 7, color = "black") +
    geom_hline(yintercept = 0, color = "white") +
    geom_vline(xintercept = 0, color = "white") +
    xlim(-25,25) +
    ylim(-25,25) +
    labs(x = "Horizontal Break", y = "Induced Vertical Break") +
    scale_fill_manual(values = USABaseball,
                       labels = c("CH", "CB", "CU", "FB", "SI", "SL", "FS"),
                       breaks = names(USABaseball)) +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid = element_line(color = col_grid),
          axis.text=element_text(color = "white", size = 15),
          axis.title = element_text(color = "white", size = 15))
  
  
  strikezone_plot <- filteredTrackmanData %>% 
    ggplot(aes(x = PlateLocSide * 12, y = PlateLocHeight * 12, fill = TaggedPitchType)) +
    geom_rect(aes(xmin = -9, xmax = 9, ymin = 19.8, ymax = 43.8), alpha = 0.0, color = "white", linetype = "dashed") +
    geom_point(shape = 21, size = 4, alpha = 0.75) +
    coord_cartesian(xlim = c(-32, 32), ylim = c(-0, 60)) +
    scale_fill_manual(values = USABaseball,
                      labels = c("CH", "CB", "CU", "FB", "SI", "SL", "FS"),
                      breaks = names(USABaseball)) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_line(color = col_grid))
  
  # release_plot <- filteredTrackmanData %>% 
  #   ggplot(aes(x = RelSide, y = RelHeight, fill = TaggedPitchType)) +
  #   geom_jitter(shape = 21, size = 4, alpha = 0.75) +
  #   geom_hline(yintercept = 0, color = "white") +
  #   geom_vline(xintercept = 0, color = "white") +
  #   xlim(-4,4) +
  #   ylim(0,8) +
  #   labs(title = "Pitch Release",
  #        x = "Release Side", y = "Release Height") +
  #   theme_minimal() +
  #   scale_fill_manual(values = USABaseball,
  #                      labels = c("CH", "CB", "CU", "FB", "SI", "SL", "FS"),
  #                      breaks = names(USABaseball)) +
  #   theme_minimal() +
  #   theme(legend.position = "none",
  #         legend.title = element_blank(),
  #         legend.text = element_text(color = "white", size = 20),
  #         panel.grid = element_line(color = col_grid),
  #         axis.text=element_text(color = "white", size = 15),
  #         axis.title = element_text(color = "white", size = 15),
  #         plot.title = element_text(hjust = 0.5, color = "white", size = 38, family = "Good Times"))
  
  ggsave(movement_plot,file=paste0("Futures Reports Images/", pitcher," - movementPlot.png"), width=4,height=3.70,units="in", dpi = 150)
  pitchCharts6 <- image_read(paste0("Futures Reports Images/", pitcher ," - movementPlot.png"))
  PitchingReport9 <- image_composite(PitchingReport8,pitchCharts6, offset= "+1815+1485")
  
  ggsave(strikezone_plot,file=paste0("Futures Reports Images/", pitcher," - strikezonePlot.png"), width=4,height=3.70,units="in", dpi = 150)
  pitchCharts7 <- image_read(paste0("Futures Reports Images/", pitcher ," - strikezonePlot.png"))
  PitchingReport10 <- image_composite(PitchingReport9,pitchCharts7, offset= "+100+1485")
  
  # ggsave(release_plot,file=paste0("Futures Reports Images/", pitcher," - releasePlot.png"), width=4.5,height=4,units="in", dpi = 150)
  # pitchCharts6 <- image_read(paste0("Futures Reports Images/", pitcher ," - releasePlot.png"))
  # PitchingReport8 <- image_composite(PitchingReport7,pitchCharts6, offset= "+1730+1435")
  
  max_speed_data <- filteredTrackmanData %>%
    filter(Pitcher == pitcher) %>% 
    group_by(Date, TaggedPitchType) %>%
    summarise(RelSpeed = max(RelSpeed, na.rm = TRUE), .groups = "drop")
  
  velocity_plot <- max_speed_data %>% 
    ggplot(aes(x = Date, y = RelSpeed, color = TaggedPitchType)) +
    geom_line(data = subset(max_speed_data, TaggedPitchType == "Fastball"), linewidth = 3, alpha = 0.75) +
    geom_line(data = subset(max_speed_data, TaggedPitchType != "Fastball"), linewidth = 2, alpha = 0.75) +
    geom_point(data = subset(max_speed_data, TaggedPitchType == "Fastball"), size = 5) +
    geom_point(data = subset(max_speed_data, TaggedPitchType != "Fastball"), size = 4) +    
    labs(y = "Velocity", x = NULL) +
    scale_color_manual(values = USABaseball,
                       labels = c("CH", "CB", "CU", "FB", "SI", "SL", "FS"),
                       breaks = names(USABaseball)) +    
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid = element_line(color = col_grid),
          axis.text=element_text(color = "white", size = 15),
          axis.title = element_text(color = "white", size = 15))
  
  
  ggsave(velocity_plot,file=paste0("Futures Reports Images/", pitcher," - velocityPlot.png"),width=6,height=3.4,units="in", dpi = 150)
  pitchCharts8<- image_read(paste0("Futures Reports Images/", pitcher," - velocityPlot.png"))
  PitchingReport11 <- image_composite(PitchingReport10,pitchCharts8,offset= "+800+1530")
  
  image_write(PitchingReport11,path = "page1.pdf",format="pdf",quality=100,density=300)
  image_write(IndexPage,path = "page2.pdf",format="pdf",quality=100,density=300)

  qpdf::pdf_combine(input = c("page1.pdf", "page2.pdf"),
                    output = paste0(pitcher_folder, "/", "Futures Pitching Report.pdf"))   
}
