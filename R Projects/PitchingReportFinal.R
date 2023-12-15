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

gamefile <- read_csv("/Users/watts/Downloads/Trackman Master - Sheet1.csv") %>% 
  mutate(ZoneCheck = case_when(between(PlateLocHeight, 1.65, 3.65) & between(PlateLocSide, -0.75, 0.75) ~ TRUE, 
                               TRUE ~ FALSE),
         Pitcher = str_split(Pitcher, pattern = ", ", simplify = TRUE) %>% 
           apply(1, function(x) paste(x[2], x[1]))) %>%
  filter(!is.na(TaggedPitchType))

armCareData <- read_csv("/Users/watts/Downloads/Armcare Master - Sheet1.csv")
armCareData$`Exam Date` <- mdy(armCareData$`Exam Date`)

armCareData <- armCareData %>%
  mutate(
    Name = paste(`First Name`, `Last Name`),
    Month = month(`Exam Date`, label = TRUE, abbr = FALSE)) %>%
  filter(
    `Exam Type` %in% c("Fresh - Quick", "Fresh - Full"),
    Month %in% c("October", "November"))

clientData <- read.csv("/Users/watts/Downloads/FullClientList.csv")
attendanceData <- read.csv("/Users/watts/Downloads/PitchingAttendance_data.csv")

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
clientData$Age <- sapply(clientData$`field.general.7.dl_date`, calculate_age)
colnames(clientData)[colnames(clientData) == "Client"] <- "Name"

pitchers <- unique(gamefile$Pitcher)

TemplatePageOne <- image_read_pdf("/Volumes/COLE'S DATA/Templates/Pitching Report Template.pdf")
IndexPage <- image_read_pdf("/Users/watts/Downloads/Pitching Report Metric Index.pdf")

USABaseball <- c("ChangeUp"="#007FFF", "Curveball"="#00FF00", "Cutter"="#FFFF00", "Fastball"="#FF0000", "Sinker"="#FFA500", "Slider"="#800080", "Splitter"="#FF69B4")

setwd("/Users/watts/Documents/Futures Performance Center/Test")

col_grid <- rgb(235, 235, 235, 25, maxColorValue = 255)

for (pitcher in pitchers){
  
  pitcher_folder <- paste0("Futures Reports/", pitcher)
  if (!dir.exists(pitcher_folder)) {
    dir.create(pitcher_folder, recursive = TRUE)
  }
  
  player_profile <- clientData %>%
    filter(Name == pitcher) %>%
    mutate(GPA = NA, Weight = NA, School = NA, Class = NA, Attendance = NA, HT_WT = paste(Height, " / ", Weight)) %>% 
    select(Name, Age, Reporting.Level..Age.Dependent., Position..Baseball.Softball., GPA, HT_WT, School, Class, Attendance)
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
  
  ggsave(p,file=paste0("Futures Reports Images/",pitcher," - playerProfile.png"), width=16,height=2.25,units="in", dpi = 150)
  playerSummary1 <- image_read(paste0("Futures Reports Images/",pitcher," - playerProfile.png"))
  PitchingReport1 <- image_composite(TemplatePageOne, playerSummary1, offset= "+50+475")
  
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
    geom_line(linewidth = 2) +
    geom_point(size = 4) +
    geom_hline(yintercept = 70, linetype="dashed", color="green") +
    labs(y = "% BW", x = NULL) +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid = element_line(color = col_grid),
          axis.text=element_text(color = "white", size = 15),
          axis.title = element_text(color = "white", size = 15))
  
  ggsave(armStrengthPlot,file=paste0("Futures Reports Images/", pitcher," - armStrengthPlot.png"),width=7,height=3,units="in", dpi = 150)
  pitchCharts1<- image_read(paste0("Futures Reports Images/", pitcher," - armStrengthPlot.png"))
  PitchingReport2 <- image_composite(PitchingReport1,pitchCharts1,offset= "+125+900")
  
  armCarePlot_one <- armCarePlotData %>%
    ggplot(aes(x = `Exam Date`)) +
    geom_line(aes(y = IR, color = "IR"), linewidth = 2) + # Assign color to IR line
    geom_line(aes(y = ER, color = "ER"), linewidth = 2) + # Assign color to ER line
    geom_point(aes(y = IR, color = "IR"), size = 4) + # Assign color to IR points
    geom_point(aes(y = ER, color = "ER"), size = 4) + # Assign color to ER points
    geom_hline(yintercept = 20, linetype="dashed", color="green") +
    labs(y = "% BW", x = NULL) + # Add label for the color legend
    theme_minimal() +
    theme(legend.position = "right",
          legend.title= element_blank(),
          legend.text = element_text(color = "white", size = 20),
          panel.grid = element_line(color = col_grid),
          axis.text=element_text(color = "white", size = 15),
          axis.title = element_text(color = "white", size = 15))
  
  
  armCarePlot_two <- armCarePlotData %>%
    ggplot(aes(x = `Exam Date`)) +
    geom_line(aes(y = Scaption, color = "Scaption"), linewidth = 2) + # Assign color to Scaption line
    geom_line(aes(y = Grip, color = "Grip"), linewidth = 2) + # Assign color to Grip line
    geom_point(aes(y = Scaption, color = "Scaption"), size = 4) + # Assign color to Scaption points
    geom_point(aes(y = Grip, color = "Grip"), size = 4) + # Assign color to Grip points
    geom_hline(yintercept = 15, linetype="dashed", color="green") +
    labs(y = "% BW", x = NULL) + # Add label for the color legend
    theme_minimal() +
    theme(legend.position = "right",
          legend.title= element_blank(),
          legend.text = element_text(color = "white", size = 20),
          panel.grid = element_line(color = col_grid),
          axis.text=element_text(color = "white", size = 15),
          axis.title = element_text(color = "white", size = 15))
  
  
  ggsave(armCarePlot_one,file=paste0("Futures Reports Images/", pitcher," - armcarePlot1.png"),width=7,height=1.75,units="in", dpi = 150)
  pitchCharts2<- image_read(paste0("Futures Reports Images/", pitcher," - armcarePlot1.png"))
  PitchingReport3 <- image_composite(PitchingReport2,pitchCharts2,offset= "+1350+825")
  
  ggsave(armCarePlot_two,file=paste0("Futures Reports Images/", pitcher," - armcarePlot2.png"),width=7.5,height=1.75,units="in", dpi = 150)
  pitchCharts3<- image_read(paste0("Futures Reports Images/", pitcher," - armcarePlot2.png"))
  PitchingReport4 <- image_composite(PitchingReport3,pitchCharts3,offset= "+1350+1100")
  
  SVRplotData <- armCareData %>%
    filter(Name == pitcher) %>%
    select(`Exam Date`, "SVR") %>% 
    arrange(`Exam Date`) %>% 
    na.omit()
  
  SVRplot <- SVRplotData %>%
    ggplot(aes(x = `Exam Date`, y = SVR, color = "")) +
    geom_line(linewidth = 2) +
    geom_point(size = 4) +
    geom_hline(yintercept = 1.60, linetype="dashed", color="green") +
    geom_hline(yintercept = 1.40, linetype="dashed", color="red") +
    theme_minimal() +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text=element_text(color = "white", size = 15),
          panel.grid = element_line(color = col_grid))
  
  ggsave(SVRplot,file=paste0("Futures Reports Images/", pitcher," - SVRplot.png"),width=10.5,height=3,units="in", dpi = 150)
  pitchCharts4<- image_read(paste0("Futures Reports Images/", pitcher," - SVRplot.png"))
  PitchingReport5 <- image_composite(PitchingReport4,pitchCharts4,offset= "+200+1550")
  
  shoulderBalance_plot <- ggplot() + 
    geom_text(aes(x = 0.5, y = 0.5, label = paste(round(shoulderBalanceAVG, digits = 2))), 
              size = 25, color = "white", family = "Good Times") +
    theme_void() + 
    theme(plot.margin = margin(1, 1, 1, 1, "cm"))
  
  ggsave(shoulderBalance_plot,file=paste0("Futures Reports Images/", pitcher," - shoulderBalance.png"),width=5,height=5,units="in", dpi = 150)
  pitchCharts5<- image_read(paste0("Futures Reports Images/", pitcher," - shoulderBalance.png"))
  PitchingReport6 <- image_composite(PitchingReport5,pitchCharts5,offset= "+1825+1325")
  
  pitches <- subset(gamefile, Pitcher == pitcher)
  total_pitches <- nrow(pitches)
  
  pitchStats <- pitches %>%
    group_by(TaggedPitchType) %>%
    summarize(`#` = n(),
              `%` = format(round((`#` / total_pitches) * 100, 1)),
              AvgVelo = format(round(mean(RelSpeed, na.rm = TRUE), 1)),
              MaxVelo = format(round(max(RelSpeed, na.rm = TRUE), 1)),
              AvgSpinRate = format(round(mean(SpinRate, na.rm = TRUE), 0)),
              AvgGyro = format(round(mean(SpinAxis3dLongitudinalAngle, na.rm = TRUE), 1)),
              AvgIVB = format(round(mean(InducedVertBreak, na.rm = TRUE), 1)),
              AvgHB = format(round(mean(HorzBreak, na.rm = TRUE), 1)),
              Zone = format(round(mean(ZoneCheck, na.rm = TRUE) * 100, 1))) %>%
    arrange(desc(AvgVelo)) %>% 
    select(-`#`)
  colnames(pitchStats) <- c('Pitch Type', '%', 'Avg Velo', 'Max Velo', 'Avg Spin', 'Avg Gyro', 'Avg IVB', 'Avg HB', "Strike %")
  
  transparent_theme <- ttheme_minimal(
    core = list(fg_params = list(col = "white"), bg_params = list(fill = "transparent", col = NA)),
    colhead = list(fg_params = list(col = "white"), bg_params = list(fill = "transparent", col = NA)),
    rowhead = list(fg_params = list(col = "white"), bg_params = list(fill = "transparent", col = NA)),
    padding = unit(c(4, 4), "mm")
  )
  
  png(paste0("Futures Reports Images/", pitcher ,"- pitchingSummary.png"),height=2, width=8,units = "in",res = 200,bg = "transparent")
  table2 <- tableGrob(pitchStats, rows = NULL, theme = transparent_theme)
  grid.arrange(table2)
  dev.off()
  
  pitchSummaries2 <- image_read(paste0("Futures Reports Images/", pitcher ,"- pitchingSummary.png"))
  PitchingReport7 <- image_composite(PitchingReport6,pitchSummaries2,offset= "+50+2225")
  
  movement_plot <- ggplot(subset(pitches), aes(HorzBreak, InducedVertBreak, color = TaggedPitchType, stroke = 1)) +
    geom_point(na.rm = TRUE, alpha = 0.5) +
    geom_hline(yintercept = 0, color = "white") +
    geom_vline(xintercept = 0, color = "white") +
    xlim(-25,25) +
    ylim(-25,25) +
    labs(title = "Break Plot",
         x = "Horizontal Break", y = "Induced Vertical Break") +
    scale_color_manual(values = USABaseball,
                       labels = c("CH", "CB", "CU", "FB", "SI", "SL", "FS"),
                       breaks = names(USABaseball)) +
    theme_minimal() +
    theme(legend.position = "right",
          legend.title = element_blank(),
          legend.text = element_text(color = "white", size = 20),
          panel.grid = element_line(color = col_grid),
          axis.text=element_text(color = "white", size = 15),
          axis.title = element_text(color = "white", size = 15),
          plot.title = element_text(hjust = 0.5, color = "#3d9be9", size = 32, family = "Good Times"))
  
  release_plot <- ggplot(subset(pitches),aes(RelSide, RelHeight, color = TaggedPitchType, stroke = 1)) +
    geom_point(na.rm = TRUE, alpha = 0.5) +
    geom_hline(yintercept = 0, color = "white") +
    geom_vline(xintercept = 0, color = "white") +
    xlim(-4,4) +
    ylim(0,8) +
    labs(title = "Release Plot",
         x = "Release Side", y = "Release Height") +
    theme_minimal() +
    scale_color_manual(values = USABaseball,
                       labels = c("CH", "CB", "CU", "FB", "SI", "SL", "FS"),
                       breaks = names(USABaseball)) +  theme_minimal() +
    theme(legend.position = "right",
          legend.title = element_blank(),
          legend.text = element_text(color = "white", size = 20),
          panel.grid = element_line(color = col_grid),
          axis.text=element_text(color = "white", size = 15),
          axis.title = element_text(color = "white", size = 15),
          plot.title = element_text(hjust = 0.5, color = "#3d9be9", size = 32, family = "Good Times"))
  
  plots_row <- ggarrange(release_plot, movement_plot, ncol = 1)
  ggsave(plots_row,file=paste0("Futures Reports Images/", pitcher," - trackmanPlots.png"), width=4,height=6,units="in", dpi = 185)
  pitchCharts6 <- image_read(paste0("Futures Reports Images/", pitcher ," - trackmanPlots.png"))
  PitchingReport8 <- image_composite(PitchingReport7,pitchCharts6, offset= "+1700+2125")
  
  max_speed_data <- gamefile %>%
    filter(Pitcher == pitcher) %>% 
    mutate(Date = ymd(Date)) %>%
    group_by(Date, TaggedPitchType) %>%
    summarise(RelSpeed = max(RelSpeed, na.rm = TRUE))
  
  velocity_plot <- ggplot(max_speed_data, aes(x = Date, y = RelSpeed, color = TaggedPitchType)) +
    geom_line(data = subset(max_speed_data, TaggedPitchType == "Fastball"), linewidth = 3, alpha = 0.75) +
    geom_line(data = subset(max_speed_data, TaggedPitchType != "Fastball"), linewidth = 2, alpha = 0.5) +
    geom_point(data = subset(max_speed_data, TaggedPitchType == "Fastball"), size = 5, alpha = 0.75) +
    geom_point(data = subset(max_speed_data, TaggedPitchType != "Fastball"), size = 4, alpha = 0.5) +    
    labs(y = "Velocity", x = NULL) +
    scale_color_manual(values = USABaseball) +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title= element_blank(),
          legend.text = element_text(color = "white", size = 20),
          panel.grid = element_line(color = col_grid),
          axis.text=element_text(color = "white", size = 15),
          axis.title = element_text(color = "white", size = 15))
  
  
  ggsave(velocity_plot,file=paste0("Futures Reports Images/", pitcher," - velocityPlot.png"),width=9.5,height=3.65,units="in", dpi = 150)
  pitchCharts7<- image_read(paste0("Futures Reports Images/", pitcher," - velocityPlot.png"))
  PitchingReport9 <- image_composite(PitchingReport8,pitchCharts7,offset= "+125+2700")
  
  attendance_plot_data <- attendanceData %>%
    filter(Client.name == pitcher) %>% 
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
    attendance_plot <- ggplot(data.frame(Client.name = "N/A", y = 50), aes(x = Client.name, y = y)) +
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
      ggplot(aes(x = Client.name, y = `Attendance Score`)) +
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
  
  ggsave(attendance_plot,file=paste0("Futures Reports Images/",pitcher,"_attendancePlot.png"), width=2.90,height=0.75,units="in", dpi = 175)
  attendancePlot <- image_read(paste0("Futures Reports Images/",pitcher,"_attendancePlot.png"))
  PitchingReport10 <- image_composite(PitchingReport9, attendancePlot, offset= "+1965+575")
  
  image_write(PitchingReport10,path = "page1.pdf",format="pdf",quality=100,density=300)
  image_write(IndexPage,path = "page2.pdf",format="pdf",quality=100,density=300)
  
  
  qpdf::pdf_combine(input = c("page1.pdf", "page2.pdf"),
                    output = paste0(pitcher_folder, "/", "Futures Pitching Report.pdf"))   
}
