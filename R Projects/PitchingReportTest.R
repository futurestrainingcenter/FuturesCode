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

# Read the CSV file into the gamefile data table
gamefile <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Trackman Data/MasterTrackmanData_october_novemeber.csv") %>% 
  mutate(ZoneCheck = case_when(between(PlateLocHeight, 1.65, 3.65) & between(PlateLocSide, -0.75, 0.75) ~ TRUE, TRUE ~ FALSE),
         Pitcher = str_split(Pitcher, pattern = ", ", simplify = TRUE) %>% 
           apply(1, function(x) paste(x[2], x[1])))

armCareData <- read_csv("/Users/watts/Downloads/Daily Log 12-04-2023 1-17-53.csv")

armCareData$`Exam Date` <- mdy(armCareData$`Exam Date`)

armCareData <- armCareData %>%
  mutate(Name = paste(`First Name`, `Last Name`),
         Month = month(armCareData$`Exam Date`, label = TRUE, abbr = FALSE))

pitchers <- unique(gamefile$Pitcher)

TemplatePageOne <- image_read_pdf("/Users/watts/Documents/Futures Performance Center/Templates/Futures Pitching Template.pdf")
TemplatePageTwo <- image_read_pdf("/Users/watts/Documents/USA Baseball/Page 2 Report Template.pdf")
TemplatePageThree <- image_read_pdf("/Users/watts/Documents/USA Baseball/Page 2 Report Template.pdf")

USABaseball <- c("ChangeUp"="#42d4f4", "Curveball"="#3cb44b", "Cutter"="#ffe119", "Fastball"="#e6194B", "Sinker"="#f58231", "Slider"="#000075", "Splitter"="#f032e6")

setwd("/Users/watts/Documents/Futures Performance Center/Test")

for (pitcher in pitchers){
  
  # Filtering data for Casey Dykstra for August and September
  first_month <- armCareData %>%
    filter(Month == "October" & Name == pitcher)
  
  second_month <- armCareData %>%
    filter(Month == "November" & Name == pitcher)
  
  # Calculate stats for August
  august_stats <- first_month %>%
    summarize(
      `Arm Score (AVG)` = round(mean(`Arm Score`, na.rm = TRUE), 1),
      `Total Strength (AVG)` = round(mean(`Total Strength`, na.rm = TRUE), 1),
      `IR (%BW)` = round(mean(`IRTARM RS`, na.rm = TRUE) * 100, 1),
      `ER (%BW)` = round(mean(`ERTARM RS`, na.rm = TRUE) * 100, 1),
      `Scaption (%BW)` = round(mean(`STARM RS`, na.rm = TRUE) * 100, 1),
      `Grip (%BW)` = round(mean(`GTARM RS`, na.rm = TRUE) * 100, 1),
      `Shoulder Balance` = round(mean(`Shoulder Balance`, na.rm = TRUE), 1)
    )
  
  # Calculate stats for September
  september_stats <- second_month %>%
    summarize(
      Name = pitcher,
      `Arm Score (AVG)` = round(mean(`Arm Score`, na.rm = TRUE), 1),
      `Total Strength (AVG)` = round(mean(`Total Strength`, na.rm = TRUE), 1),
      `IR (%BW)` = round(mean(`IRTARM RS`, na.rm = TRUE) * 100, 1),
      `ER (%BW)` = round(mean(`ERTARM RS`, na.rm = TRUE) * 100, 1),
      `Scaption (%BW)` = round(mean(`STARM RS`, na.rm = TRUE) * 100, 1),
      `Grip (%BW)` = round(mean(`GTARM RS`, na.rm = TRUE) * 100, 1),
      `Shoulder Balance` = round(mean(`Shoulder Balance`, na.rm = TRUE), 1)
    )
  
  change_stats <- data.frame(
    Name = 'Monthly Change',
    `Arm Score (AVG)` = round(september_stats$`Arm Score (AVG)` - august_stats$`Arm Score (AVG)`, 1),
    `Total Strength (AVG)` = round(september_stats$`Total Strength (AVG)` - august_stats$`Total Strength (AVG)`, 1),
    `IR (%BW)` = round(september_stats$`IR (%BW)` - august_stats$`IR (%BW)`, 1),
    `ER (%BW)` = round(september_stats$`ER (%BW)` - august_stats$`ER (%BW)`, 1),
    `Scaption (%BW)` = round(september_stats$`Scaption (%BW)` - august_stats$`Scaption (%BW)`, 1),
    `Grip (%BW)` = round(september_stats$`Grip (%BW)` - august_stats$`Grip (%BW)`, 1),
    `Shoulder Balance` = round(september_stats$`Shoulder Balance` - august_stats$`Shoulder Balance`, 1)
  )
  names(change_stats) <- c("Name", "Arm Score (AVG)", "Total Strength (AVG)", "IR (%BW)", "ER (%BW)", "Scaption (%BW)", "Grip (%BW)", "Shoulder Balance")
  
  goal_stats <- data.frame(
    Name = 'Goals',
    `Arm Score (AVG)` = "70+",
    `Total Strength (AVG)` = " ",
    `IR (%BW)` = "20+",
    `ER (%BW)` = "20+",
    `Scaption (%BW)` = "15+",
    `Grip (%BW)` = "15+",
    `Shoulder Balance` = "0.85 - 1.05"
  )
  names(goal_stats) <- c("Name", "Arm Score (AVG)", "Total Strength (AVG)", "IR (%BW)", "ER (%BW)", "Scaption (%BW)", "Grip (%BW)", "Shoulder Balance")
  
  # Combine the tables
  armCareStats <- rbind(september_stats, change_stats, goal_stats)
  
  png(paste0("Pitching Reports Images/", pitcher ,"- armcareSummary.png"),height=1.25, width=12,units = "in",res = 250)
  table1 <- tableGrob(armCareStats, rows = NULL)
  grid.arrange(table1)
  dev.off()
  
  pitchSummaries <- image_read(paste0("Pitching Reports Images/", pitcher ,"- armcareSummary.png"))
  PitchingReport0 <- image_composite(TemplatePageOne,pitchSummaries,offset= "+150+450")
  
  armCarePlotData <- armCareData %>% 
    filter(Name == pitcher) %>%
    select(`Exam Date`, `Arm Score`, `IRTARM RS`, `ERTARM RS`, `STARM RS`, `GTARM RS`) %>%
    mutate(`IRTARM RS` = `IRTARM RS` * 100,
           `ERTARM RS` = `ERTARM RS` * 100,
           `STARM RS` = `STARM RS` * 100,
           `GTARM RS` = `GTARM RS` * 100) %>% # Adjust the format as needed
    arrange(`Exam Date`) %>% 
    na.omit()
  names(armCarePlotData) <- c("Exam Date", "Arm Score", "IR", "ER", "Scaption", "Grip")
  
  armCarePlot <- armCarePlotData %>%
    pivot_longer(cols = -`Exam Date`, names_to = "Variable", values_to = "Value") %>%
    ggplot(aes(x = `Exam Date`, y = Value, group = Variable, color = Variable)) +
    geom_line() +
    geom_point() +
    labs(title = "Arm Strength Progress",
         x = NULL,
         y = "% BW",
         color = NULL) +
    theme_minimal() +
    theme(plot.title = element_text(size = 13, face = "bold"))
  
  SVRplotData <- armCareData %>%
    filter(Name == pitcher) %>%
    select(`Exam Date`, "SVR") %>% 
    arrange(`Exam Date`) %>% 
    na.omit()
  
  SVRplot <- SVRplotData %>%
    ggplot(aes(x = `Exam Date`, y = SVR)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 1.60, linetype="dashed", color="green") +
    geom_hline(yintercept = 1.40, linetype="dashed", color="red") +
    labs(title = "Strength Velocity Ratio Progress",
         x = NULL,
         y = "SVR") +
    theme_minimal() +
    theme(plot.title = element_text(size = 13, face = "bold"))
  
  plots_row <- ggarrange(armCarePlot, SVRplot, nrow = 2)
  ggsave(plots_row,file=paste0("Pitching Reports Images/", pitcher," - armcarePlot.png"),width=9,height=5.5,units="in", dpi = 300)
  pitchCharts1<- image_read(paste0("Pitching Reports Images/", pitcher," - armcarePlot.png"))
  PitchingReport1 <- image_composite(PitchingReport0,pitchCharts1,offset= "+250+850")
  
  image_write(PitchingReport1,path = "page1.pdf",format="pdf",quality=100,density=300)
  
  pitches <- subset(gamefile, Pitcher == pitcher)
  total_pitches <- nrow(pitches)
  
  pitchStats <- pitches %>%
    group_by(TaggedPitchType) %>%
    summarize(`#` = n(),
              `%` = format(round((`#` / total_pitches) * 100, 1)),
              AvgVelo = format(round(mean(RelSpeed, na.rm = TRUE), 1)),
              MaxVelo = format(round(max(RelSpeed, na.rm = TRUE), 1)),
              MinVelo = format(round(min(RelSpeed, na.rm = TRUE), 1)),
              AvgSpinRate = format(round(mean(SpinRate, na.rm = TRUE), 0)),
              Tilt = format(median(strptime(Tilt, "%H:%M"), na.rm = TRUE), "%H:%M"),
              AvgIVB = format(round(mean(InducedVertBreak, na.rm = TRUE), 1)),
              AvgHB = format(round(mean(HorzBreak, na.rm = TRUE), 1)),
              AvgExtension = format(round(mean(Extension, na.rm = TRUE), 1)),
              Zone = format(round(mean(ZoneCheck, na.rm = TRUE) * 100, 1))) %>%
    arrange(desc(AvgVelo))
  colnames(pitchStats) <- c('Pitch Type', '#', '%', 'Avg Velo', 'Max Velo', 'Min Velo', 'Avg Spin', 'Tilt', 'Avg IVB', 'Avg HB', "Avg Ext.", "Zone%")
  png(paste0("Pitching Reports Images/", pitcher ,"- pitchingSummary.png"),height=2, width=10,units = "in",res = 300)
  table2 <- tableGrob(pitchStats, rows = NULL)
  grid.arrange(table2)
  dev.off()
  
  pitchSummaries2 <- image_read(paste0("Pitching Reports Images/", pitcher ,"- pitchingSummary.png"))
  PitchingReport2 <- image_composite(TemplatePageTwo,pitchSummaries2,offset= "+150+200")
  
  all_pitches_graph <- ggplot(pitches) +
    geom_point(aes(x = PlateLocSide * 12, y = PlateLocHeight * 12, color = TaggedPitchType)) +
    geom_rect(aes(xmin = -9, xmax = 9, ymin = 19.8, ymax = 43.8), alpha = 0.0, color = "black", linetype = "dashed") +
    coord_cartesian(xlim = c(-54, 54), ylim = c(-10, 72)) +
    labs(title = "All Pitches") +
    scale_color_manual(values = USABaseball,
                       labels = c("CH", "CB", "CU", "FB", "SI", "SL", "FS"),
                       breaks = names(USABaseball)) +
    theme_minimal() +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(size = 13, hjust = 0.5, face = "bold"))
  
  
  movement_plot <- ggplot(subset(pitches), aes(HorzBreak, InducedVertBreak, color = TaggedPitchType, stroke = 1)) +
    geom_point(na.rm = T) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    xlim(-25,25) +
    ylim(-25,25) +
    xlab("Horizontal Break") +
    ylab("Induced Vertical Break") +
    ggtitle("Break Plot") +
    scale_color_manual(values = USABaseball,
                       labels = c("CH", "CB", "CU", "FB", "SI", "SL", "FS"),
                       breaks = names(USABaseball)) +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          plot.title = element_text(size = 13, face = "bold", hjust = 0.5))
  
  release_plot <- ggplot(subset(pitches),aes(RelSide, RelHeight, color = TaggedPitchType, stroke = 1)) +
    geom_point(na.rm = T) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    xlim(-4,4) +
    ylim(0,8) +
    xlab("Release Side") +
    ylab("Release Height") +
    ggtitle("Release Points") +
    scale_color_manual(values = USABaseball,
                       labels = c("CH", "CB", "CU", "FB", "SI", "SL", "FS"),
                       breaks = names(USABaseball)) +  theme_minimal() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          plot.title = element_text(size = 13, face = "bold", hjust = 0.5))
  
  plots_row <- ggarrange(release_plot, all_pitches_graph, movement_plot, nrow = 1)
  ggsave(plots_row,file=paste0("Pitching Reports Images/", pitcher," - strikeZone.png"), width=10,height=4,units="in", dpi = 300)
  pitchCharts2 <- image_read(paste0("Pitching Reports Images/", pitcher ," - strikeZone.png"))
  PitchingReport3 <- image_composite(PitchingReport2,pitchCharts2, offset= "+150+1200")
  
  image_write(PitchingReport3,path = "page2.pdf",format="pdf",quality=100,density=300)
  
  max_speed_data <- gamefile %>%
    filter(Pitcher == pitcher) %>% 
    mutate(Date = ymd(Date)) %>%
    group_by(Date, TaggedPitchType) %>%
    summarise(RelSpeed = max(RelSpeed, na.rm = TRUE))
  
  velocity_plot <- ggplot(max_speed_data, aes(x = Date, y = RelSpeed, color = TaggedPitchType)) +
    geom_line(linewidth = 1) +
    geom_point() +
    ylim(60, 100) +
    xlab("Session") +
    ylab("Velocity") +
    ggtitle("Max Velocity by Session") +
    scale_color_manual(values = USABaseball) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
      legend.title= element_blank(),
      legend.text = element_text(size = 12)
    )
  
  ggsave(velocity_plot,file=paste0("Pitching Reports Images/", pitcher," - velocityPlot.png"),width=10,height=4,units="in", dpi = 300)
  pitchCharts3<- image_read(paste0("Pitching Reports Images/", pitcher," - velocityPlot.png"))
  PitchingReport4 <- image_composite(TemplatePageThree,pitchCharts3,offset= "+200+200")
  
  image_write(PitchingReport4,path = "page3.pdf",format="pdf",quality=100,density=300)
  
  qpdf::pdf_combine(input = c("page1.pdf", "page2.pdf", "page3.pdf"),
                    output = paste0("Pitching Reports/", pitcher, ".pdf"))
}
