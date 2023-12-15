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

clientData <- read.csv("/Users/watts/Downloads/ClientList.csv")
attendanceData <- read.csv("/Users/watts/Downloads/Client Analysis, September 01, 2023 - October 31, 2023-20230901-20231031.csv")
proteusData<- read_csv("/Users/watts/Downloads/ProteusPercentiles.csv")
teambuildrData <- read_csv("/Users/watts/Downloads/Teambuildr Raw Data Report (Updated) - Sheet1.csv")
CMJdata <- read_csv("/Users/watts/Downloads/CMJpercentiles.csv")
ISOSQTdata <- read_csv("/Users/watts/Downloads/ISO_SquatPercentiles.csv")

clientData <- clientData %>% 
  rename(Name = Client)

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
clientData$Age <- sapply(clientData$`field.general.7.dl_date`, calculate_age)

merged_data <- left_join(teambuildrData, clientData, by="Name")

TemplatePageOne <- image_read_pdf("/Volumes/COLE'S DATA/Templates/Strength Report Template.pdf")

setwd("/Users/watts/Documents/Futures Performance Center/Test")


exercises <- c('Barbell Back Squat', 'Trap Bar Deadlift', 'Barbell Bench Press', 
               'Straight Arm Trunk Rotation Max Isometric Test - Crane Scale', 'Cable Lat Pull Down (1)')

merged_data$'Highest Max' <- as.numeric(as.character(merged_data$'Highest Max'))
merged_data$'Highest Max' <- round(merged_data$'Highest Max')

filteredExercise_data <- merged_data %>%
  filter(`Exercise Name` %in% exercises)

for (exercise in exercises) {
  
  exercise_data <- filteredExercise_data %>% filter(`Exercise Name` == exercise)
  
  Q1 <- quantile(exercise_data$`Highest Max`, 0.25, na.rm = TRUE)
  Q3 <- quantile(exercise_data$`Highest Max`, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Filter out the outliers
  filteredExercise_data <- filteredExercise_data %>%
    filter(!(`Exercise Name` == exercise & (`Highest Max` < lower_bound | `Highest Max` > upper_bound)))
} 

########################################################################################################
#############################################PLAYER PROFILE#############################################
########################################################################################################


player_profile <- clientData %>%
  filter(Name == "Aaron Springston") %>%
  select(Name, Age, Gender, Reporting.Level..Age.Dependent., Position..Baseball.Softball., Height)
names(player_profile) <- c("Name:", "Age:", "Gender:", "Level:", "Position:", "Height:")

player_profile_two <- proteusData %>%
  filter(Name == "Aaron Springston") %>%
  select(weight) %>%
  slice(1)
names(player_profile_two) <- c("Weight:")

combined_profile <- cbind(player_profile, player_profile_two)

combined_profile$`Weight:` <- paste(combined_profile$`Weight:`, "lbs")

combined_profile <- combined_profile %>% 
  mutate(across(c(`Name:`, `Age:`, `Gender:`, `Level:`, `Position:`, `Weight:`, `Height:`), as.character)) %>%
  pivot_longer(cols = c(`Name:`, `Age:`, `Gender:`, `Level:`, `Position:`, `Weight:`, `Height:`), names_to = "label", values_to = "value")

# Define the coordinates for the labels and values
left_labels <- c("Name:", "Age:", "Level:", "Weight:")
right_labels <- c("Gender:", "Height:", "Position:")
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
             hjust = 0, color = "#3d9be9", size = 8, fontface = "bold", family = "Good Times") +
    annotate("text", x = left_x + 0.1, y = y_positions[i], label = combined_profile$value[combined_profile$label == left_labels[i]], 
             hjust = 0, color = "white", size = 10, family = "Good Times")
}

# Add right labels and their values
for (i in 1:length(right_labels)) {
  p <- p + 
    annotate("text", x = right_x, y = y_positions[i + 1], label = right_labels[i], 
             hjust = 0, color = "#3d9be9", size = 8, fontface = "bold", family = "Good Times") +
    annotate("text", x = right_x + 0.1, y = y_positions[i + 1], label = combined_profile$value[combined_profile$label == right_labels[i]], 
             hjust = 0, color = "white", size = 10, family = "Good Times")
}

ggsave(p,file=paste0("Strength Reports Images/","playerSummary.png"), width=6,height=2,units="in", dpi = 175)
strengthSummarys <- image_read(paste0("Strength Reports Images/","playerSummary.png"))
PitchingReport0 <- image_composite(TemplatePageOne,strengthSummarys,offset= "+100+550")

########################################################################################################
#############################################  ATTENDANCE  #############################################
########################################################################################################

attendance_plot_data <- attendanceData %>%
  filter(Client.name == "Connor Cannon") %>% 
  mutate(`Attendance Score` = round((Attended / 26) * 3, digits = 1))

attendance_score <- max(attendance_plot_data$`Attendance Score`, na.rm = TRUE)

get_color <- function(score) {
  if (score < 1.5) {
    return("red")
  } else if (score >= 1.5 & score < 2) {
    return("orange")
  } else if (score >= 2 & score < 2.5) {
    return("green")
  } else {
    return("dodgerblue")
  }
}

attendance_plot <- attendance_plot_data %>% 
  ggplot(aes(x = Client.name, y = `Attendance Score`)) +
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

ggsave(attendance_plot,file=paste0("Strength Reports Images/","attendancePlot.png"), width=6,height=1,units="in", dpi = 150)
attendancePlot <- image_read(paste0("Strength Reports Images/","attendancePlot.png"))
PitchingReport1 <- image_composite(PitchingReport0, attendancePlot, offset= "+200+1665")

########################################################################################################
#############################################     CORE     #############################################
########################################################################################################

proteusData$`session createdAt` <- as.Date(proteusData$`session createdAt`, format="%Y-%m-%dT%H:%M:%S")

core_ylim_min <- min(proteusData$`power - mean`[proteusData$`exercise name` == "Straight Arm Trunk Rotation"], na.rm = TRUE)
core_ylim_max <- max(proteusData$`power - mean`[proteusData$`exercise name` == "Straight Arm Trunk Rotation"], na.rm = TRUE)

corePower_graph_data <- proteusData %>% 
  filter(Name == "Connor Cannon" & `exercise name` == "Straight Arm Trunk Rotation") %>%
  group_by(`session createdAt`) %>%
  summarize(`power - mean` = max(`power - mean`, na.rm = TRUE),
            PowerPercentileRank = max(PowerPercentileRank, na.rm = TRUE)) %>%
  ungroup()

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

corePower_graph <- corePower_graph_data %>% 
  ggplot(aes(x = `session createdAt` , y = `power - mean`, color = "Straight Arm Trunk Rotation")) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  labs(title = "Core Strength  vs  Time") +
  ylim(core_ylim_min, core_ylim_max) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(color = "white", size = 17),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text=element_text(color = "white", size = 15),
        plot.title = element_text(hjust = 0.5, color = "white", size = 18, family = "Good Times")
  )

max_core <- max(filteredExercise_data$`Highest Max`[filteredExercise_data$`Exercise Name` == "Straight Arm Trunk Rotation Max Isometric Test - Crane Scale"], na.rm = TRUE)

core_text <- ggplot() + 
  annotate("text", x = 0, y = 0, label = paste("Core Rotation ISO Max:", max_core, "N"), size = 7, family = "Good Times", color = "white") + 
  theme_void() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"))

coreAcc_graph_data <- proteusData %>%
  filter(Name == "Connor Cannon" & `exercise name` == "Straight Arm Trunk Rotation") %>%
  group_by(`session createdAt`) %>%
  summarize(`acceleration - mean` = max(`acceleration - mean`, na.rm = TRUE),
            AccelerationPercentileRank = max(AccelerationPercentileRank, na.rm = TRUE)) %>%
  ungroup()

maxAcc <- max(coreAcc_graph_data$`acceleration - mean`, na.rm = TRUE)

core_acc_percentile_graph <- coreAcc_graph_data %>% 
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
  annotate("text", x = 100, y = 1, label = paste(round(maxAcc, digits = 1), " m/s2", sep = ""), hjust = 1, vjust = -2.5, color = "white", size = 12, family = "Good Times")

ggsave(corePower_graph,file=paste0("Strength Reports Images/","coreStrengthPercentiles.png"), width=6,height=3,units="in", dpi = 150)
strengthCharts1 <- image_read(paste0("Strength Reports Images/","coreStrengthPercentiles.png"))
PitchingReport2 <- image_composite(PitchingReport1, strengthCharts1, offset= "+1425+900")

ggsave(core_power_percentile_graph,file=paste0("Strength Reports Images/","coreSpeedPercentiles.png"), width=7,height=2,units="in", dpi = 150)
strengthCharts2 <- image_read(paste0("Strength Reports Images/","coreSpeedPercentiles.png"))
PitchingReport3 <- image_composite(PitchingReport2, strengthCharts2, offset= "+1350+1375")

ggsave(core_acc_percentile_graph,file=paste0("Strength Reports Images/","coreStrengthPlot.png"), width=7,height=2,units="in", dpi = 150)
strengthCharts3 <- image_read(paste0("Strength Reports Images/","coreStrengthPlot.png"))
PitchingReport4 <- image_composite(PitchingReport3, strengthCharts3, offset= "+1350+1575")

ggsave(core_text,file=paste0("Strength Reports Images/","coreText.png"), width=5,height=1,units="in", dpi = 150)
strengthCharts4 <- image_read(paste0("Strength Reports Images/","coreText.png"))
PitchingReport5 <- image_composite(PitchingReport4, strengthCharts4, offset= "+1500+1775")

########################################################################################################
#############################################     LEGS     #############################################
########################################################################################################

ISOSQTdata$Date <- as.Date(ISOSQTdata$Date, format="%m/%d/%Y")
CMJdata$Date <- as.Date(CMJdata$Date, format="%m/%d/%Y")

legs_ylim_min <- min(ISOSQTdata$`Peak Vertical Force [N]`, na.rm = TRUE)
legs_ylim_max <- max(ISOSQTdata$`Peak Vertical Force [N]`, na.rm = TRUE)

ISOSQT_graph_data <- ISOSQTdata %>% 
  filter(Name == "Connor Cannon") %>%
  group_by(Date) %>%
  summarise(`Peak Vertical Force [N]` = max(`Peak Vertical Force [N]`, na.rm = TRUE),
            PercentileRank = max(PercentileRank),
            `Peak Vertical Force % (Asym) (%)` = max(`Peak Vertical Force % (Asym) (%)`, na.rm = TRUE)) %>%
  ungroup()

maxISOSQT <- max(ISOSQT_graph_data$`Peak Vertical Force [N]`, na.rm = TRUE)

# Processing the asymmetry data
ISOSQT_graph_data$AsymmetryValue <- as.numeric(gsub("[^0-9.]", "", ISOSQT_graph_data$`Peak Vertical Force % (Asym) (%)`))
ISOSQT_graph_data$AsymmetrySide <- ifelse(grepl("R", ISOSQT_graph_data$`Peak Vertical Force % (Asym) (%)`), "% R", "% L")

# Filtering the asymmetry data based on the 15% threshold
max_asymmetry_value <- max(ISOSQT_graph_data$AsymmetryValue[ISOSQT_graph_data$AsymmetryValue > 15], na.rm = TRUE)
max_asymmetry_side <- ISOSQT_graph_data$AsymmetrySide[which.max(ISOSQT_graph_data$AsymmetryValue == max_asymmetry_value)]
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
  annotate("text", x = 0, y = 1, label = "Strength Max: ", hjust = 0, vjust = vjust_value, color = "white", size = 12, family = "Good Times") +
  annotate("text", x = 100, y = 1, label = paste(round(maxISOSQT, digits = 1), " N", sep = ""), hjust = 1, vjust = vjust_value, color = "white", size = 12, family = "Good Times") +
  asymmetry_annotations


ISOSQT_graph <- ISOSQT_graph_data %>%
  ggplot(aes(x = Date, y = `Peak Vertical Force [N]`, color = "Isometric Belt Squat")) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  labs(title = "Lower Body Strength  vs  Time") +
  ylim(legs_ylim_min, legs_ylim_max) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(color = "white", size = 17),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text=element_text(color = "white", size = 15),
        plot.title = element_text(hjust = 0.5, color = "white", size = 18, family = "Good Times")
  )

max_deadlift <- max(filteredExercise_data$`Highest Max`[filteredExercise_data$`Exercise Name` == "Trap Bar Deadlift"], na.rm = TRUE)
max_squat <- max(filteredExercise_data$`Highest Max`[filteredExercise_data$`Exercise Name` == "Barbell Back Squat"], na.rm = TRUE)

lowerbody_text <- ggplot() + 
  annotate("text", x = 0, y = 0, label = paste("Trap Bar Deadift Max:", max_deadlift, "LBS  -  ", 
                                               "Squat Max:", max_squat, "LBS"), size = 7, family = "Good Times", color = "white") + 
  theme_void() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"))

CMJ_graph_data <- CMJdata %>% 
  filter(Name == "Connor Cannon") %>%
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

ggsave(ISOSQT_graph,file=paste0("Strength Reports Images/","ISOSqtPerctiles.png"), width=6,height=3,units="in", dpi = 150)
strengthCharts5 <- image_read(paste0("Strength Reports Images/","ISOSqtPerctiles.png"))
PitchingReport6 <- image_composite(PitchingReport5, strengthCharts5, offset= "+175+2175")

ggsave(ISOSQT_percentile_graph,file=paste0("Strength Reports Images/","CMJPercentiles.png"), width=7,height=3,units="in", dpi = 150)
strengthCharts6 <- image_read(paste0("Strength Reports Images/","CMJPercentiles.png"))
PitchingReport7 <- image_composite(PitchingReport6, strengthCharts6, offset= "+110+2600")

ggsave(CMJ_percentile_graph,file=paste0("Strength Reports Images/","legsPlot.png"), width=7,height=3,units="in", dpi = 150)
strengthCharts7 <- image_read(paste0("Strength Reports Images/","legsPlot.png"))
PitchingReport8 <- image_composite(PitchingReport7, strengthCharts7, offset= "+110+2825")

ggsave(lowerbody_text,file=paste0("Strength Reports Images/","lowerbodyText.png"), width=8,height=1,units="in", dpi = 150)
strengthCharts8 <- image_read(paste0("Strength Reports Images/","lowerbodyText.png"))
PitchingReport9 <- image_composite(PitchingReport8, strengthCharts8, offset= "+40+3075")

########################################################################################################
############################################# UPPER BODY  ##############################################
########################################################################################################

arms_ylim_min <- min(proteusData$`power - mean`[proteusData$`exercise name` == "Horizontal Row (One Hand)"], na.rm = TRUE)
arms_ylim_max <- max(proteusData$`power - mean`[proteusData$`exercise name` == "Horizontal Row (One Hand)"], na.rm = TRUE)

# Existing data processing
pushpull_power_graph_data <- proteusData %>% 
  filter(Name == "Connor Cannon" & 
           (`exercise name` == "Chest Press (One Hand)" | `exercise name` == "Horizontal Row (One Hand)" | `exercise name` == "PushPull")) %>%
  group_by(`session createdAt`, `exercise name`) %>%
  summarize(`power - mean` = mean(`power - mean`, na.rm = TRUE),
            PowerPercentileRank = max(PowerPercentileRank, na.rm = TRUE)) %>%
  ungroup()

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


# Creating the plot
pushpull_power_graph <- pushpull_power_graph_data %>%
  filter(`exercise name` == "Chest Press (One Hand)" | `exercise name` == "Horizontal Row (One Hand)") %>% 
  ggplot(aes(x = `session createdAt`, group = `exercise name`)) +
  geom_point(aes(y = `power - mean`, color = `exercise name`), size = 4) +
  geom_line(aes(y = `power - mean`, color = `exercise name`), size = 2) +
  labs(title = "Upper Body Strength  vs  Time") +
  ylim(arms_ylim_min, arms_ylim_max) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(color = "white", size = 17),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text=element_text(color = "white", size = 15),
        plot.title = element_text(hjust = 0.5, color = "white", size = 18, family = "Good Times")
  )

max_bench <- max(filteredExercise_data$`Highest Max`[filteredExercise_data$`Exercise Name` == "Barbell Bench Press"], na.rm = TRUE)
max_pulldown <- max(filteredExercise_data$`Highest Max`[filteredExercise_data$`Exercise Name` == "Cable Lat Pull Down (1)"], na.rm = TRUE)

# Create a text plot for max_deadlift
upperbody_text <- ggplot() + 
  annotate("text", x = 0, y = 0, label = paste("Bench Press Max:", max_bench, "LBS  -  ", 
                                               "Lat Pulldown Max:", max_pulldown, "LBS"), size = 7, family = "Good Times", color = "white") + 
  theme_void() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"))

pushpull_acc_graph_data <- proteusData %>% 
  filter(Name == "Connor Cannon" & 
           (`exercise name` == "Chest Press (One Hand)" | `exercise name` == "Horizontal Row (One Hand)" | `exercise name` == "PushPull")) %>%
  group_by(`session createdAt`, `exercise name`) %>%
  summarize(`acceleration - mean` = mean(`acceleration - mean`, na.rm = TRUE),
            AccelerationPercentileRank = max(AccelerationPercentileRank, na.rm = TRUE)) %>%
  ungroup()

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

ggsave(pushpull_power_graph,file=paste0("Strength Reports Images/","pushpullStrengthPercentile.png"), width=6,height=3,units="in", dpi = 150)
strengthCharts9 <- image_read(paste0("Strength Reports Images/","pushpullStrengthPercentile.png"))
PitchingReport10 <- image_composite(PitchingReport9, strengthCharts9, offset= "+1425+2175")

ggsave(pushpull_power_percentile_graph,file=paste0("Strength Reports Images/","pushpullSpeedPercentile.png"), width=7,height=3,units="in", dpi = 150)
strengthCharts10 <- image_read(paste0("Strength Reports Images/","pushpullSpeedPercentile.png"))
PitchingReport11 <- image_composite(PitchingReport10, strengthCharts10, offset= "+1350+2600")

ggsave(pushpull_acc_percentile_graph,file=paste0("Strength Reports Images/","pushpullPlot.png"), width=7,height=3,units="in", dpi = 150)
strengthCharts11 <- image_read(paste0("Strength Reports Images/","pushpullPlot.png"))
PitchingReport12 <- image_composite(PitchingReport11, strengthCharts11, offset= "+1350+2825")

ggsave(upperbody_text,file=paste0("Strength Reports Images/","upperbodyText.png"), width=8,height=1,units="in", dpi = 150)
strengthCharts12 <- image_read(paste0("Strength Reports Images/","upperbodyText.png"))
PitchingReport13 <- image_composite(PitchingReport12, strengthCharts12, offset= "+1285+3075")

########################################################################################################
###########################################  Strength Score  ###########################################
########################################################################################################

core_power_percentile_score <- max(corePower_graph_data$PowerPercentileRank, na.rm = TRUE)
core_acc_percentile_score <- max(coreAcc_graph_data$AccelerationPercentileRank, na.rm = TRUE)
legs_power_percentile_score <- max(ISOSQT_graph_data$PercentileRank, na.rm = TRUE)
legs_acc_percentile_score <- max(CMJ_graph_data$PercentileRank, na.rm = TRUE)
pushpull_power_percentile_score <- max(pushpull_power_graph_data$PowerPercentileRank, na.rm = TRUE)
pushpull_acc_percentile_score <- max(pushpull_acc_graph_data$AccelerationPercentileRank, na.rm = TRUE)

strengthScore <- (round((core_power_percentile_score + core_acc_percentile_score + legs_power_percentile_score + 
                           legs_acc_percentile_score + pushpull_power_percentile_score + 
                           pushpull_acc_percentile_score) / 6, digits = 1))


get_color_two <- function(score) {
  if (score < 50) {
    return("red")
  } else if (score >= 50 & score < 70) {
    return("orange")
  } else if (score >= 70 & score < 90) {
    return("green")
  } else {
    return("dodgerblue")
  }
}

strengthScore_plot <- attendance_plot_data %>% 
  ggplot(aes(x = Client.name, y = strengthScore)) +
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

ggsave(strengthScore_plot,file=paste0("Strength Reports Images/","strengthScore.png"), width=6,height=1,units="in", dpi = 150)
strengthScorePlot <- image_read(paste0("Strength Reports Images/","strengthScore.png"))
PitchingReport14 <- image_composite(PitchingReport13, strengthScorePlot, offset= "+1425+475")

image_write(PitchingReport14,path = "page1.pdf",format="pdf",quality=100,density=300)
image_write(PitchingReport14,path = paste0("Strength Reports/", "StrengthReportTest.pdf"),format="pdf")

# exercises <- c('Barbell Back Squat', 'Trap Bar Deadlift', 'Barbell Bench Press', 'Barbell Hip Thrust', 
#                'Cable Lat Pull Down (1)', 'Barbell Back Split Squat (2)')
# 
# merged_data$'Highest Max' <- as.numeric(as.character(merged_data$'Highest Max'))
# merged_data$'Highest Max' <- round(merged_data$'Highest Max')
# 
# filteredExercise_data <- merged_data %>%
#   filter(`Exercise Name` %in% exercises)
#          
# for (exercise in exercises) {
#   
#   exercise_data <- filteredExercise_data %>% filter(`Exercise Name` == exercise)
#   
#   Q1 <- quantile(exercise_data$`Highest Max`, 0.25, na.rm = TRUE)
#   Q3 <- quantile(exercise_data$`Highest Max`, 0.75, na.rm = TRUE)
#   IQR <- Q3 - Q1
#   
#   lower_bound <- Q1 - 1.5 * IQR
#   upper_bound <- Q3 + 1.5 * IQR
#   
#   # Filter out the outliers
#   filteredExercise_data <- filteredExercise_data %>%
#     filter(!(`Exercise Name` == exercise & (`Highest Max` < lower_bound | `Highest Max` > upper_bound)))
# }         
#          
# # Step 1: Rank athletes within each exercise for the whole facility
# unique_result <- filteredExercise_data %>%
#   arrange(Name, `Exercise Name`, desc(`Highest Max`)) %>%
#   distinct(Name, `Exercise Name`, .keep_all = TRUE) %>%
#   arrange(`Completed Date`) %>%
#   group_by(`Exercise Name`) %>%
#   mutate(`Facility Rank` = rank(-`Highest Max`, ties.method = "min")) %>%
#   ungroup()
# 
# # Step 2: Rank athletes within each exercise and level
# unique_result <- unique_result %>%
#   group_by(`Exercise Name`, Reporting.Level..Age.Dependent.) %>%
#   mutate(`Level Rank` = rank(-`Highest Max`, ties.method = "min")) %>%
#   ungroup()
# 
# # Step 3: Count Total People in each facility and level
# total_people <- unique_result %>%
#   group_by(`Exercise Name`) %>%
#   summarise(TotalPeopleInFacility = n()) %>%
#   ungroup()
# 
# total_people_in_level <- unique_result %>%
#   group_by(`Exercise Name`, Reporting.Level..Age.Dependent.) %>%
#   summarise(TotalPeopleInLevel = n()) %>%
#   ungroup()
# 
# # Step 4: Join the total counts back to the original data
# final_weightroom_data <- unique_result %>%
#   left_join(total_people, by = "Exercise Name") %>%
#   left_join(total_people_in_level, by = c("Exercise Name", "Reporting.Level..Age.Dependent."))
# 
# # Step 5: Filter for Aaron Springston and summarise
# weightroom_data <- final_weightroom_data %>% 
#   filter(Name == "Aaron Springston") %>%
#   group_by(`Exercise Name`) %>%
#   summarise(`Highest Max` = max(`Highest Max`, na.rm = TRUE),
#             `Facility Rank` = paste0(first(`Facility Rank`), "/", first(TotalPeopleInFacility)),
#             `Level Rank` = paste0(first(`Level Rank`), "/", first(TotalPeopleInLevel)),
#             .groups = "drop")
#   
# png(paste0("Strength Reports Images/","weightroomSummary.png"),height=2, width=5.5,units = "in",res = 250)
# table2 <- tableGrob(weightroom_data, rows = NULL)
# grid.arrange(table2)
# dev.off()
# 
# strengthSummarys1 <- image_read(paste0("Strength Reports Images/","weightroomSummary.png"))
# PitchingReport11 <- image_composite(TemplatePageTwo,strengthSummarys1,offset= "+600+2000")
# 
# image_write(PitchingReport11,path = "page2.pdf",format="pdf",quality=100,density=300)
# qpdf::pdf_combine(input = c("page1.pdf", "page2.pdf"),
#                   output = paste0("Strength Reports/", "test.pdf"))
