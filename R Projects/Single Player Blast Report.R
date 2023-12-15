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

blastData <- read_csv("/Users/watts/Documents/Futures Performance Center/Burgers & Bombs/Robert Brantly/Robert Brantly - Sheet1.csv", skip = 8)
hittraxData <- read_csv("/Users/watts/Downloads/Copy of Hittrax Master Data - Sheet1.csv")

blastData <- blastData %>%
  rename(Environment = `Swing Details`)

hittraxData <- hittraxData %>%
  mutate(
    Level = case_when(
      Group %in% c("8U", "9U", "10U", "11U") ~ "L1",
      Group %in% c("12U", "13U", "14U") ~ "L2",
      Group %in% c("15U", "16U", "17U", "18U") ~ "L3",
      TRUE ~ "Collegiate"
    )
  ) %>%
  arrange(Name, match(Month, month.name)) %>%
  group_by(Name) %>%
  mutate(
    MaxVel_CumMax = cummax(MaxVel),
    MaxDist_CumMax = cummax(MaxDist),
  ) %>%
  mutate(
    MaxVel_Monthly_Change = round(pmax(MaxVel_CumMax - lag(MaxVel_CumMax, order_by = match(Month, month.name), default = first(MaxVel_CumMax)), 0), 1),
    MaxVel_YTD_Change = round(cumsum(coalesce(pmax(MaxVel_CumMax - lag(MaxVel_CumMax, order_by = match(Month, month.name), default = first(MaxVel_CumMax)), 0), 0)), 1),
    
    AvgVel_Monthly_Change = round(AvgVel - lag(AvgVel, order_by = match(Month, month.name)), 1),
    AvgVel_YTD_Change = round(cumsum(coalesce(AvgVel - lag(AvgVel, order_by = match(Month, month.name)), 0)), 1),
    
    MaxDist_Monthly_Change = round(pmax(MaxDist_CumMax - lag(MaxDist_CumMax, order_by = match(Month, month.name), default = first(MaxDist_CumMax)), 0), 1),
    MaxDist_YTD_Change = round(cumsum(coalesce(pmax(MaxDist_CumMax - lag(MaxDist_CumMax, order_by = match(Month, month.name), default = first(MaxDist_CumMax)), 0), 0)), 1),
    
    AvgDist_Monthly_Change = round(AvgDist - lag(AvgDist, order_by = match(Month, month.name)), 1),
    AvgDist_YTD_Change = round(cumsum(coalesce(AvgDist - lag(AvgDist, order_by = match(Month, month.name)), 0)), 1)
  ) %>%
  ungroup() %>%
  group_by(Sport, Level, Month) %>%
  mutate(
    MaxVel_Rank = rank(-MaxVel, ties.method = "min"),
    AvgVel_Rank = rank(-AvgVel, ties.method = "min"),
    MaxDist_Rank = rank(-MaxDist, ties.method = "min"),
    AvgDist_Rank = rank(-AvgDist, ties.method = "min"),
    Total_Players = n()
  ) %>%
  ungroup() %>% 
  filter(Name == "Robert Brantly" & Month == "October")

  
TemplatePageOne <- image_read_pdf("/Volumes/COLE'S DATA/Templates/Futures Hitting Template.pdf")

setwd("/Users/watts/Documents/Futures Performance Center/Burgers & Bombs/Robert Brantly")

player_name <- "Robert Brantly"

# Save the player's name as a PNG
png_filename <- paste0("Hitting Reports Images/", player_name, "- playerNameImage.png")
png(png_filename, height=1, width=2, units = "in", res = 300, bg = "transparent")
table1 <- tableGrob(player_name, rows = NULL, theme = ttheme_minimal(base_size = 14, base_colour = "black"))
grid.arrange(table1)
dev.off()

# Read the saved PNG and the main image
pitchSummaries <- image_read(png_filename)
PitchingReport0 <- image_composite(TemplatePageOne, pitchSummaries, offset = "+2550+75")

maxVel <- hittraxData %>% 
  ggplot(aes(x = `MaxVel Rank`, y = "")) +
  geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
  geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
  geom_point(aes(fill = `MaxVel Rank`), color = "black", pch = 21, size = 12) +
  geom_text(aes(label = round(`MaxVel Rank`)), size = 6, fontface = "bold") +
  labs(title = paste(round(hittraxData$MaxVel_CumMax, digits = 1), "MPH"),
       subtitle = paste("MOM Change:", hittraxData$MaxVel_Monthly_Change, 
                        "| YTD Change:", hittraxData$MaxVel_YTD_Change,
                        "\nFacility Rank:", hittraxData$MaxVel_Rank, "/", hittraxData$Total_Players)) +
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
        plot.title = element_text(hjust = 0.5, vjust = -7, face = "bold", size = 17),
        plot.subtitle = element_text(hjust = 0.5, vjust = -10, size = 14)
  )

avgVel <- hittraxData %>% 
  ggplot(aes(x = `AvgVel Rank`, y = "")) +
  geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
  geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
  geom_point(aes(fill = `AvgVel Rank`), color = "black", pch = 21, size = 12) +
  geom_text(aes(label = round(`AvgVel Rank`)), size = 6, fontface = "bold") +
  labs(title = paste(round(hittraxData$AvgVel, digits = 1), "MPH"),
       subtitle = paste("MOM Change:", hittraxData$AvgVel_Monthly_Change, 
                        "| YTD Change:", hittraxData$AvgVel_YTD_Change,
                        "\nFacility Rank:", hittraxData$AvgVel_Rank, "/", hittraxData$Total_Players)) +
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
        plot.title = element_text(hjust = 0.5, vjust = -7, face = "bold", size = 17),
        plot.subtitle = element_text(hjust = 0.5, vjust = -10, size = 14)
  )

maxDist <- hittraxData %>% 
  ggplot(aes(x = `MaxDist Rank`, y = "")) +
  geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
  geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
  geom_point(aes(fill = `MaxDist Rank`), color = "black", pch = 21, size = 12) +
  geom_text(aes(label = round(`MaxDist Rank`)), size = 6, fontface = "bold") +
  labs(title = paste(round(hittraxData$MaxDist_CumMax, digits = 1), "Feet"),
       subtitle = paste("MOM Change:", hittraxData$MaxDist_Monthly_Change, 
                        "| YTD Change:", hittraxData$MaxDist_YTD_Change,
                        "\nFacility Rank:", hittraxData$MaxDist_Rank, "/", hittraxData$Total_Players)) +
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
        plot.title = element_text(hjust = 0.5, vjust = -7, face = "bold", size = 17),
        plot.subtitle = element_text(hjust = 0.5, vjust = -10, size = 14)
  )

avgDist <- hittraxData %>% 
  ggplot(aes(x = `AvgDist Rank`, y = "")) +
  geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
  geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
  geom_point(aes(fill = `AvgDist Rank`), color = "black", pch = 21, size = 12) +
  geom_text(aes(label = round(`AvgDist Rank`)), size = 6, fontface = "bold") +
  labs(title = paste(round(hittraxData$AvgDist, digits = 1), "Feet"),
       subtitle = paste("MOM Change:", hittraxData$AvgDist_Monthly_Change, 
                        "| YTD Change:", hittraxData$AvgDist_YTD_Change,
                        "\nFacility Rank:", hittraxData$AvgDist_Rank, "/", hittraxData$Total_Players)) +
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
        plot.title = element_text(hjust = 0.5, vjust = -7, face = "bold", size = 17),
        plot.subtitle = element_text(hjust = 0.5, vjust = -10, size = 14)
  )

ranking_row <- ggarrange(maxVel, avgVel, maxDist, avgDist, nrow = 1)
ggsave(ranking_row,file=paste0("Hitting Reports Images/",player_name," - playerPercentiles.png"), width=15.5,height=3,units="in", dpi = 175)
pitchCharts <- image_read(paste0("Hitting Reports Images/",player_name," - playerPercentiles.png"))
PitchingReport1 <- image_composite(PitchingReport0, pitchCharts, offset= "+500+450")

# maxVel_rank <- hittraxData %>%
#   ggplot(aes(x = `MaxVel Rank`, y = "")) +
#   geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
#   geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
#   geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
#   geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
#   geom_point(aes(fill = `MaxVel Rank`), color = "black", pch = 21, size = 10) +
#   geom_text(aes(label = `MaxVel Rank`)) +
#   scale_fill_gradient2(low = "#2952a3", mid = "#ffffff", high = "#cc0000", midpoint = 50, limits = c(0, 100), na.value = "grey") +
#   theme(legend.position = "none",
#         panel.background =  element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.title.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.title.x = element_blank()
#   )
# 
# avgVel_rank <- hittraxData %>%
#   ggplot(aes(x = `AvgVel Rank`, y = "")) +
#   geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
#   geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
#   geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
#   geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
#   geom_point(aes(fill = `AvgVel Rank`), color = "black", pch = 21, size = 10) +
#   geom_text(aes(label = `AvgVel Rank`)) +
#   scale_fill_gradient2(low = "#2952a3", mid = "#ffffff", high = "#cc0000", midpoint = 50, limits = c(0, 100), na.value = "grey") +
#   theme(legend.position = "none",
#         panel.background =  element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.title.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.title.x = element_blank()
#   )
# 
# maxDist_rank <- hittraxData %>% 
#   ggplot(aes(x = `MaxDist Rank`, y = "")) +
#   geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
#   geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
#   geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
#   geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
#   geom_point(aes(fill = `MaxDist Rank`), color = "black", pch = 21, size = 10) +
#   geom_text(aes(label = `MaxDist Rank`)) +
#   scale_fill_gradient2(low = "#2952a3", mid = "#ffffff", high = "#cc0000", midpoint = 50, limits = c(0, 100), na.value = "grey") +
#   theme(legend.position = "none",
#         panel.background =  element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.title.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.title.x = element_blank()
#   )
# 
# hardhitAvg_rank <- hittraxData %>%
#   ggplot(aes(x = `AvgDist Rank`, y = "")) +
#   geom_segment(aes(x = 0, xend = 100, y = "", yend = ""), color = "#9b9b9b", linewidth = 1) +
#   geom_point(aes(x = 0, y = ""), color = "#9b9b9b", size = 5) +
#   geom_point(aes(x = 50, y = ""), color = "#9b9b9b", size = 5) +
#   geom_point(aes(x = 100, y = ""), color = "#9b9b9b", size = 5) +
#   geom_point(aes(fill = `AvgDist Rank`), color = "black", pch = 21, size = 10) +
#   geom_text(aes(label = `AvgDist Rank`)) +
#   scale_fill_gradient2(low = "#2952a3", mid = "#ffffff", high = "#cc0000", midpoint = 50, limits = c(0, 100), na.value = "grey") +
#   theme(legend.position = "none",
#         panel.background =  element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.title.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.title.x = element_blank()
#   )

# ranking_row <- ggarrange(maxVel_rank, avgVel_rank, maxDist_rank, hardhitAvg_rank, nrow = 1)
# ggsave(ranking_row,file=paste0("Hitting Reports Images/",player_name," - playerRankings.png"), width=15.5,height=1,units="in", dpi = 200)
# pitchCharts1 <- image_read(paste0("Hitting Reports Images/",player_name," - playerRankings.png"))
# PitchingReport1 <- image_composite(PitchingReport0, pitchCharts1, offset= "+85+750")

player_data <- blastData %>%
  group_by(Environment) %>% 
  summarise(
    `Total Swings` = n(),
    #`Bat Speed (mph)` = round(mean(`Bat Speed (mph)`, na.rm = TRUE), 1),
    `Bat Speed (mph)` = round(mean(`Bat Speed (mph)`[`Bat Speed (mph)` >= quantile(`Bat Speed (mph)`, 0.5, na.rm = TRUE)], na.rm = TRUE), 1),
    `Peak Hand Speed (mph)` = round(mean(`Peak Hand Speed (mph)`, na.rm = TRUE), 1),
    `Rotational Acceleration (g)` = round(mean(`Rotational Acceleration (g)`, na.rm = TRUE), 1),
    `Power (kW)` = round(mean(`Power (kW)`, na.rm = TRUE), 1),
    `On Plane Efficiency (%)` = round(mean(`On Plane Efficiency (%)`, na.rm = TRUE), 1),
    `Attack Angle (deg)` = round(mean(`Attack Angle (deg)`, na.rm = TRUE), 1),
    `Early Connection (deg)` = round(mean(`Early Connection (deg)`, na.rm = TRUE), 1),
    `Connection at Impact (deg)` = round(mean(`Connection at Impact (deg)`, na.rm = TRUE), 1),
    `Vertical Bat Angle (deg)` = round(mean(`Vertical Bat Angle (deg)`, na.rm = TRUE), 1)
  ) %>%
  select(Environment, `Total Swings`, `Bat Speed (mph)`, `Peak Hand Speed (mph)`, `Rotational Acceleration (g)`,
         `Power (kW)`, `On Plane Efficiency (%)`, `Attack Angle (deg)`, `Early Connection (deg)`, 
         `Connection at Impact (deg)`, `Vertical Bat Angle (deg)`) %>% 
  mutate(Environment = factor(Environment, levels = c("Tee", "Front Toss Underhand", "Pitching Machine"))) %>%
  arrange(Environment)

  names(player_data) <- c("Environment", "Total Swings", "Bat Speed\n(mph)", "Peak Hand\nSpeed (mph)", "Rotational\nAcceleration (g)", 
                        "Power\n(kW)", "On Plane\nEfficiency (%)", "Attack Angle\n(deg)", "Early Connection\n(deg)", "Connection at\nImpact (deg)", 
                        "Vertical Bat\nAngle (deg)")
  
  # Fetch the group of the current player
  current_level <- unique(hittraxData$Level)[1]
  
  # Initialize goals_data with the same columns as player_data, filled with NA values
  goals_data <- as.data.frame(matrix(NA, ncol=ncol(player_data), nrow=1))
  colnames(goals_data) <- colnames(player_data)
  
  # Set goals based on the group
  goals_data$Environment <- "Goals"
  
  if(current_level %in% c("Collegiate")) {
    goals_data$`Bat Speed\n(mph)` <- "75"
    goals_data$`Peak Hand\nSpeed (mph)` <- "26"
    goals_data$`Rotational\nAcceleration (g)` <- "16"
    goals_data$`On Plane\nEfficiency (%)` <- "85"
    goals_data$`Power\n(kW)` <- "6.0"
    goals_data$`Attack Angle\n(deg)` <- "6 - 10"
    goals_data$`Early Connection\n(deg)` <- "85 - 105"
    goals_data$`Connection at\nImpact (deg)` <- "87 - 95"
    goals_data$`Vertical Bat\nAngle (deg)` <- "-27 to -37"
  } else if(current_level %in% c("L3")) {
    goals_data$`Bat Speed\n(mph)` <- "65"
    goals_data$`Peak Hand\nSpeed (mph)` <- "23"
    goals_data$`Rotational\nAcceleration (g)` <- "13"
    goals_data$`On Plane\nEfficiency (%)` <- "80"
    goals_data$`Power\n(kW)` <- "4.5"
    goals_data$`Attack Angle\n(deg)` <- "8 - 12"
    goals_data$`Early Connection\n(deg)` <- "85 - 105"
    goals_data$`Connection at\nImpact (deg)` <- "87 - 95"
    goals_data$`Vertical Bat\nAngle (deg)` <- "-27 to -37"
  } else if(current_level %in% c("L2")) {
    goals_data$`Bat Speed\n(mph)` <- "55"
    goals_data$`Peak Hand\nSpeed (mph)` <- "20"
    goals_data$`Rotational\nAcceleration (g)` <- "10"
    goals_data$`On Plane\nEfficiency (%)` <- "70"
    goals_data$`Power\n(kW)` <- "2.75"
    goals_data$`Attack Angle\n(deg)` <- "8 - 12"
    goals_data$`Early Connection\n(deg)` <- "85 - 110"
    goals_data$`Connection at\nImpact (deg)` <- "85 - 95"
    goals_data$`Vertical Bat\nAngle (deg)` <- "-20 to -30"
  } else if(current_level %in% c("L1")) {
    goals_data$`Bat Speed\n(mph)` <- "45"
    goals_data$`Peak Hand\nSpeed (mph)` <- "17"
    goals_data$`Rotational\nAcceleration (g)` <- "7.5"
    goals_data$`On Plane\nEfficiency (%)` <- "60"
    goals_data$`Power\n(kW)` <- "1.0"
    goals_data$`Attack Angle\n(deg)` <- "5 - 15"
    goals_data$`Early Connection\n(deg)` <- "80 - 110"
    goals_data$`Connection at\nImpact (deg)` <- "80 - 100"
    goals_data$`Vertical Bat\nAngle (deg)` <- "-15 to -25"
  } else {
    goals_data$`Bat Speed\n(mph)` <- "75"
    goals_data$`Peak Hand\nSpeed (mph)` <- "26"
    goals_data$`Rotational\nAcceleration (g)` <- "16"
    goals_data$`On Plane\nEfficiency (%)` <- "85"
    goals_data$`Power\n(kW)` <- "6.0"
    goals_data$`Attack Angle\n(deg)` <- "6 - 10"
    goals_data$`Early Connection\n(deg)` <- "85 - 105"
    goals_data$`Connection at\nImpact (deg)` <- "87 - 95"
    goals_data$`Vertical Bat\nAngle (deg)` <- "-27 to -37"
  }
  
  # Combine the dataframes only if goals_data is not NULL
  if (!is.null(goals_data)) {
    combined_player_data <- rbind(player_data, goals_data)
  } else {
    combined_player_data <- player_data
  }
  
  png(paste0("Hitting Reports Images/",player_name,"- hittingSummary.png"),height=2.5, width=14,units = "in",res = 225)
  table2 <- tableGrob(combined_player_data, rows = NULL)
  grid.arrange(table2)
  dev.off()
  
  pitchSummaries2 <- image_read(paste0("Hitting Reports Images/",player_name,"- hittingSummary.png"))
  PitchingReport2 <- image_composite(PitchingReport1,pitchSummaries2,offset= "+65+900")
  
  if (!is.null(goals_data)) {
    
    bat_speed_goal <- as.numeric(goals_data$`Bat Speed\n(mph)`)
    rot_accel_goal <- as.numeric(goals_data$`Rotational\nAcceleration (g)`)
    
    power_graph <- ggplot(blastData) +
      geom_point(aes(x = `Bat Speed (mph)`, y = `Rotational Acceleration (g)`, color = Environment)) +
      coord_cartesian(xlim = c(30, 80), ylim = c(0, 30)) +
      geom_rect(aes(xmin = bat_speed_goal, xmax = Inf, ymin = rot_accel_goal, ymax = Inf), fill = "#00FF00", alpha = 0.0020) +
      labs(title = "Power", subtitle = "Bat Speed & Rotational Acceleration") +
      theme(legend.position = "none")
    
    attack_angle_goal <- strsplit(as.character(goals_data$`Attack Angle\n(deg)`), ' - ')
    attack_angle_min <- as.numeric(attack_angle_goal[[1]][1])
    attack_angle_max <- as.numeric(attack_angle_goal[[1]][2])
    on_plane_eff_goal <- as.numeric(goals_data$`On Plane\nEfficiency (%)`)
    
    contact_graph <- ggplot(blastData) +
      geom_point(aes(x = `On Plane Efficiency (%)`, y = `Attack Angle (deg)`, color = Environment)) +
      coord_cartesian(xlim = c(20, 100), ylim = c(-10, 35)) +
      geom_rect(aes(xmin = on_plane_eff_goal, xmax = Inf, ymin = attack_angle_min, ymax = attack_angle_max), fill = "#00FF00", alpha = 0.0020) +
      labs(title = "Contact", subtitle = "On Plane Eff vs Attack Angle") +
      theme(legend.position = "none")
    
    early_connection_goal <- strsplit(as.character(goals_data$`Early Connection\n(deg)`), ' - ')
    early_connection_min <- as.numeric(early_connection_goal[[1]][1])
    early_connection_max <- as.numeric(early_connection_goal[[1]][2])
    
    load_graph <- ggplot(blastData) +
      geom_point(aes(x = `Vertical Bat Angle (deg)`, y = `Early Connection (deg)`, color = Environment)) +
      coord_cartesian(xlim = c(0, -60), ylim = c(50, 130)) +
      geom_rect(aes(xmin = Inf, xmax = -Inf, ymin = early_connection_min, ymax = early_connection_max), fill = "#00FF00", alpha = 0.0020) +
      labs(title = "Load Consistancy", subtitle = "Early Connection") +
      theme(legend.position = "none")
    
    connection_impact_goal <- strsplit(as.character(goals_data$`Connection at\nImpact (deg)`), ' - ')
    connection_impact_min <- as.numeric(connection_impact_goal[[1]][1])
    connection_impact_max <- as.numeric(connection_impact_goal[[1]][2])
    
    impact_graph <- ggplot(blastData) +
      geom_point(aes(x = `Vertical Bat Angle (deg)`, y = `Connection at Impact (deg)`, color = Environment)) +
      coord_cartesian(xlim = c(0, -60), ylim = c(50, 130)) +
      geom_rect(aes(xmin = Inf, xmax = -Inf, ymin = connection_impact_min, ymax = connection_impact_max), fill = "#00FF00", alpha = 0.0020) +
      labs(title = "Impact Position", subtitle = "Connection at Impact") +
      theme(legend.position = "none")
  } else {
    power_graph <- ggplot(blastData) +
      geom_point(aes(x = `Bat Speed (mph)`, y = `Rotational Acceleration (g)`, color = Environment)) +
      coord_cartesian(xlim = c(30, 80), ylim = c(0, 30)) +
      labs(title = "Power", subtitle = "Bat Speed & Rotational Acceleration") +
      theme(legend.position = "none")
    
    contact_graph <- ggplot(blastData) +
      geom_point(aes(x = `On Plane Efficiency (%)`, y = `Attack Angle (deg)`, color = Environment)) +
      coord_cartesian(xlim = c(20, 100), ylim = c(-10, 35)) +
      labs(title = "Contact", subtitle = "On Plane Eff vs Attack Angle") +
      theme(legend.position = "none")
    
    load_graph <- ggplot(blastData) +
      geom_point(aes(x = `Vertical Bat Angle (deg)`, y = `Early Connection (deg)`, color = Environment)) +
      coord_cartesian(xlim = c(0, -60), ylim = c(50, 130)) +
      labs(title = "Load Consistancy", subtitle = "Early Connection") +
      theme(legend.position = "none")
    
    impact_graph <- ggplot(blastData) +
      geom_point(aes(x = `Vertical Bat Angle (deg)`, y = `Connection at Impact (deg)`, color = Environment)) +
      coord_cartesian(xlim = c(0, -60), ylim = c(50, 130)) +
      labs(title = "Impact Position", subtitle = "Connection at Impact") +
      theme(legend.position = "none")
  }
  
  plots_row <- ggarrange(power_graph, contact_graph, load_graph, impact_graph, nrow = 1, common.legend = TRUE, legend = "bottom")
  ggsave(plots_row,file=paste0("Hitting Reports Images/",player_name," - swingProfile.png"), width=14,height=4.5,units="in", dpi = 225)
  pitchCharts2 <- image_read(paste0("Hitting Reports Images/",player_name," - swingProfile.png"))
  PitchingReport3 <- image_composite(PitchingReport2,pitchCharts2, offset= "+50+1500")
  
  image_write(PitchingReport3,path = "page1.pdf",format="pdf",quality=100,density=300)
  image_write(PitchingReport3,path = paste0("Hitting Reports/",player_name,".pdf"),format="pdf")
  

