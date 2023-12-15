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

# Read the CSV file into the gamefile data table
gamefile <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Trackman for Luke.csv")

pitchers <- unique(gamefile$Pitcher)

TemplatePageOne <- image_read_pdf("/Users/watts/Documents/Futures Performance Center/Templates/FuturesPitchingTemplate.pdf")
TemplatePageTwo <- image_read_pdf("/Users/watts/Documents/USA Baseball/Page 2 Report Template.pdf")

USABaseball <- c("Changeup"="#42d4f4", "Curveball"="#3cb44b", "Cutter"="#ffe119", "Fastball"="#e6194B", "Sinker"="#f58231", "Slider"="#000075", "Splitter"="#f032e6")

setwd("/Users/watts/Documents/Futures Performance Center")

for (i in 1:length(pitchers)) {
  
  pitches <- subset(gamefile, Pitcher == pitchers[i])
  
  # Pitching Line
  HBP <- sum(pitches$PitchCall == "HitByPitch")
  Strikes <- sum(pitches$PitchCall == "StrikeCalled")
  Balls <- sum(pitches$PitchCall == "BallCalled")

  # Regular Data
  totalPitches <- nrow(pitches)

  # tableSlash <- as.data.frame(cbind(pitchers[i], totalPitches, PAs, ABs, Hits, Runs, Singles, Doubles, Triples, HomeRuns, Ks, BBs, HBP, format(PitchesPerPA, digits = 3)))
  # colnames(tableSlash) <- c("Pitcher", "Pitches", "IP", "PA", "AB", "H", "R", "1B", "2B", "3B", "HR", "SO", "BB", "HBP", "Pitches Per PA")
  # png(paste0("Pitching Reports Images/",pitches$PitcherTeam[1]," vs. ",pitches$BatterTeam[1],"-",pitchers[i],"- pitchingSummary.png"), height=1, width=8.5,units = "in",res = 300)
  # table1 <- tableGrob(tableSlash, rows = NULL)
  # grid.arrange(table1)
  # dev.off()
  # 
  
  additional_pitch_stats <- pitches %>%
    group_by(TaggedPitchType) %>%
    summarize(`#` = n(),
              AvgVelo = format(round(mean(RelSpeed, na.rm = T), 1)),
              MaxVelo = format(round(max(RelSpeed), 1)),
              MinVelo = format(round(min(RelSpeed), 1)),
              AvgSpinRate = format(round(mean(SpinRate, na.rm = T), 0)),
              Tilt = format(median(strptime(Tilt, "%H:%M"), na.rm = T), "%H:%M"),
              AvgIVB = format(round(mean(InducedVertBreak, na.rm = T), 1)),
              AvgHB = format(round(mean(HorzBreak, na.rm = T), 1)),
              AvgExtension = format(round(mean(Extension, na.rm = T), 1))) %>%
    arrange(desc(AvgVelo))
  colnames(additional_pitch_stats) <- c('Pitch Type', '#', 'Avg Velo', 'Max Velo', 'Min Velo', 'Avg Spin', 'Tilt', 'Avg IVB', 'Avg HB', "Avg Ext.")
  png(paste0("Pitching Reports Images/",pitchers[i],"- pitchingSummary.png"),height=1.9, width=8,units = "in",res = 300)
  table2 <- tableGrob(additional_pitch_stats, rows = NULL)
  grid.arrange(table2)
  dev.off()
  
  pitchSummaries <- image_read(paste0("Pitching Reports Images/",pitchers[i],"- pitchingSummary.png"))
  PitchingReport0 <- image_composite(TemplatePageOne,pitchSummaries,offset= "+450+450")
  
  all_pitches_graph <- ggplot(pitches) +
    geom_point(aes(x = PlateLocSide * 12, y = PlateLocHeight * 12, color = TaggedPitchType)) +
    geom_rect(aes(xmin = -12.5, xmax = 12.5, ymin = 16.8, ymax = 46.8), alpha = 0.0, color = "black", linetype = "dashed") +
    coord_cartesian(xlim = c(-54, 54), ylim = c(-10, 72)) +
    labs(title = "All Pitches") +
    scale_color_manual(values = USABaseball,
                       labels = c("CH", "CB", "CU", "FB", "SI", "SL", "FS"),
                       breaks = names(USABaseball)) +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(size = 13, hjust = 0.5, face = "bold"))
  
  bip_graph <- ggplot(subset(pitches, PitchCall == "InPlay")) +
    geom_point(aes(x = PlateLocSide * 12, y = PlateLocHeight * 12, color = TaggedPitchType)) +
    geom_rect(aes(xmin = -12.5, xmax = 12.5, ymin = 16.8, ymax = 46.8), alpha = 0.0, color = "black", linetype = "dashed") +
    coord_cartesian(xlim = c(-54, 54), ylim = c(-10, 72)) +
    labs(title = "Balls in Play") +
    scale_color_manual(values = USABaseball,
                       labels = c("CH", "CB", "CU", "FB", "SI", "SL", "FS"),
                       breaks = names(USABaseball)) +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(size = 13, hjust = 0.5, face = "bold"))
  
  swing_miss_graph <- ggplot(subset(pitches, PitchCall == "StrikeSwinging")) +
    geom_point(aes(x = PlateLocSide * 12, y = PlateLocHeight * 12, color = TaggedPitchType)) +
    geom_rect(aes(xmin = -12.5, xmax = 12.5, ymin = 16.8, ymax = 46.8), alpha = 0.0, color = "black", linetype = "dashed") +
    coord_cartesian(xlim = c(-54, 54), ylim = c(-10, 72)) +
    labs(title = "Swing & Miss") +
    scale_color_manual(values = USABaseball,
                       labels = c("CH", "CB", "CU", "FB", "SI", "SL", "FS"),
                       breaks = names(USABaseball)) +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(size = 13, hjust = 0.5, face = "bold"))
  
  plots_row <- ggarrange(all_pitches_graph, bip_graph, swing_miss_graph, nrow = 1)
  ggsave(plots_row,file=paste0("Pitching Reports Images/",pitchers[i]," - strikeZone.png"), width=10,height=4,units="in", dpi = 300)
  pitchCharts <- image_read(paste0("Pitching Reports Images/",pitchers[i]," - strikeZone.png"))
  PitchingReport1 <- image_composite(PitchingReport0,pitchCharts, offset= "+150+1300")
  
  image_write(PitchingReport1,path = "page1.pdf",format="pdf",quality=100,density=300)
  
  movement_plot <- ggplot(subset(pitches), aes(HorzBreak, InducedVertBreak, color = TaggedPitchType, stroke = 1)) +
    geom_point(na.rm = T) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    xlim(-25,25) +
    ylim(-25,25) +
    xlab("Horizontal Break") +
    ylab("Induced Vertical Break") +
    ggtitle("Break Plot") +
    scale_color_manual(values = USABaseball) +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          plot.title = element_text(size = 13, face = "bold", hjust = 0.5))+
    guides(color = guide_legend(nrow = 2))
  
  release_plot <- ggplot(subset(pitches),aes(RelSide, RelHeight, color = TaggedPitchType, stroke = 1)) +
    geom_point(na.rm = T) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    xlim(-4,4) +
    ylim(0,8) +
    xlab("Release Side") +
    ylab("Release Height") +
    ggtitle("Release Points") +
    scale_color_manual(values = USABaseball) +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          plot.title = element_text(size = 13, face = "bold", hjust = 0.5))+
    guides(color = guide_legend(nrow = 2))
  
  
  pitch_usage = pitches %>%
    group_by(TaggedPitchType) %>%
    summarise(Total = n()) %>%
    mutate(per=paste0(round(Total/sum(Total)*100, 1), "%"))
  
  pitch_usage <- pitch_usage %>%
    arrange(desc(TaggedPitchType)) %>%
    mutate(prop = Total / sum(pitch_usage$Total) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop)
  
  pitch_usage_plot <- ggplot(pitch_usage, aes(x="", y=prop, fill=TaggedPitchType)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    theme_void() +
    geom_text(aes(y = ypos, label = per), color = "white", size=5) +
    scale_fill_manual(values = USABaseball,
                      labels = c("CH", "CB", "CU", "FB", "SI", "SL", "FS"),
                      breaks = names(USABaseball)) +
    labs(title = "Pitch Usage") +
    theme(legend.position = "bottom",
          legend.title= element_blank(),
          plot.title = element_text(size = 13, face = "bold", hjust = 0.5))
  
  
  move_rel_plots <- ggarrange(movement_plot, pitch_usage_plot, release_plot, nrow = 1)
  ggsave(move_rel_plots,file=paste0("Pitching Reports Images/",pitchers[i]," - pitchUsage.png"),width=10,height=4,units="in", dpi = 300)
  pitchCharts2 <- image_read(paste0("Pitching Reports Images/",pitchers[i]," - pitchUsage.png"))
  PitchingReport3 <- image_composite(TemplatePageTwo,pitchCharts2, offset= "+150+100")
  
  
  velocity_plot <- ggplot(subset(pitches),aes(PitchNo, RelSpeed, color = TaggedPitchType)) +
    geom_line(linewidth = 1) +
    geom_point() +
    ylim(60,100) +
    xlab("Pitch #") +
    ylab("Velocity") +
    ggtitle("Velocity by Pitch") +
    scale_color_manual(values = USABaseball) +
    theme_minimal()+
    theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
          legend.title= element_blank(),
          legend.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)
    )
  
  ggsave(velocity_plot,file=paste0("Pitching Reports Images/",pitchers[i]," - velocityPlot.png"),width=10,height=3.5,units="in", dpi = 300)
  pitchCharts3<- image_read(paste0("Pitching Reports Images/",pitchers[i]," - velocityPlot.png"))
  PitchingReport4 <- image_composite(PitchingReport3,pitchCharts3,offset= "+300+1400")
  
  image_write(PitchingReport4,path = "page2.pdf",format="pdf",quality=100,density=300)
  
  qpdf::pdf_combine(input = c("page1.pdf", "page2.pdf"),
                    output = paste0("Pitching Reports/", pitchers[i],".pdf"))
}

