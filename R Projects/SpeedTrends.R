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

sprint_data <- read_csv("/Users/watts/Downloads/smartspeed-test-export-10yard - sheet1.csv")
flying_sprint_data <- read_csv("/Users/watts/Downloads/smartspeed-test-export-flying10yard - sheet1.csv")
hop_test_data <- read_csv("/Users/watts/Downloads/forcedecks-test-export-hoptest.csv")

sprint_data$Date <- as.Date(sprint_data$Date, format="%d/%m/%Y")
flying_sprint_data$Date <- as.Date(flying_sprint_data$Date, format="%d/%m/%Y")
hop_test_data$Date <- as.Date(hop_test_data$Date, format="%m/%d/%Y")

# Convert the dates back to the desired format
sprint_data$Date <- format(sprint_data$Date, format="%m/%d/%Y")
flying_sprint_data$Date <- format(flying_sprint_data$Date, format="%m/%d/%Y")
hop_test_data$Date <- format(hop_test_data$Date, format="%m/%d/%Y")

athleteName <- unique(sprint_data$Name)

TemplatePageOne <- image_read_pdf("/Users/watts/Documents/Futures Performance Center/Templates/RSI Trends.pdf")

setwd("/Users/watts/Documents/Futures Performance Center")

# Loop through each athlete
for (i in 1:length(athleteName)) {
  
  print(paste("Processing athlete:", athleteName[i]))
  
  # Subset sprint data for the current athlete
  athlete_sprint <- sprint_data %>% 
    filter(Name == athleteName[i]) %>% 
    rename(Times = Total)
  
  # Subset flying sprint data for the current athlete
  athlete_flying_sprint <- flying_sprint_data %>% 
    filter(Name == athleteName[i]) %>% 
    rename(Times = Total)
  
  # Subset hop test data for the current athlete
  athlete_hop_test <- hop_test_data %>% 
    filter(Name == athleteName[i]) %>% 
    rename(`Mean RSI` = `Mean RSI (Jump Height/Contact Time) [m/s]`)
  
  PitchingReport <- TemplatePageOne
  
  # Conditionally generate tables and plots for each data type if there is data
  if(nrow(athlete_sprint) > 0) {
    table_sprint <- athlete_sprint %>%
      arrange(Date) %>%
      select("Date", "Times")
    
    png(paste0("RSI Reports Images/",athleteName[i],"- RSIsummary1.png"), height=14, width=2, units="in", res=175)
    table1 <- tableGrob(table_sprint, rows = NULL)
    grid.arrange(table1)
    dev.off()
    
    RSIsummarys <- image_read(paste0("RSI Reports Images/",athleteName[i],"- RSIsummary1.png"))
    PitchingReport <- image_composite(PitchingReport,RSIsummarys,offset= "+125+800")
  }
  
  if(nrow(athlete_flying_sprint) > 0) {
    table_flying_sprint <- athlete_flying_sprint %>%
      arrange(Date) %>%
      select("Date", "Times")
    
    png(paste0("RSI Reports Images/",athleteName[i],"- RSIsummary2.png"), height=12, width=2, units="in", res=175) # Adjusted width for 3 tables
    table2 <- tableGrob(table_flying_sprint, rows = NULL)
    grid.arrange(table2)
    dev.off()
    
    RSIsummarys2 <- image_read(paste0("RSI Reports Images/",athleteName[i],"- RSIsummary2.png"))
    PitchingReport <- image_composite(PitchingReport,RSIsummarys2,offset= "+550+700")
  }
  
  if(nrow(athlete_hop_test) > 0) {
    table_hop_test <- athlete_hop_test %>%
      arrange(Date) %>%
      select("Date", "Mean RSI")
    
    png(paste0("RSI Reports Images/",athleteName[i],"- RSIsummary3.png"), height=3, width=2, units="in", res=175) # Adjusted width for 3 tables
    table3 <- tableGrob(table_hop_test, rows = NULL)
    grid.arrange(table3)
    dev.off()
    
    RSIsummarys3 <- image_read(paste0("RSI Reports Images/",athleteName[i],"- RSIsummary3.png"))
    PitchingReport <- image_composite(PitchingReport,RSIsummarys3,offset= "+975+600")
  }
  
  athlete_sprint$Date <- as.Date(athlete_sprint$Date, format="%m/%d/%Y")
  trendPlot <- athlete_sprint %>% 
    ggplot(aes(x = Date, y = Times)) +
    geom_point() +
    geom_smooth(method = "lm", formula = 'y ~ x') +
    labs(title = paste("10-yard Sprint Trend for", athleteName[i]), x = "Date", y = "Times") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b")
  
  athlete_flying_sprint$Date <- as.Date(athlete_flying_sprint$Date, format="%m/%d/%Y")
  trendPlot2 <- athlete_flying_sprint %>% 
    ggplot(aes(x = Date, y = Times)) +
    geom_point() +
    geom_smooth(method = "lm", formula = 'y ~ x') +
    labs(title = paste("Flying 10-yard Sprint Trend for", athleteName[i]), x = "Date", y = "Times") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b")

  athlete_hop_test$Date <- as.Date(athlete_hop_test$Date, format="%m/%d/%Y")
  trendPlot3 <- athlete_hop_test %>% 
    ggplot(aes(x = Date, y = `Mean RSI`)) +
    geom_point() +
    geom_smooth(method = "lm", formula = 'y ~ x') +
    labs(title = paste("Hop Test Trend for", athleteName[i]), x = "Date", y = "Mean RSI")
  
  trendPlots <- ggarrange(trendPlot, trendPlot2, trendPlot3, nrow = 3)
  ggsave(trendPlots,file=paste0("RSI Reports Images/",athleteName[i]," - trendPlot.png"), width=5,height=12,units="in", dpi = 200)
  RSIgraph <- image_read(paste0("RSI Reports Images/",athleteName[i]," - trendPlot.png"))
  PitchingReport <- image_composite(PitchingReport, RSIgraph, offset= "+1450+700")
  
  image_write(PitchingReport,path = "page1.pdf",format="pdf",quality=100,density=300)
  image_write(PitchingReport,path = paste0("RSI Reports/",athleteName[i],".pdf"),format="pdf")
}
