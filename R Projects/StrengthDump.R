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

clientData <- read_csv("/Users/watts/Downloads/FullClientList.csv") %>% 
  rename(Name = Client) %>% 
  select(Name, `Sports Performance Training/Booking Level`)

proteusData<- read_csv("/Volumes/COLE'S DATA/Data/Physicality Report Data/ProteusPercentiles.csv") %>%
  rename(Date = `session createdAt`, `Exercise Name` = `exercise name`) %>%
  mutate(Date = format(Date, "%m/%d/%y")) %>% 
  select(Name, Date, `Exercise Name`, `power - mean`, `acceleration - mean`)

teambuildrData <- read_csv("/Users/watts/Downloads/Teambuildr Raw Data Report.csv") %>% 
  filter(`Exercise Name` %in% c('Barbell Back Squat', 'Trap Bar Deadlift', 'Barbell Bench Press', 
                                 'Straight Arm Trunk Rotation Max Isometric Test - Crane Scale', 'Cable Lat Pull Down')) %>%
  rename(Date = `Completed Date`) %>% 
  mutate(Name = paste(`First Name`, `Last Name`)) %>% 
  select(Name, Date, `Exercise Name`, `Highest Max`)

combinedData <- full_join(proteusData, teambuildrData, by = c("Name", "Date", "Exercise Name"))

finalData <- left_join(combinedData, clientData, by = "Name")

