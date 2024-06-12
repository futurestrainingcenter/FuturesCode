library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(gt)
library(gtExtras)
library(lubridate)

# Load hitting data
hittingData <- read_csv("HittingFacilityData.csv")
# Load pitching data
pitchingData <- read_csv("PitchingFacilityData.csv")
# Load Strength Data
strengthData <- read_csv("StrengthFacilityData.csv")
# Load Speed Data
speedData <- read_csv("SpeedFacilityData.csv")

# Get the most recent month and year from the datasets
most_recent_date <- max(hittingData$FakeDate, na.rm = TRUE)
most_recent_month <- month.name[month(most_recent_date)]
most_recent_year <- year(most_recent_date)

# Define UI for the application
ui <- navbarPage(theme = shinytheme("darkly"),
                 title = tags$img(src = "LOGO White Transparent.png", height = "30px"),
                 
                 tabPanel("Hitting Leaderboard",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("month", "Select Month:", choices = month.name, selected = most_recent_month),
                              selectInput("year", "Select Year:", choices = unique(hittingData$Year), selected = most_recent_year),
                              selectInput("level", "Select Level:", choices = unique(hittingData$Level)),
                              radioButtons("sport", "Select Sport:", choices = unique(hittingData$Sport))
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Exit Velocity", tableOutput("maxVelTable")),
                                tabPanel("Distance", tableOutput("maxDistTable")),
                                tabPanel("Bat Speed", tableOutput("batSpeedTable"))
                              )
                            )
                          )
                 ),
                 
                 tabPanel("Pitching Leaderboard",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("pitching_month", "Select Month:", choices = month.name, selected = most_recent_month),
                              selectInput("pitching_year", "Select Year:", choices = unique(pitchingData$Year), selected = most_recent_year),
                              selectInput("pitching_level", "Select Level:", choices = unique(pitchingData$Level))
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Release Speed", tableOutput("pitchingMaxVelTable"))
                              )
                            )
                          )
                 ),
                 
                 tabPanel("Strength Leaderboard",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("strength_month", "Select Month:", choices = month.name, selected = most_recent_month),
                              selectInput("strength_year", "Select Year:", choices = unique(strengthData$Year), selected = most_recent_year),
                              selectInput("strength_level", "Select Level:", choices = unique(strengthData$Level)),
                              radioButtons("gender", "Select Gender:", choices = unique(strengthData$Gender))
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                tabPanel("ISO Belt Squat", tableOutput("strengthIsoBeltSquatTable")),
                                tabPanel("Squat Jump", tableOutput("strengthSquatJumpTable")),
                                tabPanel("Proteus Power", tableOutput("strengthProteusPowerTable")),
                                tabPanel("Proteus Acceleration", tableOutput("strengthProteusAccelerationTable"))
                              )
                            )
                          )
                 ),
                 
                 tabPanel("Speed Leaderboard",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("speed_month", "Select Month:", choices = month.name, selected = most_recent_month),
                              selectInput("speed_year", "Select Year:", choices = unique(speedData$Year), selected = most_recent_year),
                              selectInput("speed_level", "Select Level:", choices = unique(speedData$Level)),
                              radioButtons("speed_gender", "Select Gender:", choices = unique(speedData$Gender))
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Top Speed", tableOutput("speedMaxVelTable")),
                                tabPanel("10 Yard Acceleration", tableOutput("speed10YardAccelTable")),
                                tabPanel("30 Yard Sprint", tableOutput("speed30YardSprintTable")),
                                tabPanel("RSI", tableOutput("speedRSITable"))
                              )
                            )
                          )
                 )
)

# Define server logic
server <- function(input, output) {
  
  # Filter and rank by Max Velocity
  output$maxVelTable <- render_gt({
    grouped_hittingData <- hittingData %>%
      group_by(FakeDate, Level, Sport) %>%
      arrange(FakeDate, Level, Sport, desc(MaxVel)) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      arrange(Name, FakeDate) %>%
      group_by(Name) %>%
      mutate(`Rank Change` = lag(Rank, order_by = FakeDate) - Rank) %>%
      rename(`Exit Velocity (mph)` = MaxVel) %>%
      ungroup()
    
    final_data <- grouped_hittingData %>%
      filter(Month == input$month, Year == input$year, Level == input$level, Sport == input$sport) %>%
      arrange(desc(`Exit Velocity (mph)`)) %>%
      select(Rank, Name, `Exit Velocity (mph)`, `Rank Change`)
    
    final_data %>%
      gt() %>%
      cols_align(
        align = "center",
        columns = everything()
      ) %>% 
      tab_style(
        style = cell_text(size = px(18), weight = "bold"),
        locations = cells_stub()
      ) %>%
      gt_fa_rank_change(
        `Rank Change`, palette = c("green", "lightgrey", "red"), font_color = "match"
      ) %>%
      cols_width(
        Name ~ px(250),
        `Exit Velocity (mph)` ~ px(200)
      ) %>%
      cols_label(`Rank Change` = "") %>% 
      tab_options(
        table.background.color = "#FFFFFF00",
        table.font.color = "white",
        table.align = "left"
      ) %>% 
      tab_options(
        table.border.top.style = "hidden"
      )
  })
  
  # Filter and rank by Max Distance
  output$maxDistTable <- render_gt({
    grouped_hittingData <- hittingData %>%
      group_by(FakeDate, Level, Sport) %>%
      arrange(FakeDate, Level, Sport, desc(MaxDist)) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      arrange(Name, FakeDate) %>%
      group_by(Name) %>%
      mutate(`Rank Change` = lag(Rank, order_by = FakeDate) - Rank) %>%
      rename(`Distance (ft)` = MaxDist) %>%
      ungroup()
    
    final_data <- grouped_hittingData %>%
      filter(Month == input$month, Year == input$year, Level == input$level, Sport == input$sport) %>%
      arrange(desc(`Distance (ft)`)) %>%
      select(Rank, Name, `Distance (ft)`, `Rank Change`)
    
    final_data %>%
      gt() %>%
      cols_align(
        align = "center",
        columns = everything()
      ) %>%
      gt_fa_rank_change(
        `Rank Change`, palette = c("green", "lightgrey", "red"), font_color = "match"
      ) %>%
      cols_width(
        Name ~ px(250),
        `Distance (ft)` ~ px(200)
      ) %>%
      cols_label(`Rank Change` = "") %>% 
      tab_options(
        table.background.color = "#FFFFFF00",
        table.font.color = "white",
        table.align = "left"
      ) %>% 
      tab_options(
        table.border.top.style = "hidden"
      )
  })
  
  # Filter and rank by Bat Speed
  output$batSpeedTable <- render_gt({
    grouped_hittingData <- hittingData %>%
      filter(!is.na(`Bat Speed (mph)`)) %>% 
      group_by(FakeDate, Level, Sport) %>%
      arrange(FakeDate, Level, Sport, desc(`Bat Speed (mph)`)) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      arrange(Name, FakeDate) %>%
      group_by(Name) %>%
      mutate(`Rank Change` = lag(Rank, order_by = FakeDate) - Rank) %>%
      ungroup()
    
    final_data <- grouped_hittingData %>%
      filter(Month == input$month, Year == input$year, Level == input$level, Sport == input$sport) %>%
      arrange(desc(`Bat Speed (mph)`)) %>%
      select(Rank, Name, `Bat Speed (mph)`, `Rank Change`)
    
    final_data %>%
      gt() %>%
      cols_align(
        align = "center",
        columns = everything()
      ) %>%
      gt_fa_rank_change(
        `Rank Change`, palette = c("green", "lightgrey", "red"), font_color = "match"
      ) %>%
      cols_width(
        Name ~ px(250),
        `Bat Speed (mph)` ~ px(200)
      ) %>%
      cols_label(`Rank Change` = "") %>% 
      tab_options(
        table.background.color = "#FFFFFF00",
        table.font.color = "white",
        table.align = "left"
      ) %>% 
      tab_options(
        table.border.top.style = "hidden"
      )
  })
  
  # Filter and rank by Pitching Speed
  output$pitchingMaxVelTable <- renderTable({
    grouped_pitchingData <- pitchingData %>%
      filter(!is.na(RelSpeed)) %>%
      mutate(FakeDate = make_date(Year, match(Month, month.name), 1)) %>%
      group_by(Name, Level, FakeDate, Month, Year) %>%
      summarize(`Release Speed (mph)` = max(RelSpeed, na.rm = TRUE), .groups = "drop") %>% 
      ungroup()
    
    ranked_pitchingData <- grouped_pitchingData %>% 
      group_by(FakeDate, Level) %>%
      arrange(FakeDate, Level, desc(`Release Speed (mph)`)) %>%
      mutate(Rank = row_number()) %>%
      ungroup() %>%
      arrange(Name, FakeDate) %>%
      group_by(Name) %>%
      mutate(`Rank Change` = lag(Rank, order_by = FakeDate) - Rank) %>%
      ungroup()
    
    filtered_pitchingData <- ranked_pitchingData %>%
      filter(Month == input$pitching_month, Year == input$pitching_year, Level == input$pitching_level) %>%
      arrange(desc(`Release Speed (mph)`)) %>% 
      select(Rank, Name, `Release Speed (mph)`, `Rank Change`)
  })
  
  # Filter and rank by ISO Belt Squat
  output$strengthIsoBeltSquatTable <- renderTable({
    grouped_ISOSQT <- strengthData %>%
      filter(`Exercise Type` == "ForceDeck: ISO Belt Squat") %>% 
      group_by(Name, Level, Month, Year, Gender) %>%
      summarize(`Peak Vertical Force (N)` = max(`Peak Vertical Force [N]`, na.rm = TRUE), .groups = 'drop')
    
    filtered_ISOSQT <- grouped_ISOSQT %>%
      filter(Month == input$strength_month, Year == input$strength_year, Level == input$strength_level, Gender == input$gender) %>%
      arrange(desc(`Peak Vertical Force (N)`)) %>%
      mutate(Rank = row_number())
    
    filtered_ISOSQT %>%
      select(Rank, Name, `Peak Vertical Force (N)`)
  })
  
  # Filter and rank by Squat Jump
  output$strengthSquatJumpTable <- renderTable({
    grouped_SJ <- strengthData %>%
      filter(`Exercise Type` == "ForceDeck: Squat Jump") %>% 
      group_by(Name, Level, Month, Year, Gender) %>%
      summarize(`Takeoff Peak Force (N)` = max(`Takeoff Peak Force [N]`, na.rm = TRUE), .groups = 'drop')
    
    filtered_SJ <- grouped_SJ %>%
      filter(Month == input$strength_month, Year == input$strength_year, Level == input$strength_level, Gender == input$gender) %>%
      arrange(desc(`Takeoff Peak Force (N)`)) %>%
      mutate(Rank = row_number())
    
    filtered_SJ %>%
      select(Rank, Name, `Takeoff Peak Force (N)`)
  })
  
  # Filter and rank by Proteus Power
  output$strengthProteusPowerTable <- renderTable({
    grouped_ProteusPower <- strengthData %>%
      filter(`Exercise Name` == "Proteus Full Test") %>% 
      group_by(Name, Level, Month, Year, Gender) %>%
      summarize(`Power (W)` = max(`power - high`, na.rm = TRUE), .groups = 'drop')
    
    filtered_ProteusPower <- grouped_ProteusPower %>%
      filter(Month == input$strength_month, Year == input$strength_year, Level == input$strength_level, Gender == input$gender) %>%
      arrange(desc(`Power (W)`)) %>%
      mutate(Rank = row_number())
    
    filtered_ProteusPower %>%
      select(Rank, Name, `Power (W)`)
  })
  
  # Filter and rank by Proteus Acceleration
  output$strengthProteusAccelerationTable <- renderTable({
    grouped_ProteusAcc <- strengthData %>%
      filter(`Exercise Name` == "Proteus Full Test") %>% 
      group_by(Name, Level, Month, Year, Gender) %>%
      summarize(`Acceleration (m/s²)` = max(`acceleration - high`, na.rm = TRUE), .groups = 'drop')
    
    filtered_ProteusAcc <- grouped_ProteusAcc %>%
      filter(Month == input$strength_month, Year == input$strength_year, Level == input$strength_level, Gender == input$gender) %>%
      arrange(desc(`Acceleration (m/s²)`)) %>%
      mutate(Rank = row_number())
    
    filtered_ProteusAcc %>%
      select(Rank, Name, `Acceleration (m/s²)`)
  })
  
  # Filter and rank by Max Running Speed
  output$speedMaxVelTable <- renderTable({
    grouped_MaxSpeed <- speedData %>%
      filter(`Exercise Name` == "Max Velocity") %>% 
      group_by(Name, Level, Month, Year, Gender) %>%
      summarize(`Top Speed (mph)` = max(MPH, na.rm = TRUE), .groups = 'drop')
    
    filtered_MaxSpeed <- grouped_MaxSpeed %>%
      filter(Month == input$speed_month, Year == input$speed_year, Level == input$speed_level, Gender == input$speed_gender) %>%
      arrange(desc(`Top Speed (mph)`)) %>%
      mutate(Rank = row_number())
    
    filtered_MaxSpeed %>%
      select(Rank, Name, `Top Speed (mph)`)
  })
  
  # Filter and rank by 10 Yard Acceleration
  output$speed10YardAccelTable <- renderTable({
    grouped_SpeedAcc <- speedData %>%
      filter(`Exercise Name` == "Acceleration") %>% 
      group_by(Name, Level, Month, Year, Gender) %>%
      summarize(`10 Yard Acceleration (sec)` = max(Split1, na.rm = TRUE), .groups = 'drop')
    
    filtered_SpeedAcc <- grouped_SpeedAcc %>%
      filter(Month == input$speed_month, Year == input$speed_year, Level == input$speed_level, Gender == input$speed_gender) %>%
      arrange(`10 Yard Acceleration (sec)`) %>%
      mutate(Rank = row_number())
    
    filtered_SpeedAcc %>%
      select(Rank, Name, `10 Yard Acceleration (sec)`)
  })
  
  # Filter and rank by 30 Yard Sprint
  output$speed30YardSprintTable <- renderTable({
    grouped_hardNinety <- speedData %>%
      filter(`Exercise Name` == "Hard 90") %>% 
      group_by(Name, Level, Month, Year, Gender) %>%
      summarize(`30 Yard Sprint (sec)` = max(Cumulative2), .groups = 'drop')
    
    filtered_hardNinety <- grouped_hardNinety %>%
      filter(Month == input$speed_month, Year == input$speed_year, Level == input$speed_level, Gender == input$speed_gender) %>%
      arrange(`30 Yard Sprint (sec)`) %>%
      mutate(Rank = row_number())
    
    filtered_hardNinety %>%
      select(Rank, Name, `30 Yard Sprint (sec)`)
  })
  
  # Filter and rank by RSI
  output$speedRSITable <- renderTable({
    grouped_RSI <- speedData %>%
      filter(`Exercise Name` == "Reactive Strength Index") %>% 
      group_by(Name, Level, Month, Year, Gender) %>%
      summarize(`RSI (Jump Height/Contact Time)` = max(`Mean RSI (Jump Height/Contact Time) [m/s]`), .groups = 'drop')
    
    filtered_RSI <- grouped_RSI %>%
      filter(Month == input$speed_month, Year == input$speed_year, Level == input$speed_level, Gender == input$speed_gender) %>%
      arrange(desc(`RSI (Jump Height/Contact Time)`)) %>%
      mutate(Rank = row_number())
    
    filtered_RSI %>%
      select(Rank, Name, `RSI (Jump Height/Contact Time)`)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
