library(dplyr)
library(readxl)
library(readr)
library(lubridate)
library(stringr)


# Hitting -----------------------------------------------------------------


blastData <- read_csv("/Volumes/COLE'S DATA/Data/Blast Master Data - Sheet1.csv")
hittraxData <- read_csv("/Volumes/COLE'S DATA/Data/Hittrax Master Data - Sheet1.csv")
clientData <- read_csv("/Users/watts/Downloads/FullClientList.csv") %>% 
  rename(Name = Client, Level = `Skill Level`)

calculate_age <- function(birthdate) {
  if (is.na(birthdate)) {
    return(NA)
  } else {
    birthdate <- mdy(birthdate) # Convert to Date using lubridate
    age <- interval(start = birthdate, end = Sys.Date()) / years(1)
    return(floor(age)) # Floor the age to get complete years
  }
}

clientData$Age <- sapply(clientData$`field-general-7.dl_date`, calculate_age)

blastData <- blastData %>%
  mutate(Date = make_date(Year, match(Month, month.name), 1)) %>% # Create a Date column combining Year and Month
  arrange(Name, Date) %>%
  group_by(Name) %>%
  mutate(
    BatSpeed_Monthly_Change = round(`Bat Speed (mph)` - lag(`Bat Speed (mph)`, order_by = Date), 1),
    BatSpeed_YTD_Change = round(cumsum(coalesce(`Bat Speed (mph)` - lag(`Bat Speed (mph)`, order_by = Date), 0)), 1),
    
    BatSpeed_Monthly_Percent_Change = ifelse(lag(`Bat Speed (mph)`) == 0, NA, 
                                             round(100 * (`Bat Speed (mph)` - lag(`Bat Speed (mph)`)) / lag(`Bat Speed (mph)`), 1)),
    BatSpeed_YTD_Percent_Change = ifelse(`Bat Speed (mph)` == first(`Bat Speed (mph)`), NA,
                                         round(100 * ((`Bat Speed (mph)` / first(`Bat Speed (mph)`)) - 1), 1)),
    
    Rotation_Monthly_Change = round(`Rotational Acceleration (g)` - lag(`Rotational Acceleration (g)`, order_by = Date), 1),
    Rotation_YTD_Change = round(cumsum(coalesce(`Rotational Acceleration (g)` - lag(`Rotational Acceleration (g)`, order_by = Date), 0)), 1),
    
    Rotation_Monthly_Percent_Change = ifelse(lag(`Rotational Acceleration (g)`) == 0, NA, 
                                             round(100 * (`Rotational Acceleration (g)` - lag(`Rotational Acceleration (g)`)) / lag(`Rotational Acceleration (g)`), 1)),
    Rotation_YTD_Percent_Change = ifelse(`Rotational Acceleration (g)` == first(`Rotational Acceleration (g)`), NA,
                                         round(100 * ((`Rotational Acceleration (g)` / first(`Rotational Acceleration (g)`)) - 1), 1)),
    
    # AttackAngle_Monthly_Change = round(`Attack Angle (deg)` - lag(`Attack Angle (deg)`, order_by = Date), 1),
    # AttackAngle_YTD_Change = round(cumsum(coalesce(`Attack Angle (deg)` - lag(`Attack Angle (deg)`, order_by = Date), 0)), 1),
    # 
    # AttackAngle_Monthly_Percent_Change = ifelse(lag(`Attack Angle (deg)`) == 0, NA, 
    #                                             round(100 * (`Attack Angle (deg)` - lag(`Attack Angle (deg)`)) / lag(`Attack Angle (deg)`), 1)),
    # AttackAngle_YTD_Percent_Change = ifelse(`Attack Angle (deg)` == first(`Attack Angle (deg)`), NA,
    #                                         round(100 * ((`Attack Angle (deg)` / first(`Attack Angle (deg)`)) - 1), 1)),
    
    Power_Monthly_Change = round(`Power (kW)` - lag(`Power (kW)`, order_by = Date), 1),
    Power_YTD_Change = round(cumsum(coalesce(`Power (kW)` - lag(`Power (kW)`, order_by = Date), 0)), 1),
    
    Power_Monthly_Percent_Change = ifelse(lag(`Power (kW)`) == 0, NA, 
                                          round(100 * (`Power (kW)` - lag(`Power (kW)`)) / lag(`Power (kW)`), 1)),
    Power_YTD_Percent_Change = ifelse(`Power (kW)` == first(`Power (kW)`), NA,
                                      round(100 * ((`Power (kW)` / first(`Power (kW)`)) - 1), 1)),
    
    # VBA_Monthly_Change = round(`Vertical Bat Angle (deg)` - lag(`Vertical Bat Angle (deg)`, order_by = Date), 1),
    # VBA_YTD_Change = round(cumsum(coalesce(`Vertical Bat Angle (deg)` - lag(`Vertical Bat Angle (deg)`, order_by = Date), 0)), 1),
    # 
    # VBA_Monthly_Percent_Change = ifelse(lag(`Vertical Bat Angle (deg)`) == 0, NA, 
    #                                     round(100 * (`Vertical Bat Angle (deg)` - lag(`Vertical Bat Angle (deg)`)) / lag(`Vertical Bat Angle (deg)`), 1)),
    # VBA_YTD_Percent_Change = ifelse(`Vertical Bat Angle (deg)` == first(`Vertical Bat Angle (deg)`), NA,
    #                                 round(100 * ((`Vertical Bat Angle (deg)` / first(`Vertical Bat Angle (deg)`)) - 1), 1)),
    
    OPE_Monthly_Change = round(`On Plane Efficiency (%)` - lag(`On Plane Efficiency (%)`, order_by = Date), 1),
    OPE_YTD_Change = round(cumsum(coalesce(`On Plane Efficiency (%)` - lag(`On Plane Efficiency (%)`, order_by = Date), 0)), 1),
    
    OPE_Monthly_Percent_Change = ifelse(lag(`On Plane Efficiency (%)`) == 0, NA, 
                                        round(100 * (`On Plane Efficiency (%)` - lag(`On Plane Efficiency (%)`)) / lag(`On Plane Efficiency (%)`), 1)),
    OPE_YTD_Percent_Change = ifelse(`On Plane Efficiency (%)` == first(`On Plane Efficiency (%)`), NA,
                                    round(100 * ((`On Plane Efficiency (%)` / first(`On Plane Efficiency (%)`)) - 1), 1)),
    
    # EC_Monthly_Change = round(`Early Connection (deg)` - lag(`Early Connection (deg)`, order_by = Date), 1),
    # EC_YTD_Change = round(cumsum(coalesce(`Early Connection (deg)` - lag(`Early Connection (deg)`, order_by = Date), 0)), 1),
    # 
    # EC_Monthly_Percent_Change = ifelse(lag(`Early Connection (deg)`) == 0, NA, 
    #                                    round(100 * (`Early Connection (deg)` - lag(`Early Connection (deg)`)) / lag(`Early Connection (deg)`), 1)),
    # EC_YTD_Percent_Change = ifelse(`Early Connection (deg)` == first(`Early Connection (deg)`), NA,
    #                                round(100 * ((`Early Connection (deg)` / first(`Early Connection (deg)`)) - 1), 1)),
    
    # CAI_Monthly_Change = round(`Connection at Impact (deg)` - lag(`Connection at Impact (deg)`, order_by = Date), 1),
    # CAI_YTD_Change = round(cumsum(coalesce(`Connection at Impact (deg)` - lag(`Connection at Impact (deg)`, order_by = Date), 0)), 1),
    # 
    # CAI_Monthly_Percent_Change = ifelse(lag(`Connection at Impact (deg)`) == 0, NA, 
    #                                     round(100 * (`Connection at Impact (deg)` - lag(`Connection at Impact (deg)`)) / lag(`Connection at Impact (deg)`), 1)),
    # CAI_YTD_Percent_Change = ifelse(`Connection at Impact (deg)` == first(`Connection at Impact (deg)`), NA,
    #                                 round(100 * ((`Connection at Impact (deg)` / first(`Connection at Impact (deg)`)) - 1), 1))
  )


hittraxData$`Date of Birth` <- as.Date(hittraxData$`Date of Birth`, format = "%B %d %Y")
hittraxData <- left_join(hittraxData, clientData, by = "Name")

hittraxData <- hittraxData %>%
  mutate(Date = make_date(Year, match(Month, month.name), 1)) %>%
  arrange(Name, Date) %>%
  group_by(Name) %>%
  mutate(
    MaxVel_CumMax = cummax(MaxVel),
    MaxDist_CumMax = cummax(MaxDist),
  ) %>%
  mutate(
    MaxVel_Monthly_Change = round(MaxVel - lag(MaxVel, order_by = Date), 1),
    MaxVel_YTD_Change = round(cumsum(coalesce(MaxVel - lag(MaxVel, order_by = Date), 0)), 1),
    
    MaxVel_Monthly_Percent_Change = ifelse(lag(MaxVel) == 0, NA, 
                                           round(100 * (MaxVel - lag(MaxVel)) / lag(MaxVel), 1)),
    MaxVel_YTD_Percent_Change = ifelse(MaxVel == first(MaxVel), NA, 
                                       round(100 * ((MaxVel / first(MaxVel)) - 1), 1)),
    
    AvgVel_Monthly_Change = round(AvgVel - lag(AvgVel, order_by = Date), 1),
    AvgVel_YTD_Change = round(cumsum(coalesce(AvgVel - lag(AvgVel, order_by = Date), 0)), 1),
    
    AvgVel_Monthly_Percent_Change = ifelse(lag(AvgVel) == 0, NA, 
                                           round(100 * (AvgVel - lag(AvgVel)) / lag(AvgVel), 1)),
    AvgVel_YTD_Percent_Change = ifelse(AvgVel == first(AvgVel), NA, 
                                       round(100 * ((AvgVel / first(AvgVel)) - 1), 1)),
    
    MaxDist_Monthly_Change = round(MaxDist - lag(MaxDist, order_by = Date), 1),
    MaxDist_YTD_Change = round(cumsum(coalesce(MaxDist - lag(MaxDist, order_by = Date), 0)), 1),
    
    MaxDist_Monthly_Percent_Change = ifelse(lag(MaxDist) == 0, NA, 
                                            round(100 * (MaxDist - lag(MaxDist)) / lag(MaxDist), 1)),
    MaxDist_YTD_Percent_Change = ifelse(MaxDist == first(MaxDist), NA, 
                                        round(100 * ((MaxDist / first(MaxDist)) - 1), 1)),
    
    AvgDist_Monthly_Change = round(AvgDist - lag(AvgDist, order_by = Date), 1),
    AvgDist_YTD_Change = round(cumsum(coalesce(AvgDist - lag(AvgDist, order_by = Date), 0)), 1),
    
    AvgDist_Monthly_Percent_Change = ifelse(lag(AvgDist) == 0, NA, 
                                            round(100 * (AvgDist - lag(AvgDist)) / lag(AvgDist), 1)),
    AvgDist_YTD_Percent_Change = ifelse(AvgDist == first(AvgDist), NA, 
                                        round(100 * ((AvgDist / first(AvgDist)) - 1), 1))
  ) %>%
  ungroup()

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
  distinct(Name, Date, LearningBlock, .keep_all = TRUE)  %>%
  group_by(Name) %>%
  mutate(FirstCheckIn = min(Date),
         LastCheckIn = max(Date),
         `Membership Length` = round(as.period(interval(FirstCheckIn, LastCheckIn)) / months(1), 1)) %>%
  ungroup()

summary_attendanceData <- attendanceData %>% 
  group_by(Name, Month, `Membership Length`) %>%
  summarise(Attendance = n(),
            .groups = 'drop')

trainingGroup <- read_csv("/Users/watts/Downloads/Memberships.csv") %>% 
  filter(Membership %in% c(".Unlimited Skill/Pitching (Annual)", ".Skill Pro 1x", ".Skill Pro 1x (Annual)", ".Skill Pro 2x", ".Skill Pro 2x (Annual)", ".Skill Pro Cage Access", ".Skill Pro Cage Access (Annual)",
                           "Learning Academy (pre-sale price)", "Learning Academy (paid in full discount)", "Learning Academy", 
                           "Position Player Skill - Base", "Position Player - Base (annual)", "Position Player Skill Training - 1", "Position Player Skill Training - 1 (Annual)", 
                           "Position Player Skill Training - 2", "Position Player Skill Training - 2 (Annual)", "Position Player Skill Training - 3", "Position Player Skill Training - 3 (Annual)",
                           "Position Player Skill Training - 4 (Annual)", "Hitters - Live AB", "Cage Access - Pro", 
                           "Futures Pro -  College or MiLB/Independent Baseball", "Futures Pro - MiLB with MLB time", "Futures Pro - MLB Minimum",
                           "Professional - MiLB or Independent Baseball", "Professional - MLB Minimum", "Professional - MiLB with MLB time",
                           "Collegiate/Pro Training - Pro w/ Coach", "Collegiate/Pro Training - Pro (Facility Access)",
                           "Corona HS Training"))

priority_list <- c(".Unlimited Skill/Pitching (Annual)", ".Skill Pro 1x", ".Skill Pro 1x (Annual)", ".Skill Pro 2x", ".Skill Pro 2x (Annual)", ".Skill Pro Cage Access", ".Skill Pro Cage Access (Annual)",
                   "Learning Academy (pre-sale price)", "Learning Academy (paid in full discount)", "Learning Academy", 
                   "Position Player Skill - Base", "Position Player - Base (annual)", "Position Player Skill Training - 1", "Position Player Skill Training - 1 (Annual)", 
                   "Position Player Skill Training - 2", "Position Player Skill Training - 2 (Annual)", "Position Player Skill Training - 3", "Position Player Skill Training - 3 (Annual)",
                   "Position Player Skill Training - 4 (Annual)", "Hitters - Live AB", "Cage Access - Pro", 
                   "Futures Pro -  College or MiLB/Independent Baseball", "Futures Pro - MiLB with MLB time", "Futures Pro - MLB Minimum",
                   "Professional - MiLB or Independent Baseball", "Professional - MLB Minimum", "Professional - MiLB with MLB time",
                   "Collegiate/Pro Training - Pro w/ Coach", "Collegiate/Pro Training - Pro (Facility Access)",
                   "Corona HS Training")

# Filter the data to include only the specified memberships
filtered_memberships <- trainingGroup %>%
  filter(Membership %in% priority_list) %>%
  mutate(
    Membership = factor(Membership, levels = priority_list),
    Name = paste(`First Name`, `Last Name`)
  ) %>%
  arrange(Membership) %>%
  group_by(Name, Email) %>%
  slice(1) %>%
  ungroup()

filtered_memberships <- filtered_memberships %>% 
  select(Name, Membership)

combined_data <- left_join(hittraxData, blastData, by = c("Name", "Month", "Year"))

#Fake date creation
month_num <- function(month_name) {
  match(month_name, month.name)
}

combined_data <- combined_data %>%
  mutate(
    MonthNum = sapply(Month, month_num), # Convert month name to number
    FakeDate = paste(Year, MonthNum, "1", sep = "-"), # Create date string in 'yyyy-mm-dd' format
    FakeDate = as.Date(FakeDate, format = "%Y-%m-%d") # Convert string to Date
  ) %>%
  select(-MonthNum)

combined_hitting_data <- left_join(combined_data, filtered_memberships, by = "Name")

final_hitting_data <- left_join(combined_hitting_data, summary_attendanceData, by = c("Name", "Month"))

final_hitting_data <- final_hitting_data %>%
  mutate(
    AttackAngle_Check = as.integer(
      (Sport == "Baseball" & Level %in% c("Professional", "Collegiate") & `Attack Angle (deg)` >= 6 & `Attack Angle (deg)` <= 10) |
        (Sport == "Baseball" & Level == "L1 (9u-11u)" & `Attack Angle (deg)` >= 5 & `Attack Angle (deg)` <= 15) |
        (Sport == "Baseball" & Level == "L2 (12u-14u)" & `Attack Angle (deg)` >= 8 & `Attack Angle (deg)` <= 12) |
        (Sport == "Baseball" & Level == "L3 (15u-18u)" & `Attack Angle (deg)` >= 8 & `Attack Angle (deg)` <= 12) |
        (Sport == "Softball" & Level == "Collegiate" & `Attack Angle (deg)` >= 6 & `Attack Angle (deg)` <= 10) |
        (Sport == "Softball" & Level == "L1 (9u-11u)" & `Attack Angle (deg)` >= 6 & `Attack Angle (deg)` <= 14) |
        (Sport == "Softball" & Level == "L2 (12u-14u)" & `Attack Angle (deg)` >= 6 & `Attack Angle (deg)` <= 10) |
        (Sport == "Softball" & Level == "L3 (15u-18u)" & `Attack Angle (deg)` >= 6 & `Attack Angle (deg)` <= 10)
    ),
    VerticalBatAngle_Check = as.integer(
      (Sport == "Baseball" & Level %in% c("Professional", "Collegiate") & `Vertical Bat Angle (deg)` >= -27 & `Vertical Bat Angle (deg)` <= -37) |
        (Sport == "Baseball" & Level == "L1 (9u-11u)" & `Vertical Bat Angle (deg)` >= -25 & `Vertical Bat Angle (deg)` <= -15) |
        (Sport == "Baseball" & Level == "L2 (12u-14u)" & `Vertical Bat Angle (deg)` >= -30 & `Vertical Bat Angle (deg)` <= -20) |
        (Sport == "Baseball" & Level == "L3 (15u-18u)" & `Vertical Bat Angle (deg)` >= -37 & `Vertical Bat Angle (deg)` <= -27) |
        (Sport == "Softball" & Level == "Collegiate" & `Vertical Bat Angle (deg)` >= -27 & `Vertical Bat Angle (deg)` <= -37) |
        (Sport == "Softball" & Level == "L1 (9u-11u)" & `Vertical Bat Angle (deg)` >= -25 & `Vertical Bat Angle (deg)` <= -15) |
        (Sport == "Softball" & Level == "L2 (12u-14u)" & `Vertical Bat Angle (deg)` >= -30 & `Vertical Bat Angle (deg)` <= -20) |
        (Sport == "Softball" & Level == "L3 (15u-18u)" & `Vertical Bat Angle (deg)` >= -37 & `Vertical Bat Angle (deg)` <= -27)
    ),
    ConnectionAtImpact_Check = as.integer(
      (Sport == "Baseball" & Level %in% c("Professional", "Collegiate") & `Connection at Impact (deg)` >= 90 & `Connection at Impact (deg)` <= 95) |
        (Sport == "Baseball" & Level == "L1 (9u-11u)" & `Connection at Impact (deg)` >= 80 & `Connection at Impact (deg)` <= 100) |
        (Sport == "Baseball" & Level == "L2 (12u-14u)" & `Connection at Impact (deg)` >= 85 & `Connection at Impact (deg)` <= 95) |
        (Sport == "Baseball" & Level == "L3 (15u-18u)" & `Connection at Impact (deg)` >= 90 & `Connection at Impact (deg)` <= 95) |
        (Sport == "Softball" & Level == "Collegiate" & `Connection at Impact (deg)` >= 90 & `Connection at Impact (deg)` <= 95) |
        (Sport == "Softball" & Level == "L1 (9u-11u)" & `Connection at Impact (deg)` >= 80 & `Connection at Impact (deg)` <= 100) |
        (Sport == "Softball" & Level == "L2 (12u-14u)" & `Connection at Impact (deg)` >= 85 & `Connection at Impact (deg)` <= 95) |
        (Sport == "Softball" & Level == "L3 (15u-18u)" & `Connection at Impact (deg)` >= 90 & `Connection at Impact (deg)` <= 95)
    ),
    EarlyConnection_Check = as.integer(
      (Sport == "Baseball" & Level %in% c("Professional", "Collegiate") & `Early Connection (deg)` >= 85 & `Early Connection (deg)` <= 105) |
        (Sport == "Baseball" & Level == "L1 (9u-11u)" & `Early Connection (deg)` >= 80 & `Early Connection (deg)` <= 110) |
        (Sport == "Baseball" & Level == "L2 (12u-14u)" & `Early Connection (deg)` >= 85 & `Early Connection (deg)` <= 110) |
        (Sport == "Baseball" & Level == "L3 (15u-18u)" & `Early Connection (deg)` >= 85 & `Early Connection (deg)` <= 105) |
        (Sport == "Softball" & Level == "Collegiate" & `Early Connection (deg)` >= 85 & `Early Connection (deg)` <= 105) |
        (Sport == "Softball" & Level == "L1 (9u-11u)" & `Early Connection (deg)` >= 80 & `Early Connection (deg)` <= 110) |
        (Sport == "Softball" & Level == "L2 (12u-14u)" & `Early Connection (deg)` >= 85 & `Early Connection (deg)` <= 110) |
        (Sport == "Softball" & Level == "L3 (15u-18u)" & `Early Connection (deg)` >= 85 & `Early Connection (deg)` <= 105)
    )
  )

athlete_data <- final_hitting_data %>%
  filter(!is.na(Gender), !is.na(Level)) %>% 
  select(Name, Month, Year, FakeDate, Sport, Membership, `Membership Length`, Attendance, Level,
    MaxVel, MaxVel_Monthly_Change, MaxVel_Monthly_Percent_Change, MaxVel_YTD_Change, MaxVel_YTD_Percent_Change, `MaxVel Rank`,
    AvgVel, AvgVel_Monthly_Change, AvgVel_Monthly_Percent_Change, AvgVel_YTD_Change, AvgVel_YTD_Percent_Change, `AvgVel Rank`,
    MaxDist, MaxDist_Monthly_Change, MaxDist_Monthly_Percent_Change, MaxDist_YTD_Change, MaxDist_YTD_Percent_Change, `MaxDist Rank`,
    AvgDist, AvgDist_Monthly_Change, AvgDist_Monthly_Percent_Change, AvgDist_YTD_Change, AvgDist_YTD_Percent_Change, `AvgDist Rank`,
    `Bat Speed (mph)`, BatSpeed_Monthly_Change, BatSpeed_Monthly_Percent_Change, BatSpeed_YTD_Change, BatSpeed_YTD_Percent_Change,
    `Rotational Acceleration (g)`, Rotation_Monthly_Change, Rotation_Monthly_Percent_Change, Rotation_YTD_Change, Rotation_YTD_Percent_Change,
    `Attack Angle (deg)`, AttackAngle_Check, #AttackAngle_Monthly_Change, AttackAngle_Monthly_Percent_Change, AttackAngle_YTD_Change, AttackAngle_YTD_Percent_Change,
    `Power (kW)`, Power_Monthly_Change, Power_Monthly_Percent_Change, Power_YTD_Change, Power_YTD_Percent_Change,
    `Vertical Bat Angle (deg)`, VerticalBatAngle_Check, #VBA_Monthly_Change, VBA_Monthly_Percent_Change, VBA_YTD_Change, VBA_YTD_Percent_Change,
    `On Plane Efficiency (%)`, OPE_Monthly_Change, OPE_Monthly_Percent_Change, OPE_YTD_Change, OPE_YTD_Percent_Change,
    `Early Connection (deg)`, EarlyConnection_Check, #EC_Monthly_Change, EC_Monthly_Percent_Change, EC_YTD_Change, EC_YTD_Percent_Change,
    `Connection at Impact (deg)`, ConnectionAtImpact_Check #CAI_Monthly_Change, CAI_Monthly_Percent_Change, CAI_YTD_Change, CAI_YTD_Percent_Change
  )


write_csv(athlete_data, "/Users/watts/Documents/Futures Performance Center/Data/Facility Data/HittingFacilityData.csv", na = '')


# Pitching ----------------------------------------------------------------



trackmanData <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/practice/masterTrackmanData.csv") %>%
  mutate(Name = str_split(Pitcher, pattern = ", ", simplify = TRUE) %>% 
           apply(1, function(x) paste(x[2], x[1])),
         Date = as.Date(Date, format = "%m/%d/%y"),
         Month = month(Date, label = TRUE, abbr = FALSE)) %>% 
  filter(!is.na(TaggedPitchType) & PitchSession == "Live") %>% 
  select(Name, Date, Month, TaggedPitchType, RelSpeed, RelHeight, RelSide, Extension, InducedVertBreak, HorzBreak, 
         PlateLocHeight, PlateLocSide, PlayID)

trackmanFB_data <- trackmanData %>%
  filter(TaggedPitchType == "Fastball") %>% 
  arrange(Name, Date) %>%
  group_by(Name, Month) %>%
  mutate(First_Row_Per_Group = row_number() == 1,
         MonthlyFastballMaxVelocity = max(RelSpeed, na.rm = TRUE),
         MonthlyFastballAvgVelocity = mean(RelSpeed, na.rm = TRUE),
         MonthlyFastballAvgInducedVertBreak = mean(InducedVertBreak, na.rm = TRUE),
         MonthlyFastballAvgHorzBreak = mean(HorzBreak, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Name) %>%
  mutate(YTD_FB_MaxVelocity = ifelse(First_Row_Per_Group, MonthlyFastballMaxVelocity - first(MonthlyFastballMaxVelocity), NA_real_),
         YTD_FB_AvgVelocity = ifelse(First_Row_Per_Group, MonthlyFastballAvgVelocity - first(MonthlyFastballAvgVelocity), NA_real_),
         YTD_FB_AvgInducedVertBreak = ifelse(First_Row_Per_Group, MonthlyFastballAvgInducedVertBreak - first(MonthlyFastballAvgInducedVertBreak), NA_real_),
         YTD_FB_AvgHorzBreak = ifelse(First_Row_Per_Group, MonthlyFastballAvgHorzBreak - first(MonthlyFastballAvgHorzBreak), NA_real_),
         MonthlyChange_FB_MaxVelocity = ifelse(First_Row_Per_Group, MonthlyFastballMaxVelocity - lag(MonthlyFastballMaxVelocity), NA_real_),
         MonthlyChange_FB_AvgVelocity = ifelse(First_Row_Per_Group, MonthlyFastballAvgVelocity - lag(MonthlyFastballAvgVelocity), NA_real_),
         MonthlyChange_FB_AvgInducedVertBreak = ifelse(First_Row_Per_Group, MonthlyFastballAvgInducedVertBreak - lag(MonthlyFastballAvgInducedVertBreak), NA_real_),
         MonthlyChange_FB_AvgHorzBreak = ifelse(First_Row_Per_Group, MonthlyFastballAvgHorzBreak - lag(MonthlyFastballAvgHorzBreak), NA_real_),
         YTD_FB_MaxVelocity_Percent = if_else(First_Row_Per_Group & first(MonthlyFastballMaxVelocity) != 0,
                                                   ((MonthlyFastballMaxVelocity - first(MonthlyFastballMaxVelocity)) / first(MonthlyFastballMaxVelocity)) * 100, NA_real_),
         MonthlyChange_FB_MaxVelocity_Percent = if_else(First_Row_Per_Group & lag(MonthlyFastballMaxVelocity) != 0,
                                              ((MonthlyFastballMaxVelocity - lag(MonthlyFastballMaxVelocity)) / lag(MonthlyFastballMaxVelocity)) * 100, NA_real_),
         YTD_FB_AvgVelocity_Percent = if_else(First_Row_Per_Group & first(MonthlyFastballAvgVelocity) != 0,
                                              ((MonthlyFastballAvgVelocity - first(MonthlyFastballAvgVelocity)) / first(MonthlyFastballAvgVelocity)) * 100, NA_real_),
         MonthlyChange_FB_AvgVelocity_Percent = if_else(First_Row_Per_Group & lag(MonthlyFastballAvgVelocity) != 0,
                                                        ((MonthlyFastballAvgVelocity - lag(MonthlyFastballAvgVelocity)) / lag(MonthlyFastballAvgVelocity)) * 100, NA_real_),
         YTD_FB_AvgInducedVertBreak_Percent = if_else(First_Row_Per_Group & first(MonthlyFastballAvgInducedVertBreak) != 0,
                                              ((MonthlyFastballAvgInducedVertBreak - first(MonthlyFastballAvgInducedVertBreak)) / first(MonthlyFastballAvgInducedVertBreak)) * 100, NA_real_),
         MonthlyChange_FB_AvgInducedVertBreak_Percent = if_else(First_Row_Per_Group & lag(MonthlyFastballAvgInducedVertBreak) != 0,
                                                        ((MonthlyFastballAvgInducedVertBreak - lag(MonthlyFastballAvgInducedVertBreak)) / lag(MonthlyFastballAvgInducedVertBreak)) * 100, NA_real_),
         YTD_FB_AvgHorzBreak_Percent = if_else(First_Row_Per_Group & first(MonthlyFastballAvgHorzBreak) != 0,
                                                      ((MonthlyFastballAvgHorzBreak - first(MonthlyFastballAvgHorzBreak)) / first(MonthlyFastballAvgHorzBreak)) * 100, NA_real_),
         MonthlyChange_FB_AvgHorzBreak_Percent = if_else(First_Row_Per_Group & lag(MonthlyFastballAvgHorzBreak) != 0,
                                                                ((MonthlyFastballAvgHorzBreak - lag(MonthlyFastballAvgHorzBreak)) / lag(MonthlyFastballAvgHorzBreak)) * 100, NA_real_)) %>%
  mutate(
    YTD_FB_MaxVelocity = if_else(row_number() == 1, NA_real_, YTD_FB_MaxVelocity),
    YTD_FB_AvgVelocity = if_else(row_number() == 1, NA_real_, YTD_FB_AvgVelocity),
    YTD_FB_AvgInducedVertBreak = if_else(row_number() == 1, NA_real_, YTD_FB_AvgInducedVertBreak),
    YTD_FB_AvgHorzBreak = if_else(row_number() == 1, NA_real_, YTD_FB_AvgHorzBreak),
    YTD_FB_MaxVelocity_Percent = if_else(row_number() == 1, NA_real_, YTD_FB_MaxVelocity_Percent),
    YTD_FB_AvgVelocity_Percent = if_else(row_number() == 1, NA_real_, YTD_FB_AvgVelocity_Percent),
    YTD_FB_AvgInducedVertBreak_Percent = if_else(row_number() == 1, NA_real_, YTD_FB_AvgInducedVertBreak_Percent),
    YTD_FB_AvgHorzBreak_Percent = if_else(row_number() == 1, NA_real_, YTD_FB_AvgHorzBreak_Percent)
  ) %>% 
  ungroup() %>% 
  select(MonthlyChange_FB_MaxVelocity, YTD_FB_MaxVelocity, MonthlyChange_FB_AvgVelocity, YTD_FB_AvgVelocity, 
         MonthlyChange_FB_AvgInducedVertBreak, YTD_FB_AvgInducedVertBreak, MonthlyChange_FB_AvgHorzBreak, YTD_FB_AvgHorzBreak, 
         YTD_FB_MaxVelocity_Percent, YTD_FB_AvgVelocity_Percent, YTD_FB_AvgInducedVertBreak_Percent, YTD_FB_AvgHorzBreak_Percent, 
         MonthlyChange_FB_MaxVelocity_Percent, MonthlyChange_FB_AvgVelocity_Percent, MonthlyChange_FB_AvgInducedVertBreak_Percent, 
         MonthlyChange_FB_AvgHorzBreak_Percent, PlayID)

updated_trackmanData <- left_join(trackmanData, trackmanFB_data, by = "PlayID")

updated_trackmanData <- updated_trackmanData %>% 
  arrange(Name, Date) %>% 
  group_by(Name, Month) %>% 
  mutate(First_Row_Per_Group = row_number() == 1,
         MonthlyRelHeight = mean(RelHeight, na.rm = TRUE),
         MonthlyRelSide = mean(RelSide, na.rm = TRUE),
         MonthlyExtension = mean(Extension, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Name) %>%
  mutate(YTD_RelHeight = ifelse(First_Row_Per_Group, MonthlyRelHeight - first(MonthlyRelHeight), NA_real_),
         YTD_RelSide = ifelse(First_Row_Per_Group, MonthlyRelSide - first(MonthlyRelSide), NA_real_),
         YTD_Extension = ifelse(First_Row_Per_Group, MonthlyExtension - first(MonthlyExtension), NA_real_),
         MonthlyChange_RelHeight = ifelse(First_Row_Per_Group, MonthlyRelHeight - lag(MonthlyRelHeight), NA_real_),
         MonthlyChange_RelSide = ifelse(First_Row_Per_Group, MonthlyRelSide - lag(MonthlyRelSide), NA_real_),
         MonthlyChange_Extension = ifelse(First_Row_Per_Group, MonthlyExtension - lag(MonthlyExtension), NA_real_),
         YTD_RelHeight_Percent = if_else(First_Row_Per_Group & first(MonthlyRelHeight) != 0,
                                              ((MonthlyRelHeight - first(MonthlyRelHeight)) / first(MonthlyRelHeight)) * 100, NA_real_),
         MonthlyChange_RelHeight_Percent = if_else(First_Row_Per_Group & lag(MonthlyRelHeight) != 0,
                                                        ((MonthlyRelHeight - lag(MonthlyRelHeight)) / lag(MonthlyRelHeight)) * 100, NA_real_),
         YTD_RelSide_Percent = if_else(First_Row_Per_Group & first(MonthlyRelSide) != 0,
                                         ((MonthlyRelSide - first(MonthlyRelSide)) / first(MonthlyRelSide)) * 100, NA_real_),
         MonthlyChange_RelSide_Percent = if_else(First_Row_Per_Group & lag(MonthlyRelSide) != 0,
                                                   ((MonthlyRelSide - lag(MonthlyRelSide)) / lag(MonthlyRelSide)) * 100, NA_real_),
         YTD_Extension_Percent = if_else(First_Row_Per_Group & first(MonthlyExtension) != 0,
                                       ((MonthlyExtension - first(MonthlyExtension)) / first(MonthlyExtension)) * 100, NA_real_),
         MonthlyChange_Extension_Percent = if_else(First_Row_Per_Group & lag(MonthlyExtension) != 0,
                                                 ((MonthlyExtension - lag(MonthlyExtension)) / lag(MonthlyExtension)) * 100, NA_real_)) %>% 
  mutate(
    YTD_RelHeight = if_else(row_number() == 1, NA_real_, YTD_RelHeight),
    YTD_RelSide = if_else(row_number() == 1, NA_real_, YTD_RelSide),
    YTD_Extension = if_else(row_number() == 1, NA_real_, YTD_Extension),
    YTD_RelHeight_Percent = if_else(row_number() == 1, NA_real_, YTD_RelHeight_Percent),
    YTD_RelSide_Percent = if_else(row_number() == 1, NA_real_, YTD_RelSide_Percent),
    YTD_Extension_Percent = if_else(row_number() == 1, NA_real_, YTD_Extension_Percent)
  ) %>% 
  ungroup() %>% 
  select(Name, Date, Month, TaggedPitchType, RelSpeed, RelHeight, RelSide, Extension, InducedVertBreak, HorzBreak, PlateLocHeight, 
         PlateLocSide, MonthlyChange_RelHeight, YTD_RelHeight, MonthlyChange_RelSide, YTD_RelSide, MonthlyChange_Extension, 
         YTD_Extension, YTD_RelHeight_Percent, MonthlyChange_RelHeight_Percent, YTD_RelSide_Percent, MonthlyChange_RelSide_Percent, 
         YTD_Extension_Percent, MonthlyChange_Extension_Percent, MonthlyChange_FB_MaxVelocity, YTD_FB_MaxVelocity, 
         MonthlyChange_FB_AvgVelocity, YTD_FB_AvgVelocity, MonthlyChange_FB_AvgInducedVertBreak, YTD_FB_AvgInducedVertBreak, 
         MonthlyChange_FB_AvgHorzBreak, YTD_FB_AvgHorzBreak, YTD_FB_MaxVelocity_Percent, YTD_FB_AvgVelocity_Percent, 
         YTD_FB_AvgInducedVertBreak_Percent, YTD_FB_AvgHorzBreak_Percent, MonthlyChange_FB_MaxVelocity_Percent, 
         MonthlyChange_FB_AvgVelocity_Percent, MonthlyChange_FB_AvgInducedVertBreak_Percent, MonthlyChange_FB_AvgHorzBreak_Percent,PlayID)

armCareData <- read_csv("/Users/watts/Downloads/ArmCare_data.csv")
armCareData$`Exam Date` <- mdy(armCareData$`Exam Date`)

armCareData <- armCareData %>%
  mutate(
    Name = paste(`First Name`, `Last Name`),
    Month = month(`Exam Date`, label = TRUE, abbr = FALSE)) %>%
  filter(
    `Exam Type` %in% c("Fresh - Quick", "Fresh - Full")) %>% 
  rename(Date = `Exam Date`) %>% 
  select(Name, Date, Month, `Arm Score`, `Total Strength`, `Shoulder Balance`)

updated_armCareData <- armCareData %>% 
  arrange(Name, Date) %>% 
  group_by(Name, Month) %>% 
  mutate(First_Row_Per_Group = row_number() == 1,
         MonthlyArmScore = mean(`Arm Score`, na.rm = TRUE),
         MonthlyStrength = mean(`Total Strength`, na.rm = TRUE),
         MonthlyShoulderBalance = mean(`Shoulder Balance`, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Name) %>%
  mutate(YTD_ArmScore = ifelse(First_Row_Per_Group, MonthlyArmScore - first(MonthlyArmScore), NA_real_),
         YTD_Strength = ifelse(First_Row_Per_Group, MonthlyStrength - first(MonthlyStrength), NA_real_),
         YTD_ShoulderBalance = ifelse(First_Row_Per_Group, MonthlyShoulderBalance - first(MonthlyShoulderBalance), NA_real_),
         MonthlyChange_ArmScore = ifelse(First_Row_Per_Group, MonthlyArmScore - lag(MonthlyArmScore), NA_real_),
         MonthlyChange_Strength = ifelse(First_Row_Per_Group, MonthlyStrength - lag(MonthlyStrength), NA_real_),
         MonthlyChange_ShoulderBalance = ifelse(First_Row_Per_Group, MonthlyShoulderBalance - lag(MonthlyShoulderBalance), NA_real_),
         YTD_ArmScore_Percent = if_else(First_Row_Per_Group & first(MonthlyArmScore) != 0,
                                         ((MonthlyArmScore - first(MonthlyArmScore)) / first(MonthlyArmScore)) * 100, NA_real_),
         MonthlyChange_ArmScore_Percent = if_else(First_Row_Per_Group & lag(MonthlyArmScore) != 0,
                                                   ((MonthlyArmScore - lag(MonthlyArmScore)) / lag(MonthlyArmScore)) * 100, NA_real_),
         YTD_Strength_Percent = if_else(First_Row_Per_Group & first(MonthlyStrength) != 0,
                                        ((MonthlyStrength - first(MonthlyStrength)) / first(MonthlyStrength)) * 100, NA_real_),
         MonthlyChange_Strength_Percent = if_else(First_Row_Per_Group & lag(MonthlyStrength) != 0,
                                                  ((MonthlyStrength - lag(MonthlyStrength)) / lag(MonthlyStrength)) * 100, NA_real_),
         YTD_ShoulderBalance_Percent = if_else(First_Row_Per_Group & first(MonthlyShoulderBalance) != 0,
                                        ((MonthlyShoulderBalance - first(MonthlyShoulderBalance)) / first(MonthlyShoulderBalance)) * 100, NA_real_),
         MonthlyChange_ShoulderBalance_Percent = if_else(First_Row_Per_Group & lag(MonthlyShoulderBalance) != 0,
                                                  ((MonthlyShoulderBalance - lag(MonthlyShoulderBalance)) / lag(MonthlyShoulderBalance)) * 100, NA_real_)) %>% 
  mutate(
    YTD_ArmScore = if_else(row_number() == 1, NA_real_, YTD_ArmScore),
    YTD_Strength = if_else(row_number() == 1, NA_real_, YTD_Strength),
    YTD_ShoulderBalance = if_else(row_number() == 1, NA_real_, YTD_ShoulderBalance),
    YTD_ArmScore_Percent = if_else(row_number() == 1, NA_real_, YTD_ArmScore_Percent),
    YTD_Strength_Percent = if_else(row_number() == 1, NA_real_, YTD_Strength_Percent),
    YTD_ShoulderBalance_Percent = if_else(row_number() == 1, NA_real_, YTD_ShoulderBalance_Percent)
  ) %>% 
  ungroup()  
  
pitching_data <- full_join(updated_trackmanData, updated_armCareData, by = c("Date", "Name", "Month"))

attendanceData <- read_csv("/Users/watts/Downloads/CheckIns.csv") %>%
  rename(Name = Client) %>% 
  mutate(Date = as.Date(Date, format = "%b %d, %Y"),
         Month = month(Date, label = TRUE, abbr = FALSE),
         LearningBlock = case_when(
           `Service Name` == "Learning Academy - Block 1" ~ "Learning Academy - Attended",
           `Service Name` == "Learning Academy - Block 2" ~ "Learning Academy - Attended",
           TRUE ~ `Service Name`
         )) %>%
  filter(LearningBlock %in% c("Academy Pitching G1", "Academy Pitching G2", "Academy Pitching G3", 
                              "Baseball Pitching L1", "Baseball Pitching L2", "Baseball Pitching L3", 
                              "Pitching 1on1", "Arm Care", "Learning Academy - Attended", "Collegiate/Pro Pitching Block", "Professional - Facility Access")) %>% 
  distinct(Name, Date, LearningBlock, .keep_all = TRUE)  %>%
  group_by(Name) %>%
  mutate(FirstCheckIn = min(Date),
         LastCheckIn = max(Date),
         `Membership Length` = round(as.period(interval(FirstCheckIn, LastCheckIn)) / months(1), 1)) %>%
  ungroup()

summary_attendanceData <- attendanceData %>%
  group_by(Name, Month, `Membership Length`) %>%
  summarise(Attendance = n(),
            .groups = 'drop')

trainingGroup <- read_csv("/Users/watts/Downloads/Memberships.csv") %>% 
  filter(Membership %in% c(".Unlimited Skill/Pitching (Annual)", ".Pitching Pro 1x", ".Pitching Pro 1x (Annual)", ".Pitching Pro Arm Care", ".Pitching Pro Arm Care (Annual)", 
                           "Learning Academy (pre-sale price)", "Learning Academy (paid in full discount)", "Learning Academy", 
                           "Pitching - Live AB", "Pitching Development - Pro Double", "Pitching Development - Pro Single", "Pitching Development - Pro Single (Annual)", 
                           "Futures Pro -  College or MiLB/Independent Baseball", "Futures Pro - MiLB with MLB time", "Futures Pro - MLB Minimum",
                           "Professional - MiLB or Independent Baseball", "Professional - MLB Minimum", "Professional - MiLB with MLB time",
                           "Collegiate/Pro Training - Pro w/ Coach", "Collegiate/Pro Training - Pro (Facility Access)",
                           ".Add-On Arm Care", "Pitching 1on1", "Pitching Arm Care", "Arm Care add-on",  "Pitching 1on1 Membership"))

priority_list <- c(".Unlimited Skill/Pitching (Annual)", ".Pitching Pro 1x", ".Pitching Pro 1x (Annual)", ".Pitching Pro Arm Care", ".Pitching Pro Arm Care (Annual)", 
                   "Learning Academy (pre-sale price)", "Learning Academy (paid in full discount)", "Learning Academy", 
                   "Pitching - Live AB", "Pitching Development - Pro Double", "Pitching Development - Pro Single", "Pitching Development - Pro Single (Annual)", 
                   "Futures Pro -  College or MiLB/Independent Baseball", "Futures Pro - MiLB with MLB time", "Futures Pro - MLB Minimum",
                   "Professional - MiLB or Independent Baseball", "Professional - MLB Minimum", "Professional - MiLB with MLB time",
                   "Collegiate/Pro Training - Pro w/ Coach", "Collegiate/Pro Training - Pro (Facility Access)",
                   ".Add-On Arm Care", "Pitching 1on1", "Pitching Arm Care", "Arm Care add-on",  "Pitching 1on1 Membership")

# Filter the data to include only the specified memberships
filtered_memberships <- trainingGroup %>%
  filter(Membership %in% priority_list) %>%
  mutate(
    Membership = factor(Membership, levels = priority_list),
    Name = paste(`First Name`, `Last Name`)
  ) %>%
  arrange(Membership) %>%
  group_by(Name) %>%
  slice(1) %>%
  ungroup()

filtered_memberships <- filtered_memberships %>% 
  select(Name, Membership)

clientData <- read_csv("/Users/watts/Downloads/FullClientList.csv") %>% 
  rename(Name = Client, Level = `Pitching Level`) %>% 
  select(Name, Level)

summary_pitching_data <- left_join(pitching_data, filtered_memberships, by = "Name")

final_summary_pitching_data <- left_join(summary_pitching_data, clientData, by = "Name")

final_pitching_data <- left_join(final_summary_pitching_data, summary_attendanceData, by = c("Name", "Month"))

final_pitching_data <- final_pitching_data %>%
  filter(!is.na(Level)) %>% 
  mutate(Year = year(Date)) %>% 
  select(Name, Date, Month, Year, Membership, `Membership Length`, Attendance, Level, TaggedPitchType, RelSpeed, RelHeight, RelSide, Extension, 
         InducedVertBreak, HorzBreak, PlateLocHeight, PlateLocSide, MonthlyChange_RelHeight, YTD_RelHeight, MonthlyChange_RelSide, 
         YTD_RelSide, MonthlyChange_Extension, YTD_Extension, MonthlyChange_FB_MaxVelocity, YTD_FB_MaxVelocity, 
         MonthlyChange_FB_AvgVelocity, YTD_FB_AvgVelocity, MonthlyChange_FB_AvgInducedVertBreak, YTD_FB_AvgInducedVertBreak, 
         MonthlyChange_FB_AvgHorzBreak, YTD_FB_AvgHorzBreak, `Arm Score`, `Total Strength`, `Shoulder Balance`, 
         MonthlyChange_ArmScore, YTD_ArmScore, MonthlyChange_Strength, YTD_Strength, MonthlyChange_ShoulderBalance, YTD_ShoulderBalance,YTD_FB_MaxVelocity_Percent,
         MonthlyChange_FB_MaxVelocity_Percent,YTD_FB_AvgVelocity_Percent,MonthlyChange_FB_AvgVelocity_Percent,YTD_FB_AvgInducedVertBreak_Percent,
         MonthlyChange_FB_AvgInducedVertBreak_Percent, YTD_FB_AvgHorzBreak_Percent,MonthlyChange_FB_AvgHorzBreak_Percent, YTD_RelHeight_Percent,
         MonthlyChange_RelHeight_Percent,YTD_RelSide_Percent,MonthlyChange_RelSide_Percent,YTD_Extension_Percent,MonthlyChange_Extension_Percent,
         YTD_ArmScore_Percent,MonthlyChange_ArmScore_Percent,YTD_Strength_Percent,MonthlyChange_Strength_Percent,YTD_ShoulderBalance_Percent,
         MonthlyChange_ShoulderBalance_Percent)

write_csv(final_pitching_data, "/Users/watts/Documents/Futures Performance Center/Data/Facility Data/PitchingFacilityData.csv", na = '')


# Strength ----------------------------------------------------------------


teambuildrData <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Strength Percentiles/teambuilderPercentiles.csv") %>%
  filter(`Exercise Name` %in% c('Barbell Back Squat', 'Trap Bar Deadlift', 'Barbell Bench Press', 'Safety Squat Bar Split Squat',
                                'Straight Arm Trunk Rotation Max Isometric Test - Crane Scale', 'Cable Lat Pull Down')) %>%
  mutate(`Exercise Type` = "Weightroom",
         Month = format(ymd(`Added Date`), "%B")) %>% 
  rename(Date = `Added Date`) %>% 
  select(Date, Month, Name, Level, Gender, `Exercise Type`, `Exercise Name`, `Max Value`, PercentileRank)

#teambuildrData$Date <- as.Date(teambuildrData$Date, format="%m/%d/%y")

proteusData<- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Strength Percentiles/ProteusPercentiles.csv") %>%
  mutate(`Exercise Type` = "Proteus") %>% 
  rename(Date = `session createdAt`, `Exercise Name` = `exercise name`)

#proteusData$Date <- as.Date(proteusData$Date, format="%m/%d/%y")

CMJdata <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Strength Percentiles/CMJpercentiles.csv") %>%
  mutate(`Exercise Type` = "ForceDeck: CMJ") %>% 
  rename(`Exercise Name` = `Test Type`) %>% 
  select(-ExternalId, -Time, -`BW [KG]`, -Reps, -Tags, -`Additional Load [lb]`)

CMJdata$Date <- as.Date(CMJdata$Date, format="%y-%m-%d")

ISOSQTdata <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Strength Percentiles/ISO_SquatPercentiles.csv") %>%
  mutate(`Exercise Type` = "ForceDeck: ISO Squat Hold") %>% 
  rename(`Exercise Name` = `Test Type`) %>% 
  select(-ExternalId, -Time, -`BW [KG]`, -Reps, -Tags)

ISOBeltSQTdata <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Strength Percentiles/ISO_BeltSquatPercentiles.csv") %>%
  mutate(`Exercise Type` = "ForceDeck: ISO Belt Squat") %>% 
  rename(`Exercise Name` = `Test Type`) %>% 
  select(-ExternalId, -Time, -`BW [KG]`, -Reps, -Tags)

SQTJumpData <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Strength Percentiles/SQTJumpPercentiles.csv") %>% 
  mutate(`Exercise Type` = "ForceDeck: Squat Jump",
         `Exercise Name` = `Test Type`) %>% 
  select(-ExternalId, -Time, -`BW [KG]`, -Reps, -Tags, -`Additional Load [lb]`)

SQTJumpData$Date <- as.Date(SQTJumpData$Date, format="%y-%m-%d")

shoulderData <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Strength Percentiles/ShoulderISOPercentiles.csv") %>%
  mutate(`Exercise Type` = "ForceDeck: ShoulderISO",
         `Exercise Name` = `Test Type`)

RSIdata <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Speed Percentiles/RSIpercentiles.csv") %>% 
  mutate(`Exercise Type` = "ForceDeck: RSI",
         `Exercise Name` = Name) %>% 
  select(-`BW [KG]`, -`Jump Height (Flight Time) [cm]`, -`Contact Time [ms]`)

attendanceData <- read_csv("/Users/watts/Downloads/CheckIns.csv") %>%
  rename(Name = Client) %>% 
  mutate(Date = as.Date(Date, format = "%b %d, %Y"),
         Month = month(Date, label = TRUE, abbr = FALSE),
         LearningBlock = case_when(
           `Service Name` == "Learning Academy - Block 1" ~ "Learning Academy - Attended",
           `Service Name` == "Learning Academy - Block 2" ~ "Learning Academy - Attended",
           TRUE ~ `Service Name`
         )) %>%
  filter(LearningBlock %in% c("Strength Base L2", "Strength Base L2/L3", "Strength Base L3", 
                               "Strength Pro L2", "Strength Pro L2/L3", "Strength Pro L3", 
                               "Strength New Athlete Class", "Futures Physicality Test (Proteus and Force Plate)",
                               "Professional - Facility Access", "Learning Academy - Attended")) %>% 
  distinct(Name, Date, LearningBlock, .keep_all = TRUE) %>%
  group_by(Name) %>%
  mutate(FirstCheckIn = min(Date),
         LastCheckIn = max(Date),
         `Membership Length` = round(as.period(interval(FirstCheckIn, LastCheckIn)) / months(1), 1)) %>%
  ungroup()

summary_attendanceData <- attendanceData %>%
  group_by(Name, Month, `Membership Length`) %>%
  summarise(Attendance = n(), 
            .groups = 'drop')

trainingGroup <- read_csv("/Users/watts/Downloads/Memberships.csv") %>% 
  filter(Membership %in% c(".Unlimited Base Strength/Speed/Mobility (Annual)", ".Unlimited Pro Strength/Speed/Mobility (Annual)", ".Strength & Speed Base", ".Strength & Speed Base (Annual)", ".Strength & Speed Pro", 
                           "Learning Academy (pre-sale price)", "Learning Academy (paid in full discount)", "Learning Academy", 
                           "Strength Training - Base", "Strength Training - Base (Annual)", "Strength Training - Pro", "Strength Training - Pro (Annual)", 
                           "Futures Pro -  College or MiLB/Independent Baseball", "Futures Pro - MiLB with MLB time", "Futures Pro - MLB Minimum",
                           "Professional - MiLB or Independent Baseball", "Professional - MLB Minimum", "Professional - MiLB with MLB time",
                           "Collegiate/Pro Training - Pro w/ Coach", "Collegiate/Pro Training - Pro (Facility Access)", 
                           ".Proteus & Force Plate Testing"))

priority_list <- c(".Unlimited Base Strength/Speed/Mobility (Annual)", ".Unlimited Pro Strength/Speed/Mobility (Annual)", ".Strength & Speed Base", ".Strength & Speed Base (Annual)", ".Strength & Speed Pro", 
                   "Learning Academy (pre-sale price)", "Learning Academy (paid in full discount)", "Learning Academy", 
                   "Strength Training - Base", "Strength Training - Base (Annual)", "Strength Training - Pro", "Strength Training - Pro (Annual)", 
                   "Futures Pro -  College or MiLB/Independent Baseball", "Futures Pro - MiLB with MLB time", "Futures Pro - MLB Minimum",
                   "Professional - MiLB or Independent Baseball", "Professional - MLB Minimum", "Professional - MiLB with MLB time",
                   "Collegiate/Pro Training - Pro w/ Coach", "Collegiate/Pro Training - Pro (Facility Access)", 
                   ".Proteus & Force Plate Testing")

# Filter the data to include only the specified memberships
filtered_memberships <- trainingGroup %>%
  filter(Membership %in% priority_list) %>%
  mutate(
    Membership = factor(Membership, levels = priority_list),
    Name = paste(`First Name`, `Last Name`)
  ) %>%
  arrange(Membership) %>%
  group_by(Name, Email) %>%
  slice(1) %>%
  ungroup()

filtered_memberships <- filtered_memberships %>% 
  select(Name, Membership)

combined_strength <- bind_rows(proteusData, CMJdata, ISOSQTdata, ISOBeltSQTdata, shoulderData, teambuildrData, SQTJumpData, RSIdata)

updated_strength_data <- left_join(combined_strength, summary_attendanceData, by = c("Name", "Month"))

final_strength_data <- left_join(updated_strength_data, filtered_memberships, by = "Name")

final_strength_data <- final_strength_data %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  arrange(Name, `Exercise Name`, Date) %>%
  group_by(Name, `Exercise Name`, Month) %>%
  mutate(
    Mean_Value_Power = if_else(`Exercise Name` == "Proteus Full Test", mean(`power - high`, na.rm = TRUE), NA_real_),
    Mean_Value_Acceleration = if_else(`Exercise Name` == "Proteus Full Test", mean(`acceleration - high`, na.rm = TRUE), NA_real_),
    First_Row_Per_Group = row_number() == 1
  ) %>% 
  group_by(Name, `Exercise Name`) %>% 
  mutate(
    `Monthly Change - Power` = if_else(First_Row_Per_Group, Mean_Value_Power - lag(Mean_Value_Power), NA_real_),
    `Year to Date Change - Power` = if_else(First_Row_Per_Group, Mean_Value_Power - first(Mean_Value_Power), NA_real_),
    `Monthly Change - Acceleration` = if_else(First_Row_Per_Group, Mean_Value_Acceleration - lag(Mean_Value_Acceleration), NA_real_),
    `Year to Date Change - Acceleration` = if_else(First_Row_Per_Group, Mean_Value_Acceleration - first(Mean_Value_Acceleration), NA_real_),
    `Monthly Change % - Power` = if_else(First_Row_Per_Group & lag(Mean_Value_Power) != 0,
                                         ((Mean_Value_Power - lag(Mean_Value_Power)) / lag(Mean_Value_Power)) * 100, NA_real_),
    `Year to Date Change % - Power` = if_else(First_Row_Per_Group & first(Mean_Value_Power) != 0,
                                              ((Mean_Value_Power - first(Mean_Value_Power)) / first(Mean_Value_Power)) * 100, NA_real_),
    `Monthly Change % - Acceleration` = if_else(First_Row_Per_Group & lag(Mean_Value_Acceleration) != 0,
                                                ((Mean_Value_Acceleration - lag(Mean_Value_Acceleration)) / lag(Mean_Value_Acceleration)) * 100, NA_real_),
    `Year to Date Change % - Acceleration` = if_else(First_Row_Per_Group & first(Mean_Value_Acceleration) != 0,
                                                     ((Mean_Value_Acceleration - first(Mean_Value_Acceleration)) / first(Mean_Value_Acceleration)) * 100, NA_real_)
  ) %>%
  mutate(
    `Year to Date Change - Power` = if_else(row_number() == 1, NA_real_, `Year to Date Change - Power`),
    `Year to Date Change % - Power` = if_else(row_number() == 1, NA_real_, `Year to Date Change % - Power`),
    `Year to Date Change - Acceleration` = if_else(row_number() == 1, NA_real_, `Year to Date Change - Acceleration`),
    `Year to Date Change % - Acceleration` = if_else(row_number() == 1, NA_real_, `Year to Date Change % - Acceleration`)
  ) %>% 
  ungroup()

final_strength_data <- final_strength_data %>% 
  arrange(Name, `Exercise Name`, Date) %>%
  group_by(Name, `Exercise Name`, Month) %>%
  mutate(
    Mean_Monthly_Value = case_when(
      `Exercise Type` == "Weightroom" ~ mean(`Max Value`, na.rm = TRUE),
      `Exercise Type` == "ForceDeck: CMJ" ~ mean(`Concentric Peak Force [N]`, na.rm = TRUE),
      `Exercise Type` == "ForceDeck: ISO Squat Hold" ~ mean(`Peak Vertical Force [N]`, na.rm = TRUE),
      `Exercise Type` == "ForceDeck: ISO Belt Squat" ~ mean(`Peak Vertical Force [N]`, na.rm = TRUE),
      `Exercise Type` == "ForceDeck: ShoulderISO" ~ mean(`Peak Vertical Force [N]`, na.rm = TRUE),
      `Exercise Type` == "ForceDeck: Squat Jump" ~ mean(`Takeoff Peak Force [N]`, na.rm = TRUE),
      `Exercise Type` == "ForceDeck: RSI" ~ mean(`Mean RSI (Jump Height/Contact Time) [m/s]`, na.rm = TRUE),
      TRUE ~ NA_real_
    ),
    First_Row_Per_Group = row_number() == 1
  ) %>% 
  group_by(Name, `Exercise Name`) %>%
  mutate(
    `Monthly Change` = if_else(First_Row_Per_Group, Mean_Monthly_Value - lag(Mean_Monthly_Value), NA_real_),
    `Year to Date Change` = if_else(First_Row_Per_Group, Mean_Monthly_Value - first(Mean_Monthly_Value), NA_real_),
    `Monthly Change %` = if_else(First_Row_Per_Group & lag(Mean_Monthly_Value) != 0,
                                 ((Mean_Monthly_Value - lag(Mean_Monthly_Value)) / lag(Mean_Monthly_Value)) * 100, NA_real_),
    `Year to Date Change %` = if_else(First_Row_Per_Group & first(Mean_Monthly_Value) != 0,
                                      ((Mean_Monthly_Value - first(Mean_Monthly_Value)) / first(Mean_Monthly_Value)) * 100, NA_real_)
  ) %>%
  mutate(
    `Year to Date Change` = if_else(row_number() == 1, NA_real_, `Year to Date Change`),
    `Year to Date Change %` = if_else(row_number() == 1, NA_real_, `Year to Date Change %`)
  ) %>% 
  ungroup()

final_strength_data <- final_strength_data %>% 
  mutate(Year = year(Date)) %>% 
  select("Date", "Month", "Year", "Name", "Membership", "Membership Length", "Attendance", "Level", "Gender", "Exercise Type", "Exercise Name", "power - high", "acceleration - high", 
         "Monthly Change - Power", "Monthly Change % - Power", "Year to Date Change - Power", "Year to Date Change % - Power", 
         "Monthly Change - Acceleration", "Monthly Change % - Acceleration", "Year to Date Change - Acceleration", "Year to Date Change % - Acceleration", 
         "PowerPercentileRank", "AccelerationPercentileRank", "Mean RSI (Jump Height/Contact Time) [m/s]", "Jump Height (Imp-Mom) [cm]", "Jump Height (Imp-Mom) in Inches [in]", 
         "Eccentric Duration [ms]", "Takeoff Peak Force [N]", "Peak Landing Force % (Asym) (%)", "Concentric Peak Force [N]", "Peak Vertical Force [N]",
         "Max Value", "PercentileRank", "Monthly Change", "Monthly Change %", "Year to Date Change", "Year to Date Change %")

write_csv(final_strength_data, "/Users/watts/Documents/Futures Performance Center/Data/Facility Data/StrengthFacilityData.csv", na = '')



# Speed -------------------------------------------------------------------



hardNinety <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Speed Percentiles/Hard90percentiles.csv") %>% 
  rename(`Exercise Type` = Name, Name = FullName) %>% 
  mutate(`Exercise Name` = "Hard 90",
         Date = ymd(Date)) %>%
  filter(Month != "July")

accelerationData <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Speed Percentiles/AccelerationPercentiles.csv") %>% 
  rename(`Exercise Type` = Name, Name = FullName) %>% 
  mutate(`Exercise Name` = "Acceleration",
         Date = ymd(Date)) %>%
  filter(Month != "July")

maxVeloData <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Speed Percentiles/MaxVelocityPercentiles.csv") %>% 
  rename(`Exercise Type` = Name, Name = FullName, Split1 = `10 yard fly`) %>% 
  mutate(`Exercise Name` = "Max Velocity",
         Date = ymd(Date)) %>%
  filter(Month != "July")

fortyYard <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Speed Percentiles/40yardpercentiles.csv") %>% 
  rename(`Exercise Type` = Name, Name = FullName) %>% 
  mutate(`Exercise Name` = "40 Yard Dash",
         Date = ymd(Date)) %>%
  filter(Month != "July")

RSIdata <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Speed Percentiles/RSIpercentiles.csv") %>% 
  rename(`Exercise Type` = Name, Name = FullName) %>% 
  mutate(`Exercise Name` = "Reactive Strength Index",
         Date = ymd(Date)) %>%
  filter(Month != "July")

attendanceData <- read_csv("/Users/watts/Downloads/CheckIns.csv") %>%
  rename(Name = Client) %>% 
  mutate(Date = as.Date(Date, format = "%b %d, %Y"),
         Month = month(Date, label = TRUE, abbr = FALSE),
         LearningBlock = case_when(
           `Service Name` == "Learning Academy - Block 1" ~ "Learning Academy - Attended",
           `Service Name` == "Learning Academy - Block 2" ~ "Learning Academy - Attended",
           TRUE ~ `Service Name`
         )) %>%
  filter(LearningBlock %in% c("Speed L1", "Speed L2", "Speed L3", 
                               "Professional - Speed Training", "Learning Academy - Attended")) %>% 
  distinct(Name, Date, LearningBlock, .keep_all = TRUE) %>%
  group_by(Name) %>%
  mutate(FirstCheckIn = min(Date),
         LastCheckIn = max(Date),
         `Membership Length` = round(as.period(interval(FirstCheckIn, LastCheckIn)) / months(1), 1)) %>%
  ungroup()

summary_attendanceData <- attendanceData %>%
  group_by(Name, Month, `Membership Length`) %>%
  summarise(Attendance = n(), 
            .groups = 'drop')

trainingGroup <- read_csv("/Users/watts/Downloads/Memberships.csv") %>% 
  filter(Membership %in% c(".Unlimited Base Strength/Speed/Mobility (Annual)", ".Unlimited Pro Strength/Speed/Mobility (Annual)", 
                           ".Speed Training", ".Speed Training (Annual)", ".Strength & Speed Base", ".Strength & Speed Base (Annual)", ".Strength & Speed Pro", 
                           "Learning Academy (pre-sale price)", "Learning Academy (paid in full discount)", "Learning Academy", 
                           "Speed Training", "Speed Training - Base/Pro", "Speed Training (Annual)",
                           "Futures Pro -  College or MiLB/Independent Baseball", "Futures Pro - MiLB with MLB time", "Futures Pro - MLB Minimum",
                           "Professional - MiLB or Independent Baseball", "Professional - MLB Minimum", "Professional - MiLB with MLB time",
                           "Collegiate/Pro Training - Pro w/ Coach", "Collegiate/Pro Training - Pro (Facility Access)",
                           "SoCal Birds Membership (youth) - Speed Training"))

priority_list <- c(".Unlimited Base Strength/Speed/Mobility (Annual)", ".Unlimited Pro Strength/Speed/Mobility (Annual)", 
                   ".Speed Training", ".Speed Training (Annual)", ".Strength & Speed Base", ".Strength & Speed Base (Annual)", ".Strength & Speed Pro", 
                   "Learning Academy (pre-sale price)", "Learning Academy (paid in full discount)", "Learning Academy", 
                   "Speed Training", "Speed Training - Base/Pro", "Speed Training (Annual)",
                   "Futures Pro -  College or MiLB/Independent Baseball", "Futures Pro - MiLB with MLB time", "Futures Pro - MLB Minimum",
                   "Professional - MiLB or Independent Baseball", "Professional - MLB Minimum", "Professional - MiLB with MLB time",
                   "Collegiate/Pro Training - Pro w/ Coach", "Collegiate/Pro Training - Pro (Facility Access)",
                   "SoCal Birds Membership (youth) - Speed Training")

# Filter the data to include only the specified memberships
filtered_memberships <- trainingGroup %>%
  filter(Membership %in% priority_list) %>%
  mutate(
    Membership = factor(Membership, levels = priority_list),
    Name = paste(`First Name`, `Last Name`)
  ) %>%
  arrange(Membership) %>%
  group_by(Name, Email) %>%
  slice(1) %>%
  ungroup()

filtered_memberships <- filtered_memberships %>% 
  select(Name, Membership)

speedData <- bind_rows(hardNinety, accelerationData, maxVeloData, fortyYard, RSIdata)

updated_speed_data <- left_join(speedData, summary_attendanceData, by = c("Name", "Month"))

final_speed_data <- left_join(updated_speed_data, filtered_memberships, by = "Name")

final_speed_data <- final_speed_data %>%
  arrange(Name, `Exercise Name`, Date) %>%
  group_by(Name, `Exercise Name`, Month) %>%
  mutate(
    Mean_Monthly_Value = case_when(
      `Exercise Name` == "Hard 90" ~ mean(Cumulative2, na.rm = TRUE),
      `Exercise Name` == "Acceleration" ~ mean(Split1, na.rm = TRUE),
      `Exercise Name` == "Max Velocity" ~ mean(MPH, na.rm = TRUE),
      `Exercise Name` == "40 Yard Dash" ~ mean(Cumulative3, na.rm = TRUE),
      `Exercise Name` == "Reactive Strength Index" ~ mean(`Mean RSI (Jump Height/Contact Time) [m/s]`, na.rm = TRUE),
      TRUE ~ NA_real_
    ),
    # Indicate the first row of each group
    First_Row_Per_Group = row_number() == 1
  ) %>%
  group_by(Name, `Exercise Name`) %>%
  mutate(
    `Monthly Change` = if_else(First_Row_Per_Group,
                               case_when(
                                 `Exercise Name` == "Hard 90" ~ lag(Mean_Monthly_Value) - Mean_Monthly_Value,
                                 `Exercise Name` == "Acceleration" ~ lag(Mean_Monthly_Value) - Mean_Monthly_Value,
                                 `Exercise Name` == "Max Velocity" ~ Mean_Monthly_Value - lag(Mean_Monthly_Value),
                                 `Exercise Name` == "40 Yard Dash" ~ lag(Mean_Monthly_Value) - Mean_Monthly_Value,
                                 `Exercise Name` == "Reactive Strength Index" ~ Mean_Monthly_Value - lag(Mean_Monthly_Value),
                                 TRUE ~ NA_real_
                               ), NA_real_),
    `Year to Date Change` = if_else(First_Row_Per_Group,
                                    case_when(
                                      `Exercise Name` == "Hard 90" ~ first(Mean_Monthly_Value) - Mean_Monthly_Value,
                                      `Exercise Name` == "Acceleration" ~ first(Mean_Monthly_Value) - Mean_Monthly_Value,
                                      `Exercise Name` == "Max Velocity" ~ Mean_Monthly_Value - first(Mean_Monthly_Value),
                                      `Exercise Name` == "40 Yard Dash" ~ first(Mean_Monthly_Value) - Mean_Monthly_Value,
                                      `Exercise Name` == "Reactive Strength Index" ~ Mean_Monthly_Value - first(Mean_Monthly_Value),
                                      TRUE ~ NA_real_
                                    ), NA_real_),
    `Monthly Change %` = if_else(First_Row_Per_Group,
                                 case_when(
                                   `Exercise Name` == "Hard 90" ~ ((lag(Mean_Monthly_Value) - Mean_Monthly_Value) / lag(Mean_Monthly_Value)) * 100,
                                   `Exercise Name` == "Acceleration" ~ ((lag(Mean_Monthly_Value) - Mean_Monthly_Value) / lag(Mean_Monthly_Value)) * 100,
                                   `Exercise Name` == "Max Velocity" ~ ((Mean_Monthly_Value - lag(Mean_Monthly_Value)) / lag(Mean_Monthly_Value)) * 100,
                                   `Exercise Name` == "40 Yard Dash" ~ ((lag(Mean_Monthly_Value) - Mean_Monthly_Value) / lag(Mean_Monthly_Value)) * 100,
                                   `Exercise Name` == "Reactive Strength Index" ~ ((Mean_Monthly_Value - lag(Mean_Monthly_Value)) / lag(Mean_Monthly_Value)) * 100,
                                   TRUE ~ NA_real_
                                 ), NA_real_),
    `Year to Date Change %` = if_else(First_Row_Per_Group,
                                      case_when(
                                        `Exercise Name` == "Hard 90"~ ((first(Mean_Monthly_Value) - Mean_Monthly_Value) / first(Mean_Monthly_Value)) * 100,
                                        `Exercise Name` == "Acceleration" ~ ((first(Mean_Monthly_Value) - Mean_Monthly_Value) / first(Mean_Monthly_Value)) * 100,
                                        `Exercise Name` == "Max Velocity" ~ ((Mean_Monthly_Value - first(Mean_Monthly_Value)) / first(Mean_Monthly_Value)) * 100,
                                        `Exercise Name` == "40 Yard Dash"~ ((first(Mean_Monthly_Value) - Mean_Monthly_Value) / first(Mean_Monthly_Value)) * 100,
                                        `Exercise Name` == "Reactive Strength Index" ~ ((Mean_Monthly_Value - first(Mean_Monthly_Value)) / first(Mean_Monthly_Value)) * 100,
                                        TRUE ~ NA_real_
                                      ), NA_real_)
  ) %>% 
  mutate(
    `Year to Date Change` = if_else(row_number() == 1, NA_real_, `Year to Date Change`),
    `Year to Date Change %` = if_else(row_number() == 1, NA_real_, `Year to Date Change %`)
  ) %>% 
  ungroup() 

final_speed_data <- final_speed_data %>% 
  mutate(Year = year(Date)) %>% 
  select("Date", "Month", "Year", "Name", "Membership", "Membership Length", "Attendance", "Level", "Gender", "Exercise Name", 
         "Exercise Type", "Split1", "Split2", "Split3", "Cumulative1", "Cumulative2", "Cumulative3", "MPH", 
         "Mean RSI (Jump Height/Contact Time) [m/s]", "Jump Height (Flight Time) [cm]", "Contact Time [ms]", "PercentileRank", 
         "Monthly Change", "Monthly Change %", "Year to Date Change", "Year to Date Change %")

write_csv(final_speed_data, "/Users/watts/Documents/Futures Performance Center/Data/Facility Data/SpeedFacilityData.csv", na = '')


