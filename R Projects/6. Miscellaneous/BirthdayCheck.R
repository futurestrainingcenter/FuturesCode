library(dplyr)
library(readxl)
library(readr)
library(lubridate)

clientData <- read_csv("/Users/watts/Downloads/FullClientList.csv") %>% 
  rename(Name = Client, Level = `Reporting Level (Age-Dependent)`) %>% 
  filter(!is.na(Level) & `Client Type` %in% c("Active Member", "Active Pass Holder"))

calculate_age <- function(birthdate) {
  if (is.na(birthdate)) {
    return(NA)
  } else {
    birthdate <- ymd(birthdate) # Convert to Date using lubridate
    age <- interval(start = birthdate, end = Sys.Date()) / years(1)
    return(floor(age)) # Floor the age to get complete years
  }
}

clientData$Age <- sapply(clientData$`field-general-7.dl_date`, calculate_age)

check_level <- function(age) {
  if (age >= 9 & age <= 11) {
    return("L1 (9u-11u)")
  } else if (age >= 12 & age <= 14) {
    return("L2 (12u-14u)")
  } else if (age >= 15 & age <= 18) {
    return("L3 (15u-18u)")
  } else {
    return(NA) # For ages outside the specified range
  }
}

# Add a column with the correct level based on age
clientData <- clientData %>%
  mutate(CorrectLevel = sapply(Age, check_level))

# Identify athletes with incorrect level assignment
incorrectlyAssigned <- clientData %>%
  filter(Level != CorrectLevel) %>% 
  select(Name, Age, CorrectLevel)
