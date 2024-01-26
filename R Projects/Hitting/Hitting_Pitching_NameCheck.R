# Load necessary library
library(plyr)

# Read the CSV files
blastData <- read.csv("/Volumes/COLE'S DATA/Data/Blast Master Data - Sheet1.csv") %>% 
  filter(Month == "November")

hittraxData <- read.csv("/Volumes/COLE'S DATA/Data/Hittrax Master Data - Sheet1.csv") %>% 
  filter(Month == "November")

# Extract names from each dataset
blastNames <- blastData$Name
hittraxNames <- hittraxData$Name

# Find names in hittraxData not in blastData
names_in_hittrax_not_in_blast <- setdiff(hittraxNames, blastNames)

# Find names in blastData not in hittraxData
names_in_blast_not_in_hittrax <- setdiff(blastNames, hittraxNames)

# Create two separate data frames
df1 <- data.frame(Names_In_Hittrax_Not_In_Blast = names_in_hittrax_not_in_blast)
df2 <- data.frame(Names_In_Blast_Not_In_Hittrax = names_in_blast_not_in_hittrax)

# Combine the data frames
results <- rbind.fill(df1, df2)

# Output to CSV
write.csv(results, "/Users/watts/Downloads/HittingNameCheck.csv", row.names = FALSE, na = "")
