# Load required packages

library(sf)
library(terra)
library(RSQLite)

#-----------------------------------------------------------------------
# Import data
#-----------------------------------------------------------------------

HWSD_SMU <- read.csv("C:/Users/lmills96/OneDrive - UBC/MSc Thesis Info/Global Analysis/HWSD2_SMU.csv")
HWSD_SMU <- HWSD_SMU[,-c(3:9,11:23)] 

names(HWSD_SMU)[1] <- "soil_type"
names(HWSD_SMU)[2] <- "code"

soildf <- MPdf[,-c(3:10,12,13,14)]
soildfprac <- soildf

# Remove decimals from soildf "soil_type" column (could use 
# floor(soildf$soil_type) if wanting to round down)
soildf$soil_type_code <- as.integer(round(soildf$soil_type))
soildf <- soildf[,-3]
names(soildf)[3] <- "soil_type"

# Determine the number of unique values in column
length(unique(soildf$soil_type))
unique_soil_type <- unique(soildf$soil_type)

soil_data <- data.frame()

# Match values in soil_type column from HWSD_SMU to soildf (match values in a 
# column from one data frame to another data frame)
soil_data <- soildf %>%
  left_join(distinct(HWSD_SMU, soil_type, .keep_all = TRUE), by = "soil_type")

# Note: manually added soil type based on code to soil_data code 
# (called soil_types_data)
write.csv(soil_data, "C:/Users/lmills96/OneDrive - UBC/MSc Thesis Info/Global Analysis/Global MP Distribution/soil_data.csv")




# Other code for the data frame: 

# To see if and where a number appears in a data frame:
which(soildf== 7001, arr.ind=TRUE)

# Find the rows of a data frame where certain values appear in any of the columns
library(dplyr)
HWSD_SMU %>% filter_all(any_vars(. %in% c('5394')))

# Select rows where a certain number appears in any column
soildf %>% filter_all(any_vars(. %in% c(7001)))
