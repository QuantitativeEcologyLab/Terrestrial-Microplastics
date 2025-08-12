# Load required packages
library(dplyr)

# Load MPdf
MPdf <- read.csv("./Data/MPdf.csv")
df <- MPdf

# Note: HWSD2.sqlite file is in Scripts
# Note: require(RSQLite) provides an interface between R and SQLite databases
  # this allows you to create SQLite databases, read/write tables, database 
  # inference (DBI), etc.
      # DBI - dbGetQuery() runs an SWL query and returns the result
      #     - dbListTables() lists all tables in the database 
      #     - dbReadTable() read a table into R as a dataframe

# --------------------------------------------------------------------------
# Set up SQLite and dataframes
# --------------------------------------------------------------------------

# Import the SQL look-up database
require(RSQLite)
# Create a connection interface to a specific type of database in R (in this
# case, we're telling R what kind of databaswe we are connecting to - SQLite)
m <- dbDriver("SQLite")  
# Uses the driver object to open the connection to the .sqlite database file 
# (is connecting to the dateabse so now R can run queries and read/write data)
con <- dbConnect(m, dbname="./Data/HWSD2.sqlite") 


# Build look-up table
lookup1 <- dbGetQuery(con, "SELECT HWSD1_SMU_ID, 
                     WRB4 FROM HWSD2_SMU")

lookup2 <- dbGetQuery(con,
                      "SELECT CODE, VALUE FROM D_WRB4")

lookup <- lookup1 %>%
              left_join(lookup2, by = c("WRB4" = "CODE")) %>%
              rename(soil_name = VALUE)

# lookup <- dbGetQuery(con,
#                     "SELECT
#                       H.HWSD1_SMU_ID,
#                       H.WRB4,
#                       D.VALUE AS soil_name
#                     FROM
#                       HWSD2_SMU H
#                     LEFT JOIN
#                       D_WRB4 D
#                     ON
#                       H.WRB4 = D.CODE")

# --------------------------------------------------------------------------
# Add soil types and soil names based off current soil codes in MPdf
# --------------------------------------------------------------------------

# Rounding soil_code to nearest whole number
df$soil_code <- round(df$soil_code)

# Duplicated WRB4 values for different codes 
lookup_dup <- lookup1[duplicated(lookup1$WRB4),]

# Removing duplicates by using only the first value that shows up in lookup df
# lookup_unique <- lookup %>%
#   group_by(HWSD1_SMU_ID) %>%
#   summarise(WRB4 = first(WRB4))

lookup_unique <- lookup %>%
  distinct(HWSD1_SMU_ID, .keep_all = TRUE)

# Ensure soil codes are characters
df$soil_code <- as.character(df$soil_code)
lookup_unique$HWSD1_SMU_ID <- as.character(lookup_unique$HWSD1_SMU_ID)
# Note: use character when numbers are simply identifiers / labels without 
# any numerical meaning (ID #) - use factor when numbers represent distinct
# categories / groups

# Add soil types back into MPdf
df <- left_join(df, lookup_unique, by = c("soil_code" = "HWSD1_SMU_ID")) %>%
  rename("soil_type" = "WRB4")

# Identify which rows of soil_type have NAs
NA_rows <- df[!complete.cases(df),]

# --------------------------------------------------------------------------
# Need to change existing soil_code values that produce NAs in soil_type column 
# to valid HWSD1_SMU_ID codes
# --------------------------------------------------------------------------

# Convert back to numeric
df$soil_code <- as.numeric(df$soil_code)
lookup_unique$HWSD1_SMU_ID <- as.numeric(lookup_unique$HWSD1_SMU_ID)

# Flag unmatched codes in MPdf
df$unmatched <- !(df$soil_code %in% lookup_unique$HWSD1_SMU_ID)

# Replace the soil codes producing NAs to the nearest code in lookup df
find_closest <-
  
  # defining a function that takes 2 arguments: 
    # code: a single number from MPdf that doesn't exist in lookup
    # reference_values: a numeric vector (all valid soil codes from HMSD1)
  function(code, reference_values) {
    
  # subtracts the code from each other code value (absolute to keep it positive)
  # which.min() finds the smallest value - i.e., the closest match  
    reference_values[which.min(abs(reference_values - code))]
  }

# Duplicate soil_code column
df$corrected_soil_code <- df$soil_code

# Run function
  # subsetting the column corrected_soil_code only for rows where there are NAs 
  # i.e., unmatched == TRUE
df$corrected_soil_code[df$unmatched] <- 
  # sapply() to return a simplified vector over a list (if possible) - ideal
  # when we only want one output per input
    sapply(
    df$soil_code[df$unmatched],
    find_closest,
    reference_values = lookup_unique$HWSD1_SMU_ID
)

# --------------------------------------------------------------------------
# Re-add soil types and soil names based off new soil codes
# --------------------------------------------------------------------------

# One HMSD1_SMU_ID does not have a corresponding WRB4 - manually changing this
df[1688, "corrected_soil_code"] <- 47742

# Add values back into MPdf where NAs were originally
df <- df[,-c(12,13)]
df <- left_join(df, lookup_unique, by = c("corrected_soil_code" = "HWSD1_SMU_ID")) %>%
  rename("soil_type" = "WRB4")

# Ensure no more NAs 
NA_rows <- df[!complete.cases(df),]
  # Several WRB4 values appear to be a combo of soils / don't have just one name
  df$soil_name[is.na(df$soil_name)] <- "unknown"



# Remove soil_code, corrected_soil_code, and unmatched columns
df <- df[,-c(9,12,13)]
MPdf <- df

# Save final dataset
MPdf$study <- as.factor(MPdf$study)
write.csv(MPdf, file = "./Data/MPdf.csv", row.names = FALSE)