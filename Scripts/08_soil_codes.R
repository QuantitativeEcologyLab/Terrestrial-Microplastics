
# Note: HWSD2.sqlite file is in Scripts

# Import the SQL lookup database
require(RSQLite)
m <- dbDriver("SQLite")
con <- dbConnect(m, dbname="./Scripts/HWSD2.sqlite")

# Build lookup table
soil_classes <- dbGetQuery(con, "select * from D_WRB4")

# Build a table of soil class names for Canadian soils
SOILS$Code <- NA
SOILS$Name <- NA
# Loop over all of the soil classes in Canada
for(i in 1:nrow(SOILS)){
  
  # Get soil class
  SOILS$Code[i] <- dbGetQuery(con, paste("select * from HWSD2_SMU where HWSD2_SMU_ID = ", SOILS[i,"value"]))$WRB4
  
  # Get soil name
  if(SOILS$Code[i] %in% soil_classes$CODE) {  SOILS$Name[i] <- soil_classes[soil_classes$CODE == SOILS$Code[i],"VALUE"]} else{
    
    SOILS$Name[i] <- "Unknown"
  }
  
}