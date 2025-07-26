
message("Processing the MP dataset")

# Load required packages
library(terra)

#--------------------------------------------------------------------------
# Load required rasters 
#--------------------------------------------------------------------------
soil <- rast("./Rasters/soi_raster_processed.tif")
HFI <- rast("./Rasters/HFI_raster_processed.tif")
Elevation_m <- rast("./Rasters/elev_raster_processed.tif")

#--------------------------------------------------------------------------
# Cleaning up MP dataset
#--------------------------------------------------------------------------     

# Load MPdf
MPdf <- read.csv("./Data/MPdf_total.csv") 

# Remove calculation columns and DOI, notes, title, and year_pub columns
MPdf <- MPdf[,-c(1:6,9,10,15:27)]

# Ensure the study number is a factor
as.factor(MPdf$study_num)

# Remove non-workable papers
MPdf <- na.omit(MPdf)

#--------------------------------------------------------------------------
# Data processing
#--------------------------------------------------------------------------

# Define the projection system
crs_wintri <- "ESRI:53018"

# Projections are easting, northing
locations <- st_as_sf(MPdf, coords = c("longitude", "latitude"), crs="+proj=longlat +datum=WGS84")

# Convert to spactVect class
locations <- vect(locations)

# Reproject
locations <- terra::project(locations, crs_wintri)

# Get local HFI values
MPdf$HFI <- terra::extract(HFI, locations)[,2]

# Get local elevation values
MPdf$Elevation_m <- terra::extract(Elevation_m, locations)[,2]

# Get local soil types
MPdf$soil_type <- terra::extract(soil, locations)[,2]

# Storing the projected coordinates in case they are needed
MPdf$x <- geom(locations)[,3]
MPdf$y <- geom(locations)[,4]

names(MPdf)[9] <- "HFI"

# Need "Study" as a factor 
MPdf$Study <- as.factor(MPdf$Study)

#MPdf$HFI_new <- MPdf_new$HFI


#--------------------------------------------------------------------------
# Finalizing dataset 
#--------------------------------------------------------------------------

# Remove row 633 from study 19 (outlier)
MPdf <- MPdf[-c(663)]

# Save final dataset
write.csv(MPdf, file = "./Data/MPdf.csv", row.names = FALSE)
