
message("Processing the MP dataset")

# Load required packages
library(terra)

#--------------------------------------------------------------------------
# Load required rasters 
#--------------------------------------------------------------------------

soil <- rast("./Rasters/soil_processed.tif")
HFI <- rast("./Rasters/HFI_processed.tif")
elevation_m <- rast("./Rasters/elev_processed.tif")

#--------------------------------------------------------------------------
# Cleaning up MP dataset
#--------------------------------------------------------------------------     

# Load MPdf
MPdf <- read.csv("./Data/MPdf_total.csv") 

# Remove calculation columns and DOI, notes, title, and year_pub columns
MPdf <- MPdf[,-c(1:6,9,10,15:27)]

# Ensure the study number is a factor
# Need "Study" as a factor 
names(MPdf)[2] <- "study"
MPdf$study <- as.factor(MPdf$study)

# Remove non-workable papers
MPdf <- na.omit(MPdf)

#--------------------------------------------------------------------------
# Data processing
#--------------------------------------------------------------------------

# Define the projection system
crs_Mollweide <- "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"

# Projections are currently in lat/long so crs here needs to reflect that 
locations <- st_as_sf(MPdf, coords = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84")

# Convert to SpatVector (locations must be either SpatVector or SpatRaster to use terra::project())
locations <- vect(locations)

# Reproject SpatVector to the same CRS as all rasters
locations <- terra::project(locations, crs(HFI))

# Make sure it works: 
plot(HFI)
points(locations, col = "red")

# Get local HFI values
MPdf$HFI <- terra::extract(HFI, locations)[,2]

# Get local elevation values
MPdf$elevation_m <- terra::extract(elevation_m, locations)[,2]

# Get local soil types
MPdf$soil_type <- terra::extract(soil, locations)[,2]

# Storing the projected coordinates in case they are needed
MPdf$x <- geom(locations)[,3]
MPdf$y <- geom(locations)[,4]

#--------------------------------------------------------------------------
# Finalizing dataset 
#--------------------------------------------------------------------------

# Remove row 633 from study 19 (outlier)
MPdf <- MPdf[-c(632),]

# Save final dataset
write.csv(MPdf, file = "./Data/MPdf.csv", row.names = FALSE)
