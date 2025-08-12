
## Note: do not run anything on this script unless you are updating the cropped 
    # rasters - everything can be loaded in 06c_regression-Kriging.R

# ---------------------------------------------------------------------------
# Load required items
# ---------------------------------------------------------------------------

# Load required packages
library(terra)
library(future.apply)
library(sf)

# # Load required rasters
HFI <- rast("/home/lmills96/Documents/GDMP/Rasters/HFI_processed.tif")
elevation_m <- rast("/home/lmills96/Documents/GDMP/Rasters/elev_processed.tif")

# Load MPdf dataset 
MPdf <- read.csv("/home/lmills96/Documents/GDMP/Data/MPdf.csv")
MPdf$study <- as.factor(MPdf$study)

# Load gam 
model <- readRDS("/home/lmills96/Documents/GDMP/Data/model.RDS")

# ---------------------------------------------------------------------------
# Decrease raster resolutions
# ---------------------------------------------------------------------------

# Decrease raster resolution
HFI <- aggregate(HFI, fact = 3)
# Match resolution
elevation_m <- terra::resample(elevation_m, HFI, method = "bilinear")

# Need to mask elevation to HFI to ensure no water (initially cropped to 
# Mollweide projection in original raster but it looks like elevation raster 
# contains some additional water around shores (noticible in the newdf))
  # Mask the raster to HFI
  elevation_m <- terra::mask(elevation_m, HFI)

  res(HFI) == res(elevation_m) # TRUE
  same.crs(HFI, elevation_m) # TRUE
  ext(HFI) == ext(elevation_m) # TRUE

# Save updated rasters
writeRaster(HFI, filename = "/home/lmills96/Documents/GDMP/Rasters/HFI_300res.tif", overwrite = TRUE)
writeRaster(elevation_m, filename = "/home/lmills96/Documents/GDMP/Rasters/elev_300res.tif", overwrite = TRUE)

#----------------------------------------------------------------------
# Get bounding box of case example 
#----------------------------------------------------------------------

# Using study # 61 for a case study/example: 
# 50 observations in China 
# Note: coordinates were estimated, concentrations were given

# Subset data
MPdf_study <- MPdf[MPdf$study == "61",]
MPdf_study$study <- as.factor(MPdf_study$study)

# Convert df to sf object
locations_study <- st_as_sf(MPdf_study, coords = c("x", "y"), crs = crs(HFI))
same.crs(locations_study, HFI) # TRUE

plot(HFI)
points(locations_study, co = "red")

# Determine the bounding box from coordinates of study # 2
EXT <- ext(locations_study)

# Calculate height & width of bbox
# EXT[1] = xmin (left edge of raster)
# EXT[2] = xmax (right edge of raster)
# EXT[3] = ymin (bottom edge of raster)
# EXT[4] = ymax (top edge of raster)
height <- EXT[4] - EXT[3]
width <- EXT[2] - EXT[1]

# Add a buffer around edge of bbox (using max() to ensure that the buffer is 
# large enough to cover both dimensions symmetrically - choosing the larger
# of the two so that both x and y directions are padded equally by the same
# size)
size <- max(height*2, width*2)

# Center the extent to keep the same midpoint after adding the buffer
x_center <- (EXT[1] + EXT[2])/ 2 # finding midpoint / avg between the two coords
y_center <- (EXT[3] + EXT[4])/ 2 # finding midpoint / avg between the two coords
# Note: x_center is the midpoint of original raster

# Create a new square extent
# Note: 'size' is the desired FULL length side of the new bbox (including 
# the buffer)
# Note: dividing by 2 here to define how far from center to extend bbox - 
# by dividing by 2, we are evenly buffering the area 
new_EXT <- ext(x_center - size/2, # defines a bbox that extends half of 'size' to left of midpoint...
               x_center + size/2, # ...and half of 'size' to the right (total width = size)
               y_center - size/2,
               y_center + size/2)

# ---------------------------------------------------------------------------
# Crop rasters to bounding box
# ---------------------------------------------------------------------------

# Required rasters
HFI <- rast("/home/lmills96/Documents/GDMP/Rasters/HFI_300res.tif")
elevation_m <- rast("/home/lmills96/Documents/GDMP/Rasters/elev_300res.tif")

# Crop rasters to bbox
HFI_crop <- terra::crop(HFI, new_EXT)
elev_crop <- terra::crop(elevation_m, HFI_crop)

# See how samples look on cropped raster
plot(elev_crop)
points(locations_study, co = "red")

same.crs(HFI_crop, elev_crop) # TRUE
res(HFI_crop) == res(elev_crop) # TRUE
ext(HFI_crop) == ext(elev_crop) # TRUE

# Save updated rasters for Korea only
writeRaster(HFI_crop, filename = "/home/lmills96/Documents/GDMP/Rasters/HFI_crop.tif", overwrite = TRUE)
writeRaster(elev_crop, filename = "/home/lmills96/Documents/GDMP/Rasters/elev_crop.tif", overwrite = TRUE)

# ---------------------------------------------------------------------------
# Convert rasters to dataframe
# ---------------------------------------------------------------------------

# Convert HFI SpatRaster into dataframe
newdf <- terra::as.data.frame(HFI_crop, xy = TRUE, row.names = FALSE)
names(newdf)[3] <- "HFI"

# Extract elevation from elevation_global SpatRaster (elev_crop)
el_values <- terra::extract(elev_crop, cbind(newdf$x, newdf$y))

# Add elevation to the HFI data frame 
  # Note: this will change depending on the resolution used
newdf$elevation_m <- el_values$wc2.1_10m_elev

# Define the depth wanted to predict for (here just surface level)
newdf$max_depth_cm <- as.integer(0)

# Set a "study" Only needed for predicting
#newdf$study <- as.factor('prediction')
newdf$study <- factor("61", levels=levels(MPdf$study))
head(newdf)

# Predict MP concentrations using the fitted model
newdf$mu <- predict(model,
                    newdata = newdf,
                    type = 'link',
                    se.fit = FALSE,
                    exclude = "s(study)",
                    na.rm = TRUE)
head(newdf)

# Save newdf 
saveRDS(newdf, file = "/home/lmills96/Documents/GDMP/Data/newdf.RDS")


#----------------------------------------------------------------------
# Get bounding box of case example for study # 2 - NOT USING 
#----------------------------------------------------------------------

# Using study # 2 for a case study/example: 
# 100 observations in Korea 
# Note: coordinates were given in paper (table 2), along with [MP] broken 
# up into sizes. Particles <1mm and 1-5mm were added together to obtain
# total [MP]

# Subset data
MPdf_study <- MPdf[MPdf$study == "2",]
MPdf_study$study <- as.factor(MPdf_study$study)

# Convert df to sf object
locations_study <- st_as_sf(MPdf_study, coords = c("x", "y"), crs = crs(HFI))
same.crs(locations_study, HFI) # TRUE

plot(HFI)
points(locations_study, co = "red")

# Determine the bounding box from coordinates of study # 2
EXT <- ext(locations_study)

# Calculate height & width of bbox
# EXT[1] = xmin (left edge of raster)
# EXT[2] = xmax (right edge of raster)
# EXT[3] = ymin (bottom edge of raster)
# EXT[4] = ymax (top edge of raster)
height <- EXT[4] - EXT[3]
width <- EXT[2] - EXT[1]

# Add a buffer around edge of bbox (using max() to ensure that the buffer is 
# large enough to cover both dimensions symmetrically - choosing the larger
# of the two so that both x and y directions are padded equally by the same
# size)
size <- max(height*2, width*2)

# Center the extent to keep the same midpoint after adding the buffer
x_center <- (EXT[1] + EXT[2])/ 2 # finding midpoint / avg between the two coords
y_center <- (EXT[3] + EXT[4])/ 2 # finding midpoint / avg between the two coords
# Note: x_center is the midpoint of original raster

# Create a new square extent
# Note: 'size' is the desired FULL length side of the new bbox (including 
# the buffer)
# Note: dividing by 2 here to define how far from center to extend bbox - 
# by dividing by 2, we are evenly buffering the area 
new_EXT <- ext(x_center - size/2, # defines a bbox that extends half of 'size' to left of midpoint...
               x_center + size/2, # ...and half of 'size' to the right (total width = size)
               y_center - size/2,
               y_center + size/2)

