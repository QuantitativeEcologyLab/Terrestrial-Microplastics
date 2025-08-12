
message("Predicting MP concentrations from the fitted model")

#----------------------------------------------------------------------
# Load all packages, data, models, and rasters
#----------------------------------------------------------------------

# Load required packages
library(sp)
library(terra)
library(sf)
library(gstat) # for Kriging
library(tidyverse)
library(ggplot2)
library(tidyterra)

# Load required rasters 
HFI <- rast("./Rasters/HFI_crop.tif")
elevation_m <- rast("./Rasters/elev_crop.tif")

# Load MPdf dataset 
MPdf <- read.csv("./Data/MPdf.csv")

# Load gam 
model <- readRDS("./Data/MPdf_model.RDS")

# Load newdf --- NOT NEEDED as GAM prediction raster is saved below
newdf <- readRDS("./Data/newdf.RDS")

# Load the GAM prediction raster
MP_prediction_model <- rast("./Rasters/GAM_prediction_model.tif")

# Load Kriged residuals
# OK_100 <- loadRDS("./Data/OK_100.RDS")

# Load var1 raster
# var1_resampled <- rast("./Rasters/var1_resampled.tif")




#----------------------------------------------------------------------
# Predictions from the GAM
#----------------------------------------------------------------------

# Converting the predictions in the newdf (containing coordinates & predicted 
# values) into a SpatRaster
MP_prediction_model <- terra::rast(newdf[c("x", "y", "mu")], type="xyz", crs = crs(HFI))

# Make sure it looks fine
mp_pred_model_pic <- plot(MP_prediction_model)

# Save GAM prediction raster
writeRaster(MP_prediction_model, 
            filename = "/home/lmills96/Documents/GDMP/Rasters/GAM_prediction_model.tif", 
            overwrite = TRUE)

#----------------------------------------------------------------------
# Kriging the residuals
#----------------------------------------------------------------------

# Import the residuals 
MPdf$residuals <- residuals(model, type = "working") 

#Subset data
MPdf_study <- MPdf[MPdf$study == "61",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))

# Empirical variogram for the MP model residuals (study 2)
mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 80000)
plot(mp.vg)

# Note: 'Fit' model is the result of fitting a theoretical variogram
  # model to the empirical variogram, providing estimates for model parameters
  # that characterize the spatial structure of the residuals
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 


# Convert HFI to an sf crs object
crs_sf <- sf::st_crs(terra::crs(HFI))

# Convert the new_EXT bbox to an sf polygon 
China_bbox_sf <- st_as_sfc(st_bbox(new_EXT))

# Assign the same CRS to China_bbox_sf as other sf objects
sf::st_crs(China_bbox_sf) <- crs_sf

# Create a grid of square polygons over the bbox (at 100 res)
grid_10_sf <- st_make_grid(China_bbox_sf, cellsize = c(10, 10))

# Ordinary Kriging of the residuals (will be an sf data frame)
OK_10 <- 
  krige(residuals ~ 1, # kriging the residuals = the formula
        vg.data, # provides coords & residuals from sample locations = the locations
        grid_10_sf, # gives the locations to predict the residuals at = newdata
        model = mp.vg.fit) # gives theoretical vg model describing how residuals vary with distance = model
saveRDS(OK_10, file = "/home/lmills96/Documents/GDMP/Rasters/OK_10.tif")


# Convert the Kriging back to SpatRaster 
  # Note: using the tidyterra package for convenience, 
  # not because it's the only way to do this
tester <- tidyterra::as_spatraster(OK_10, crs = crs(HFI))
  # Note: tester["var1.pred] is returning a subset of tester with ONLY the
  # column of "var1.pred" vs. tester$var1.pred would return a vector
tester <- tester["var1.pred"]

plot(tester)

# Crop 
tester_cropped <- tester %>%
  terra::crop(HFI_crop, mask = T)

# Match all resolution, extent, and CRS of Kriged residuals to the GAM raster
var1_resampled <- terra::resample(tester_cropped, MP_prediction_model, 
                                  method = "bilinear")

# Save the Kriging predictions 
writeRaster(var1_resampled, filename = "./Rasters/var1_resampled.tif", 
            overwrite = TRUE)

# Plot to visualize
plot(var1_resampled)
points(vg.data, col = "red", pch = 20)

#----------------------------------------------------------------------
# Combined: regression-Kriging predictions
#----------------------------------------------------------------------

# Get the regression-Kriging predictions by summing the two
RK <- MP_prediction_model + var1_resampled

# Convert back to the correct scale 
RK <- exp(RK) 
plot(RK)
writeRaster(RK, filename = "./Rasters/RK_raster.tif", overwrite = TRUE)




# -------------------------------------

# Global Kriging ------------------------------------------------------
# This is currently at 100,000 resolution (low)

# Get bounding box of elevation raster
elev_bbox <- st_bbox(Elevation_m)

# Convert elevation bounding box to an sf polygon
elev_sf <- st_as_sfc(elev_bbox)

# Create a grid of square polygons over the bounding box (at 100000 res)
grid_100000 <- st_make_grid(elev_sf, cellsize = c(100000, 100000))
# note: pixel size is large for fast computation time

grid_100000_sf <- sf::st_sf(grid_100000) # as sf object rather than sfc?
#saveRDS(grid_100000, file = "grid_100000.RDS")
test_crs <- readRDS("/home/lmills96/Downloads/kriging_100000.RDS")
# Ordinary Kriging of the residuals
kriging_100000 <-
  krige(residuals ~ 1, # kriging the residuals = the formula
        vg.data, # provides coordinates & residuals from sample locations = the locations
        grid_100000_sf, # gives the locations to predict the residuals at = newdata
        model = mp.vg.fit) # gives theoretical vg model describing how residuals vary with distance = model
# note: if you were to do universal kriging, formula would be z ~ x+y rather than z ~ 1
#saveRDS(kriging_100000, file = "./Data/kriging_100000.RDS")
