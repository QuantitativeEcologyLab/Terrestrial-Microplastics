
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

# Load bounding box of case study (new_EXT)
new_EXT <- readRDS("./Data/new_EXT.RDS")

# Load Kriged residuals
# OK_500 <- loadRDS("./Data/OK_500.RDS")

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
#MPdf$residuals <- residuals(model, type = "working") 

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


print(mp.vg.fit) 
# Note: 
  # 1) vg range = 12,527 meters, Kriging is based of bounding box of 306,864 m^2 
  # 2) total sill = nugget + partial sill = 2.493077
      # the portion of variance explained by spatial structure = partial/total
      # = 0.4855 / 48.6%
      # 51.4% is the nugget (variation at very small spatial scales - i.e., 
      # measurement error, microscale randomness, etc.)
  # 3) if half the variation is from nugget, predicting at very fine scales may
      # mean the unaccounted-for variation by the nugget makes predictions      
      # less accurate
  # 4) using a 500m x 500m grid size for predictions


# Create a grid of square polygons over the bbox (at 100 res)
grid_500_sf <- st_make_grid(China_bbox_sf, cellsize = c(500, 500))

# Ordinary Kriging of the residuals (will be an sf data frame)
OK_500 <- 
  krige(residuals ~ 1, # kriging the residuals = the formula
        vg.data, # provides coords & residuals from sample locations = the locations
        grid_500_sf, # gives the locations to predict the residuals at = newdata
        model = mp.vg.fit) # gives theoretical vg model describing how residuals vary with distance = model
saveRDS(OK_500, file = "/home/lmills96/Documents/GDMP/Rasters/OK_500.tif")


# Convert the Kriging back to SpatRaster 
  # Note: using the tidyterra package for convenience, 
  # not because it's the only way to do this
Kriging_raster <- tidyterra::as_spatraster(OK_500, crs = crs(HFI))
 
# Extract the predicted values at each location (the interpolated residuals) 
  # Note: Kriging_raster["var1.pred] is returning a subset of Kriging_raster 
    # with ONLY the column of "var1.pred" vs. Kriging_raster$var1.pred would 
    # return a vector
  # Note: Kriging_raster["var1.var"] is the Kriging variance (uncertainty of the 
    # prediction at each location) so we do not want this
Kriging_raster <- Kriging_raster["var1.pred"]

plot(Kriging_raster)

# Crop 
Kriging_raster_cropped <- Kriging_raster %>%
  terra::crop(HFI_crop, mask = T)

# Match all resolution, extent, and CRS of Kriged residuals to the GAM raster
var1_resampled <- terra::resample(Kriging_raster_cropped, MP_prediction_model, 
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