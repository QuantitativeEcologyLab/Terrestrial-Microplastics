
message("Predicting global MP concentrations from the fitted model")

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

# Load required rasters 
HFI <- rast("./Rasters/HFI_raster_processed.tif")
Elevation_m <- rast("./Rasters/elev_raster_processed.tif")

# Load MPdf dataset 
MPdf <- read.csv("./Data/MPdf.csv")

# Load gam 
model <- readRDS("./Data/MPdf_model.RDS")

# Load newdf --- not needed as GAM prediction raster below is saved
#newdf <- readRDS("./Data/MP_Prediction_newdf.RDS")

# Load the GAM prediction raster
MP_prediction_model <- rast("./Rasters/GAM_prediction_model.tif")

# Load Kriged residuals
kriging_100000 <- loadRDS("./Data/kriging_100000.RDS")

# Load var1 raster
var1_resampled <- rast("./Rasters/var1_resampled.tif")

# Define the projection system
crs_wintri <- "ESRI:53018"

# Get world_wintri:
# Get world boundaries for clipping (not critical, but makes maps nicer looking)
world_sf <- st_as_sf(rworldmap::getMap(resolution = "low"))
#Drop Antarctica
world_sf <- subset(world_sf, continent != "Antarctica")
# Reproject
world_wintri <- lwgeom::st_transform_proj(world_sf, crs = "ESRI:53018")
# Convert to SpatVector class
world_wintri <- vect(world_wintri) 



#----------------------------------------------------------------------
# Predictions from the GAM
#----------------------------------------------------------------------

# Convert SpatRaster into dataframe
newdf <- terra::as.data.frame(HFI, xy = TRUE, row.names = FALSE)
names(newdf)[3] <- "HFI"

# Extract elevation from elevation_global SpatRaster (Elevation_m)
el_values <- terra::extract(Elevation_m, cbind(newdf$x, newdf$y))
#el_values_clipped <- terra::extract(clipped_elev, cbind(newdf$x, newdf$y))


# Add elevation to the HFI data frame. This will change depending on the
# resolution you use
newdf$Elevation_m <- el_values$wc2.1_10m_elev


# Define the depth we want to predict for (here just surface level)
newdf$Max_Depth_cm <- as.integer(0)

# Set a "study" Only needed for predicting
newdf$Study <- as.factor('prediction')
head(newdf)


# Predict MP concentrations using the fitted model
newdf$mu <- predict(model,
                    newdata = newdf,
                    type = 'link',
                    se.fit = FALSE,
                    exclude = "Study",
                    na.rm = TRUE)


# Converting the predictions in the newdf (containing coordinates & predicted 
# values) into a SpatRaster
# Converting newdf with coordinates & predicted values into SpatRaster
MP_prediction_model <- terra::rast(newdf[c("x", "y", "mu")], type="xyz", crs = crs_wintri)
 #writeRaster(MP_prediction_model, filename = "./Rasters/GAM_prediction_model.tif", overwrite = TRUE)
mp_pred_model_pic <- plot(MP_prediction_model)


#----------------------------------------------------------------------
# Kriging the residuals
#----------------------------------------------------------------------

# Import the residuals
  # Averaging all residuals that have the same coordinates so that there are
  # only one value at each pair of coordinates
MPdf$residuals <- MPdf$Items_kg - predict(model, type = "link")
data <- aggregate(residuals ~ x + y, data = MPdf, mean)

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs="ESRI:53018"))

# Plot to make sure it looks OK
plot(Elevation_m)
points(vg.data)


# Empirical variogram for the MP model residuals
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg, ylim = c(0, 9e9))

# Note: 'Fit' model --- this is the result of fitting a theoretical variogram
# model to the empirical variogram, providing estimates for model parameters
# that characterize the spatial structure of the residual

# B/c we're doing a variogram of the residuals, it already does this correction
# (moving the y to 0 - by definition the mean of my residuals is mean = 0) ->
# if the mean wasn't = 0 then my predictions are off by a certain amount so
# E(e) = E(y-mu) = 0
mp.vg.fit <- fit.variogram(mp.vg, vgm("Exp", nugget = TRUE, psill = 99000000, range = 73000))
plot(mp.vg, mp.vg.fit)


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

# Ordinary Kriging of the residuals
kriging_100000 <-
  krige(residuals ~ 1, # kriging the residuals = the formula
        vg.data, # provides coordinates & residuals from sample locations = the locations
        grid_100000_sf, # gives the locations to predict the residuals at = newdata
        model = mp.vg.fit) # gives theoretical vg model describing how residuals vary with distance = model
# note: if you were to do universal kriging, formula would be z ~ x+y rather than z ~ 1
  #saveRDS(kriging_100000, file = "./Data/kriging_100000.RDS")


# Convert prediction to SpatRaster (using the tidyterra package for convenience,
# not because it's the only way to do this)
tester <- tidyterra::as_spatraster(kriging_100000, crs = "ESRI:53018")
tester <- tester["var1.pred"]
# note: tester["var1.pred] is returning a subset of tester with ONLY the
# column of "var1.pred" vs. tester$var1.pred would return a vector
plot(tester)

tester_cropped <- tester %>%
  terra::crop(world_wintri, mask = T)

# Match all resolution, extent, and CRS of Kriged residuals to MP_prediction_model raster
var1_resampled <- terra::resample(tester_cropped, MP_prediction_model, method = "bilinear")
  #writeRaster(var1_resampled, filename = "./Rasters/var1_resampled.tif", overwrite = TRUE)
plot(var1_resampled)
# Currently not in the same crs?
var1_resampled1 <- var1_resampled
crs(var1_resampled1) <- crs(MP_prediction_model)
plot(var1_resampled1)

#----------------------------------------------------------------------
# Combined: regression-Kriging predictions
#----------------------------------------------------------------------

# Get the regression-Kriging predictions by summing the two
RK <- MP_prediction_model + var1_resampled1

# Convert back to the correct scale 
RK <- exp(RK) 
plot(RK)
  #writeRaster(RK, filename = "./Rasters/RK_raster.tif", overwrite = TRUE)




#----------------------------------------------------------------------
 ## Old - no longer needed
#----------------------------------------------------------------------
# # Step 1: Define a grid based on the bounding box of the elevation raster
# # Currently using tidy verse, but better to use base R
# grd_100_sf_100000 <- test %>%
#   st_bbox() %>%
#   st_as_sfc() %>%
#   st_make_grid(
#     cellsize = c(100000, 100000) # pixel size (Made it large here for fast computation time, should be as small as computationally feasible)
#   )
# 
# # Ordinary Kriging of the residuals
# mp_predictions_krige_100000 <- krige(
#   residuals ~ 1,
#   vg.data,
#   grd_100_sf_100000,
#   model = mp.vg.fit)

# # # newdf "Study" contains less levels than MPdf "Study"
# #   # MPdf has 21 factors but is from 1:24 currently. Need to correct this:
# # # Set factor levels directly to the range 1:21
# # MPdf$Study <- factor(MPdf$Study, levels = as.character(1:21))
# # 
# # # Verify the unique values and levels
# # unique(MPdf$Study)  # Check unique values
# # levels(MPdf$Study)  # Check levels again
# # 
# # # Match newdf's Study levels to MPdf's levels
# # newdf$Study <- factor(newdf$Study, levels = levels(MPdf$Study))
# 
# #newdf1 <- newdf[1:1000,]

# Putting regression-Kriging back on the response scale just to visualize it:
# MP_prediction_model_response_scale <- exp(MP_prediction_model)
# plot(MP_prediction_model_response_scale)