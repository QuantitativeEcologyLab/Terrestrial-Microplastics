
# Load required packages

library(sp)
library(terra)
library(sf)
library(gstat)
library(tidyverse)
library(ggplot2)

#----------------------------------------------------------------------
# Predictions from the GAM
#----------------------------------------------------------------------

message("Predicting global MP concentrations from the fitted model")



# Convert SpatRaster into dataframe 
newdf <- terra::as.data.frame(HFI, xy = TRUE, row.names = FALSE)

# Extract elevation from elevation_global SpatRaster (Elevation_km)
el_values <- terra::extract(Elevation_km, cbind(newdf$x, newdf$y))



#el_values_clipped <- terra::extract(clipped_elev, cbind(newdf$x, newdf$y))


# Add elevation to the HFI data frame. This will change depending on the
# resolution you use 
newdf$Elevation_km <- el_values$wc2.1_10m_elev


#Define the depth we want to predict for (here just surface level)
newdf$Max_Depth_cm <- as.integer(0)

#Set a "study" Only needed for predicting
newdf$Study <- as.factor('prediction')

# Rename the HFI column to match the model
names(newdf)[3] <- "HFI"
head(newdf)


# # newdf "Study" contains less levels than MPdf "Study"
#   # MPdf has 21 factors but is from 1:24 currently. Need to correct this:
# # Set factor levels directly to the range 1:21
# MPdf$Study <- factor(MPdf$Study, levels = as.character(1:21))
# 
# # Verify the unique values and levels
# unique(MPdf$Study)  # Check unique values
# levels(MPdf$Study)  # Check levels again
# 
# # Match newdf's Study levels to MPdf's levels
# newdf$Study <- factor(newdf$Study, levels = levels(MPdf$Study))



# Predict MP concentrations using the fitted model
#newdf$mu 
newdf <- predict(model,
                    newdata = newdf[1:1000,],
                    type = 'response',
                    se.fit = FALSE,
                    exclude = "Study",
                    na.rm = TRUE)


 #save(newdf, file = "MP_Prediction_newdf_10res.Rda")

# Create a raster from the predictions using entire data set
MP_prediction_model <- terra::rast(newdf[c("x", "y", "mu")], type="xyz", crs = crs_wintri)

MP_prediction_model <- terra::rast(newdf_test, crs = crs_wintri, type = NA)

# The prediction here will be a data frame which will have to be 
# converted back to a SpatRaster.
mp_pred_model_pic <- plot(MP_prediction_model)

dev.copy(png, filename = "mp_pred_model_pic.png")
dev.off()



#----------------------------------------------------------------------
# Kriging the residuals
#----------------------------------------------------------------------

# Import the elevation raster 
test <- Elevation_km

test_noNA <- ifel(is.na(Elevation_km), 0, Elevation_km)

# Import the residuals

# Averaging all residuals that have the same coordinates so that there are only 
# one value at each pair of coordinates.
MPdf$residuals <- MPdf$Items_kg - predict(model, type = "response")
data <- aggregate(residuals ~ x + y, data = MPdf, mean)

#Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs="ESRI:53018"))

#Plot to make sure it looks OK
plot(test)
plot(test_noNA)
points(vg.data)


# Empirical variogram for the MP model residuals
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg, ylim = c(0, 9e9))

#adjust the empirical variogram - look at number of points etc. 
#look at these before using fitted 

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


# Step 1: Define a grid based on the bounding box of the elevation raster
# Currently using tidy verse, but better to use base R
grd_100_sf_100000 <- test %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_make_grid(
    cellsize = c(100000, 100000) # pixel size (Made it large here for fast computation time, should be as small as computationally feasible)
  )

#save(grd_100_sf_100000, file = "grd_100_sf_100000.Rda")


# Ordinary Kriging of the residuals
mp_predictions_krige_100000 <- krige(
  residuals ~ 1,
  vg.data,
  grd_100_sf_100000,
  model = mp.vg.fit)

# save(mp_predictions_krige_100000, file = "mp_predictions_krige_100000.Rda")


# Convert prediction to SpatRaster (using the tidyterra package for convenience, 
# not because it's the only way to do this)
tester <- tidyterra::as_spatraster(mp_predictions_krige_100000, crs = "ESRI:53018")
tester <- tester["var1.pred"]

tester_cropped <- tester %>%
  terra::crop(world_wintri, mask = T)

# Match all resolution based HFI resolution
var1_resampled <- terra::resample(tester_cropped, MP_prediction_model, method = "bilinear")


plot(tester)

#----------------------------------------------------------------------
# Combined regression-kriging predictions
#----------------------------------------------------------------------

# Get the regression kriging predictions by summing the two
MP_prediction <- MP_prediction_model + var1_resampled

# Convert back to the correct scale
MP_prediction <- exp(MP_prediction) 
plot(MP_prediction)
# writeRaster(MP_prediction, filename = "C:/Users/lmills96/OneDrive - UBC/MSc Thesis Info/Global Analysis/Global MP Distribution/Rasters/kriging_raster.tif", overwrite = TRUE)


# Putting regression-kriging back on the response scale 
MP_prediction_model_response_scale <- exp(MP_prediction_model)
plot(MP_prediction_model_response_scale)




 ##QUESTION - why am I putting it back on the response scale? 
#Isn't it already on the response scale if my residuals are on the response? 
#And if it's not...still don't know why I'm doing this

# Is this true?
#Predicting on the link scale is crucial because it's the scale on which the 
#linear combination of smooth (and possibly linear) terms is computed

