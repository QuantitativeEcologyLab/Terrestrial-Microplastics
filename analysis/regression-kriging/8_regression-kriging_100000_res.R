setwd("C:/Users/lmills96/OneDrive - UBC/MSc Thesis Info/Global Analysis/Global MP Distribution/Rasters")

library(sp)
library(terra)
library(sf)
library(gstat)
library(tidyverse)



#----------------------------------------------------------------------
# Predictions from the GAM
#----------------------------------------------------------------------

message("Predicting global MP concentrations from the fitted model")

#Turn HFI into a dataframe w/ lat long info (x, y coords), get the elevation
#values for those coordinates, and predict. The predictions will be a dataframe
#which will have to be converted back to a raster.

#Turn HFI into raster, predict from the model, then map it.

# Convert SpatRaster into dataframe 
newdf <- terra::as.data.frame(HFI, xy = TRUE, row.names = FALSE)

# Exract elevation from elevation_global SpatRaster (Elevation_km)
el_values <- terra::extract(Elevation_km, cbind(newdf$x, newdf$y))

#newdf1 <- terra::as.data.frame(Elevation_km, xy = TRUE) #does this have to be extracted from the spatraster itself or does a dataframe work?

# Add elevation to the HFI data frame. This will change depending on the
# resolution you use 
#newdf$Elevation_km <- el_values$wc2.1_2.5m_elev
newdf$Elevation_km <- el_values$wc2.1_10m_elev


#Define the depth we want to predict for (here just surface level)
newdf$Max_Depth_cm <- as.integer(0)

#Set a "study" Only needed for predicting
newdf$Study <- 'prediction'

# Rename the HFI column to math the model
names(newdf)[3] <- "HFI"
head(newdf)


# Predict MP concentrations using the fitted model
# newdf$mu <- predict(model,
#                     newdata = newdf,
#                     type = 'link',
#                     se.fit = FALSE,
#                     exclude = "Study")
# 
# save(newdf, file = "MP_Prediction_newdf_10res.Rda")

# Create a raster from the predictions using entire data set
MP_prediction_model <- rast(newdf[c("x", "y", "mu")], type="xyz", crs = crs_wintri)

mp_pred_model_pic <- plot(MP_prediction_model)

dev.copy(png, filename = "mp_pred_model_pic.png")
dev.off()




#----------------------------------------------------------------------
# Kriging the residuals
#----------------------------------------------------------------------


#Import the elevation raster 
test <- Elevation_km
#test1 <- project(test1, "ESRI:53018")

#Import the residuals
data <- read.csv("C:/Users/lmills96/OneDrive - UBC/MSc Thesis Info/Global Analysis/Global MP Distribution/Avg_Resid.csv")


#Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs="ESRI:53018"))

#Plot to make sure it looks ok
plot(test)
points(vg.data)


# Empirical variogram for the MP model residuals
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)

mp.vg.fit <- fit.variogram(mp.vg, vgm("Exp", nugget = T)) 

plot(mp.vg, mp.vg.fit,
     xlab = expression(bold("Lag Distance (m)")),
     ylab = expression(bold("Semivariance")))
  




# Global -----------------------------------------------------------------------


#Step 1: define a grid based on the bounding box of the elevation raster
#Using tidy verse, but better to use base R
# grd_100_sf_100000 <- test %>% 
#   st_bbox() %>% 
#   st_as_sfc() %>% 
#   st_make_grid(
#     cellsize = c(100000, 100000) # pixel size (Made it large here for fast computation time, should be as small as computatioanlly feasible)
#   )

#save(grd_100_sf_100000, file = "grd_100_sf_100000.Rda")


# Ordinary Kriging of the residuals
# mp_predictions_krige_100000 <- krige(
#   residuals ~ 1,
#   vg.data,
#   grd_100_sf_100000,
#   model = mp.vg.fit)
# 
# save(mp_predictions_krige_100000, file = "mp_predictions_krige_100000.Rda")


#Convert prediction to SpatRaster (using the tidyterra package for convenience, not because it's the only way to do this)
tester <- tidyterra::as_spatraster(mp_predictions_krige_100000, crs = "ESRI:53018")
tester <- tester["var1.pred"]

tester_cropped <- tester %>%
  terra::crop(world_wintri, mask = T)

# Match all resolution based HFI resolution
var1_resampled <- terra::resample(tester_cropped, MP_prediction_model, method = "bilinear")


#plot(var1_cropped)

#Get the regression kriging predictions by summing the two
MP_prediction <- MP_prediction_model + var1_resampled

#Convert back to the correct scale
MP_prediction <- exp(MP_prediction) 

plot(MP_prediction)

MP_prediction_model_response_scale <- exp(MP_prediction_model)
plot(MP_prediction_model_response_scale)

