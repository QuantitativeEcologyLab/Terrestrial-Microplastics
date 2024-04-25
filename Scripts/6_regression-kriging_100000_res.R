setwd("C:/Users/lmills96/OneDrive - UBC/MSc Thesis Info/Global Analysis/Terrestrial-Microplastics/Rasters")

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

# Add elevation to the HFI data frame. This will change depending on the
# resolution you use 
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
 
# save(newdf, file = "MP_Prediction_newdf_10res.Rda")

# Create a raster from the predictions using entire data set
MP_prediction_model <- rast(newdf[c("x", "y", "mu")], type="xyz", crs = crs_wintri)

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
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE, psill = 40000000, range = 400000)) 


# Plot variogram 
plot(mp.vg, mp.vg.fit)

# Plot in ggplot (this is to find where we are missing data - will need to 
# redo variogram with correct variogram afterwards)
V = variogramLine(mp.vg.fit, maxdist = max(mp.vg$dist))
head(Vtest)

RA_Variogram <- 
  ggplot(mp.vg, aes(x = dist, y = gamma)) +
  geom_point() +
  geom_line(data = V) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "white", fill = "white"),
        plot.background = element_rect(colour = "white", fill = "white"),
        legend.position="top",
        legend.title.align=0.5,
        legend.title=element_text(color="black", size=14, family = "sans", 
                                  face= "bold"),
        legend.text=element_text(color="black", size=12, family = "sans"),
        legend.key.size = unit(0.25, "cm"),
        legend.key.width = unit(4, "cm"),
        legend.background=element_blank(),
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(limits = c(0,6e+06), expand = c(0,10000)) +
  scale_x_sqrt(breaks = c(10000+1,36000+1, 100000+1,500000+1, 670000+1, 1020000+1),
                labels = c(10000,36000, 100000,500000,670000, 1020000)) +
  geom_vline(xintercept = c(36000,670000,1020000), linetype = 2)

ggsave("RA_Variogram.png", plot = Variogram, width = 8, height = 6,
        dpi = 600, units = "in")


# Global Kriging ------------------------------------------------------

# This is currently at 100,000 resolution (low)


# Step 1: Define a grid based on the bounding box of the elevation raster
# Currently using tidy verse, but better to use base R
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

# save(mp_predictions_krige_100000, file = "mp_predictions_krige_100000.Rda")


# Convert prediction to SpatRaster (using the tidyterra package for convenience, 
# not because it's the only way to do this)
tester <- tidyterra::as_spatraster(mp_predictions_krige_100000, crs = "ESRI:53018")
tester <- tester["var1.pred"]

tester_cropped <- tester %>%
  terra::crop(world_wintri, mask = T)

# Match all resolution based HFI resolution
var1_resampled <- terra::resample(tester_cropped, MP_prediction_model, method = "bilinear")



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

