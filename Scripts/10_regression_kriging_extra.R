
# Load required packages
library(gstat)
library(geodata)
library(terra)
library(sf)

#----------------------------------------------------------------------
# Predictions from the GAM
#----------------------------------------------------------------------

#Turn HFI into a dataframe w/ lat long info (x, y coords), get the elevation
#values for those coordinates, and predict. The predictions will be a dataframe
#which will have to be converted back to a raster.

#Turn HFI into raster, predict from the model, then map it.

message("Predicting global MP concentrations from the fitted model")

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
newdf$mu <- predict(model,
                    newdata = newdf,
                    type = 'link',
                    se.fit = FALSE,
                    exclude = "Study")

save(newdf, file = "MP_Prediction_newdf_10res.Rda")

# Create a raster from the predictions using entire data set
MP_prediction_model <- rast(newdf[c("x", "y", "mu")], type="xyz", crs = crs_wintri)
plot(MP_prediction_model)

# Create a raster from the predictions for China only
china <- gadm(country= "China", level=0, path = tempdir()) #level= 1 you want country & province / level 2 is counties 
crs_wintri <- "ESRI:53018"
china <- project(china, crs_wintri) 
plot(china)

china <- rast(china) #turning it into a SpatRaster 




# 
# #----------------------------------------------------------------------
# # Kriging the residuals
# #----------------------------------------------------------------------
# 
# #Get the spatially explicit model residuals
# MPdf$residuals <- residuals(model)
# 
# avg_resid_df <- MPdf
# 
# total_dup <- sum(duplicated(avg_resid_df[, c("x", "y")]))
# total_dup
# mean_resid <- aggregate(residuals ~ x + y, data = avg_resid_df, mean)
# 
# 
# #Convert to an sf object
# vg.data <- mean_resid[,c("x", "y", "residuals"),]
# 
# 
# #duplicated_coords <- duplicated(MPdf$y)
# #vg.data <- vg.data[!duplicated(vg.data[,c("x", "y")]),] #take average of the duplicates of the residuals 
# 
# vg.data <- st_as_sf(vect(vg.data, geom=c("x", "y"), crs=crs_wintri))
# 
# 
# # Empirical variogram for the MP model residuals
# mp.vg <- variogram(residuals ~ 1, data  = vg.data)
# 
# mp.vg.fit <- fit.variogram(mp.vg, vgm("Exp", nugget = T)) #need to minimize the root mean squared error of the variogram
# 
# plot(mp.vg, mp.vg.fit)
# 
# n_x <- length(unique(newdf$x))
# n_y <- length(unique(newdf$y))
# 
# # Larger grid to predict over
# 
# #can change resolutoin in st_make_grid 
# test <- st_make_grid(HFI, crs = crs_wintri, n = c(n_x,n_y))
# 
# #bc / china --- cant update in bc cause there's nothing there 
# 
# #model prediction for bc 
# 
# # Krig the residuals
# MP_prediction_krig <- krige(residuals ~ 1,
#                             test2,
#                             newdata = test,
#                             model=mp.vg.fit)
# 
# 
# plot(MP_prediction_krig)
# 

#----------------------------------------------------------------------
# Combined regression-kriging predictions
#----------------------------------------------------------------------


MP_prediction <- MP_prediction_model + MP_prediction_krig


