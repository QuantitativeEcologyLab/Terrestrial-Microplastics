
#----------------------------------------------------------------------
# Data import and carpentry
#----------------------------------------------------------------------

#Import and process all of the spatial data (note: can be slow)
#Returns 3 spatRaster class objects: HFI, Elevation_km, soil
#Also defines the project projection system in a character vector called "crs_wintri"
source("1_spatial_carpentry.R")


#Import and process the MP dataset
#Generates a dataframe called "MPdf" that contains MP concentrations,
#Coordinates, and covariate information
#Clean-up of final dataset 
source("2_MP_data_processing.R")

#----------------------------------------------------------------------
# Interaction terms and model
#----------------------------------------------------------------------

#Determining if there are interaction terms in the model
#GAM Model
source("3_gam_model.R")

#----------------------------------------------------------------------
# Current trends in terrestrial microplastic pollution
#----------------------------------------------------------------------

#Current MP trends based off HFI, elevation, and soil depth
source("4_current_trends.R")

#----------------------------------------------------------------------
# Predictions for terrestrial microplastic pollution
#----------------------------------------------------------------------

#Predicting MP concentrations from HFI, elevation, and soil depth
source("5_MP_predictions.R")

#----------------------------------------------------------------------
# Predictions from model and regression-kriging
#----------------------------------------------------------------------

#Predictions from the model
#Kriging the residuals
#Combined regression-kriging predictions
source("6_regression-kriging_100000.R")

#Regression kriging maps
source("7_regression-kriging_maps")

#----------------------------------------------------------------------
# Identifying soil types 
#----------------------------------------------------------------------

#Identifying soil types at each coordinate in data set
source("8_soil_codes")

#----------------------------------------------------------------------
# Other
#----------------------------------------------------------------------

#Determining and removing outliers from data
source("9_removing_outlier_MPdf.R")

#Determining top model
source("9_model_selction.R")

#Frequency of data 
source("9_histograms")

#Global map of the sampled locations to data 
source("9_global_map")

