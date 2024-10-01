# Load required packages

library(ggplot2)
library(mgcv)
library(dplyr)
library(gridExtra)
library(khroma)

# Load colourblind friendly colour palette
bright <- color("bright")
plot_scheme(bright(7), colours = TRUE, names = TRUE, size = 0.9)


#--------------------------------------------------------------------------
# Predicted trends in terrestrial microplastics   
#--------------------------------------------------------------------------   

# Make "Predictions" column in dataframe
MPdf$Predictions <- predict(model, data = MPdf, type = "response", se = FALSE)

# Don't use these -
mean(MPdf$Predictions) #10,638.92
# in comparison to mean of response --> 
mean(MPdf$Items_kg) #10,700.36


# Estimated mean concentration of MPs
summary(model)
exp(coef(model)["(Intercept)"]) #678.8272
exp(6.5204+c(-1.96,1.96)*0.4228)
#This assumes  you have a Gaussian distribution on log scale (that is why you 
#get Gaussian confidence intervals). Need to exp to go back to response scale. 



data.frame(Max_Depth_cm = seq(from = 3, to = 250, length.out = 771),
           HFI = rep(mean(MPdf$HFI)),
           Elevation_km = rep(mean(MPdf$Elevation_km)),
           Study = rep('new study'))





# MP Conc Predictions at low and high HFI -----------------------------------

# Make a new dataframe
newdf_hfi1 <- data.frame(Max_Depth_cm =  0,
                          HFI = c(0,1),
                          Elevation_km = mean(MPdf$Elevation_km),
                          Study = 'new study')

prediction_hfi1 <- predict(model, 
                               newdata = newdf_hfi1,
                               se.fit = TRUE,
                               type = 'response')

print(prediction_hfi1)

#CI at low HFI
2230.203+c(-1.96,1.96)*1572.390
exp(-851.6814)
#CI at high HFI
3123.269+c(-1.96,1.96)*2150.271
exp(-1091.262)


# MP Conc Predictions at low and high elevation -----------------------------

# Make a new dataframe
newdf_elev1 <- data.frame(Max_Depth_cm = 0,
                         HFI = rep(mean(MPdf$HFI)),
                         Elevation_km = c(0.602411, 2330.078),
                         Study = 'new study')

prediction_elev1 <- predict(model, 
                           newdata = newdf_elev1,
                           se.fit = TRUE,
                           type = 'response')
print(prediction_elev1)

#CI at low HFI
707.9077+c(-1.96,1.96)*456.3083
exp(-186.4566)
#CI at high HFI
488.4801+c(-1.96,1.96)*422.3384
exp(-339.3032)




# MP Conc Predictions at low and high depths -------------------------------

# Make a new dataframe
newdf_depth1 <- data.frame(Max_Depth_cm = c(0,250),
                          HFI = rep(mean(MPdf$HFI)),
                          Elevation_km = mean(MPdf$Elevation_km),
                          Study = 'new study')

prediction_depth1 <- predict(model, 
                            newdata = newdf_depth1,
                            se.fit = TRUE,
                            type = 'response')

print(prediction_depth1)

#CI at low HFI
1666.0302+c(-1.96,1.96)*1034.5653
exp(-361.7178)
#CI at high HFI
308.7578+c(-1.96,1.96)*212.5221
exp(-107.7855)
