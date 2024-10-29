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
                               type = 'link')

print(prediction_hfi1)

#Need to exp() b/c it is currently on link scale. Need it on response scale. 
exp(7.709848) #2230.20 MP at 0 HFI
exp(8.046635) #3123.27 MP at 1 HFI

#CI at low HFI
exp(7.709848 +c(-1.96,1.96)*0.7050437) 
#CI at high HFI
exp(8.046635+c(-1.96,1.96)*0.6884680 )


# MP Conc Predictions at low and high elevation -----------------------------

# Make a new dataframe
newdf_elev1 <- data.frame(Max_Depth_cm = 0,
                         HFI = rep(mean(MPdf$HFI)),
                         Elevation_km = c(0.602411, 2330.078),
                         Study = 'new study')

prediction_elev1 <- predict(model, 
                           newdata = newdf_elev1,
                           se.fit = TRUE,
                           type = 'link')
print(prediction_elev1)
exp(6.562314) #707.91 MP at min elevation
exp(6.191299) #488.48 MP at max elevaition

#CI at low elevation
exp(6.562314 +c(-1.96,1.96)*0.6445873) 
#CI at high elevation
exp(6.191299 +c(-1.96,1.96)*0.7050437) 




# MP Conc Predictions at low and high depths -------------------------------

# Make a new dataframe
newdf_depth1 <- data.frame(Max_Depth_cm = c(0,250),
                          HFI = rep(mean(MPdf$HFI)),
                          Elevation_km = mean(MPdf$Elevation_km),
                          Study = 'new study')

prediction_depth1 <- predict(model, 
                            newdata = newdf_depth1,
                            se.fit = TRUE,
                            type = 'link')

print(prediction_depth1)
exp(7.418199) #1666.03 MP at depth of 0
exp(5.732557) #308.76 MP at max depth

#CI at low elevation
exp(7.418199  +c(-1.96,1.96)*0.6209763) 
#CI at high elevation
exp(5.732557  +c(-1.96,1.96)*0.6883131) 
