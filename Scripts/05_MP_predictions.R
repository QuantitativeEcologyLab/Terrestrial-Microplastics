
# Load required packages
library(ggplot2)
library(mgcv)
library(dplyr)
library(gridExtra)
library(khroma)
library(grid)

# Load MPdf dataset 
MPdf <- read.csv(".Data/MPdf.csv")

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
texp(coef(model)["(Intercept)"]) #678.8272
exp(6.5204+c(-1.96,1.96)*0.4228)
#This assumes  you have a Gaussian distribution on log scale (that is why you 
#get Gaussian confidence intervals). Need to exp to go back to response scale. 


#--------------------------------------------------------------------------
# Depth Predictions
#--------------------------------------------------------------------------

# Make a new dataframe
newdf_depth <- data.frame(Max_Depth_cm = seq(from = 3, to = 250, length.out = 585),
                          HFI = rep(mean(MPdf$HFI)),
                          Elevation_m = rep(mean(MPdf$Elevation_m)),
                          Study = rep('new study'))

prediction_depth <- newdf_depth
# mu here is the same thing as saying "predictions/projections" -> we are using 
# pred$mu to make a new column in the df with the title of mu
newdata = newdf_depth
prediction_depth$mu <- predict(model, 
                               newdata = newdf_depth,
                               se.fit = FALSE,
                               type = 'response')
# Note: the warning message here is indicating that there are new categories in 
# the 'study' variable that were not present in the initial MPdf


# Plot the predicted depth
# Note: these predictions are only accounting for depth under the assumption 
# that HFI is at 1 and elevation is at 0. Therefore, variance is unexplained 
# b/c some of the depth / elevation is different within the plot  
ggplot() + 
  geom_point(aes(Max_Depth_cm, Items_kg), MPdf) +
  geom_line(aes(Max_Depth_cm, mu), prediction_depth, col = 'red', lwd = 1) +
  scale_y_log10() #good to have the log10 scale here 



# Create credible intervals 
# Note: we need type = "link" b/c we want to estimate credible intervals on the 
# link scale - we can't do it otherwise. Also, mgcv is inherently Bayesian, so 
# credible intervals are what we are looking at here (vs. confidence intervals)
# Also, for non-normal distributions and non-identity link functions, se.fit = T
# yields standard errors of the prediction, i.e., a measure of uncertainty for 
# the predicted value. This prediction, by one of the Central Value Theorems, 
# can be assumed to be normally distributed at the link scale, and hence its 
# standard error can be given as the standard deviation of a normal distribution.
fit_depth <- predict(model, newdata = prediction_depth, se.fit = TRUE,
                               type = 'link') 

#exp() is the exponential function, and in this context, it's used to reverse 
#the log transformation that might have been applied during the modeling process
mu_depth <- exp(fit_depth$fit)
lower_95_depth <- exp(fit_depth$fit - 1.96 * fit_depth$se.fit) 
upper_95_depth <- exp(fit_depth$fit + 1.96 * fit_depth$se.fit)


# Combine dataframes
prediction_depth_ci <- data.frame(prediction_depth,
                                  fit_depth,
                                  mu = mu_depth,
                                  lower_95 = lower_95_depth,
                                  upper_95 = upper_95_depth)

# could also use dplyer() 


# Plot the depth prediction
pred_depth <-
  ggplot() +
  geom_point(aes(Max_Depth_cm, Items_kg), MPdf, col = "#924900") +
  geom_ribbon(aes(Max_Depth_cm, ymin = lower_95, ymax = upper_95), prediction_depth_ci, fill = '#924900', alpha = 0.4) +
  geom_line(aes(Max_Depth_cm, mu), prediction_depth, col = 'black', lwd = 1) +
  scale_y_log10(labels = scales::label_number()) +
  scale_x_log10() +
  labs(y=NULL, x= "Soil Depth (cm)") +
  # ylab("MP Concentrations (items/kg)") +
  # xlab("Soil Depth (cm)") 
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=15, family = "sans", face = "bold"),
        axis.title.x = element_text(size=15, family = "sans", face = "bold"),
        axis.text.y = element_text(size=15, family = "sans"),
        axis.text.x  = element_text(size=15, family = "sans"),
        plot.title = element_text(size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        legend.key.size = (unit(0.5, "cm")),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.5,0.2,0.2), "cm")) +
  ggtitle("B)")
  
  
ggsave("./Figures/pred_depth.png", width = 8, height = 6,
       dpi = 600, units = "in")


#--------------------------------------------------------------------------
# HFI Predictions 
#--------------------------------------------------------------------------

# Make a new dataframe
newdf_HFI <- data.frame(HFI = seq(from = 0, to = 1, length.out = 585),
                          Max_Depth_cm = rep(mean(MPdf$Max_Depth_cm), 585),
                          Elevation_m = rep(mean(MPdf$Elevation_m), 585),
                          Study = 'new study')

prediction_HFI <- newdf_HFI
prediction_HFI$mu <- predict(model,
                               newdata = newdf_HFI,
                               se.fit = FALSE,
                               type = 'response')

# Plot the predicted HFI
ggplot() + 
  geom_point(aes(HFI, Items_kg), MPdf) +
  geom_line(aes(HFI, mu), prediction_HFI, col = 'red', lwd = 1) +
  scale_y_log10() 

# Create credible intervals
fit_HFI <- predict(model, newdata = prediction_HFI, se.fit = TRUE,
                     type = 'link')

mu_HFI <- exp(fit_HFI$fit)
lower_95_HFI <- exp(fit_HFI$fit - 1.96 * fit_HFI$se.fit)
upper_95_HFI <- exp(fit_HFI$fit + 1.96 * fit_HFI$se.fit)

# Combine dataframes
prediction_HFI_ci <- data.frame(prediction_HFI,
                                  fit_HFI,
                                  mu = mu_HFI,
                                  lower_95_HFI = lower_95_HFI,
                                  upper_95_HFI = upper_95_HFI)

# Plot the HFI prediction
pred_HFI <-
  ggplot() +
  geom_point(aes(HFI, Items_kg), MPdf, col = "#924900") +
  geom_ribbon(aes(HFI, ymin = lower_95_HFI, ymax = upper_95_HFI), prediction_HFI_ci, fill = "#924900", alpha = 0.4) +
  geom_line(aes(HFI, mu), prediction_HFI, col = 'black', lwd = 1) +
  scale_y_log10(labels = scales::label_number()) +
  labs(y=NULL, x= "HFI") +
  # ylab("MP Concentrations (items/kg)") +
  # xlab("HFI") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=15, family = "sans", face = "bold"),
        axis.title.x = element_text(size=15, family = "sans", face = "bold"),
        axis.text.y = element_text(size=15, family = "sans"),
        axis.text.x  = element_text(size=15, family = "sans"), 
        legend.position = "none",
        legend.key.size = (unit(0.5, "cm")),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"),
        plot.title = element_text(size = 12, family = "sans", face = "bold")) +
  ggtitle("A)")
 
ggsave("./Figures/pred_HFI.png", width = 8, height = 6,
       dpi = 600, units = "in")

#--------------------------------------------------------------------------
# Elevation Predictions 
#--------------------------------------------------------------------------

# Make a new dataframe
newdf_elev <- data.frame(Elevation_m = seq(from=0, to=2473, length.out=585),
                        HFI = rep(mean(MPdf$HFI), 585),
                        Max_Depth_cm = rep(mean(MPdf$Max_Depth_cm), 585),
                        Study = 'new study')

prediction_elev <- newdf_elev
prediction_elev$mu <- predict(model,
                             newdata = newdf_elev,
                             se.fit = FALSE,
                             type = 'response')

# Plot the predicted elevation
ggplot() + 
  geom_point(aes(Elevation_m, Items_kg), MPdf) +
  geom_line(aes(Elevation_m, mu), prediction_elev, col = 'red', lwd = 1) +
  scale_y_log10() 

# Create credible intervals
fit_elev <- predict(model, newdata = prediction_elev, se.fit = TRUE,
                   type = 'link')
mu_elev <- exp(fit_elev$fit)
lower_95_elev <- exp(fit_elev$fit - 1.96 * fit_elev$se.fit)
upper_95_elev <- exp(fit_elev$fit + 1.96 * fit_elev$se.fit)

# Combine dataframes
prediction_elev_ci <- data.frame(prediction_elev,
                                fit_elev,
                                mu = mu_elev,
                                lower_95 = lower_95_elev,
                                upper_95 = upper_95_elev)

# Plot the elevation prediction
pred_elev <-
  ggplot() +
  geom_point(aes(Elevation_m, Items_kg), MPdf, col = "#924900") +
  geom_ribbon(aes(Elevation_m, ymin = lower_95_elev, ymax = upper_95_elev), prediction_elev_ci, fill = "#924900", alpha = 0.4) +
  geom_line(aes(Elevation_m, mu), prediction_elev, col = 'black', lwd = 1) +
  scale_y_log10(labels = scales::label_number()) +
  scale_x_log10() +
  labs(y=NULL, x= "Elevation (m)") +
  # ylab("MP Concentrations (items/kg)") +
  # xlab("Elevation (m)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=15, family = "sans", face = "bold",
                                    margin = margin(t= 1500)),
        axis.title.x = element_text(size=15, family = "sans", face = "bold"),
        axis.text.y = element_text(size=15, family = "sans"),
        axis.text.x  = element_text(size=15, family = "sans"), 
        legend.position = "none",
        legend.key.size = (unit(0.5, "cm")),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"),
        plot.title = element_text(size = 12, family = "sans", face = "bold")) +
        ggtitle("C)")

ggsave("./Figures/pred_elev.png", width = 8, height = 6,
          dpi = 600, units = "in")


#--------------------------------------------------------------------------
# Save all plots together
#--------------------------------------------------------------------------

trends2 <-
  grid.arrange(pred_HFI,pred_depth,
               ncol = 2,
               nrow = 1)

yleft <- textGrob(expression(bold("MP Concentration (items/kg)")), 
                  rot = 90, gp = gpar(fontsize = 20))

trends22 <- grid.arrange(trends2, pred_elev, 
                       ncol = 1,
                       nrow = 2,
                       left = yleft)

ggsave("./Figures/combined_proj_plot.png", plot = trends22, width = 8, height = 6,
       dpi = 600, units = "in")


# Another way to code the above:
# pred_elev_f_ci <-
#   bind_cols(newd_elev_f, #could also use cbind() here  
#             as.data.frame(predict(FIT.2.1, newdata = pred_elev_f, se.fit = TRUE, type = 'link')))
# 
# 
# pred_elev_f_ci <- mutate(pred_elev_f_ci, #mutate() is using dplyr 
#                          mu = exp(fit), 
#                          lower_95 = exp(fit - 1.96 * se.fit),
#                          upper_95 = exp(fit + 1.96 * se.fit))


#--------------------------------------------------------------------------
# Observed vs. Predicted Plot   
#--------------------------------------------------------------------------   

# Using a tweedie distribution so it is normal for variance to look 
#heteroskedastic (variance scales with the mean)

ggplot(MPdf, aes(Items_kg, Predictions)) +
  geom_abline(intercept = 0, slope = 1, color = 'red') +
  geom_point() 
