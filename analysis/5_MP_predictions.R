# Load required packages

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




# Depth Predictions 

# Make a new dataframe
newdf_depth <- data.frame(Max_Depth_cm = seq(3, 250, by = 1),
                          HFI = 1,
                          Elevation_km = 0,
                          Study = 'new study')

prediction_depth <- newdf_depth
prediction_depth$mu <- predict(model,
                               newdata = newdf_depth,
                               se.fit = FALSE,
                               type = 'response')
# Note: the warning message here is indicating that there are new categories in 
# the 'study' variable that were not present in the initial MPdf

# Plot the predicted depth
# Note: these predictions are only accounting for depth under the assumption 
# that HFI is at 1 and elevation is at 0.
ggplot() + 
  geom_point(aes(Max_Depth_cm, Items_kg), MPdf) +
  geom_line(aes(Max_Depth_cm, mu), prediction_depth, col = 'red', lwd = 1) +
  scale_y_log10() #good to have the log10 scale here 

# Create credible intervals
fit_depth <- predict(model, newdata = prediction_depth, se.fit = TRUE,
                               type = 'link')
mu_depth <- exp(fit_depth$fit)
lower_95_depth <- exp(fit_depth$fit - 1.96 * fit_depth$se.fit)
upper_95_depth <- exp(fit_depth$fit + 1.96 * fit_depth$se.fit)

# Combine dataframes
prediction_depth_ci <- data.frame(prediction_depth,
                                  fit_depth,
                                  mu = mu_depth,
                                  lower_95 = lower_95_depth,
                                  upper_95 = upper_95_depth)



# Plot the depth prediction
#pred_depth <-
  ggplot() +
  geom_point(aes(Max_Depth_cm, Items_kg), MPdf, col = "#924900") +
  geom_ribbon(aes(Max_Depth_cm, ymin = lower_95, ymax = upper_95), prediction_depth_ci, fill = '#924900', alpha = 0.4) +
  geom_line(aes(Max_Depth_cm, mu), prediction_depth, col = 'black', lwd = 1) +
  scale_y_log10(labels = scales::label_number()) +
  scale_x_log10() +
  ylab("MP Concentrations (items/kg)") +
  xlab("Soil Depth (cm)") +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=15, family = "sans", face = "bold"),
        axis.title.x = element_text(size=15, family = "sans", face = "bold"),
        axis.text.y = element_text(size=15, family = "sans"),
        axis.text.x  = element_text(size=15, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        legend.key.size = (unit(0.5, "cm")),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.5,0.2,0.2), "cm")) 
  
ggsave("pred_depth.png", plot = trends22, width = 8, height = 6,
       dpi = 600, units = "in") 



# HFI Predictions 

# Make a new dataframe
newdf_HFI <- data.frame(HFI = seq(0, 1, by = 0.001),
                          Max_Depth_cm = 1,
                          Elevation_km = 10,
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
                                  lower_95 = lower_95_HFI,
                                  upper_95 = upper_95_HFI)

# Plot the HFI prediction
#pred_HFI <-
  ggplot() +
  geom_point(aes(HFI, Items_kg), MPdf, col = "#924900") +
  geom_ribbon(aes(HFI, ymin = lower_95, ymax = upper_95), prediction_HFI_ci, fill = "#924900", alpha = 0.4) +
  geom_line(aes(HFI, mu), prediction_HFI, col = 'black', lwd = 1) +
  scale_y_log10(labels = scales::label_number()) +
  ylab("MP Concentrations (items/kg)") +
  xlab("HFI") +
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
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
 
ggsave("pred_HFI.png", plot = trends22, width = 8, height = 6,
       dpi = 600, units = "in")


# Elevation Predictions 

# Make a new dataframe
newdf_elev <- data.frame(Elevation_km = seq(0, 2473, by = 1),
                        HFI = 1,
                        Max_Depth_cm = 10,
                        Study = 'new study')

prediction_elev <- newdf_elev
prediction_elev$mu <- predict(model,
                             newdata = newdf_elev,
                             se.fit = FALSE,
                             type = 'response')

# Plot the predicted elevation
ggplot() + 
  geom_point(aes(Elevation_km, Items_kg), MPdf) +
  geom_line(aes(Elevation_km, mu), prediction_elev, col = 'red', lwd = 1) +
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
#pred_elev <-
  ggplot() +
  geom_point(aes(Elevation_km, Items_kg), MPdf, col = "#924900") +
  geom_ribbon(aes(Elevation_km, ymin = lower_95, ymax = upper_95), prediction_elev_ci, fill = "#924900", alpha = 0.4) +
  geom_line(aes(Elevation_km, mu), prediction_elev, col = 'black', lwd = 1) +
  scale_y_log10(labels = scales::label_number()) +
  scale_x_log10() +
  ylab("MP Concentrations (items/kg)") +
  xlab("Elevation (m)") +
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
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggsave("pred_elev.png", plot = trends22, width = 8, height = 6,
         dpi = 600, units = "in")



trends2 <-
  grid.arrange(pred_HFI,pred_depth,
               ncol = 2,
               nrow = 1)

trends22 <-
  grid.arrange(trends2, pred_elev, 
               ncol=1,
               nrow=2,
               heights = c(8,5.5),
               widths = 6)

ggsave("combined_proj_plot.png", plot = trends22, width = 8, height = 6,
       dpi = 600, units = "in")



#--------------------------------------------------------------------------
# Observed vs. Predicted Plot   
#--------------------------------------------------------------------------   

# Using a tweedie distribution so it is normal for variance to look 
#heteroskedastic (variance scales with the mean)

ggplot(MPdf, aes(Items_kg, Predictions)) +
  geom_abline(intercept = 0, slope = 1, color = 'red') +
  geom_point() 