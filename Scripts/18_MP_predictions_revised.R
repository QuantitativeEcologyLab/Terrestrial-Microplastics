
#NEED PREDICTIONS FROM THE GAM - THE LINE NOR THE GEOM SMOOTH ARE PREDICTIONS - 
#THOSE ARE JUST A SMOOTH SO NEED TO REDO MP_PREDICTIONS WITH ACTUAL PREDICTIONS FROM MODEL


as.factor(MPdf$Study)


MPdf_prac <- MPdf[MPdf$Study !=23, ] 
MPdf_prac <- na.omit(MPdf_prac)

ggplot(data = MPdf.prac, 
       aes(y = log10(Items_kg) +1, x = HFI, size = Weights)) +
  geom_point(aes(col = Study), alpha = 0.8, pch = 16) +
  scale_size_continuous(range = size_limits)


model_prac <- gam(Items_kg ~
               # global terms
               s(HFI, k = 8, bs = "ad") + #npd or npi instead of tp) + #bs="tp" is the default so I don't need to specify?
               s(sqrt(Max_Depth_cm), k = 5) +
               s(sqrt(Elevation_km), k = 6) + #EDIT KNOTS - 6 AT THE ABSOLUTE MOST
               #ti(Elevation_km, HFI, k = 6, bs = "tp") + #don't need study in here b/c interaction term
               #ti(Max_Depth_cm, HFI, k = 5, bs = "tp") +
               # study-level terms
               s(Study, bs = 're'), #random int - it doesn't really have random slopes 
             #weights = Weights,
             family = tw(link = 'log'),
             data = MPdf_prac,
             method = "REML")
#try with and without square root

model_prac <- gam(Items_kg ~
                    # global terms
                    s(HFI, k = 10, bs = "ad") + #npd or npi instead of tp) + #bs="tp" is the default so I don't need to specify?
                    s(sqrt(Max_Depth_cm), k = 6) +
                    s(sqrt(Elevation_km), k = 5) + #EDIT KNOTS - 6 AT THE ABSOLUTE MOST
                    #ti(Elevation_km, HFI, k = 6, bs = "tp") + #don't need study in here b/c interaction term
                    #ti(Max_Depth_cm, HFI, k = 5, bs = "tp") +
                    # study-level terms
                    s(Study, bs = 're'), #random int - it doesn't really have random slopes 
                  #weights = Weights,
                  family = tw(link = 'log'),
                  data = MPdf,
                  method = "REML")


summary(model_prac)

par(mar = c(5, 4, 4, 2) + 0.1)

plot(model_prac, pages = 2, scheme = 2, scale = 0)

plot(model_prac, pages = 1, scheme = 1, scale = 0, trans = exp)

plot(model_prac, select = 1, scheme = 1, residuals = TRUE, xlim = c(0, 1))

layout(matrix(1:4, ncol = 2))
gam.check(model_prac)
abline(a = 0, b = 1, col = 'red')
layout(1)

AIC(model_prac)
#shape constraint
#scam package 

#--------------------------------------------------------------------------
# Predicted trends in terrestrial microplastics   
#--------------------------------------------------------------------------   

# Make "Predictions" column in dataframe
MPdf$Predictions <- predict(model, data = MPdf_prac, type = "response", se = FALSE)



# Depth Predictions -------------------------------------------------------

# Make a new dataframe
newdf_depth_prac <- data.frame(Max_Depth_cm = seq(from = 3, to = 250, length.out = 1011),
                          HFI = rep(mean(MPdf_prac$HFI)),
                          Elevation_km = rep(mean(MPdf_prac$Elevation_km)),
                          Study = rep('new study'))

prediction_depth_prac <- newdf_depth_prac

newdata_prac = newdf_depth_prac
prediction_depth_prac$mu <- predict(model_prac, 
                               newdata = newdf_depth_prac,
                               se.fit = FALSE,
                               type = 'response')

ggplot() + 
  geom_point(aes(Max_Depth_cm, Items_kg), MPdf_prac) +
  geom_line(aes(Max_Depth_cm, mu), prediction_depth_prac, col = 'red', lwd = 1) +
  scale_y_log10() #good to have the log10 scale here 


fit_depth_prac <- predict(model_prac, newdata = prediction_depth_prac, se.fit = TRUE,
                     type = 'link') 

mu_depth_prac <- exp(fit_depth_prac$fit)
lower_95_depth_prac <- exp(fit_depth_prac$fit - 1.96 * fit_depth_prac$se.fit) 
upper_95_depth_prac <- exp(fit_depth_prac$fit + 1.96 * fit_depth_prac$se.fit)


# Combine dataframes
prediction_depth_ci_prac <- data.frame(prediction_depth_prac,
                                  fit_depth_prac,
                                  mu = mu_depth,
                                  lower_95_depth = lower_95_depth,
                                  upper_95_depth = upper_95_depth)

# Plot the depth prediction
#pred_depth <-
ggplot() +
  geom_point(aes(Max_Depth_cm, Items_kg), MPdf_prac, col = "#924900") +
  geom_ribbon(aes(Max_Depth_cm, ymin = lower_95_depth_prac, ymax = upper_95_depth_prac), prediction_depth_ci_prac, fill = '#924900', alpha = 0.4) +
  geom_line(aes(Max_Depth_cm, mu), prediction_depth_prac, col = 'black', lwd = 1) +
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





# HFI Predictions ---------------------------------------------------------

# Make a new dataframe
newdf_HFI_prac <- data.frame(HFI = seq(from = 0, to = 1, length.out = 1011),
                        Max_Depth_cm = rep(mean(MPdf_prac$Max_Depth_cm), 1011),
                        Elevation_km = rep(mean(MPdf_prac$Elevation_km), 1011),
                        Study = 'new study')

prediction_HFI_prac <- newdf_HFI_prac
prediction_HFI_prac$mu <- predict(model_prac,
                             newdata = newdf_HFI_prac,
                             se.fit = FALSE,
                             type = 'response')

# Plot the predicted HFI
ggplot() + 
  geom_point(aes(HFI, Items_kg), MPdf_prac) +
  geom_line(aes(HFI, mu), prediction_HFI_prac, col = 'red', lwd = 1) +
  scale_y_log10() 

# Create credible intervals
fit_HFI_prac <- predict(model_prac, newdata = prediction_HFI_prac, se.fit = TRUE,
                   type = 'link')

mu_HFI_prac <- exp(fit_HFI_prac$fit)
lower_95_HFI_prac <- exp(fit_HFI_prac$fit - 1.96 * fit_HFI_prac$se.fit)
upper_95_HFI_prac <- exp(fit_HFI_prac$fit + 1.96 * fit_HFI_prac$se.fit)

# Combine dataframes
prediction_HFI_ci_prac <- data.frame(prediction_HFI_prac,
                                fit_HFI_prac,
                                mu = mu_HFI_prac,
                                lower_95_HFI = lower_95_HFI_prac,
                                upper_95_HFI = upper_95_HFI_prac)

# Plot the HFI prediction
#pred_HFI <-
ggplot() +
  geom_point(aes(HFI, Items_kg), MPdf_prac, col = "#924900") +
  geom_ribbon(aes(HFI, ymin = lower_95_HFI, ymax = upper_95_HFI), prediction_HFI_ci_prac, fill = "#924900", alpha = 0.4) +
  geom_line(aes(HFI, mu), prediction_HFI_prac, col = 'black', lwd = 1) +
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

ggsave("pred_HFI.png", width = 8, height = 6,
       dpi = 600, units = "in")


# Elevation Predictions ---------------------------------------------------

# Make a new dataframe
newdf_elev_prac <- data.frame(Elevation_km = seq(from=0, to=2473, length.out=1011),
                         HFI = rep(mean(MPdf_prac$HFI), 1011),
                         Max_Depth_cm = rep(mean(MPdf_prac$Max_Depth_cm), 1011),
                         Study = 'new study')

prediction_elev_prac <- newdf_elev_prac
prediction_elev_prac$mu <- predict(model_prac,
                              newdata = newdf_elev_prac,
                              se.fit = FALSE,
                              type = 'response')

# Plot the predicted elevation
ggplot() + 
  geom_point(aes(Elevation_km, Items_kg), MPdf_prac) +
  geom_line(aes(Elevation_km, mu), prediction_elev_prac, col = 'red', lwd = 1) +
  scale_y_log10() 

# Create credible intervals
fit_elev_prac <- predict(model_prac, newdata = prediction_elev_prac, se.fit = TRUE,
                    type = 'link')
mu_elev_prac <- exp(fit_elev_prac$fit)
lower_95_elev <- exp(fit_elev_prac$fit - 1.96 * fit_elev_prac$se.fit)
upper_95_elev <- exp(fit_elev_prac$fit + 1.96 * fit_elev_prac$se.fit)

# Combine dataframes
prediction_elev_ci_prac <- data.frame(prediction_elev_prac,
                                 fit_elev_prac,
                                 mu = mu_elev,
                                 lower_95_elev = lower_95_elev,
                                 upper_95_elev = upper_95_elev)

# Plot the elevation prediction
#pred_elev <-
ggplot() +
  geom_point(aes(Elevation_km, Items_kg), MPdf_prac, col = "#924900") +
  geom_ribbon(aes(Elevation_km, ymin = lower_95_elev, ymax = upper_95_elev), prediction_elev_ci_prac, fill = "#924900", alpha = 0.4) +
  geom_line(aes(Elevation_km, mu), prediction_elev_prac, col = 'black', lwd = 1) +
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

ggsave("pred_elev.png", width = 8, height = 6,
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




