setwd("C:/Users/lmills96/OneDrive - UBC/R/Thesis Code")


#--------------------CHAPTER 2 --------------------

#FIGURE 1
  #SAMPLED LOCATIONS TO DATE (MAP)
library(maps)
library(ggplot2)


locations_df <- read.csv('C:/Users/lmills96/OneDrive - UBC/MSc Thesis Info/Soil Coordinates.csv')

locations_df <- locations_df[, !names(locations_df) %in% c("X", "X.1")]
locations_df

map <- map_data("world")

map_locations <- ggplot() +
  geom_polygon(data = map, aes(x = long, y = lat, group = group),
               fill = NA, color = "grey") +
  geom_point(data = locations_df, aes(x = long, y = lat), col = "red", size = 1.3) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_minimal() +
  coord_sf()

ggsave(map_locations, file = "C:/Users/lmills96/OneDrive - UBC/R/map.png", 
       width = 3.23, height = 3, dpi = 600, units = "in")






#FIGURE 2
  #CURRENT TRENDS IN MP DISTRIBUTION

HFI_plot <- 
  ggplot(data = MP_DF, 
         aes(y = MP_Conc_items_kg+1, x = HFI)) +
  geom_point(aes(col = Elevation), alpha = 0.8, pch = 16) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=8, family = "sans", face = "bold"),
        axis.title.x = element_text(size=8, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "none",
        legend.key.size = (unit(0.5, "cm")),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm")) +
  scale_x_continuous(limits = c(0,1),expand = c(0,0.02)) +
  scale_y_log10(breaks = c(0.1+1,1+1,10+1,100+1,1000+1,10000+1),
                labels = c(0.1,1,10,100,1000,10000)) +
  scale_colour_gradient(low = "red", high = "blue") +
  ylab("MP Concentrations (items/kg)") +
  xlab("HFI") +
  annotation_logticks(outside = TRUE,
                      size = 0.3,
                      short = unit(0.05, "cm"),
                      mid = unit(0.05, "cm"),
                      long = unit(0.1, "cm")) +
  ggtitle("A)") +
  geom_smooth(method = "gam",
              color = "grey17",
              formula = y ~ x,
              method.args = list(family = tw),
              se = FALSE)

ggsave(HFI_plot, file = "C:/Users/lmills96/OneDrive - UBC/R/HFI Plot.png", 
       width = 3.23, height = 3, dpi = 600, units = "in")



Depth_plot <- 
  ggplot(data = MP_DF, 
         aes(y = MP_Conc_items_kg+1, x = Max_Depth)) +
  geom_point(aes(col = Elevation), alpha = 0.8, pch = 16) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=8, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "none",
        legend.key.size = (unit(0.5, "cm")),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm")) +
  #scale_y_continuous(limits = c(0,13000), expand = c(0,0.2)) +
  #scale_x_log10(breaks = c(0,1, 10, 100),
  #             labels = c(0,1, 10, 100)) +
  #scale_x_continuous(limits = c(0,100), expand = c(0,0.2)) +
  #scale_x_continuous(breaks = c(0,1,2,3),
  #                 labels = c(0,1,10,100)) +
  scale_y_log10(breaks = c(0.1+1,1+1,10+1,100+1,1000+1,10000+1),
                labels = c(0.1,1,10,100,1000,10000)) +
  scale_colour_gradient(low = "red", high = "blue") +
  ylab("MP Concentrations (items/kg)") +
  xlab("Soil Depth (cm)") +
  annotation_logticks(outside = TRUE,
                      size = 0.3,
                      short = unit(0.05, "cm"),
                      mid = unit(0.05, "cm"),
                      long = unit(0.1, "cm")) +
  ggtitle("B)") +
  geom_smooth(method = "gam",
              color = "grey17",
              formula = y ~ x,
              method.args = list(family = tw),
              se = FALSE)


ggsave(HFI_plot, file = "C:/Users/lmills96/OneDrive - UBC/R/Depth Plot.png", 
       width = 3.23, height = 3, dpi = 600, units = "in")



Elev_plot <- 
  ggplot(data = MP_DF, 
         aes(y = MP_Conc_items_kg+1, x = log10(Elevation))) +
  geom_point(aes(col = Elevation), alpha = 0.8, pch = 16) +
  
  geom_smooth(method = "gam",
              color = "grey17",
              formula = y ~ x,
              method.args = list(family = tw),
              se = FALSE) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=8, family = "sans", face = "bold"),
        axis.title.x = element_text(size=8, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "none",
        legend.key.size = (unit(0.5, "cm")),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm")) +
  #scale_y_continuous(limits = c(0,13000), expand = c(0,0.2)) +
  #scale_x_log10(expand = c(0,0.1))
  #scale_x_continuous(limits = c(0,101), expand = c(0,0.2)) +
  scale_x_continuous(breaks = c(0,1,2,3),
                     labels = c(1, 10, 100, 1000)) +
  scale_y_log10(breaks = c(1+1,10+1,100+1,1000+1,10000+1),
                labels = c(1,10,100,1000,10000)) +
  scale_colour_gradient(low = "red", high = "blue") +
  ylab("MP Concentrations (items/kg)") +
  xlab("Elevation (m)") +
  annotation_logticks(outside = TRUE,
                      size = 0.3,
                      short = unit(0.05, "cm"),
                      mid = unit(0.05, "cm"),
                      long = unit(0.1, "cm")) +
  ggtitle("C)")


ggsave(plot1, file = "C:/Users/lmills96/OneDrive - UBC/R/Elev Plot.png", 
       width = 3.23, height = 3, dpi = 600, units = "in")



BOT <-
  grid.arrange(HFI_plot,Depth_plot,
               ncol = 2,
               nrow = 1)

FIG <-
  grid.arrange(BOT, Elev_plot, 
               ncol=1,
               nrow=2,
               heights = c(8,5.5),
               widths = 5)

ggsave(FIG, file = "C:/Users/lmills96/OneDrive - UBC/R/Current Trends Plot.png", 
       width = 3.23, height = 3, dpi = 600, units = "in")






#FIGURE 3
  #VARIOGRAM FOR SPATIAL AUTOCORRELATION

#Deviance residuals are appropriate for models with a non-Gaussian distribution and a log link function
RES <- data.frame(res = residuals(MOD.glm,
                                  type = "deviance"),  
                  x = MP_DF$Lat,
                  y = MP_DF$Long)
coordinates(RES) <- c("x", "y")

#Create variogram
vg.Residuals <- variogram(res ~ 1, data = RES)

#Plot variogram of residuals
png(file = "Variogram.png", width = 6.86, height = 6, res = 600, units = "in")
plot(vg.Residuals,
     col = "red",
     pch = 19,
     xlab = "Distance",
     ylab = "Semivariance",
     main = "Semivariogram",
     ylim = c(0,18)) 
dev.off()






#FIGURE 4
  #CURRENT VS PREDICTED MP CONCENTRATIONS

png(file = "Predictions.png", width = 6.86, height = 6, res = 600, units = "in")
plot(MP_DF$MP_Conc_items_kg ~ predict(MOD.glm, type = "response"),
     xlab = "Predicted MP Concentration (items/kg)",
     ylab = "Observed MP Concentration (items/kg)",
     col = "blue",
     pch = 19)
abline(0,1, col = "grey19")
dev.off()




#--------------------CHAPTER 3 --------------------

#FIGURE 1
  #PREDICTING TRENDS: Observed vs. Predicted

MPs_Depth <- exp(6.5325928 + 0.0018720 * seq(1,100, 0.1))
Depth_plot <- plot(MPs_Depth ~ seq(1,100, 0.1) , type = "l", 
     ylab = "MP Concentrations (items/kg)", 
     xlab = "Soil Depth", col = "blue")

MPs_HFI <- exp(6.5325928 + 1.7624277  * seq(0,1, 0.01))
HFI_plot <- plot(MPs_HFI ~ seq(0,1, 0.01) , type = "l", 
     ylim = c(0,10000), 
     ylab = "MP Concentrations (items/kg)", 
     xlab = "HFI", col = "blue")

MPs_Elev <- exp(6.5325928 + -0.1985244 * seq(1,1000, 0.1))
Elev_plot <-  plot(MPs_Elev ~ seq(1,1000, 0.1), 
     type = "l", ylim = c(0,10000),
     ylab = "MP Concentrations (items/kg)", 
     xlab = "Elevation", col = "blue")


#COMBINING PLOTS INTO ONE FIGURE
png(file = "current_trends.png", height = 11, width = 8, res = 600, units = "in")
par(mfrow=c(3,1))
plot(MPs_Depth ~ seq(1,100, 0.1) , type = "l", 
                   ylab = "MP Concentrations (items/kg)", 
                   xlab = "Soil Depth", col = "blue")
plot(MPs_HFI ~ seq(0,1, 0.01) , type = "l", 
                 ylim = c(0,10000), 
                 ylab = "MP Concentrations (items/kg)", 
                 xlab = "HFI", col = "blue")
plot(MPs_Elev ~ seq(1,1000, 0.1), 
                   type = "l", ylim = c(0,10000),
                   ylab = "MP Concentrations (items/kg)", 
                   xlab = "Elevation", col = "blue")
dev.off()




