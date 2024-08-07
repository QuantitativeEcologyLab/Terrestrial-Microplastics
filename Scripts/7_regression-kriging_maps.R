
# Load required packages
library(ggplot2)
library(tidyterra)
library(terra)
library(viridis)

#res = 1 and 5 indicating the level of detail. 1 is high 5 is low
world <- world(resolution=5, level=0, path = tempdir(), version="latest")

# Convert to sf object
world_sf <- st_as_sf(world)

#define the projection system
crs_wintri <- "ESRI:53018" 

# Convert CRS to lat/long projection
world_sf <- st_transform(world_sf, crs = crs_wintri)
st_crs(world_sf)

mp_coord <- read.csv("C:/Users/lmills96/OneDrive - UBC/MSc Thesis Info/Global Analysis/Global MP Distribution/global_coords.csv")



# ----------------------------------------------------------------------
# Model Prediction 
# ----------------------------------------------------------------------


#with data ponts 
model_prediction_map <- 
  ggplot() +
  geom_spatraster(data = MP_prediction_model_response_scale, maxcell = 5e+06) +
  geom_point(data = mp_coord, aes(x = long, y = lat), color = "#924900", 
             size = 0.8, shape = 1) +
  scale_fill_viridis(name = "",
                     na.value = "white",
                     option = "turbo",
                     breaks=c(500,1000,1500,2000,2500,3000,3500),
                     labels=c(500,1000,1500,2000,2500,3000,3500),
                     limits=c(0,4000)) +
  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
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
        legend.text.align = 0,
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks = element_blank(),
        strip.background=element_blank()) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) 
guides(fill=guide_colourbar(title.position = "top", title="", barwidth = 30, ticks.colour = "grey20"))


ggsave("model_prediction_map.png", plot = model_prediction_map, width = 6,
       height = 4, units = "in", dpi = 300)




# ----------------------------------------------------------------------
# Regression Kriging Model
# ----------------------------------------------------------------------

var1 <- exp(var1_resampled)

regression_krig_var_map <- 
  ggplot() +
  geom_spatraster(data = var1, maxcell = 5e+06) +
  geom_point(data = mp_coord, aes(x = long, y = lat), color = "#924900", 
             size = 0.8, shape = 1) +
  scale_fill_viridis(name = "",
                     na.value = "white",
                     option = "turbo") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(colour = "white", fill = "white"),
        plot.background = element_rect(colour = "white", fill = "white"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks = element_blank(),
        legend.position="top",
        legend.title.align=0.5,
        legend.key.size = unit(0.25, "cm"),
        legend.key.width = unit(3, "cm")) 
  


ggsave("regression_krig_map.png", plot = regression_krig_var_map, width = 6,
       height = 4, units = "in", dpi = 300)




# ----------------------------------------------------------------------
# Final Regression Kriging Model
# ----------------------------------------------------------------------

final_regression_krig_map <- 
  ggplot() +
  geom_spatraster(data = MP_prediction, maxcell = 5e+06) +
  geom_point(data = mp_coord, aes(x = long, y = lat), color = "#924900", 
             size = 0.8, shape = 1) +
  scale_fill_viridis(name = "",
                     na.value = "white",
                     option = "turbo",
                     breaks=c(500,1000,1500,2000,2500,3000,3500),
                     labels=c(500,1000,1500,2000,2500,3000,3500),
                     limits=c(0,4000)) +
  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
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
        legend.text.align = 0,
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks = element_blank(),
        strip.background=element_blank()) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) 
  guides(fill=guide_colourbar(title.position = "top", title="", barwidth = 30, ticks.colour = "grey20"))

  
ggsave("final_regression_krig_map.png", plot =final_regression_krig_map, width = 6,
       height = 4, units = "in", dpi = 300)











