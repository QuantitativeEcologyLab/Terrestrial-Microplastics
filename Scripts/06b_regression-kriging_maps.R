
# Load required packages
library(ggplot2)
library(tidyterra)
library(terra)
library(viridis)

# Res = 1 and 5 indicating the level of detail. 1 is high 5 is low
world <- world(resolution=5, level=0, path = tempdir(), version="latest")
# Convert to sf object
world_sf <- st_as_sf(world)
#define the projection system
crs_wintri <- "ESRI:53018" 
# Convert CRS to lat/long projection
world_sf <- st_transform(world_sf, crs = crs_wintri)
st_crs(world_sf)

# Get coordinates of MP data
mp_coord <- MPdf[,-c(1:12,15)]


# ----------------------------------------------------------------------
# Model Prediction 
# ----------------------------------------------------------------------

#with data points 
#model_prediction_map <- 
  ggplot() +
  geom_spatraster(data = MP_prediction_model, maxcell = 5e+06) +
  geom_point(data = mp_coord, aes(x = x, y = y), col = "white", size = 1.1, shape = 16) +
  geom_point(data = mp_coord, aes(x = x, y = y), col = "red", size = 0.9, shape = 16) +
  scale_fill_viridis(name = "",
                     na.value = "white",
                     option = "cividis",
                     breaks=c(500,1000,1500,2000,2500),
                     labels=c(500,1000,1500,2000,2500),
                     limits=c(0,2500)) +
  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(colour = "white", fill = "white"),
        plot.background = element_rect(colour = "white", fill = "white"),
        legend.position="top",
        legend.title.align=0.5,
        legend.title=element_text(color="black", size=14, family = "sans", face= "bold"),
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
  scale_x_continuous(expand = c(0,0)) +
guides(fill=guide_colourbar(title.position = "top", 
                            title="Predicted MP Conentrations from GAM (Items/kg)",
                            barwidth = 30, 
                            ticks.colour = "grey20"))

ggsave("./Figures/model_prediction_map.png", plot = model_prediction_map, width = 6,
       height = 4, units = "in", dpi = 300)


# ----------------------------------------------------------------------
# Kriging Model
# ----------------------------------------------------------------------

#var1 <- exp(var1_resampled) 

#kriging_map <- 
  ggplot() +
  geom_spatraster(data = var1_resampled, maxcell = 5e+06) +
  geom_point(data = mp_coord, aes(x = x, y = y), col = "white", size = 1.1, shape = 16) +
  geom_point(data = mp_coord, aes(x = x, y = y), col = "red", size = 0.9, shape = 16) +
  scale_fill_viridis(name = "",
                     na.value = "white",
                     option = "cividis") +
                     # breaks=c(-2.5,-2,-1.5,-1,0,1),
                     # labels=c(-2.5,-2,-1.5,-1,0,1),
                     #limits=c(-2.5,1)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(colour = "white", fill = "white"),
          plot.background = element_rect(colour = "white", fill = "white"),
          legend.position="top",
          legend.title.align=0.5,
          legend.title=element_text(color="black", size=14, family = "sans", face= "bold"),
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
    scale_x_continuous(expand = c(0,0)) +
    guides(fill=guide_colourbar(title.position = "top", 
                                title="Kriging Estimates of MP Concentrations (Items/kg)",
                                barwidth = 30, 
                                ticks.colour = "grey20"))
  
ggsave("./Figures/kriging_map.png", plot = kriging_map, width = 6,
       height = 4, units = "in", dpi = 300)


# ----------------------------------------------------------------------
# Regression Kriging Model
# ----------------------------------------------------------------------

#final_regression_krig_map <- 
  ggplot() +
  geom_spatraster(data = RK, maxcell = 5e+06) +
  geom_point(data = mp_coord, aes(x = x, y = y), col = "white", size = 1.1, shape = 16) +
  geom_point(data = mp_coord, aes(x = x, y = y), col = "red", size = 0.9, shape = 16) +
  scale_fill_viridis(name = "",
                     na.value = "white",
                     option = "cividis") +
                     #breaks=c(500,1000,1500,2000),
                     #labels=c(500,1000,1500,2000),
                     #limits=c(0,1000000)) +
  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(colour = "white", fill = "white"),
        plot.background = element_rect(colour = "white", fill = "white"),
        legend.position="top",
        legend.title.align=0.5,
        legend.title=element_text(color="black", size=14, family = "sans", face= "bold"),
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
  scale_x_continuous(expand = c(0,0)) +
  guides(fill=guide_colourbar(title.position = "top", title="Regression-Kriging MP Concentration Estimates (Items/kg)", barwidth = 30, ticks.colour = "grey20"))

ggsave("./Figures/final_regression_krig_map.png", plot =final_regression_krig_map, width = 6,
       height = 4, units = "in", dpi = 300)
