setwd("C:/Users/lmills96/OneDrive - UBC/MSc Thesis Info/Global Analysis/Global MP Distribution/Figures")

# Load required packages
library(sf)
library(ggplot2)
library(geodata)
library(khroma) # colourblind friendly colours 
library(terra)
library(tidyterra)
library(viridis)

# ----------------------------------------------------------------------
# Global map with sampled locations
# ----------------------------------------------------------------------

#res = 1 and 5 indicating the level of detail. 1 is high 5 is low
world <- world(resolution=5, level=0, path = tempdir(), version="latest")

# Convert to sf object
world_sf <- st_as_sf(world)

# Remove Antarctica
#subset(world_sf, continent != "Antarctica")

#define the projection system
crs_wintri <- "ESRI:53018" 

# Convert CRS to lat/long projection
world_sf <- st_transform(world_sf, crs = crs_wintri)
st_crs(world_sf)

#import my data
mp_coord <- read.csv("C:/Users/lmills96/OneDrive - UBC/MSc Thesis Info/Global Analysis/Global MP Distribution/global_coords.csv")



bright <- color("bright")
plot_scheme(bright(7), colours = TRUE, names = TRUE, size = 0.9)

world_map <-
ggplot() + 
  geom_sf(data = world_sf, fill = NA, color = "grey", size = 1) +
  geom_point(data = mp_coord, aes(x = long, y = lat), color = "#924900", size = 0.6) +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) 


ggsave("world_map.png", plot = world_map, width = 6,
       height = 4, units = "in", dpi = 300)


# ----------------------------------------------------------------------
# Global map with HFI
# ----------------------------------------------------------------------

# Load in HFI Data
rast_hfi <- rast("C:/Users/lmills96/OneDrive - UBC/MSc Thesis Info/Global Analysis/Global MP Distribution/Rasters/HFI_proj.tif")

#Load in the sample location data
#data <- read.csv("Global_Samples.csv")

#Reproject everything
crs_wintri <- "ESRI:53018"
#HFI <- terra::project(HFI, crs_wintri)
plot(rast_hfi)

#Clip HFI after reprojecting to avoid odd artefacts (slow so saved the projected raster)
world_sf <- st_as_sf(rworldmap::getMap(resolution = "low"))
world_sf <- subset(world_sf, continent != "Antarctica")
world_wintri <- lwgeom::st_transform_proj(world_sf, crs = crs_wintri)
clipped_HFI <- mask(rast_hfi, vect(world_wintri))
plot(clipped_HFI)

# locations <- st_as_sf(data, coords = c("Long", "Lat"))
# st_crs(locations) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# locations <- lwgeom::st_transform_proj(locations, crs = crs_wintri)
# locations <- as.data.frame(st_coordinates(locations))




#darkturquoise / springgreen - both look a little horrible tho 
world_map_hfi <- 
  ggplot() +
  geom_spatraster(data = clipped_HFI, maxcell = 5e+06) +
  scale_fill_viridis(name = "Human Footprint Index",
                     na.value = "white",
                     option = "magma",
                     breaks=c(0,0.25,0.5,0.75,1),
                     labels=c(0,0.25,0.5,0.75,1),
                     limits=c(0,1)) +
  #Add locations of study sites
  # geom_point(data = locations, aes(x = X, y = Y), col = "darkturquoise", size = 1, shape = 16) +
  # geom_point(data = locations, aes(x = X, y = Y), col = "black", size = 0.5, shape = 16) +
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
        legend.key.width = unit(1.5, "cm"),
        legend.background=element_blank(),
        legend.key = element_blank(),
        legend.text.align = 0,
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks = element_blank(),
        strip.background=element_blank()) +
  ylab(expression(paste("Latitude"))) +
  xlab(expression(paste("Longitude"))) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  guides(fill=guide_colourbar(title.position = "top", title="Human Footprint Index", barwidth = 30, ticks.colour = "grey20")) + 
  coord_sf()

  
ggsave("world_map_HFI.png", plot = world_map_hfi, width = 6,
       height = 4, units = "in", dpi = 300)







# ----------------------------------------------------------------------
# Global map with elevation
# ----------------------------------------------------------------------

# Load in HFI Data
rast_elev <- rast("C:/Users/lmills96/OneDrive - UBC/MSc Thesis Info/Global Analysis/Global MP Distribution/Rasters/ELEVATION.tif")

#Load in the sample location data
#data <- read.csv("Global_Samples.csv")

#Reproject everything
crs_wintri <- "ESRI:53018"
rast_elev <- terra::project(rast_elev, crs_wintri)

#Clip HFI after reprojecting to avoid odd artefacts (slow so saved the projected raster)
world_sf <- st_as_sf(rworldmap::getMap(resolution = "low"))
world_sf <- subset(world_sf, continent != "Antarctica")
world_wintri <- lwgeom::st_transform_proj(world_sf, crs = crs_wintri)
clipped_elev <- mask(Elevation_km, vect(world_wintri))
plot(clipped_elev)



world_map_elev <- 
  ggplot() +
  geom_spatraster(data = clipped_elev, maxcell = 5e+06) +
  scale_fill_viridis(name = "Elevation",
                     na.value = "white",
                     option = "magma",
                     breaks=c(0,500,1000,1500,2000,2500,3000),
                     labels=c(0,500,1000,1500,2000,2500,3000),
                     limits=c(0,3000)) +
  
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
  legend.key.width = unit(0.5, "cm"),
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
    guides(fill=guide_colourbar(title.position = "top", title="Elevation", barwidth = 30, ticks.colour = "grey20"))
  
  

ggsave("world_map_elev.png", plot = world_map_elev, width = 6,
       height = 4, units = "in", dpi = 300)


