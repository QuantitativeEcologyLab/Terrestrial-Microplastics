library(ggplot2)
library(geodata)
library(terra)
library(sf)

bc <- gadm(country="Canada", level = 2, tempdir())
bc <- bc[bc$NAME_2=="Thompson-Nicola", ]

plot(bc)

no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())


RA_Coords <- read.csv("C:/Users/lmills96/OneDrive - UBC/MSc Thesis Info/RA, Fieldwork, Lab Work, Soil Analyses/RA_Coords.csv")
RA_Coords <- RA_Coords[,-c(1,4:10)]
RA_Coords <- na.omit(RA_Coords)

#Projections are easting, northing
RA_Coords <- st_as_sf(RA_Coords, coords = c("Long", "Lat"), crs="+proj=longlat +datum=WGS84")

#Convert to spactVect class
RA_Coords <- vect(RA_Coords)

# Confirm CRS of 'bc'
bc_crs <- crs(bc)  # Get CRS of 'bc'

# Reproject 'RA_Coords' to match 'bc'
RA_Coords <- terra::project(RA_Coords, bc_crs)
crs(RA_Coords) == crs(bc_crs)

# Convert SpatVector back into df
RA_Coords <- st_as_sf(RA_Coords)

# Extract x and y coordinates (easting & northing) directly from the geometry 
# column (st_coordinates extracts the numeric x and y coordinates from the POINT
# geometries after reprojection)
RA_Coords <- cbind(RA_Coords, st_coordinates(RA_Coords))

crs(bc) == crs(RA_Coords)

# Note: don't use RA_Coords$x <- geom(RA_Coords)[,1] here for instance b/c it is
# extracting from a SpatVector 


#plot_bc <- 
  ggplot() +
  geom_sf(data=bc, fill = "#6a994e", color="black", size=.15, show.legend = FALSE) +
  geom_point(data = RA_Coords, aes(x = X, y = Y), color = "#924900", size = 0.6) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none") +
  no_axis

plot_bc






# ----------------- Adding RA coordinates to map ------------------

library(leaflet)
library(sf)

RA_map <- 
  leaflet(RA_Coords) %>%
  addTiles() %>%
  setView(lng = -120.530275, lat = 50.457125, zoom = 9) %>%
  addCircleMarkers(~X, ~Y,
                   radius = 5,
                   color = "black",
                   fillColor = "magenta",
                   fillOpacity = .5,
                   stroke = TRUE,
                   weight = 1)

# Saving as a static 
library(mapview)
webshot::install_phantomjs()
mapshot(RA_map, file= "figures/RA_map.png")


# HSN and HSR only

Hamilton_Stipa <- RA_Coords[-c(1:29,32,34,36:47,49,52),]



HS_map <- 
  leaflet(Hamilton_Stipa) %>%
  addTiles() %>%
  setView(lng = -120.448376, lat = 50.073852, zoom = 15) %>%
  addCircleMarkers(~X, ~Y,
                   radius = 5,
                   color = "black",
                   fillColor = "magenta",
                   fillOpacity = .5,
                   stroke = TRUE,
                   weight = 1)

mapshot(HS_map, file= "figures/HS_map.png")



