
#Import the HS data with coordinates
HS_Coords <- read.csv("C:/Users/lmills96/OneDrive - UBC/MSc Thesis Info/Global Analysis/Terrestrial-Microplastics/Scripts/RA/HS_Coords.csv")
HS_Coords <- HS_Coords[,-c(3,4)]
names(HS_Coords)[1] <- "Long"
names(HS_Coords)[2] <- "Lat"

#projections are easting, northing
HS_locations <- st_as_sf(HS_Coords, coords = c("Long", "Lat"), crs="+proj=longlat +datum=WGS84")


#Convert to spactVect class
HS_locations <- vect(HS_locations)

#Reproject
HS_locations <- terra::project(HS_locations, crs_wintri)

#Get local HFI values
HS_Coords$HFI <- terra::extract(HFI, HS_locations)[,2]

#Get local elevation values
HS_Coords$Elevation_km <- terra::extract(Elevation_km, HS_locations)[,2]

#Get local soil types
HS_Coords$soil_type <- terra::extract(soil, HS_locations)[,2]

#Storing the projected coordinates in case they are needed
HS_Coords$x <- geom(HS_locations)[,3]
HS_Coords$y <- geom(HS_locations)[,4]

#Add depth at 15cm for all sites
HS_Coords$Max_Depth_cm <- rep(15, nrow(HS_Coords))

#Add in MP conc for each 
HS_Items_kg <- c(0.000662, 0, 0, 0, 0.000571, 0, 0, 0.00215)
HS_Coords$HS_Items_kg <- HS_Items_kg


HS_Coords$Study <- "newstudy"
HS_Coords$mu <- predict(model, 
                        newdata = HS_Coords,
                        se.fit = FALSE,
                        type = 'response')



#Adding Site ID to data set for ease
HS_Coords$Site_ID <- c("HSN.I.1", "HSN.I.2", "HSN.O.1", "HSN.O.3", 
                       "HSR.I.1", "HSR.I.3", "HSR.O.1", "HSR.O.3")
