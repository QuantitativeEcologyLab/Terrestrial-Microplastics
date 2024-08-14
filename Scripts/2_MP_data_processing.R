
message("Processing the MP dataset.")

#Import the MP data with coordinates
MPdf <- read.csv("C:/Users/lmills96/OneDrive - UBC/MSc Thesis Info/Global Analysis/Terrestrial-Microplastics/Scripts/Global_MP_R.csv")

#projections are easting, northing
locations <- st_as_sf(MPdf, coords = c("Long", "Lat"), crs="+proj=longlat +datum=WGS84")

#Convert to spactVect class
locations <- vect(locations)

#Reproject
locations <- terra::project(locations, crs_wintri)

#Get local HFI values
MPdf$HFI <- terra::extract(HFI, locations)[,2]

#Get local elevation values
MPdf$Elevation_km <- terra::extract(Elevation_km, locations)[,2]

#Get local soil types
MPdf$soil_type <- terra::extract(soil, locations)[,2]

#Storing the projected coordinates in case they are needed
MPdf$x <- geom(locations)[,3]
MPdf$y <- geom(locations)[,4]

#Need "Study" as a factor 
MPdf$Study <- as.factor(MPdf$Study)

#--------------------------------------------------------------------------
# Cleaning up final dataset
#--------------------------------------------------------------------------     

# Filtering out study 12 and 19 from dataset, removing NA's, and removing 
# one outlier from study 24 (row 139)

MPdf <- MPdf[-c(435:445, 550:664, 1139), ] 
MPdf <- MPdf[MPdf$Study !=23, ] 
MPdf <- na.omit(MPdf)
