
# Removing all NAs from MPdf to get a model to predict for the NAs

# Remove all NAs
MPdf_subset <- na.omit(MPdf)

# Get GAM based off this dataset
model_MPdf_subset <- 
              gam(Items_kg ~
              # global terms
              s(HFI, k = 10, bs = "ad") + 
              s(sqrt(max_depth_cm), k = 5) +
              s(elevation_m, k = 6) + 
              # study-level terms
              s(study, bs = 're'), #random int - it doesn't really have random slopes 
            family = tw(link = 'log'),
            data = MPdf_subset,
            method = "REML",
            na.action = na.fail)

# Create dataset with coordinates where there were NAs
MPdf_NAs <- MPdf[!complete.cases(MPdf),]
MPdf_NAs <- MPdf_NAs[,c(5,6)]

# Interpolate NA values 








# # Projections are currently in lat/long so crs here needs to reflect that 
# NA_locations <- st_as_sf(MPdf_NAs, coords = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84")
# 
# # Convert to SpatVector (locations must be either SpatVector or SpatRaster to use terra::project())
# NA_locations <- vect(NA_locations)
# 
# # Reproject SpatVector to the same CRS as all rasters
# NA_locations <- terra::project(NA_locations, crs(HFI))
# 
# # Make sure it works: 
# plot(HFI)
# points(NA_locations, col = "red")
# 
# # Get local HFI values
# MPdf_NAs$HFI <- terra::extract(HFI, NA_locations)[,2]
# 
# # Get local elevation values
# MPdf_NAs$elevation_m <- terra::extract(elevation_m, NA_locations)[,2]
# 
# # Get local soil types
# MPdf_NAs$soil_type <- terra::extract(soil, NA_locations)[,2]
