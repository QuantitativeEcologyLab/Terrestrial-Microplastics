
message("Starting data import")

# Load required packages
library(raster)
library(terra) #note: need to be careful about the loading of raster and terra at the same time
library(ncdf4)
library(sf)
library(geodata)

#=======================================================================
# Note: this script can be skipped as the rasters are already saved and 
# can be loaded in 02_MP_data_processing.R
#=======================================================================


#-----------------------------------------------------------------------
# Initial raster import and setup
#-----------------------------------------------------------------------

# Define the projection system
crs_wintri <- "ESRI:53018"
crs__Mollweide <- "EPSG:6326"

# Import soil and HFI
soil <- raster("./Rasters/HWSD2.bil")
hfi_raster <- raster("./Rasters/ml_hfi_v1_2019.nc")

# Download elevation raster (Global)
Elevation <- elevation_global(10, "./Rasters")

# Get world boundaries for clipping (not critical, but makes maps nicer looking)
world_sf <- st_as_sf(rworldmap::getMap(resolution = "low"))

#Drop Antarctica
world_sf <- subset(world_sf, continent != "Antarctica")

# Reproject
world_wintri <- lwgeom::st_transform_proj(world_sf, crs = crs(x))

# Convert to SpatVector class
world_wintri <- vect(world_wintri) 
plot(world_wintri)


#-----------------------------------------------------------------------
# setup the elevation raster
#-----------------------------------------------------------------------

message("Processing the elevation raster.")

# Reproject
Elevation <- terra::project(Elevation, crs_wintri)

# Clip the elevation raster to the extent of the transformed world map
Elevation <- terra::mask(Elevation, world_wintri)
Elevation_m <- Elevation
# # Match all resolution based HFI resolution
# Elevation <- terra::resample(Elevation, x, method = "bilinear")

#-----------------------------------------------------------------------
# setup the HFI raster
#-----------------------------------------------------------------------

message("Processing the HFI raster.")

x <- rast("./Rasters/hfp_2021_100m_v1-2_cog.tif")/1000

x <- terra::crop(x, world_wintri)

#x <- terra::project(x, Elevation, method = "bilinear")

x <- terra::mask(x, world_wintri)

x <- terra::resample(x, Elevation, method = "bilinear")
HFI <- x
# ----------------------
# # Convert to spatrast
# hfi_raster <- rast(hfi_raster)
# 
# # Reproject
# hfi_raster <- terra::project(hfi_raster, crs_wintri)
# 
# # Clip the raster data (HFI) to the extent of the transformed world map
# HFI <- terra::mask(hfi_raster, world_wintri)


#-----------------------------------------------------------------------
# setup the soil raster
#-----------------------------------------------------------------------

message("Processing the soil type raster.")

# Convert to spatRast
soil <- rast(soil)

# Reproject
soil <- terra::project(soil, crs_wintri, method = "near") #specificy categorical (as factor) on import of code from Mike

# Clip the soil type raster to the extent of the transformed world map
soil <- terra::mask(soil, world_wintri)

# Match all resolution based HFI resolution
soil <- terra::resample(soil, Elevation, method = "bilinear")

#-----------------------------------------------------------------------
# Some checks to confirm everything is lined up correctly
#-----------------------------------------------------------------------
#(these should all come up as TRUE)

message("Running checks on the processed rasters. If any are FALSE, there is a problem.")

res(Elevation) == res(x)
crs(Elevation) == crs(x)


res(soil) == res(x)
crs(soil) == crs(x)


res(Elevation) == res(soil)
crs(Elevation) == crs(soil)


writeRaster(HFI, filename = "C:/Users/lmills96/OneDrive - UBC/MSc/Global Analysis/Terrestrial-Microplastics/Rasters/HFI_raster_processed.tif", overwrite = TRUE)
writeRaster(Elevation, filename = "C:/Users/lmills96/OneDrive - UBC/MSc/Global Analysis/Terrestrial-Microplastics/Rasters/elev_raster_processed.tif", overwrite = TRUE)
writeRaster(soil, filename = "C:/Users/lmills96/OneDrive - UBC/MSc/Global Analysis/Terrestrial-Microplastics/Rasters/soi_raster_processed.tif", overwrite = TRUE)
