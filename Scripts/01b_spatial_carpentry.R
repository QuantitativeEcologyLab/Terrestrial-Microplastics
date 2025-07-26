
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

# Define projection system
  # Note: mollweide is area-preserving: every km^2 of Earth appaears as the
    # same size on the map; ideal for quantitative data
  # Note: mollweide is a projection, not a full crs with an EPSG code and so
    # using just 'crs_Mollweide <- "EPSG:6326"' would not work as there is
    # currently no PROJ string
crs_Mollweide <- "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"

# Import soil and HFI
soil <- raster("./Rasters/HWSD2.bil")
#hfi_raster <- raster("./Rasters/ml_hfi_v1_2019.nc")
x <- rast("./Rasters/hfp_2021_100m_v1-2_cog.tif")/1000

# Download elevation raster (Global)
elevation <- elevation_global(10, "./Rasters")

# Get world boundaries for clipping (not critical, but makes maps nicer looking)
world_sf <- st_as_sf(rworldmap::getMap(resolution = "low"))

#Drop Antarctica
#world_sf <- subset(world_sf, continent != "Antarctica")

# Reproject
world_mollweide <- lwgeom::st_transform_proj(world_sf, crs = crs_Mollweide)

# Convert to SpatVector class
world_mollweide <- vect(world_mollweide) 
plot(world_mollweide)


#-----------------------------------------------------------------------
# Setup the HFI raster
#-----------------------------------------------------------------------

message("Processing the HFI raster")

# Crop raster to Mollweide
hfi_raster <- terra::crop(x, world_mollweide)

# Clip the raster to the extent of the world map
hfi_raster <- terra::mask(hfi_raster, world_mollweide)
plot(hfi_raster)
# Reproject raster to ensure HFI raster is in Mollweide projection
HFI <- terra::project(hfi_raster, crs_Mollweide)


#-----------------------------------------------------------------------
# Setup the elevation raster
#-----------------------------------------------------------------------

message("Processing the elevation raster")

# Reproject raster
elevation <- terra::project(elevation, crs_Mollweide)

# Mask the raster to Mollweide
elevation <- terra::mask(elevation, world_mollweide)

# Match the elevation raster resolution to the HFI resolution
elevation <- terra::resample(elevation, HFI, method = "bilinear")

# Note: method="bilinear" specifies how the pixel values are interpolated when
# resamplng a raster. "bilinear" is ideal for continuous data as it is the 
# weighted average of the 4 nearest neighbouring cell values (thus smoothing
# the data). If using method="near", it grabs the closes pixel and can introduce
# blocky artifacts. Could be OK for categorical data. 

#-----------------------------------------------------------------------
# Setup the soil raster
#-----------------------------------------------------------------------

message("Processing the soil raster")


# CAN I NOT LOAD SOIL AS TERRA::RAST??? 


# Convert to spatRast
# ???? soil <- rast(soil)

# Reproject
soil <- terra::project(soil, crs_Mollweide)

# Clip the soil type raster to the extent of the transformed world map
soil <- terra::mask(soil, world_mollweide)

# Match all resolution based HFI resolution
soil <- terra::resample(soil, HFI, method = "bilinear")



#-----------------------------------------------------------------------
# Checking all rasters are aligned correctly
#-----------------------------------------------------------------------

message("Running checks on processed rasters. If any are FALSE, there is a problem")

res(HFI) == res(elevation)
crs(HFI) == crs(elevation)

res(HFI) == res(soil)
crs(HFI) == crs(soil)

res(elevation) == res(soil)
crs(elevation) == crs(soil)


# Save rasters
writeRaster(HFI, filename = "./Rasters/HFI_processed.tif", overwrite = TRUE)
writeRaster(elevation, filename = "./Rasters/elev_processed.tif", overwrite = TRUE)
writeRaster(soil, filename = "./Rasters/soi_processed.tif", overwrite = TRUE)
