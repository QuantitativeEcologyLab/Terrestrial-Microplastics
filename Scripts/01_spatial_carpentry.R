
library(raster)
library(terra) #note: need to be careful about the loading of raster and terra at the same time
library(ncdf4)
library(sf)
library(geodata)

#=======================================================================
# Note: this script can be skipped as rasters are already saved and can be 
# loaded in 02_MP_data_processing.R
#=======================================================================


#-----------------------------------------------------------------------
# Initial raster import and setup
#-----------------------------------------------------------------------

message("Starting data import.")

#define the projection system
crs_wintri <- "ESRI:53018"

#import soil and HFI
soil <- raster("C:/Users/lmills96/OneDrive - UBC/MSc/Global Analysis/Terrestrial-Microplastics/Rasters/HWSD2.bil")
hfi_raster <- raster("C:/Users/lmills96/OneDrive - UBC/MSc/Global Analysis/Terrestrial-Microplastics/Rasters/ml_hfi_v1_2019.nc")

# Download elevation raster (Global)
Elevation_m <- elevation_global(10, "C:/Users/lmills96/OneDrive - UBC/MSc/Global Analysis/Terrestrial-Microplastics/Rasters")

#Get world boundaries for clipping (not critical, but makes maps nicer looking)
world_sf <- st_as_sf(rworldmap::getMap(resolution = "low"))

#Drop antarctica
world_sf <- subset(world_sf, continent != "Antarctica")

#Reproject
world_wintri <- lwgeom::st_transform_proj(world_sf, crs = crs_wintri)

# Convert to spatvector class
world_wintri <- vect(world_wintri) 


#-----------------------------------------------------------------------
# setup the HFI raster
#-----------------------------------------------------------------------

message("Processing the HFI raster.")

#convert to spatrast
hfi_raster <- rast(hfi_raster)

#reproject
hfi_raster <- terra::project(hfi_raster, crs_wintri)

#clip the raster data (HFI) to the extent of the transformed world map
HFI <- terra::mask(hfi_raster, world_wintri)

#-----------------------------------------------------------------------
# setup the elevation raster
#-----------------------------------------------------------------------

message("Processing the elevation raster.")

#reproject
Elevation_m <- terra::project(Elevation_m, crs_wintri)

#clip the elevation raster to the extent of the transformed world map
Elevation_m <- terra::mask(Elevation_m, world_wintri)

# Match all resolution based HFI resolution
Elevation_m <- terra::resample(Elevation_m, HFI, method = "bilinear")

#-----------------------------------------------------------------------
# setup the soil raster
#-----------------------------------------------------------------------

message("Processing the soil type raster.")

#convert to spatRast
soil <- rast(soil)

#reproject
soil <- terra::project(soil, crs_wintri)

#clip the soil type raster to the extent of the transformed world map
soil <- terra::mask(soil, world_wintri)

# Match all resolution based HFI resolution
soil <- terra::resample(soil, HFI, method = "bilinear")

#-----------------------------------------------------------------------
# Some checks to confirm everything is lined up correctly
#-----------------------------------------------------------------------
#(these should all come up as TRUE)

message("Running checks on the processed rasters. If any are FALSE, there is a problem.")

res(Elevation_m) == res(HFI)
crs(Elevation_m) == crs(HFI)


res(soil) == res(HFI)
crs(soil) == crs(HFI)


res(Elevation_m) == res(soil)
crs(Elevation_m) == crs(soil)


writeRaster(HFI, filename = "C:/Users/lmills96/OneDrive - UBC/MSc/Global Analysis/Terrestrial-Microplastics/Rasters/HFI_raster_processed.tif", overwrite = TRUE)
writeRaster(Elevation_m, filename = "C:/Users/lmills96/OneDrive - UBC/MSc/Global Analysis/Terrestrial-Microplastics/Rasters/elev_raster_processed.tif", overwrite = TRUE)
writeRaster(soil, filename = "C:/Users/lmills96/OneDrive - UBC/MSc/Global Analysis/Terrestrial-Microplastics/Rasters/soi_raster_processed.tif", overwrite = TRUE)
