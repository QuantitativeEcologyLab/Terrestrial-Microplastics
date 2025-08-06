
# ---------------------------------------------------------------------------
# Load required items
# ---------------------------------------------------------------------------

# Load required packages
library(terra)
library(future.apply)

# Load required rasters
HFI <- rast("/home/lmills96/Documents/GDMP/Rasters/HFI_processed.tif")
elevation_m <- rast("/home/lmills96/Documents/GDMP/Rasters/elev_processed.tif")

# Load MPdf dataset 
MPdf <- read.csv("./Data/MPdf.csv")

# ---------------------------------------------------------------------------
# Decrease raster resolutions
# ---------------------------------------------------------------------------

# Decrease raster resolution 
HFI <- aggregate(HFI, fact = 3)
# Match resolution
elevation_m <- terra::resample(elevation_m, HFI, method = "bilinear")

  res(HFI) == res(elevation_m) # TRUE
  same.crs(HFI, elevation_m) # TRUE
  ext(HFI) == ext(elevation_m) # TRUE

# Save updated rasters
writeRaster(HFI, filename = "/home/lmills96/Documents/GDMP/Rasters/HFI_300res.tif", overwrite = TRUE)
writeRaster(elevation_m, filename = "/home/lmills96/Documents/GDMP/Rasters/elev_300res.tif", overwrite = TRUE)


#----------------------------------------------------------------------
# Get bounding box of case example 
#----------------------------------------------------------------------

# Using study # 2 for a case study/example: 
# 100 observations in Korea 
# Note: coordinates were given in paper (table 2), along with [MP] broken 
# up into sizes. Particles <1mm and 1-5mm were added together to obtain
# total [MP]

# Subset data
MPdf_study <- MPdf[MPdf$study == "2",]

# Convert df to sf object
locations_study <- st_as_sf(MPdf_study, coords = c("x", "y"), crs = crs(HFI))
same.crs(locations_study, HFI) # TRUE

# Determine the bounding box from coordinates of study # 2
EXT <- ext(locations_study)

# Calculate height & width of bbox
# EXT[1] = xmin (left edge of raster)
# EXT[2] = xmax (right edge of raster)
# EXT[3] = ymin (bottom edge of raster)
# EXT[4] = ymax (top edge of raster)
height <- EXT[4] - EXT[3]
width <- EXT[2] - EXT[1]

# Add a buffer around edge of bbox (using max() to ensure that the buffer is 
# large enough to cover both dimensions symmetrically - choosing the larger
# of the two so that both x and y directions are padded equally by the same
# size)
size <- max(height*2, width*2)

# Center the extent to keep the same midpoint after adding the buffer
x_center <- (EXT[1] + EXT[2])/ 2 # finding midpoint / avg between the two coords
y_center <- (EXT[3] + EXT[4])/ 2 # finding midpoint / avg between the two coords
# Note: x_center is the midpoint of original raster

# Create a new square extent
# Note: 'size' is the desired FULL length side of the new bbox (including 
# the buffer)
# Note: dividing by 2 here to define how far from center to extend bbox - 
# by dividing by 2, we are evenly buffering the area 
new_EXT <- ext(x_center - size/2, # defines a bbox that extends half of 'size' to left of midpoint...
               x_center + size/2, # ...and half of 'size' to the right (total width = size)
               y_center - size/2,
               y_center + size/2)

# ---------------------------------------------------------------------------
# Crop rasters to bounding box
# ---------------------------------------------------------------------------

# Crop rasters to match bbox
HFI_crop <- 







# ---------------------------------------------------------------------------
# Creating the function to parallelize code 
# ---------------------------------------------------------------------------

#Parallelize the code
plan(multisession, workers = 10)

# Create a function
  # Note: can run function for just one raster (rather than raster stack) b/c 
  # both HFI and elev are the same - just need to ensure you are CROPPING the 
  # stacked raster
process_block <- function(i) {

  row_start <- ((i - 1) * rows_per_block) + 1
  row_end <- min(i * rows_per_block, rows)
  
  # Get the y (latitude) coordinates of the rows
  y_start <- yFromRow(HFI, row_start)  # top edge of first row in block
  y_end <- yFromRow(HFI, row_end)      # top edge of last row in block
  
  # Raster y-axis usually goes from top (max) to bottom (min), so set correct 
  # extent
  y_min <- min(y_start, y_end)
  y_max <- max(y_start, y_end)
  
  # Full x extent (all columns)
  x_min <- xmin(HFI)
  x_max <- xmax(HFI)
  
  # Define spatial extent to crop
  ext_block <- ext(x_min, x_max, y_min, y_max)
  
  block <- crop(stacked_rasters, ext_block)
  
  as.data.frame(block, xy = TRUE)
}

# Parallel processing
chunks <- future_lapply(1:blocks, process_block)

# Combine all into one data frame
newdf <- do.call(rbind, chunks)
names(newdf)[3] <- "HFI"

# Save df
write.csv(newdf, file = "/home/lmills96/Documents/GDMP/Data/newdf.csv")




# -------------------------------------------------------------------------
# Annotated code:
# -------------------------------------------------------------------------

# Note: do not need to create an empty list (chunks <- list()) here because we
# are NOT using a for loop. future_lapply() is for parallel processing
    # i.e., a for loop executes iterations one after another in a sequential 
    # order (each iteration must complete before the next one begins) ---
    # future_lapply() enables parallel / asynchronous execution of tasks; it 
    # distributes the iterations across multiple cores 
    # future_lapply() is beneficial when iterations are independent of each 
    # each other which allows them to run concurrently 
# chunks <- future_lapply(1:blocks, process_block) already constructs and fills
# a list for you

# To parallelize code, could also do this:
#CORES <- future::availableCores(logical = FALSE) - 1
#plan(multisession, workers = CORES)
  # this returns the number of available cores and leaves one core free ---
  # didn't do this because multiple people using various cores on Linux 

# Defines a function that takes a single input i - i.e., represents the block 
# index [from 1 to block 10])
process_block <- function(i) {
  
  # calculating which rows of the raster the block should cover
    # row_start: the first row of block i
        # e.g., if rows_per_block = 5858 and i=2, row_start = (2-1)*5858 +1 = 
        # 5859
  # (i-1) * rows_per_block -> calculates how many rows come BEFORE the 
    # current block (to ensure there is no overlap/gaps) 
        # for block 1 (i=1), this is (1-1)*5858 = 0 rows before - so start at 
        # row 1
        # for block 2 (i=2), this is (2-1)*5858 = 5858 rows before - so the 
        # second 
        # block starts right after ther first 5858 rows
  # '+1' because row numbering starts at 1 (not 0), so need 1 to move to the 
    # the first row of the current block 
  row_start <- ((i - 1) * rows_per_block) + 1
  
  # row_end: the last row of block i (capped at the total number of rows ('rows)
    # so you don't go past the raster's bottom edge)
  # i*rows_per_block calculates the last row number for block i if all blocks 
    # were exactly the same size 
        # e.g., if you split your rster's rows into equal-sized chunks (blocks),
        # each block contains exactly rows_per_block rows (except maybe the last
        # one if rows don't divide evently). if it doesn't divide evenly,
        # you need to ensure the raster doesn't overshoot n the last block
            # (i * rows_per_block) -- for block 1 (i=1), the last row should be
            # (1*5858) = 5858, block 2 should be (2*5858) = 11716, but if they 
            # are not evenly-sized chunks, the last chunk may be smaller so you
            # want to pick the smaller value: either the CALCULATED END ROW, or 
            # the TOTAL ROWS in the raster
                # e.g., for 58583 rows, chunk 10 would be:
                # (i * rows_perblock) = (10 * 5858) = 58,580... but in reality,
                # the last chunk has 58,583 - min prevents going over but will
                # not include those last 3 rows --- not a problem necessarily 
                # if you have MORE rows, but will lead to issues if you have
                # LESS rows / the rows don't exist 
  row_end <- min(i * rows_per_block, rows)
  
  # get the y (latitude) coordinates of the rows
  # converting the row numbers (row_start / row_end) to spatial y-coords
  #yFromRow gives the y-coordinate (center) of that pixel in that row 
      # a raster is a grid of pixels coverign a geographic area - each row in 
      # the raster corresponds to a horizontal slize of pixels. each pixel has 
      # a specific spatial location defined by (x,y) --- yFromRow(HFI,row_start)
      # returns the y-coord (lat) of the center of the pixels in that specific 
      # row. this is allowing you to determine which latitudes are in that block
  y_start <- yFromRow(HFI, row_start)  # top edge of first row in block
  y_end <- yFromRow(HFI, row_end)      # top edge of last row in block
  
  # raster y-axis usually goes from top (max) to bottom (min), so set correct 
  # extent ensures the y_min is the smaller (southern/lower) coordinate and 
  # y_max is larger (northern/higher) 
      # this orders the coords to ensure that y_min is always the lower value
  # yFromRow() gets the real-world coordinates for your block of rows vs. min()
    # ensures they are properly ordered when creating the bbox / spatial extent
      # ymin()/ymax() refers to latitude/northing
          # ymax(HFI) = the TOP edge of raster (north-most lat/max northing)
          # ymin(HFI) = the BOTTOM edge of raster (south-most lat/min northing)
              # thus, this is the VERTICAL extent (height) of raster
  y_min <- min(y_start, y_end)
  y_max <- max(y_start, y_end)
  
  # full x extent (all columns)
  # getting the FULL horizontal extent of the raster (leftmost and rightmost 
  # coordinates) --- ensuring we cover all columns in width
      # xmin()/xmax() refers to width, not height:  
          # they refer to the longitude/easting 
          # xmin(HFI) = the LEFT edge of  raster (west-most long/min easting)
          # xmax(HFI) = the RIGHT edge of  raster (east-most long/max easting)
              # thus, this is the HORIZONTAL extent (width) of raster 
  # don't need xmin(x_start, x_end) because the block spans all columns / full
    # width (you're looking at rows, not columns)
  x_min <- xmin(HFI)
  x_max <- xmax(HFI)
  
  # defining the spatial extent to crop
  # geting the bounding box that corresponds to the area covered by the rows 
    # assinged, and spanning the full width of the raster
  ext_block <- ext(x_min, x_max, y_min, y_max)
  
  # extracting that portion of the HFI raster within the bbox of that block
  block <- crop(HFI, ext_block)
  
  # converts the cropped raster block into a dataframe (returning one dataframe
  # per chunk)
  as.data.frame(block, xy = TRUE)
}

# Parallel processing
# (1:blocks): each i will be passed into process_block
# process_block: takes block i, determines the rows to extract from the raster,
  # crops the raster to these rows, then returns a df 
# chunks is now a list of dataframes
chunks <- future_lapply(1:blocks, process_block)

# Combining all into one data frame
# Binds all dataframes into one --- do.call() is used when you want to apply a 
  # function (like rbind) to each element of a list
newdf <- do.call(rbind, chunks)






# OLD - LIKELY DON'T NEED TO DO THIS ANYMORE
# # ---------------------------------------------------------------------------
# # Determining how many chunks to break raster into
# # ---------------------------------------------------------------------------
# 
# # Determine how many rows in raster
#   # Note: only need to look at one raster as they are both the exact same
# nrow(HFI) # 58580 
# 
# # Manually divide raster by rows
# rows <- nrow(HFI) # 58580 rows
# 
# # Define the number of blocks 
# blocks <- 10
# 
# # Dividing the raster into 10 chunks (5858 rows each)
# rows_per_block <- ceiling(rows / blocks)