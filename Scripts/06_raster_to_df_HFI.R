## Breaking up large raster into blocks 
library(terra)
library(future.apply)


# Manually divide raster by rows
rows <- nrow(HFI) # 58580 rows

# Define the number of blocks 
blocks <- 10

# Dividing the raster into 10 chunks (5858 rows each)
rows_per_block <- ceiling(rows / blocks)

# Create empty list
#chunks <- list()

#Parallelize the code
#CORES <- future::availableCores(logical = FALSE) - 1
plan(multisession, workers = 10)

# Create a for loop
process_block <- function(i) {

  row_start <- ((i - 1) * rows_per_block) + 1
  row_end <- min(i * rows_per_block, rows)
  
  # Get the y (latitude) coordinates of the rows
  y_start <- yFromRow(HFI, row_start)  # top edge of first row in block
  y_end <- yFromRow(HFI, row_end)      # top edge of last row in block
  
  # Raster y-axis usually goes from top (max) to bottom (min), so set correct extent
  y_min <- min(y_start, y_end)
  y_max <- max(y_start, y_end)
  
  # Full x extent (all columns)
  x_min <- xmin(HFI)
  x_max <- xmax(HFI)
  
  # Define spatial extent to crop
  ext_block <- ext(x_min, x_max, y_min, y_max)
  
  block <- crop(HFI, ext_block)
  
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
# Annotated for loop:
# -------------------------------------------------------------------------

# defines a function that takes a single input i - i.e., represents the block 
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
      # a specific spatial location defined by (x,y) --- yFromRow(HFI, row_start)
      # returns the y-coord (lat) of the center of the pixels in that specific 
      # row. this is allowing you to determine which latitudes are in that block
  y_start <- yFromRow(HFI, row_start)  # top edge of first row in block
  y_end <- yFromRow(HFI, row_end)      # top edge of last row in block
  
  # raster y-axis usually goes from top (max) to bottom (min), so set correct extent
  # ensures the y_min is the smaller (southern/lower) coordinate and y_max is 
  # larger (northern/higher) 
      # this orders the coordinates to ensure that y_min is always the lower value
  # yFromRow() gets teh real-world coordinates for your block of rows vs. min()
    # ensures they are properly ordered when creating the bbox / spatial extent
      # ymin()/ymax() refers to latitude/northing
          # ymax(HFI) is the TOP edge of the raster (north-most lat or max northing)
          # ymin(HFI) is the BOTTOM edge of the raster (sourth-most lat or min northing)
              # thus, this is the VERTICAL extent (height) of raster
  y_min <- min(y_start, y_end)
  y_max <- max(y_start, y_end)
  
  # full x extent (all columns)
  # getting the FULL horizontal extent of the raster (leftmost and rightmost 
  # coordinates) --- ensuring we cover all columns in width
      # xmin()/xmax() refers to width, not height:  
          # they refer to the longtiude/easting 
          # xmin(HFI) is the LEFT edge of the raster (west-most long or min easting)
          # xmax(HFI) is the RIGHT edge of the raster (east-most long or max easting)
              # thus, this is the HORIZONTAL extent (width) of raster 
  # you don't need xmin(x_start, x_end) because the block spans all columns / full
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


# parallel processing
# (1:blocks): each i will be passed into process_block
# process_block: takes block i, determines the rows to extract from the raster,
  # crops the raster to these rows, then returns a df 
# chunks is now a list of dataframes
chunks <- future_lapply(1:blocks, process_block)

# combining all into one data frame
# binds all dataframes into one --- do.call() is used when you want to apply a 
  # function (like rbind) to each element of a list
newdf <- do.call(rbind, chunks)