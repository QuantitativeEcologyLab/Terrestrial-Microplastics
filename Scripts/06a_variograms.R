
## Determining which variograms from MPdf are viable / could be used for RK

# Load required packages 
library(sf)
library(sp)
library(terra)
library(gstat)
library(dplyr)
library(ggplot2)
library(forecast)
library(gridExtra)
library(nlme)
library(ape)

# Load MPdf dataset 
MPdf <- read.csv("./Data/MPdf.csv")

# Note: see last section for each study's variogram

#--------------------------------------------------------------------------
# Determining mean range, sill, and nugget for all viable variograms
#--------------------------------------------------------------------------

# Excluding studies where variograms are not viable (either too little data or
  # non-stationary data)
    # Note: non-stationary in this context is in regards to the semivariance
    # behaving differently in different spatial directions (i.e., mean changes
    # depending on location). Variograms without a clear sill may indicate 
    # large-scale trends or a non-constant mean and thus the underlying spatial 
    # process is not constant throughout the study area


# Data frame consisting only of viable variograms
MPdf_vg <- MPdf[!MPdf$study %in% c("4","8","11","12","18","20","22","29","34",
                                   "36","37","38","41","42","44","48","49",
                                   "50","51","54","55","59","60"),]


# Create a list of the studies for the for loop
# Note: does not need to be a list as for loop can iterate over vectors
study_IDs <- unique(MPdf_vg$study)

# Each variogram has different parameters; make a list of each 
vg_param <- list(
  "1" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 9000, width = 100)),
  "2" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 10000)),
  "3" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 100000)),
  "5" = (variogram(residuals ~ 1, data  = vg.data)),
  "6" = (variogram(residuals ~ 1, data  = vg.data, width = 1)),
  "7" = (variogram(residuals ~ 1, data  = vg.data)),
  "9" = (variogram(residuals ~ 1, data  = vg.data)),
  "10" = (variogram(residuals ~ 1, data  = vg.data)),
  "13" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 150000)),
  "14" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 1500, width = 10)),
  "15" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 2000, width = 5)),
  "16" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 1000)),
  "17" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 10000, width = 10)),
  "19" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 3000)),
  "21" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 10000)),
  "23" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 300, width = 20)),
  "24" = (variogram(residuals ~ 1, data  = vg.data)),
  "25" = (variogram(residuals ~ 1, data  = vg.data)),
  "26" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 9000)),
  "27" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 300000)),
  "28" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 9000, width = 100)),
  "30" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 70000)),
  "31" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 40000, width = 2500)),
  "32" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 40000)),
  "33" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 400000, width = 500)),
  "35" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 50000, width = 4500)),
  "39" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 30000)),
  "40" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 500000, width = 900)),
  "43" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 900, width = 10)),
  "45" = (variogram(residuals ~ 1, data  = vg.data)),
  "46" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 800, width = 5)),
  "47" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 500, width = 1)),
  "52" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 150000)),
  "53" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 200, width = 10)),
  "56" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 20, width = 1)),
  "57" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 12000, width = 100)),
  "58" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 250000, width = 100)),
  "61" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 80000)),
  "62" = (variogram(residuals ~ 1, data  = vg.data, cutoff = 800)))

# Create empty vectors for parameters
range <- numeric(length(vg_param))
sill <- numeric(length(vg_param))
nugget <- numeric(length(vg_param))

# Create a for loop
for (i in seq_along(vg_param)) {
  
  study_IDs <- names(vg_param)[i] 
  vg <- vg_param[[i]] # need double brackets to extract 'i'
  
  # Fitting the variogram 
  vg.fit <- fit.variogram(vg, vgm("Sph", nugget = TRUE))
  
  # Extract range, sill, and nugget 
  range[i] <- vg.fit$range[2]
  sill[i] <- vg.fit$psill[1] + vg.fit$psill[2]
  nugget[i] <- vg.fit$psill[1]
}


# Determine mean values
mean_range <- mean(range) # 8402.096
mean_sill <- mean(sill) # 9.460706
mean_nugget <- mean(nugget) # 4.713404

# Create empty data frame for variogram points
vg_points <- data.frame()

# Obtain dist (lag distance) and gamma (semivariance) values for each variogram
for (i in seq_along(vg_param)) {
  study_IDs <- names(vg_param)[i]
  vg <- vg_param[[i]]
  
  # Add study ID to each variogram point
  vg_df <- as.data.frame(vg)
  vg_df$study <- study_IDs
  
  # Combine into master data frame
  vg_points <- bind_rows(vg_points, vg_df)
}

# Create the variogram line with mean values for plot
mean_vg_line <- variogramLine(
  vgm(psill = mean_sill - mean_nugget, # partial sill here
      model = "Sph", 
      nugget = mean_nugget, 
      range = mean_range),
  maxdist = max(vg_points$dist))

#vg_plot <- 
  ggplot() +
  geom_point(data = vg_points, aes(x = dist, y = gamma), alpha = 0.3) +
  # reminder: using partial sill for plot
  geom_line(data = mean_vg_line, aes(x = dist, y = gamma), color = "red", size = 1.2) + 
  theme_bw()

  ggsave(vg_plot, file = "./Figures/vg_plot.png")


# ------------------------------
# One variogram for all studies:
# ------------------------------

data <- aggregate(residuals ~ x + y, data = MPdf_vg, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 20000, width = 1000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)




##--------------------------------------------------------------------------
# Producing a variogram for each study individually
##--------------------------------------------------------------------------

# --------------------------
# STUDY 1 
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "1",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))

# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 11000, width = 10)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 10000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 9000, width = 100)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.1 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 9000, width = 100)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# --------------------------
# STUDY 2
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "2",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))

# Variogram
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 10000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.2 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 10000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# --------------------------
# STUDY 3
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "3",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))

# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 100000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 100000, width = 50)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 100000, width = 3000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.3 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 100000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# --------------------------
# STUDY 4
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "4",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 
# TOO LITTLE DATA / NOT INCLUDING

# --------------------------
# STUDY 5
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "5",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, width = 1000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.5 <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# --------------------------
# STUDY 6
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "6",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, width = 1)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.6 <- variogram(residuals ~ 1, data  = vg.data, width = 1)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# --------------------------
# STUDY 7
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "7",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 50000, width = 100)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 50000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.7 <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# --------------------------
# STUDY 8
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "8",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 200000, width = 10)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 200000, width = 1)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 
# TOO LITTLE DATA / NOT INCLUDING 

# --------------------------
# STUDY 9
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "9",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, width = 10000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, width = 10000, cutoff = 350000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.9 <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# --------------------------
# STUDY 10
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "10",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 50, width = 1)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.10 <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# --------------------------
# STUDY 11
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "11",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, width = 1)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 
# TOO LITTLE DATA / NOT INCLUDING

# --------------------------
# STUDY 12
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "12",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 70000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 20000, width = 100)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 
# TOO LITTLE DATA / NOT INCLUDING

# --------------------------
# STUDY 13
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "13",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 150000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 150000, width = 1000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)  

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 200000, width = 50)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.13 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 150000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# --------------------------
# STUDY 14
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "14",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 1500, width = 10)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.14 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 1500, width = 10)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# --------------------------
# STUDY 15
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "15",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 4000, width = 5)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 2000, width = 5)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.15 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 2000, width = 5)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)

# --------------------------
# STUDY 16
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "16",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 1000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 3000, width = 5)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.16 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 1000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)

# --------------------------
# STUDY 17
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "17",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 10000, width = 10)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.17 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 10000, width = 10)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# --------------------------
# STUDY 18
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "18",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 5000, width = 1)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 
# TOO LITTLE DATA / NOT INCLUDING 

# --------------------------
# STUDY 19
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "19",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 20000, width = 10)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 3000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 3000, width = 100)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.19 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 3000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# --------------------------
# STUDY 20
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "20",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 15000, width = 1)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 3000, width = 10)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 
# TOO LITTLE DATA / NOT INLCUDING

# --------------------------
# STUDY 21
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "21",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 10000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 10000, width = 500)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 15000, width = 50)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.21 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 10000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# --------------------------
# STUDY 22
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "22",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 
# TOO LITTLE DATA / NOT INLCUDING

# --------------------------
# STUDY 23
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "23",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 500, width = 20)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 300, width = 20)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.23 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 300, width = 20)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# --------------------------
# STUDY 24
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "24",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 20000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 20000, width = 500)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.24 <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)

# --------------------------
# STUDY 25
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "25",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 100000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 100000, width = 3000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.25 <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# --------------------------
# STUDY 26
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "26",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 5000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 5000, width = 700)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 9000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.26 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 9000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# --------------------------
# STUDY 27
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "27",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 300000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 300000, width = 1000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 100000, width = 3000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.27 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 300000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# --------------------------
# STUDY 28
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "28",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 9000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 9000, width = 100)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 9000, width = 10)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.28 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 9000, width = 100)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# --------------------------
# STUDY 29
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "29",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 200)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 
# TOO LITTE DATA / NOT INCLUDING

# --------------------------
# STUDY 30
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "30",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 70000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 80000, width = 10)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.30 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 70000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# --------------------------
# STUDY 31
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "31",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 50000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 40000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 40000, width = 2500)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.31 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 40000, width = 2500)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# --------------------------
# STUDY 32
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "32",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, width = 500)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 50000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 40000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 40000, width = 500)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 50000, width = 400)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.32 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 40000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# --------------------------
# STUDY 33
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "33",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 400000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 1500, width = 5)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 400000, width = 500)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.33 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 400000, width = 500)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# --------------------------
# STUDY 34
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "34",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 1000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 900, width = 10)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 250, width = 10)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)
# TOO LITTLE DATA / NOT INCLUDING

# --------------------------
# STUDY 35
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "35",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 50000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 50000, width = 5000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 50000, width = 4500)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.35 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 50000, width = 4500)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)

# --------------------------
# STUDY 36
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "36",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 300)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 300, width = 1)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 150, width = 1)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 
# TOO LITTLE DATA / NOT INCLUDING

# --------------------------
# STUDY 37
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "37",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 100)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 
# TOO LITTLE DATA / NOT INLCUDING

# --------------------------
# STUDY 38
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "38",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 500)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 
# TOO LITTLE DATA / NOT INLCUDING

# --------------------------
# STUDY 39
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "39",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 30000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 30000, width = 500)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 40000, width = 500)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 30000, width = 50)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.39 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 30000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# --------------------------
# STUDY 40
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "40",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 25000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 500000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 500000, width = 900)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.40 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 500000, width = 900)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# --------------------------
# STUDY 41
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "41",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 
# TOO LITTLE DATA / NOT INCLUDING 

# --------------------------
# STUDY 42
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "42",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 500000, width = 50)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 
# TOO LITTLE DATA / NOT INCLUDING

# --------------------------
# STUDY 43
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "43",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 1000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 1700)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 900, width = 10)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.43 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 900, width = 10)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# --------------------------
# STUDY 44
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "44",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 10000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 9500)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 
# TOO LITTLE DATA / NOT INCLUDING

# --------------------------
# STUDY 45
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "45",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 60000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 60000, width = 100)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 50000, width = 500)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.45 <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# --------------------------
# STUDY 46
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "46",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 3000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 1000, width = 10)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 500, width = 1)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 800, width = 5)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.46 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 800, width = 5)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# --------------------------
# STUDY 47
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "47",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 450, width = 1)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 800, width = 1)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 500, width = 1)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.47 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 500, width = 1)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# --------------------------
# STUDY 48
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "48",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 500)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 100, width = 1)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 
# TOO LITTLE DATA / NOT INCLUDING

# --------------------------
# STUDY 49
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "49",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 70000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 70000, width = 50)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 
# TOO LITTLE DATA / NOT INCLUDING

# --------------------------
# STUDY 50
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "50",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 30000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 800000, width = 500)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 
# TOO LITTLE DATA / NOT INCLUDING

# --------------------------
# STUDY 51
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "51",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 500)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 500, width = 10)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 
# TOO LITTLE DATA / NOT INCLUDING

# --------------------------
# STUDY 52
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "52",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 150000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 150000, width = 500)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 150000, width = 1000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 50000, width = 10)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 60000, width = 10)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 70000, width = 10)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.52 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 150000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# --------------------------
# STUDY 53
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "53",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 300)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 200, width = 10)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.53 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 200, width = 10)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)

# --------------------------
# STUDY 54
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "54",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 20)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 20, width = 1)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)
# TOO LITTLE DATA / NOT INCLUDING

# --------------------------
# STUDY 55
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "55",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 500)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 500, width = 1)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)
# TOO LITTLE DATA / NOT INCLUDING

# --------------------------
# STUDY 56
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "56",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 20)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 20, width = 1)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 15, width = 1)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 20, width = 1)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)

# Best variogram:
vg.56 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 20, width = 1)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)

# --------------------------
# STUDY 57
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "57",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 1000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 1300)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 1200, width = 1)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 12000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 12000, width = 100)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)

# Best variogram:
vg.57 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 12000, width = 100)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)

# --------------------------
# STUDY 58
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "58",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 200000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 250000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 250000, width = 100)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)

# Best variogram:
vg.58 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 250000, width = 100)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)

# --------------------------
# STUDY 59
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "59",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 4000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 4000, width = 100)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 2000, width = 100)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)
# TOO LITTLE DATA / NOT INCLUDING

# --------------------------
# STUDY 60
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "60",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 5000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 5000, width = 1)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 10000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)
# TOO LITTLE DATA / NOT INCLUDING

# --------------------------
# STUDY 61
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "61",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 80000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# Best variogram:
vg.61 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 80000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

# --------------------------
# STUDY 62
# --------------------------

# Subset data
MPdf_study <- MPdf[MPdf$study == "62",]

# Aggregate residuals by location
data <- aggregate(residuals ~ x + y, data = MPdf_study, FUN = 
                    "median")

# Convert to an sf object
vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs=crs(HFI)))


# Variogram 
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 800)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 800, width = 50)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 500, width = 50)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)

# Best variogram:
vg.62 <- variogram(residuals ~ 1, data  = vg.data, cutoff = 800)
plot(mp.vg)
mp.vg.fit <- fit.variogram(vg.62, vgm("Sph", nugget = TRUE))
plot(vg.62, mp.vg.fit) 


