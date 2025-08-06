
# Load required packages 
library(sf)
library(sp)
library(terra)
library(gstat)

#--------------------------------------------------------------------------
# Testing the variogram for each study  
#--------------------------------------------------------------------------

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

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 9000, width = 10)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 
# NO GOOD PLATEAU AT SILL?

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
# LOOKS OK MAYBE? NOT SURE - INTITIAL ONE LOOKS OK BUT THEN NOT SURE IF OTHERS LOOK BETTER OR NOT

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
# TOO MANY POINTS ABOVE LINE THAT IT DOESN'T PLATEAU NICELY?

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
# ONLY ONE COORDINATE FOR ALL 9 VALUES FOR THIS STUDY SO CAN'T COMPUTE A VARIOGRAM?

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
# LOOKS OK MAYBE?

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
# DOESN'T LOOK GREAT

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
# TOO MANY POINTS ABOVE LINE THAT IT DOESN'T PLATEAU NICELY?

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
# DOESN'T LOOK VERY GOOD?

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
# INITIAL ONE LOOKS OK BUT THEN NOT VERY GOOD IN OTHER ONES?

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
# NOT A VERY OBVIOUS TREND?

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
# ONLY 3 DIFFERENT COORDS SO NOT MUCH TO WORK WITH HERE?

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
# DOESN'T LOOK GREAT?

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
# DOESN'T LOOK GREAT? NOT A VERY CLEAR TREND?

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
# DOESN'T LOOK GREAT?

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
# DOESN'T LOOK GREAT?

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
# MAYBE? TOP RIGHT CORNER THO?

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
# DOESN'T LOOK GREAT?

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
# DOESN'T LOOK GREAT --- FEW DATA POINTS

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
# DOESN'T LOOK GREAT? NOT REALLY ANY CLEAR TREND ABOVE PLATEAU

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
# DOESN'T LOOK GREAT?

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
# DOESN'T LOOK LIKE ANY CLEAR TREND? --- MAYBE SECOND ONE?

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
# ONLY ONE COORDINATE SO CAN'T PRODUCE VARIOGRAM

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
# DOESN'T LOOK GREAT - NO CLEAR TRENDS - SECOND ONE MAYBE BEST?

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
# DOESN'T LOOK GREAT BUT MAYBE LAST ONE? DESPITE POINTS NEAR TOP?

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
# DOESN'T LOOK GREAT - NO CLEAR TRENDS - LAST ONE MAYBE?

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
# DOESN'T LOOK GREAT - BUT MAYBE? UNLIKELY WITH THE POINT AT TOP RIGHT CORNER BUT MAYBE 3RD ONE?

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
# DOESN'T LOOK LIKE THERE IS ANY TREND?

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
# DOESN'T LOOK LIKE THERE IS ANY TREND?

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
# ONLY 3 DATA POINTS - NOT USEFUL

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
# DOESN'T LOOK LIKE THERE IS ANY TREND?

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
# MAYBE?

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
# MAYBE?

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

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 400000, width = 500)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 
# NO CLEAR TREND?

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
# NO CLEAR TREND?

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
# NO CLEAR TREND WITH POINT AT TOP RIGHT?

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
# NO CLEAR TREND WITH POINT AT TOP RIGHT?

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
# DOESN'T LOOK GREAT...

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
# DOESN'T LOOK GREAT...

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
# MAYBE?

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
# MAYBE? THINKING NO.....

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
# ONLY ONE COORDINATE FOR ALL 3 POINTS - CAN'T PRODUCE VARIOGRAM

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
# THINKING NO.....

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
# MAYBE? NOT SURE ON THIS ONE.... 

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
# THINKING NO.... 

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
# UNSURE BUT THINKING NO.... 

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

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 800, width = 5)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 
# MAYBE BUT THINKING NO.... 

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

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 100)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 500, width = 1)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 
# NOT LOOKING GOOD ESPECIALLY BASED ON SO LITTLE DATA FOR THIS STUDY.... 

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
# NOT REALLY ANY KIND OF TREND.... 

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
# THINKING NO.... 

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
# NOT LOOKING GOOD ESPECIALLY BASED ON SO LITTLE DATA FOR THIS STUDY.... 

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
# NOT MUCH OF A TREND...

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
# MAYBE??? DOESN'T REALLY LOOK GREAT.... 

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

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 300, width = 10)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 200, width = 10)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit) 
# NOT SURE...


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
# DOESN'T LOOK GREAT...

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
# MAYBE...

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
# DOESN'T LOOK GREAT...

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

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 12000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 12000, width = 100)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)
# MAYBE?? BUT A LOT OF POINTS ABOVE LINE

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
# LOOKS OK MAYBE?

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
# MAYBE?? 

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

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 5000, width = 10)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)

mp.vg <- variogram(residuals ~ 1, data  = vg.data, cutoff = 10000)
plot(mp.vg)
mp.vg.fit <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE))
plot(mp.vg, mp.vg.fit)
# MAYBE?? DOESN'T LOOK GREAT...

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
# LOOKS DECENT? 

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
# MAYBE?
