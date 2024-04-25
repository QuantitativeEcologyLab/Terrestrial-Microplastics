
library(geosphere)

RA_data <- read.csv("C:/Users/lmills96/OneDrive - UBC/MSc Thesis Info/Global Analysis/Terrestrial-Microplastics/RA/RA_Coords.csv")
RA_data <- RA_data[,-c(1,4:10)]
RA_data <- RA_data[complete.cases(RA_data),]

RA_coords <- distGeo(RA_data)
