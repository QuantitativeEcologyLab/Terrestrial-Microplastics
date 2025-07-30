
# Removing all NAs from MPdf to get model and predictions

# Remove all NAs
MPdf_subset <- na.omit(MPdf)

# See which rows have NAs
MPdf_NAs <- MPdf[!complete.cases(MPdf),]

# Get model based off dataset with no NAs (for predictions)
model_HFI <- 
              gam(HFI ~
              # global terms
              s(Items_kg) + 
              s(sqrt(max_depth_cm), k = 5) +
              s(elevation_m, k = 6) + 
              # study-level terms
              s(study, bs = 're'), #random int - it doesn't really have random slopes 
            family = tw(link = 'log'),
            data = MPdf_subset,
            method = "REML",
            na.action = na.fail)

# Create dataset with coordinates where there were NAs for each raster separately
MPdf_NAs_HFI <- MPdf[!complete.cases(MPdf$HFI),]

# Note: need at least elevation or soil type to estimate HFI - if all 3 rasters
# were NA, you'd need to estimate one prior to predicting HFI
MPdf_NAs_HFI$HFI_pred <- predict(model_HFI,
                             newdata = MPdf_NAs_HFI)

# Add predicted HFI values back into MPdf
MPdf_NAs_HFI <- MPdf_NAs_HFI[,-7]
names(MPdf_NAs_HFI)[11] <- "HFI"

na_rows <- which(is.na(MPdf$HFI))
MPdf$HFI[na_rows] <- MPdf_NAs_HFI$HFI



# Do the same for soil type
model_soil <- 
  gam(soil_type ~
        # global terms
        s(Items_kg) + 
        s(sqrt(max_depth_cm), k = 5) +
        s(elevation_m, k = 6) + 
        s(HFI, k = 10, bs = "ad") + 
        # study-level terms
        s(study, bs = 're'), #random int - it doesn't really have random slopes 
      family = tw(link = 'log'),
      data = MPdf_subset,
      method = "REML",
      na.action = na.fail)

# Create dataset with coordinates where there were NAs for each raster separately
MPdf_NAs_soil <- MPdf[!complete.cases(MPdf$soil_type),]

# Note: need at least elevation or soil type to estimate HFI - if all 3 rasters
# were NA, you'd need to estimate one prior to predicting HFI
MPdf_NAs_soil$soil_pred <- predict(model_HFI,
                                 newdata = MPdf_NAs_soil)

# Add predicted soil values back into MPdf
MPdf_NAs_soil <- MPdf_NAs_soil[,-9]
names(MPdf_NAs_soil)[11] <- "soil_type"

na_rows <- which(is.na(MPdf$soil_type))
MPdf$soil_type[na_rows] <- MPdf_NAs_soil$soil_type


# Save final dataset
write.csv(MPdf, file = "./Data/MPdf.csv", row.names = FALSE)



# Note: this prediction code differs from 05_MP_predictions.R as that script
# is using average values to predict overall trends. Here we are interested
# in predicting values based on variables we already have in the dataset