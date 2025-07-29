
# General overview:
ggplot(data = MPdf, aes(x = max_depth_cm, y = Items_kg, col = elevation_m)) +
  geom_point() 

ggplot(data = MPdf, aes(x = HFI, y = log10(Items_kg), col = elevation_m)) +
  geom_point() 

ggplot(data = MPdf, aes(x = log10(elevation_m), y = log10(Items_kg), col = HFI)) +
  geom_point() 



# Specifying the min/max size of the weights
size_limits <- c(0.75, 2.5)

# All data
ggplot(data = MPdf, 
       aes(y = log10(Items_kg) +1, x = HFI, size = Weights)) +
  geom_point(aes(col = Study), alpha = 0.8, pch = 16) +
  scale_size_continuous(range = size_limits) +
  ggtitle("A)") +
  geom_smooth(method = "gam",
              color = "grey17",
              formula = y ~ s(x),
              method.args = list(family = tw),
              se = FALSE)

# Removing study row 663 from initial dataframe to confirm outlier
MPdf_prac <- MPdf[-(633), ]

ggplot(data = MPdf_prac, 
       aes(y = log10(Items_kg) +1, x = HFI, size = Weights)) +
  geom_point(aes(col = Study), alpha = 0.8, pch = 16) +
  scale_size_continuous(range = size_limits) +
  ggtitle("A)") +
  geom_smooth(method = "gam",
              color = "grey17",
              formula = y ~ s(x),
              method.args = list(family = tw),
              se = FALSE)


sortdf <- MPdf[-c(1:945),-c(1,3,4,5,6,7,10,11,12,13,14,15)]

sortdf$numbers <- rep(1:67)
as.factor(sortdf$numbers)


ggplot(data = sortdf, 
       aes(y = Items_kg +1, x = HFI)) +
  geom_point(aes(col = as.factor(numbers)), alpha = 0.8, pch = 16) +
  scale_size_continuous(range = size_limits) +
  geom_smooth(method = "gam",
              color = "grey17",
              formula = y ~ s(x),
              method.args = list(family = tw),
              se = FALSE)

sortdf <- sortdf[order(-sortdf$HFI),]


# Removing possible outlier from study #24 
Mpdf_prac1 <- MPdf
MPdf_prac1 <- MPdf_prac1[-c(1010), ]

ggplot(data = MPdf_prac1, 
       aes(y = log10(Items_kg) +1, x = HFI, size = Weights)) +
  geom_point(aes(col = Study), alpha = 0.8, pch = 16) +
  scale_size_continuous(range = size_limits) +
  ggtitle("A)") +
  geom_smooth(method = "gam",
              color = "grey17",
              formula = y ~ s(x),
              method.args = list(family = tw),
              se = FALSE)

# Model using the revised dataset 
model_prac <- gam(Items_kg ~
                # global terms
                s(HFI, k = 12, bs = "tp") + 
                s(Max_Depth_cm) +
                s(Elevation_km) +
                ti(Elevation_km, HFI, k = 5, bs = "tp") + #don't need study in here b/c interaction term
                ti(Max_Depth_cm, HFI, k = 5, bs = "tp") +
                # study-level terms
                s(Study, bs = 're'), #random int - it doesn't really have random slopes 
              weights = Weights,
              family = tw(link = 'log'),
              data = MPdf_prac1,
              method = "REML",
              na.action = "na.fail")

plot(model_prac, scale = 0)
plot(model1, scale = 0)


